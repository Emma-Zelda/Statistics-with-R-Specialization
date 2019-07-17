##########################################################
##                 PumpedVol_Cal_Methods.R              ##
## Purpose: compare two ways for evaluating PV relation ##
##          1. 500 psi to shut-in pressure              ##
##          2. 1000 psi below shut-in pressure to       ##
##              shut-in pressure                        ##
## Edited by Emma Teng                                  ##
## Last Modified on: 07/12/2019                         ##
##########################################################
rm(list=ls())
library('RODBC')
library('ggplot2')
library('dplyr')
library('parallel')
library('doParallel')
library('reshape')
library('splines2')
library('fda')
library(IPT)
library(gridExtra)

##############################
##    Functions               ##
##############################
ClipDepres_refined <- function(pressure, drop = 2) {
  ## only check the last 10% of pressure
  startpoint <- 0.8*length(pressure)
  endpoint <- length(pressure)
  for (i in startpoint:endpoint) {
    
    if (abs(pressure[i] - pressure[i-1]) >= drop) {
      return(pressure[1:(i-1)])
    }
  }
  return(pressure)
}

PressureExtract_maxpres_Clip <- function(testID, attemptID) {
  
  DSN = "SVResearchDatabase"
  USERNAME = "SVRDBU"
  PASSWORD = "SVRDBU"
  conn = RODBC::odbcConnect(DSN, USERNAME, PASSWORD)
  
  startTimeQuery = paste("SELECT HpBegin FROM TestAttempt WHERE TestID='", testID, 
                         "' AND AttemptID='", attemptID, "'", sep="")
  endTimeQuery = paste("SELECT HpEnd FROM TestAttempt WHERE TestID='", 
                       testID, 
                       "' AND AttemptID='", attemptID, "'", sep="")
  
  startTime = RODBC::sqlQuery(conn, startTimeQuery, as.is=TRUE)[,1]
  endTime = RODBC::sqlQuery(conn, endTimeQuery, as.is=TRUE)[,1]
  
  pressureQuery = paste("SELECT P FROM TestData WHERE TestID='", testID, 
                        "' AND AttemptID='", attemptID,
                        "' AND T BETWEEN '", startTime, 
                        "' AND '", endTime, "'", sep="")
  pressure = RODBC::sqlQuery(conn, pressureQuery)[,1]
  
  RODBC::odbcClose(conn)
  
  max_p <- max(pressure)
  max_p_pos <- which(pressure == max_p)[1]
  
  pressure <- pressure[max_p_pos:length(pressure)]
  
  pressure = ClipDepres_refined(pressure)
  
  return(pressure)
}

TargetPressureExtract <- function(testID, attemptID) {
  DSN = "SVResearchDatabase"
  USERNAME = "SVRDBU"
  PASSWORD = "SVRDBU"
  conn = RODBC::odbcConnect(DSN, USERNAME, PASSWORD)
  
  query = paste("SELECT TargetPressure FROM TestAttempt WHERE TestID='", testID, 
                "' AND AttemptID='", attemptID, "'", sep="")
  TP = sqlQuery(conn, query, as.is=TRUE)[,1]
  
  RODBC::odbcClose(conn)
  
  return(as.numeric(TP))
  
}

ShutinPressureExtract <- function(testID, attemptID) {
  
  DSN = "SVResearchDatabase"
  USERNAME = "SVRDBU"
  PASSWORD = "SVRDBU"
  
  conn = RODBC::odbcConnect(DSN, USERNAME, PASSWORD)
  
  ShutinTimeQuery = paste("SELECT HpBegin FROM TestAttempt WHERE TestID='", testID, 
                          "' AND AttemptID='", attemptID, "'", sep="")
  
  ShutinTime = RODBC::sqlQuery(conn, ShutinTimeQuery, as.is=TRUE)[,1]
  
  pressureQuery = paste("SELECT P FROM TestData WHERE TestID='", testID, 
                        "' AND AttemptID='", attemptID,
                        "' AND T <= '", ShutinTime,"'", 
                        sep="")
  pressurization = RODBC::sqlQuery(conn, pressureQuery)
  RODBC::odbcClose(conn)
  
  shutin_p <- pressurization[nrow(pressurization),]
  
  
  
  return(shutin_p)
}

VolumeExtract <- function(testID, attemptID) {
  
  DSN = "SVResearchDatabase"
  USERNAME = "SVRDBU"
  PASSWORD = "SVRDBU"
  conn = RODBC::odbcConnect(DSN, USERNAME, PASSWORD)
  
  
  VolumeQuery <- paste("select P, R from testdata td
                       inner join TestAttempt ta
                       on td.TestID = ta.TestID and td.AttemptID = ta.AttemptID
                       where R >0 and td.T <= ta.HpBegin and td.TestID='", testID, 
                       "' AND td.AttemptID='", attemptID,
                       "'", sep="")
  PV_all <- RODBC::sqlQuery(conn, VolumeQuery)
  
  RODBC::odbcClose(conn)
  
  PV_pressurization <- subset(PV_all, P >= 500 & R > 0) ## R: bbl/min
  
  ## caculate accumulated Volume
  if (all(is.na(PV_pressurization$R))){
    return(NA)
  } else {
    PV_pressurization$V <- NA
    
    PV_pressurization$V[1] <- PV_pressurization$R[1]/60 ## bbl
    
    for(i in 2:nrow(PV_pressurization)) {
      PV_pressurization$V[i] <- PV_pressurization$V[i-1] + PV_pressurization$R[i]/60
    }
    
    delta_p <- PV_pressurization$P[nrow(PV_pressurization)] -
      PV_pressurization$P[1]  ## psi
    
    delta_v <- PV_pressurization$V[nrow(PV_pressurization)] -
      PV_pressurization$V[1] ## bbl
    
    return(delta_p/delta_v)
  }
}

VolumeExtract_1000below <- function(testID, attemptID, shutinPre) {
  
  DSN = "SVResearchDatabase"
  USERNAME = "SVRDBU"
  PASSWORD = "SVRDBU"
  conn = RODBC::odbcConnect(DSN, USERNAME, PASSWORD)
  
  
  VolumeQuery <- paste("select P, R from testdata td
                       inner join TestAttempt ta
                       on td.TestID = ta.TestID and td.AttemptID = ta.AttemptID
                       where R >0 and td.T <= ta.HpBegin and td.TestID='", testID, 
                       "' AND td.AttemptID='", attemptID,
                       "'", sep="")
  PV_all <- RODBC::sqlQuery(conn, VolumeQuery)
  
  RODBC::odbcClose(conn)
  
  PV_pressurization <- subset(PV_all, P >= shutinPre-1000 & R > 0) ## R: bbl/min
  
  ## caculate accumulated Volume
  if (all(is.na(PV_pressurization$R))){
    return(NA)
  } else {
    PV_pressurization$V <- NA
    
    PV_pressurization$V[1] <- PV_pressurization$R[1]/60 ## bbl
    
    for(i in 2:nrow(PV_pressurization)) {
      PV_pressurization$V[i] <- PV_pressurization$V[i-1] + PV_pressurization$R[i]/60
    }

    delta_p <- PV_pressurization$P[nrow(PV_pressurization)] -
      PV_pressurization$P[1]  ## psi
    
    delta_v <- PV_pressurization$V[nrow(PV_pressurization)] -
      PV_pressurization$V[1] ## bbl
    
    return(delta_p/delta_v)
  }
}

##############################
##    Testbed               ##
##############################
## all benchamrks
mudBm = read.csv("../AllMudBmTests.csv",
               header = TRUE)
mudBm = mudBm[,-1]
mudBm$IsBm = 1

swBm = read.csv("../AllSWBmTests.csv",
                 header = TRUE)
swBm = swBm[,-1]
swBm$IsBm = 1

##############################
###  Add shut-in Pressure  ###
##############################
no_cores <- detectCores() - 1

# Initiate cluster
cl <- makeCluster(no_cores)
registerDoParallel(cl)

ptm <- proc.time()

x <- foreach(i = 1:nrow(mudBm), .combine =  'rbind',
             .packages = c("RODBC")) %dopar% {
               testid <- mudBm$testID[i]
               attemptid <- mudBm$attemptID[i]
               
               shutin_p <- tryCatch(ShutinPressureExtract(testid, attemptid),
                                    error = function(e) NA )
               return(c(i, shutin_p))
             }

# Stop the clock
proc.time() - ptm
## shut down cluster
stopCluster(cl)

mudBm$ShutinPressure <- x[,2]

no_cores <- detectCores() - 1

# Initiate cluster
cl <- makeCluster(no_cores)
registerDoParallel(cl)

ptm <- proc.time()

x <- foreach(i = 1:nrow(swBm), .combine =  'rbind',
             .packages = c("RODBC")) %dopar% {
               testid <- swBm$testID[i]
               attemptid <- swBm$attemptID[i]
               
               shutin_p <- tryCatch(ShutinPressureExtract(testid, attemptid),
                                    error = function(e) NA )
               return(c(i, shutin_p))
             }

# Stop the clock
proc.time() - ptm
## shut down cluster
stopCluster(cl)

swBm$ShutinPressure <- x[,2]


##############################
##      Add Pumped Volume   ##
## 500 psi above            ##
##############################
# Calculate the number of cores
no_cores <- detectCores() - 1

# Initiate cluster
cl <- makeCluster(no_cores)
registerDoParallel(cl)

ptm <- proc.time()

x <- foreach(i = 1:nrow(mudBm), .combine =  'rbind',
             .packages = c("RODBC")) %dopar% {
               
               testid <- mudBm$testID[i]
               attemptid <- mudBm$attemptID[i]
               
               slope <- tryCatch(VolumeExtract(testid, attemptid),
                                        error = function(e) NA )
               return(c(i, slope))
             }

# Stop the clock
proc.time() - ptm
## shut down cluster
stopCluster(cl)

mudBm$slope_500above <- x[,2]

# Calculate the number of cores
no_cores <- detectCores() - 1

# Initiate cluster
cl <- makeCluster(no_cores)
registerDoParallel(cl)

ptm <- proc.time()

x <- foreach(i = 1:nrow(swBm), .combine =  'rbind',
             .packages = c("RODBC")) %dopar% {
               
               testid <- swBm$testID[i]
               attemptid <- swBm$attemptID[i]
               
               slope <- tryCatch(VolumeExtract(testid, attemptid),
                                 error = function(e) NA )
               return(c(i, slope))
             }

# Stop the clock
proc.time() - ptm
## shut down cluster
stopCluster(cl)

swBm$slope_500above <- x[,2]

##############################
##      Add Pumped Volume   ##
## 1000 psi below           ##
##############################
# Calculate the number of cores
no_cores <- detectCores() - 1

# Initiate cluster
cl <- makeCluster(no_cores)
registerDoParallel(cl)

ptm <- proc.time()

x <- foreach(i = 1:nrow(mudBm), .combine =  'rbind',
             .packages = c("RODBC")) %dopar% {
               
               testid <- mudBm$testID[i]
               attemptid <- mudBm$attemptID[i]
               ShutinPressure <- mudBm$ShutinPressure[i]
               
               slope <- tryCatch(VolumeExtract_1000below(testid, attemptid,
                                                                ShutinPressure),
                                        error = function(e) NA )
               return(c(i, slope))
             }

# Stop the clock
proc.time() - ptm
## shut down cluster
stopCluster(cl)

mudBm$slope_1000below <- x[,2]

# Calculate the number of cores
no_cores <- detectCores() - 1

# Initiate cluster
cl <- makeCluster(no_cores)
registerDoParallel(cl)

ptm <- proc.time()

x <- foreach(i = 1:nrow(swBm), .combine =  'rbind',
             .packages = c("RODBC")) %dopar% {
               
               testid <- swBm$testID[i]
               attemptid <- swBm$attemptID[i]
               ShutinPressure <- swBm$ShutinPressure[i]
               
               slope <- tryCatch(VolumeExtract_1000below(testid, attemptid,
                                                         ShutinPressure),
                                 error = function(e) NA )
               return(c(i, slope))
             }

# Stop the clock
proc.time() - ptm
## shut down cluster
stopCluster(cl)

swBm$slope_1000below <- x[,2]


####################################
##         Analyzing results      ##
####################################
################
##  mud       ##
################
mudBm = mudBm %>%
  filter(!is.na(ShutinPressure),
         !is.na(slope_500above),
         !is.na(slope_1000below),
         ShutinPressure >= 2000) %>%
  select(testID, attemptID, I_FluidWeight,
         System, ShutinPressure, slope_500above,
         slope_1000below) %>%
  mutate(slope_diff = (slope_1000below - slope_500above)/slope_500above*100) %>%
  mutate(Sys = case_when(grepl('manifold', 
                               System, ignore.case = TRUE) ~ 'Manifold',
                         grepl(paste(c('single', 'one'), collapse = "|"), 
                               System, ignore.case = TRUE) ~ 'Single',
                         grepl(paste(c('double', 'two'), collapse = "|"), 
                               System, ignore.case = TRUE) ~ 'Double',
                         TRUE ~ 'Other'))

mudBm %>%
  ggplot(aes(x = factor(Sys,
                        levels = c('Manifold','Single','Double','Other')),
             y = slope_diff, group = factor(Sys,
                                            levels = c('Manifold',
                                                       'Single',
                                                       'Double',
                                                       'Other')))) +
  geom_boxplot(aes(fill = factor(Sys,
                                  levels = c('Manifold',
                                             'Single',
                                             'Double',
                                             'Other')))) +
  ylim(c(-100, 100)) +
  ylab('slope difference (%)') +
  xlab('System') +
  scale_fill_discrete(name = 'System')

mudBm %>%
  group_by(factor(Sys,
                  levels = c('Manifold','Single','Double','Other'))) %>%
  summarise(count = n(),
            median_diff = floor(median(slope_diff)),
            iqr = floor(IQR(slope_diff)),
            q1 = floor(quantile(slope_diff, 0.25)),
            q3 = floor(quantile(slope_diff, 0.75)),
            min_diff = floor(min(slope_diff)),
            max_diff = floor(max(slope_diff)))

temp = mudBm %>%
  group_by(Sys) %>%
  summarise(median_diff = floor(median(slope_diff)),
         iqr = floor(IQR(slope_diff))) %>%
  mutate(lower = median_diff - 1.5*iqr,
         upper = median_diff + 1.5*iqr)

temp = merge(temp, mudBm, by = 'Sys',
             all.x = TRUE)

temp %>%
  group_by(factor(Sys,
                  levels = c('Manifold','Single','Double','Other'))) %>%
  mutate(is_outlier = slope_diff < lower | slope_diff > upper) %>%
  summarise(count = sum(is_outlier == TRUE),
            pct = sum(is_outlier == TRUE)/n()*100)

#################################
##     Manifold                ##
#################################
manifold = mudBm %>%
  filter(Sys == 'Manifold')

testID = 14664
attemptID = '12'
ShutinPressure = 5659

DSN = "SVResearchDatabase"
USERNAME = "SVRDBU"
PASSWORD = "SVRDBU"
conn = RODBC::odbcConnect(DSN, USERNAME, PASSWORD)
VolumeQuery <- paste("select P, R from testdata td
                     inner join TestAttempt ta
                     on td.TestID = ta.TestID and td.AttemptID = ta.AttemptID
                     where R >0 and td.T <= ta.HpBegin and td.TestID='", testID, 
                     "' AND td.AttemptID='", attemptID,
                     "'", sep="")
PV_all <- RODBC::sqlQuery(conn, VolumeQuery)
RODBC::odbcClose(conn)

PV_pressurization <- subset(PV_all) ## R: bbl/min
PV_pressurization$V <- NA
PV_pressurization$V[1] <- PV_pressurization$R[1]/60 ## bbl

for(i in 2:nrow(PV_pressurization)) {
  PV_pressurization$V[i] <- PV_pressurization$V[i-1] + PV_pressurization$R[i]/60
}

PV_pressurization %>%
  ggplot(aes(x = V, y = P)) +
  geom_point() +
  geom_hline(yintercept = 500, col = 'red', 
             size = 0.6) +
  geom_hline(yintercept = ShutinPressure - 1000, col = 'blue',
             size = 0.6) +
  xlab("pumped volume (bbl)") +
  ylab("pressure (psi)")

################
##  sw       ##
################
swBm = swBm %>%
  filter(!is.na(ShutinPressure),
         !is.na(slope_500above),
         !is.na(slope_1000below),
         ShutinPressure >= 2000) %>%
  select(testID, attemptID, I_FluidWeight,
         System, ShutinPressure, slope_500above,
         slope_1000below) %>%
  mutate(slope_diff = (slope_1000below - slope_500above)/slope_500above*100) %>%
  mutate(Sys = case_when(grepl('manifold', 
                               System, ignore.case = TRUE) ~ 'Manifold',
                         grepl(paste(c('single', 'one'), collapse = "|"), 
                               System, ignore.case = TRUE) ~ 'Single',
                         grepl(paste(c('double', 'two'), collapse = "|"), 
                               System, ignore.case = TRUE) ~ 'Double',
                         TRUE ~ 'Other'))

swBm %>%
  ggplot(aes(x = factor(Sys,
                        levels = c('Manifold','Single','Double','Other')),
             y = slope_diff, group = factor(Sys,
                                            levels = c('Manifold',
                                                       'Single',
                                                       'Double',
                                                       'Other')))) +
  geom_boxplot(aes(fill = factor(Sys,
                                 levels = c('Manifold',
                                            'Single',
                                            'Double',
                                            'Other')))) +
  ylim(c(-100, 100)) +
  ylab('slope difference (%)') +
  xlab('System') +
  scale_fill_discrete(name = 'System')

swBm %>%
  group_by(factor(Sys,
                  levels = c('Manifold','Single','Double','Other'))) %>%
  summarise(count = n(),
            median_diff = floor(median(slope_diff)),
            iqr = floor(IQR(slope_diff)),
            q1 = floor(quantile(slope_diff, 0.25)),
            q3 = floor(quantile(slope_diff, 0.75)),
            min_diff = floor(min(slope_diff)),
            max_diff = floor(max(slope_diff)))

temp = swBm %>%
  group_by(Sys) %>%
  summarise(median_diff = floor(median(slope_diff)),
            iqr = floor(IQR(slope_diff))) %>%
  mutate(lower = median_diff - 1.5*iqr,
         upper = median_diff + 1.5*iqr)

temp = merge(temp, swBm, by = 'Sys',
             all.x = TRUE)

temp %>%
  group_by(factor(Sys,
                  levels = c('Manifold','Single','Double','Other'))) %>%
  mutate(is_outlier = slope_diff < lower | slope_diff > upper) %>%
  summarise(count = sum(is_outlier == TRUE),
            pct = sum(is_outlier == TRUE)/n()*100)


