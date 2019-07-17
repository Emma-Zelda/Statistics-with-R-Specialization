##############################
## simple linear regression
##############################


library(statsr)
library(dplyr)
library(ggplot2)

#########
# at_bats
#########
data(mlb11)
colnames(mlb11)

mlb11 %>%
  summarise(cor(runs, at_bats))

plot_ss(x = at_bats, y = runs, data = mlb11)

plot_ss(x = at_bats, y = runs, data = mlb11, showSquares = TRUE)

m1 <- lm(runs ~ at_bats, data = mlb11)
summary(m1)

m2 <- lm(runs ~ homeruns, data = mlb11)
summary(m2)

# plot model fit
ggplot(data = mlb11, aes(x = at_bats, y = runs)) +
  geom_point() +
  stat_smooth(method = "lm", se = FALSE)

# check residuals for at_bats = 5579
mlb11 %>%
  filter(at_bats == 5579) %>%
  select(runs)

predict(m1, data.frame(at_bats = 5579)) - 713

# check residuals
ggplot(data = m1, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  xlab("Fitted values") +
  ylab("Residuals")

# check normality
ggplot(data = m1, aes(x = .resid)) +
  geom_histogram(binwidth = 25) +
  xlab("Residuals")

# qq-plot for residuals
ggplot(data = m1, aes(sample = .resid)) +
  stat_qq()

#########
# hits
#########
m2 <- lm(runs ~ hits, data = mlb11)
summary(m2)

ggplot(data = m2, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  xlab("Fitted values") +
  ylab("Residuals")

ggplot(data = m2, aes(x = .resid)) +
  geom_histogram(binwidth = 25) +
  xlab("Residuals")

ggplot(data = m2, aes(sample = .resid)) +
  stat_qq()

#########
# wins
#########
m3 <- lm(runs ~ wins, data = mlb11)
summary(m3)

ggplot(data = mlb11, aes(x = wins, y = runs)) +
  geom_point() +
  stat_smooth(method = "lm", se = FALSE)

ggplot(data = m3, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  xlab("Fitted values") +
  ylab("Residuals")

ggplot(data = m3, aes(x = .resid)) +
  geom_histogram(binwidth = 25) +
  xlab("Residuals")

ggplot(data = m3, aes(sample = .resid)) +
  stat_qq()

#########
# bat_avg
#########
m4 <- lm(runs ~ bat_avg, data = mlb11)
summary(m4)

ggplot(data = mlb11, aes(x = bat_avg, y = runs)) +
  geom_point() +
  stat_smooth(method = "lm", se = FALSE)

ggplot(data = m4, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  xlab("Fitted values") +
  ylab("Residuals")

ggplot(data = m4, aes(x = .resid)) +
  geom_histogram(binwidth = 25) +
  xlab("Residuals")

ggplot(data = m4, aes(sample = .resid)) +
  stat_qq()

#########
# new_onbase
#########
m5 <- lm(runs ~ new_onbase, data = mlb11)
summary(m5)

ggplot(data = mlb11, aes(x = new_onbase, y = runs)) +
  geom_point() +
  stat_smooth(method = "lm", se = FALSE)

ggplot(data = m5, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  xlab("Fitted values") +
  ylab("Residuals")

ggplot(data = m5, aes(x = .resid)) +
  geom_histogram(binwidth = 25) +
  xlab("Residuals")

ggplot(data = m5, aes(sample = .resid)) +
  stat_qq()
#########
# new_slug
#########
m6 <- lm(runs ~ new_slug, data = mlb11)
summary(m6)

ggplot(data = mlb11, aes(x = new_slug, y = runs)) +
  geom_point() +
  stat_smooth(method = "lm", se = FALSE)

ggplot(data = m6, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  xlab("Fitted values") +
  ylab("Residuals")

ggplot(data = m6, aes(x = .resid)) +
  geom_histogram(binwidth = 25) +
  xlab("Residuals")

ggplot(data = m6, aes(sample = .resid)) +
  stat_qq()

#########
# new_obs
#########
m7 <- lm(runs ~ new_obs, data = mlb11)
summary(m7)

ggplot(data = mlb11, aes(x = new_obs, y = runs)) +
  geom_point() +
  stat_smooth(method = "lm", se = FALSE)

ggplot(data = m7, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  xlab("Fitted values") +
  ylab("Residuals")

ggplot(data = m7, aes(x = .resid)) +
  geom_histogram(binwidth = 25) +
  xlab("Residuals")

ggplot(data = m7, aes(sample = .resid)) +
  stat_qq()






