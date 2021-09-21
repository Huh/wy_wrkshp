# Simulate Fit
# Josh Nowak
# 09/2021
################################################################################
# Packages
# library(dplyr)

################################################################################
# Data
## No external data needed

################################################################################
# Model of the Mean
dat <- rnorm(100, 5, 1)

mean(dat)
hist(dat, breaks = 100)
abline(v = 5, col = "red", lwd = 2)

################################################################################
# Regression
int <- 1.2
slope <- .3
rain <- rnorm(50, 0, 1)

y_det <- int + slope * rain

y <- rnorm(length(rain), y_det, 1)

plot(rain, y, bty = "l", pch = 19, col = "dodgerblue")

summary(lm(y ~ rain))

mod <- lm(y ~ rain)

coef(mod)

pred <- coef(mod)[1] + coef(mod)[2] * rain

lines(rain, pred)

################################################################################
# GLM
# Linear Predictor
# Link
# Stochastic bit, Distribution that describes the error
int <- qlogis(0.9)
slope <- 0.5
covar <- rnorm(50, 0, 0.5)

y_det <- int + slope * covar
range(y_det)
y_link <- plogis(y_det)
range(y_link)

y <- rbinom(length(covar), prob = y_link, size = 1)

plot(covar, y, pch = 19)
lines(covar, y_link)

fit <- glm(y ~ covar, family = binomial)

summary(fit)

################################################################################
# State-space
int <- 1.2
slope <- .3
rain <- rnorm(50, 0, 1)
p_err <- 0.8

y_det <- int + slope * rain

# Process error - biology
y <- rnorm(length(rain), y_det, 0.2)

# Observation error - humans ain't perfect
y_obs <- rnorm(length(rain), y, p_err)

plot(rain, y, bty = "l", pch = 19, col = "dodgerblue")
points(rain, y_obs, pch = 19)

summary(lm(y_obs ~ rain))

################################################################################