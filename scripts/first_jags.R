# Bayesian GLM
# Josh Nowak
# 09/2021
################################################################################
# Packages
library(jagsUI)

################################################################################
# No external data needed

################################################################################
# Model File
tmpfl <- tempfile(fileext = ".txt")

writeLines("
  model{
    # Prior
    int ~ dunif(-10, 10)
    slope ~ dunif(-10, 10)

    # Linear Predictor
    for(i in 1:n_obs) {
      logit(p[i]) <- int + slope * covar[i]
    }

    # Likelihood
    for(i in 1:n_obs) {
      y_obs[i] ~ dbin(p[i], 1)
    }
  }
", con = tmpfl)
################################################################################
# Data Creation
int <- 2
slope <- 1.1
covar <- rnorm(500)

y_det <- int + slope * covar
y <- rbinom(length(covar), prob = plogis(y_det), size = 1)

################################################################################
# JAGS Prep
jdat <- list(
  n_obs = length(covar),
  covar = covar,
  y_obs = y
)

jinit <- function() {
  list(
    int = rnorm(1),
    slope = rnorm(1)
  )
}

jparam <- c("int", "slope")

################################################################################
# Run Model
fit <- jagsUI::jags(
  data = jdat,
  inits = jinit,
  parameters.to.save = jparam,
  model.file = tmpfl,
  n.chains = 3,
  n.adapt = 5000,
  n.iter = 10000,
  n.burnin = 5000,
  n.thin = 1,
  modules = c("glm", "dic"),
  DIC = TRUE
)


################################################################################
# Summarizes Results
