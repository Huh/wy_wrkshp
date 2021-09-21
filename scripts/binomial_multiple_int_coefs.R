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
    for(s in 1:n_site){
      int[s] ~ dnorm(0, 0.3)
      logit(mu_int[s]) <- int[s]
    }

    r_eff ~ dnorm(0, 0.3)
    g_eff ~ dnorm(0, 0.3)

    # Linear Predictor
    for(i in 1:n_obs) {
      logit(p[i]) <- int[site[i]] + r_eff * rain[i] + g_eff * ndvi[i]
    }

    # Likelihood
    for(i in 1:n_obs) {
      y_obs[i] ~ dbin(p[i], 1)
    }
  }
", con = tmpfl)
################################################################################
# Data Creation
int <- qlogis(c(0.9, 0.85))
site <- sample(1:2, 500, replace = T)
r_eff <- 0.5
g_eff <- 1.2
rain <- rnorm(length(site), 0, 1)
ndvi <- rnorm(length(site), 0, 1)

y_det <- int[site] + r_eff * rain + g_eff * ndvi
y <- rbinom(length(rain), prob = plogis(y_det), size = 1)

################################################################################
# JAGS Prep
jdat <- list(
  n_obs = length(rain),
  n_site = length(int),
  site = site,
  ndvi = ndvi,
  rain = rain,
  y_obs = y
)

jinit <- function() {
  list(
    int = rnorm(2)
  )
}

jparam <- c("int", "r_eff", "g_eff", "mu_int")

################################################################################
# Run Model
fit <- jagsUI::jags(
  data = jdat,
  inits = jinit,
  parameters.to.save = jparam,
  model.file = tmpfl,
  n.chains = 3,
  n.adapt = NULL,
  n.iter = 10000,
  n.burnin = 5000,
  n.thin = 1,
  modules = c("glm", "dic"),
  DIC = TRUE
)


################################################################################
# Summarizes Results
