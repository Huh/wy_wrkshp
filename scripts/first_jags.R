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
#tmpfl <- tempfile(fileext = "txt")
sink("C:/tmp/something.txt")
cat("
  model{

    # Prior
    int ~ dnorm(0, 0.001)
    slope ~ dnorm(0, 0.001)

    # Linear Predictor
    for(i in 1:n_obs) {
      logit(p[i]) <- int + slope * covar[i]
    }

    # Likelihood
    for(i in 1:n_obs) {
      y_obs[i] ~ dbinom(p[i], 1)
    }

  }
  ", fill = TRUE
)
sink()

################################################################################
# Data Creation
int <- qlogis(0.9)
slope <- 0.5
covar <- rnorm(50, 0, 0.5)

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
  model.file = "C:/tmp/something.txt",
  n.chains = 3,
  n.adapt = NULL,
  n.iter = 1000,
  n.burnin = 500,
  n.thin = 1,
  modules = c("glm", "dic"),
  DIC = TRUE
)


################################################################################
# Summarizes Results
