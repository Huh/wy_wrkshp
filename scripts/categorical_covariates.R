# Categorical Covariates
# 09/2021
library("jagsUI")

################################################################################
# Model File
catcovar <- tempfile(fileext = ".txt")

writeLines("
  model{
    # Prior
    int ~ dunif(-10, 10)
    cat2 ~ dunif(-10, 10)
    cat3 ~ dunif(-10, 10)

    # Linear Predictor
    for(i in 1:n_obs) {
      logit(p[i]) <- int + cat2 * covar[i,1] + cat3 * covar[i,2]
    }

    # Likelihood
    for(i in 1:n_obs) {
      y_obs[i] ~ dbin(p[i], 1)
    }

    # Derived
    mu_cat1 <- int
    mu_cat2 <- int + cat2
    mu_cat3 <- int + cat3

    # Is cat2 different than cat1?
    cat2_big <- mu_cat2 > mu_cat1
    cat2_small <- 1 - cat2_big
    cat3_big <- mu_cat3 > mu_cat1

    # Is cat1 different than 0?
    cat1_not0 <- mu_cat1 > 0

  }
", con = catcovar)
################################################################################
# Data Creation
int <- 2
coef <- c(1.1, -3)
covar <- factor(sample(1:3, 50, replace = T))

# More efficient probably, but less obvious
model.matrix(~ covar)

covar_tbl <- tibble::tibble(
  covar2 = as.integer(covar == "2"),
  covar3 = as.integer(covar == "3")
)

# # Code 2 categories
# sex_covar <- sample(c("F", "M"), 10, replace = T)
# as.integer(sex_covar == "F")

y_det <- int + coef[1] * covar_tbl[,1][[1]] + coef[2] * covar_tbl[,2][[1]]
y <- rbinom(nrow(covar_tbl), prob = plogis(y_det), size = 1)

# JAGS Prep
jdat <- list(
  n_obs = length(covar),
  covar = covar_tbl,
  y_obs = y
)

jinit <- function() {
  list(
    int = runif(1, -10, 10)
  )
}

jparam <- c("int", "cat2", "cat3", "mu_cat1", "mu_cat2", "mu_cat3", "cat2_big",
            "cat3_big", "cat1_not0", "cat2_small")

################################################################################
# Run Model
fit <- jagsUI::jags(
  data = jdat,
  inits = jinit,
  parameters.to.save = jparam,
  model.file = catcovar,
  n.chains = 3,
  n.adapt = 5000,
  n.iter = 10000,
  n.burnin = 5000,
  n.thin = 1,
  modules = c("glm", "dic"),
  DIC = TRUE
)
