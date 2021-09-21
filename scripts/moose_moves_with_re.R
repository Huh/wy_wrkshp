# Mean displacemnt (m)
global_int <- 330
global_sd <- 0.5
n_moose <- 15
n_steps <- 10
temp_eff <- -70

covs <- matrix(rnorm(n_moose * n_steps), nrow = n_moose, ncol = n_steps)

moose_moves <- function(n_steps, int, coef, covs, sd_err) {
  int + coef * covs + rnorm(1, 0, sd_err)
}

mobs <- matrix(NA_real_, nrow = n_moose, ncol = n_steps)

for(i in 1:n_moose) {
  mobs[i,] <- moose_moves(n_steps, global_int, temp_eff, covs[i,], global_sd)
}

# Model
tmp_re <- tempfile(fileext = ".txt")

writeLines("
  model{
    # Priors
    int ~ dnorm(0, 0.001)
    temp_eff ~ dnorm(0, 0.001)

    tau ~ dgamma(0.01, 0.01)
    sd <- sqrt(1/tau)

    sd_moose ~ dunif(0, 50)
    tau_moose <- 1/(sd_moose^2)

    for(i in 1:n_moose){
      moose_eff[i] ~ dnorm(0, tau_moose)
    }

    # Likelihood
    for(i in 1:n_moose){ # Rows
      for(j in 1:n_steps){ # Columns
        y_det[i,j] <- int + temp_eff * temp[i,j] + moose_eff[i]
        y[i,j] ~ dnorm(y_det[i,j], tau)
      }
    }

    # Derived
  }
", con = tmp_re)

jdat <- list(
  n_moose = n_moose,
  n_steps = n_steps,
  temp = covs,
  y = mobs
)

jinits <- function() {
  list(
    int = runif(1, 280, 380),
    temp_eff = runif(1, 0, 1),
    sd = runif(1, 200, 500),
    sd_moose = runif(1, 1, 5)
  )
}

jparams <- c("int", "temp_eff", "sd", "sd_moose")

fit <- jagsUI::jags(
  data = jdat,
  inits = NULL,
  parameters.to.save = jparams,
  model.file = tmp_re,
  n.chains = 3,
  n.adapt = 5000,
  n.burnin = 5000,
  n.iter = 10000,
  n.thin = 1,
  modules = c("glm", "dic")
)

