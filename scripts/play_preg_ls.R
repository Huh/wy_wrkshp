n_deer <- 50
mu_preg <- 2.1
max_litter <- 2
mu_litter <- 0.84

p <- rbinom(n_deer, size = 1, prob = plogis(mu_preg))
fawns <- rbinom(n_deer, size = max_litter, prob = plogis(mu_litter) * p)

# Model
tmp_seq <- tempfile(fileext = ".txt")

writeLines("
  model{
    # Priors
    preg ~ dbeta(1, 1)
    ls_prob ~ dbeta(1, 1)

    for(i in 1:n_obs) {
      p_obs[i] ~ dbern(preg)
      mu_fawns[i] <- p_obs[i] * ls_prob
      fawn_obs[i] ~ dbin(mu_fawns[i], max_fawns)
    }
  }
", con = tmp_seq)

jdat <- list(
  p_obs = p,
  fawn_obs = fawns,
  n_obs = n_deer,
  litter_size = 2,
  max_fawns = max_litter
)

jinits <- function() {
  list(
    preg = runif(1, 0.8, 0.99),
    litter_size = 2
  )
}

jparams <- c("preg", "litter_size", "mu_fawns")

fit <- jagsUI::jags(
  data = jdat,
  inits = jinits,
  parameters.to.save = jparams,
  model.file = tmp_seq,
  n.chains = 3,
  n.adapt = 5000,
  n.burnin = 5000,
  n.iter = 10000,
  n.thin = 1,
  modules = c("glm", "dic")
)
