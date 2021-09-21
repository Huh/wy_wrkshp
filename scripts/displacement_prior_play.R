
# Mean displacemnt (m)
int <- 330
temp_eff <- -70
temp <- rnorm(500)

hist(temp)

y_det <- int + temp_eff * temp
y <- rnorm(length(temp), y_det, 20)

plot(temp, y, pch = 19)

lm(y ~ temp)

# Model
tmp_disp <- tempfile(fileext = ".txt")

writeLines("
  model{
    # Priors
    int ~ dnorm(0, 0.001)
    temp_eff ~ dnorm(0, 0.001)

    # Too strong prior
    #int ~ dnorm(0, 5)
    #temp_eff ~ dnorm(0, 10)

    # Get weird for fun?
    #int ~ dgamma(0.01, 0.01)
    #temp_eff ~ dunif(-500,0)

    # Stochastic
    sd ~ dunif(0, 5000)
    # Deterministic
    tau <- 1/sd^2

    # Likelihood
    for(i in 1:n_obs){
      y_det[i] <- int + temp_eff * temp[i]
      y[i] ~ dnorm(y_det[i], tau)
    }

    # Derived
  }
", con = tmp_disp)

jdat <- list(
  temp = temp,
  y = y,
  n_obs = length(y)
)

jinits <- function() {
  list(
    int = runif(1, 10, 300),
    temp_eff = runif(1, -100, 0),
    sd = runif(1, 0, 50)
  )
}

jparams <- c("int", "temp_eff", "sd")

fit <- jagsUI::jags(
  data = jdat,
  inits = jinits,
  parameters.to.save = jparams,
  model.file = tmp_disp,
  n.chains = 3,
  n.adapt = 5000,
  n.burnin = 5000,
  n.iter = 10000,
  n.thin = 1,
  modules = c("glm", "dic")
)
