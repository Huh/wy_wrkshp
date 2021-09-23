library(jagsUI)
library(dplyr)

ipm_file <- tempfile(fileext = ".txt")

writeLines("
  model{

    # Priors
    mu_r ~ dnorm(r_prior[1,1], r_prior[1,2])

    for(age in 2:nage) {
      for(sex in 1:nsex) {
        mu_s[age, sex] ~ dnorm(s_prior[age,sex,1], 1/s_prior[age,sex,2])
      }
    }

    js_sd ~ dunif(0, 50)
    js_tau <- 1/(js_sd^2)

    ad_sd ~ dunif(0, 50)
    ad_tau <- 1/(ad_sd^2)

    gj_eff ~ dnorm(0, 0.001)
    gaf_eff ~ dnorm(0, 0.001)
    gam_eff ~ dnorm(0, 0.001)

    # Linear Predictors
    for(yr in 1:nyear) {
      js_eff[yr] ~ dnorm(0, js_tau)T(-10, 10)
      ad_eff[yr] ~ dnorm(0, ad_tau)T(-10, 10)
      logit(R[yr]) <- mu_r
      logit(S[yr, 2, 1]) <- mu_s[2,1] + gj_eff * ndvi[yr] + js_eff[yr]
      S[yr, 2, 2] <- S[yr, 2, 1]
      logit(S[yr, 3, 1]) <- mu_s[3,1] + gaf_eff * ndvi[yr] + ad_eff[yr]
      logit(S[yr, 3, 2]) <- mu_s[3,2] + gam_eff * ndvi[yr] + ad_eff[yr]
    }

    # First year, condition on...
    for(age in 1:nage) {
      for(sex in 1:nsex) {
        N[1, age, sex] ~ dnorm(n1[age, sex, 1], 1/n1[age, sex, 2])
      }
    }

    Ntot[1] <- sum(N[1,1:nage,1:nsex])

    # Process Model
    # Indices follow year, age, sex
    for(yr in 2:nyear) {
      for(sex in 1:nsex) {
        # Fawns (0-4 months)
        muN[yr, 1, sex] <- N[yr, 3, 1] * R[yr] * 0.5
        N[yr, 1, sex] ~ dpois(muN[yr, 1, sex])

        # Juveniles (4-16 months)
        # Survival in absence of harvest!
        muN[yr, 2, sex] ~ dbin(S[yr-1, 2, sex], round(N[yr-1, 1, sex]))
        N[yr, 2, sex] <- muN[yr, 2, sex] - harv[yr, 2, sex]

        # Adults (16+ months)
        # Survival in absence of harvest!
        muN[yr, 3, sex] ~ dbin(
          S[yr-1, 3, sex],
          round(N[yr-1, 2, sex] + N[yr-1, 3, sex])
        )
        N[yr, 3, sex] <- muN[yr, 3, sex] - harv[yr, 3, sex]
      }
      Ny[yr] <- N[yr,1,1] + N[yr,1,2]
      Nf[yr] <- N[yr,2,1] + N[yr,3,1]
      Nm[yr] <- N[yr,2,2] + N[yr,3,2]
      Ntot[yr] <- Ny[yr] + Nf[yr] + Nm[yr]
    }

    # Observation Models
    #for(yr in 1:nyear) {
    #  for(age in 2:nage) {
    #    for(sex in 1:nsex) {
    #      harvest[yr, age, sex] ~ dbinom(harv_rate, N[yr, age, sex])
    #    }
    #  }
    #}


    # Abundance from Aerial Survey
    for(i in 1:nn) {
      n_est[i,2] ~ dnorm(Ntot[n_est[i,1]], 1/n_est[i,3])
    }

    # Survival

    # Repro
    for(i in 1:nr) {
      yf_est[i,2] ~ dnorm(R[yf_est[i,1]], 1/yf_est[i,3])
    }

    # MF Ratio
    for(i in 1:nmf) {
      mf_est[i,2] ~ dnorm(mfrat[mf_est[i,1]], 1/mf_est[i,3])
    }

    # Ratios
    for(yr in 1:nyear) {
      mfrat[yr] <- (N[yr,1,1] + N[yr,1,2])/N[yr,3,1]
    }

    # Lambda

  }
", con = ipm_file)

###############################################################################
# Make some data
nyear <- 10
nage <- 3
nsex <- 2

# Container to hold values
N <- R <- S <- array(
  NA_real_,
  dim = c(nyear, nage, nsex),
  dimnames = list(
    year = 1:10,
    age = c("Y", "J", "A"),
    sex = c("F", "M")
  )
)

R[,3,1] <- 0.5
S[,2,] <- 0.65
S[,3,] <- 0.85

harv <- array(
  c(10),
  dim = c(nyear, nage, nsex),
  dimnames = list(
    year = 1:nyear,
    age = 1:nage,
    sex = 1:nsex
  )
)

harv[,1,] <- NA_integer_

# Dev processes
#N[2, 1, 1] <- N[2, 3, 1] * R
#N[2, 2, 1] <- N[1, 1, 1] * S[1]
#N[2, 3, 1] <- (N[1, 2, 1] + N[1, 3, 1]) * S[2]

init_n <- round(matrix(
  c(150, 150*.65, 600, 150, 150*.65, 200),
  nrow = nage,
  ncol = nsex
))

for(age in 1:nage) {
  for(sex in 1:nsex) {
    N[1, age, sex] <- init_n[age, sex]
  }
}

for(yr in 2:nyear) {
  for(sex in 1:nsex) {
    N[yr, 1, sex] <- N[yr-1, nage, 1] * R[yr,3,1] * 0.5
    N[yr, 2, sex] <- N[yr-1, 1, sex] * S[yr-1,2,sex] - harv[yr,2,sex]
    N[yr, 3, sex] <- (N[yr-1, 2, sex] + N[yr-1, 3, sex]) * S[yr-1,3,sex] -
      harv[yr,3,sex]
  }
}

plot(
  1:nyear,
  apply(N, 1, sum),
  main = "Truth",
  pch = 19,
  ylab = "N",
  xlab = "Year"
)

# Create abundance monitoring program
total_pop <- apply(N, 1, sum)
N_obs <- rnorm(10, total_pop, sqrt(2 * mean(total_pop)))

plot(
  1:nyear,
  apply(N, 1, sum),
  main = "Truth - Obs",
  pch = 19,
  ylab = "N",
  xlab = "Year",
  ylim = c(min(c(total_pop, N_obs)), max(c(total_pop, N_obs)))
)
points(1:10, N_obs, pch = 19, col = "red")

# Format abundance obs
n_est <- tibble::tibble(
  year = 1:length(N_obs),
  mean = N_obs,
  var = 2 * N_obs
)

n_est %>%
  dplyr::mutate(
    LCL = mean - 1.96 * sqrt(var),
    UCL = mean + 1.96 * sqrt(var)
  )

# Do comp flight
yf_obs <- (N[,1,1] + N[,1,2])/N[,3,1]

yf_est <- tibble::tibble(
  year = 1:length(yf_obs),
  mean = yf_obs,
  var = 0.005
)

yf_est %>%
  dplyr::mutate(
    LCL = mean - 1.96 * sqrt(var),
    UCL = mean + 1.96 * sqrt(var)
  )

mf_obs <- N[,3,2]/N[,3,1]

mf_est <- tibble::tibble(
  year = 1:length(mf_obs),
  mean = mf_obs,
  var = 0.005
)

mf_est %>%
  dplyr::mutate(
    LCL = mean - 1.96 * sqrt(var),
    UCL = mean + 1.96 * sqrt(var)
  )
################################################################################
# Create Priors
r_prior <- matrix(c(0.4, 0.01), nrow = 1)

s_prior <- array(
  c(NA_real_, 0.6, 0.85, NA_real_, 0.6, 0.78, rep(0.01, 6)),
  dim = c(nage, nsex, 2)
)

n1 <- array(
  c(118, 70, 788, 118, 70, 150, rep(10000*3, 6)),
  dim = c(nage, nsex, 2)
)

# JAGS Prep
jdat <- list(
  nage = nage,
  nsex = nsex,
  nyear = nyear,
  nn = nrow(n_est),
  n_est = n_est,
  nr = nrow(yf_est),
  yf_est = yf_est,
  nmf = nrow(mf_est),
  mf_est = mf_est,
  r_prior = r_prior,
  s_prior = s_prior,
  n1 = n1,
  harv = harv,
  ndvi = rnorm(nyear)
)

jinit <- function() {
  list(
    mu_r = rnorm(1, -0.4, 0.001)
  )
}

jparam <- c("N", "mfrat", "S", "R", "deviance", "gj_eff", "gam_eff", "gaf_eff")

################################################################################
# Call Model
fit <- jagsUI::jags(
  data = jdat,
  inits = jinit,
  parameters.to.save = jparam,
  model.file = ipm_file,
  n.chains = 3,
  n.adapt = 5000,
  n.iter = 10000,
  n.burnin = 5000,
  n.thin = 1,
  modules = c("glm", "dic"),
  DIC = TRUE
)

################################################################################