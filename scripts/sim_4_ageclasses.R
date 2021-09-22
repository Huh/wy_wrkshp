nyear <- 10
nage <- 4
nsex <- 2

# Container to hold values
N <- R <- S <- array(
  NA_real_,
  dim = c(nyear, nage, nsex),
  dimnames = list(
    year = 1:10,
    age = c("Y", "J", "S", "A"),
    sex = c("F", "M")
  )
)

R[,nage,1] <- 0.5
S[,2,] <- 0.85
S[,3,] <- 0.9
S[,nage,] <- 0.9

# Dev processes
#N[2, 1, 1] <- N[2, 3, 1] * R
#N[2, 2, 1] <- N[1, 1, 1] * S[1]
#N[2, 3, 1] <- (N[1, 2, 1] + N[1, 3, 1]) * S[2]

init_n <- round(matrix(
  c(150, 150 * .85, 150 * .85 * .9, 600,
    150, 150 * .85, 150 * .85 * .9, 200),
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
    N[yr, 1, sex] <- rpois(1, N[yr-1, nage, 1] * R[yr,nage,1] * 0.5)
    N[yr, 2, sex] <- rbinom(1, size = N[yr-1, 1, sex], prob = S[yr-1,2,sex])
    N[yr, 3, sex] <- rbinom(1, size = N[yr-1, 2, sex], prob = S[yr-1,3,sex])
    N[yr, 4, sex] <- rbinom(
      1,
      size = N[yr-1, 3, sex] + N[yr-1, 4, sex],
      prob = S[yr-1, 4, sex]
    )
  }
}

########################################
# for(yr in 2:nyear) {
#   for(sex in 1:nsex) {
#     N[yr, 1, sex] <- rpois(1, N[yr-1, nage, 1] * R[yr,nage,1] * 0.5)
#     for(age in 2:(nage-1)) {
#       N[yr, age, sex] <- rbinom(
#         1,
#         size = N[yr-1, age-1, sex],
#         prob = S[yr-1,age,sex]
#       )
#     }
#     N[yr, nage, sex] <- rbinom(
#         1,
#         size = N[yr-1, nage-1, sex] + N[yr-1, nage, sex],
#         prob = S[yr-1, nage, sex]
#       )
#   }
# }

