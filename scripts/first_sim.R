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
    N[yr, 2, sex] <- N[yr-1, 1, sex] * S[yr-1,2,sex]
    N[yr, 3, sex] <- (N[yr-1, 2, sex] + N[yr-1, 3, sex]) * S[yr-1,3,sex]
  }
}

