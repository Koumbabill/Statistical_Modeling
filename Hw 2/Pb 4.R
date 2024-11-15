X <- 1:8
nreps <- 100000
sum <- 0

for (i in 1:nreps) {
  dice1 <- sample(X, 1)
  dice2 <- sample(X, 1)
  
  sum <- sum + dice1 + dice2
}

EX <- sum/nreps
