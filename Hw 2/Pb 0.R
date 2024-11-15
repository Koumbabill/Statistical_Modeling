EX <- 0
EXsq <- 0
EY <- 0
EYsq <- 0

EXY <- 0

nreps <- 100000

for (i in 1:nreps) {
  X <- sample(c(1,2,4),1,prob=c(0.2,0.5,0.3))
  
  if (X == 2) {
    Y <- runif(1) < 0.7
  }
  else Y <- runif(1) < 0.5
  EX <- EX + X/nreps
  EXsq <- EXsq + (X**2)/nreps
  EY <- EY + Y/nreps
  EYsq <- EYsq + (Y**2)/nreps
  EXY <- EXY + X*Y/nreps
}

varX <- EXsq - EX**2
varY <- EYsq - EY**2

covX_Y <- EXY - EX*EY

cat("var(X+Y) = ", varX + varY - 2*covX_Y)