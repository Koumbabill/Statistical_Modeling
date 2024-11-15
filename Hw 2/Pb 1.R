nreps <- 100000
nstops <- 2
L1 <- 0
EL1 <- 0
EL1sq <- 0
L2 <- 0
EL2 <- 0
EL2sq <- 0
EL1_L2 <- 0
EL1_L2sq <- 0

for (i in 1:nreps) {
  passengers <- 0
  for (j in 1:nstops) {
    if (passengers > 0)
      for (k in 1:passengers)
        if (runif(1) < 0.2)
          passengers <- passengers - 1
    newpass <- sample(0:2,1,prob=c(0.5,0.4,0.1))
    passengers <- passengers + newpass
    
    if (j == 1) { #First stop
      L1 <- passengers
      EL1 <- EL1 + L1
      EL1sq <- EL1sq + L1**2
    }
    else { #Second stop
      L2 <- passengers
      EL2 <- EL2 + L2
      EL2sq <- EL2sq + L2**2
    }

  }
  EL1_L2 <- EL1_L2 + (L1 - L2)
  EL1_L2sq <- EL1_L2sq + (L1 - L2)**2
}

EL1 <- EL1/nreps
EL1sq <- EL1sq/nreps
EL2 <- EL2/nreps
EL2sq <- EL2sq/nreps
EL1_L2 <- EL1_L2/nreps
EL1_L2sq <- EL1_L2sq/nreps

varL1 <- EL1sq - EL1**2
varL2 <- EL2sq - EL2**2
varL1_L2 <- EL1_L2sq - EL1_L2**2

cat("E(L1) =", EL1, "\n")
cat("E(L1^2) =", EL1sq, "\n")
cat("var(L1) =", varL1, "\n")


cat("E(L2) =", EL2, "\n")
cat("E(L2^2) =", EL2sq, "\n")
cat("var(L2) =", varL2, "\n")

cat("E(L1 - L2) =", EL1_L2, "\n")
cat("E((L1 - L2)^2) =", EL1_L2sq, "\n")
cat("var(L1 - L2) =", varL1_L2, "\n")
