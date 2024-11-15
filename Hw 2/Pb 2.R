consec_head <- 0
r <- 4
s <- 7
X <- 0
nreps <- 100000

for (i in 1:nreps) {
  for (j in 1:s) {
    if (runif(1) < 0.5) {
      consec_head <- consec_head + 1
    }
    else {
      consec_head <- 0
    }
    if (consec_head == r) break
  }
  X <- X + j
}

cat("E(X) =", X/nreps)