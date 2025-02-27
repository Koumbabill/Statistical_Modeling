# finds P(X1 = 2), P(X2 = 2) and P(X2 = 2|X1 = 1) in ALOHA example
sim <- function(p,q,nreps) {
  countx2eq2 <- 0
  countx1eq1 <- 0
  countx1eq2 <- 0
  countx2eq2givx1eq1 <- 0
  # simulate nreps repetitions of the experiment
  for (i in 1:nreps) {
    numsend <- 0 # no messages sent so far
    # simulate A and B’s decision on whether to send in epoch 1
    for (j in 1:2)
      if (runif(1) < p) numsend <- numsend + 1
    if (numsend == 1) X1 <- 1 
    else X1 <- 2
    if (X1 == 2) countx1eq2 <- countx1eq2 + 1
    # now simulate epoch 2
    # if X1 = 1 then one node may generate a new message
    numactive <- X1
    if (X1 == 1 && runif(1) < q) numactive <- numactive + 1
    # send?
    if (numactive == 1)
      if (runif(1) < p) X2 <- 0
    else X2 <- 1
    else { # numactive = 2
      numsend <- 0
      for (i in 1:2)
        if (runif(1) < p) numsend <- numsend + 1
      if (numsend == 1) X2 <- 1
      else X2 <- 2
    }
    if (X2 == 2) countx2eq2 <- countx2eq2 + 1
    if (X1 == 1) { # do tally for the cond. prob.
      countx1eq1 <- countx1eq1 + 1
      if (X2 == 2) countx2eq2givx1eq1 <- countx2eq2givx1eq1 + 1
    }
  }
  # print results
  cat("P(X1 = 2):",countx1eq2/nreps,"\n")
  cat("P(X2 = 2):",countx2eq2/nreps,"\n")
  cat("P(X2 = 2 | X1 = 1):",countx2eq2givx1eq1/countx1eq1,"\n")
} 

sim(0.4, 0.2, 10000)