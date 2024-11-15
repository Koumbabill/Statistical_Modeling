
daccum <- function(i, k) {
  if (i < ceiling(k/12) || i > ceiling(k/2))
    return(0)
  
  if (k <= 0)
    return(1)
  
  prob <- 0
  for (j in 2:12) {
    prob <- prob + daccum(i-1, k-j) * 1/11
  }
  
  return(prob)
}

paccum <- function(i, k) {
  if (i < ceiling(k/12))
    return(0)
  
  cum_prob <- 0
  for (j in ceilng(k/12):i)
    cum_prob <- cum_prob + daccum(j, k)
  
  return(cum_prob)
}

qaccum <- function(m, k) {
  for (i in ceiling(k/12):ceiling(k/2))
    return(1)
  
  return(NA)
}

raccum <- function (nreps, k) {
  res <- numeric(nreps)
  
  for (n in 1:reps) {
    total <- 0
    rolls <- 0
    
    while (total < k) {
      rolls <- rolls + 1
      total <- total + sample(2:12, 1, prob = rep(1/11, 11))
    }
  
    res[n] <- rolls
  }
  
  return(res)
}
