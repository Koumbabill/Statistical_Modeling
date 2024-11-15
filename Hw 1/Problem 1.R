
num_simulations <- 1000000

add_to_18 <- 0

for (i in 1:num_simulations) {
  pockets <- 0:14
  
  spin <- runif(1, 0, 14) %/% 1
  
  # Value of first spin
  if (spin == 5) {
    spin <- spin + runif(1, 0, 14) %/% 1
    spin <- spin + runif(1, 0, 14) %/% 1
    
    if (spin == 18) {
      add_to_18 <- add_to_18 + 1
    }
  }
  
}

Prob_tot_18 <- add_to_18 / num_simulations