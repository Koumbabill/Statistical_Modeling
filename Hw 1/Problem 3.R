
num_simulations <- 100000

count_2_5s <- 0
count_4_diams <- 0

for (i in 1:num_simulations) {
  deck <- 1:52
  hand <- sample(deck, 5)
  
  ranks <- (hand - 1) %% 13 + 1
  suits <- (hand - 1) %/% 13 + 1
  
  num_5s <- sum(ranks == 5)
  num_diams <- sum(suits == 2)
  
  if (num_5s == 2) {
    count_2_5s <- count_2_5s + 1
  }
  if (num_diams == 4) {
    count_4_diams <- count_4_diams + 1
  }
  
}

prob_2_5s <- count_2_5s / num_simulations
prob_4_diams <- count_4_diams / num_simulations

