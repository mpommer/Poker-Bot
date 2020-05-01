# Poker Bot 
pot <- 0
einsatz <- 0
p <- 3

pot_odds <- pot/raise

v1 <- "AH AC"

# first two cards
win_probability <- win_prob(v1, 5000, p)
poker_equity <- (1-win_probability)/win_probability
raise <- readline("What is the raise? >")
raise <- as.numeric(raise)
pot <- readline("What is the pot? >")
pot <- as.numeric(pot)
   pot_odds <- pot/raise
 wkt_1 <- runif(n=1,min=0,max=1)
 wkt_2 <- runif(n=1,min=0,max=10)
 
   if(poker_equity+5<pot_odds){
     my_raise <- rnorm(1, mean=100, sd=10)
   } else if (poker_equity<pot_odds){
     my_raise <- rnorm(1, mean=20, sd=5)
   } else if (poker_equity<pot_odds+2) {
     my_raise <- rnorm(1, mean=0, sd=5)
   } else {
     my_raise <- 0
   }
   if(my_raise>raise){
     print("raise")
     print(my_raise)
   } else if(poker_equity<pot_odds){
     print("call")
   } else if (poker_equity<pot_odds+wkt_2 && wkt_1<0.3){
     print("call")
   } else {
     print("fold")
   }


 
 
 
 

 
 
 
 
 