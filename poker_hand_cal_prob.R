# poker hand simlaute hands




v1 <- "2H 4C 6H 9S QD"
v3 <- "TD 5H"
v2 <- convertion(v1)
v2 <- c(2,4,12,34, 34)
win_prob(v1, 1000)




win_prob <- function(v1, n){
  v1 <- convertion(v1)
  lenghts <- length(v1)
 hand_one <- v1

 
  
  # simulate other hand and cards in the middle
  deck <- c(0:51)
  left_deck <- setdiff(deck,hand_one)
  
  
  # empty matrix
  m <- matrix(, nrow = 2, ncol = 9)
  
  
  win <- 0
  count <- 0

  
  for(i in 1:100){
    hand_one[(lenghts+1):9] <- sample(left_deck, (9-lenghts), replace=F)
    
    # matrix befüllen
    
    for(j in 1:9){
      m[1,j] <- hand_one[j] %/% 4 + 2
      m[2, j] <- hand_one[j]%%4
    }
    
    
    # both final hands
    hand_num_1 <- c(m[1,1:7])
    hand_col_1 <- c(m[2,1:7])
    
    
    
    hand_num_2 <- c(m[1,3:9])
    hand_col_2 <- c(m[2,3:9])

    
    # compare hands
    
    if(cal_scores(hand_num_1, hand_col_1)>cal_scores(hand_num_2, hand_col_2)){
      win <- win +1
    }
    
    
    
    count <- count +1
  }
  prob <- win/count  
  return(prob)
}








cal_scores <- function(v, col){
  if(straight_flush(v, col)!=0){
    score <- straight_flush(v, col)
    return(score)
  } else if (four_of_kind(v)!=0) {
    score <- four_of_kind(v)
    return(score)
  } else if (full_house(v)!=0){
    score <- full_house(v)
    return(score)
  } else if (flush(col)!=0){
    score <- flush(col)
    return(score)
  } else if (straight(v)!=0){
    score <- straight(v)
    return(score)
  } else if (three(v)!=0){
    score <- three(v)
    return(score)
  } else if (two_pair(v)!=0){
    score <- two_pair(v)
    return(score)
  } else if (one_pair(v)!=0){
    score <- one_pair(v)  
    return(score)
  } else {
    score <- highest_card(v)
    return(score)
  }
}







