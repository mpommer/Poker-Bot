# poker hand winning probability texas holdem



v1 <- "8H 8C"
v2 <- convertion(v1)
cal_scores_new(v1)
win_prob(v1, 5000, 6)




win_prob <- function(v1, n, p){
  v1 <- convertion(v1)
  lenghts <- length(v1)
  hand_one <- v1
  
  
  
  # simulate other hand and cards in the middle
  deck <- c(0:51)
  left_deck <- setdiff(deck,hand_one)
  
  
  # empty matrix
  m <- matrix(, nrow = 2, ncol = 17)
  
  
  win <- 0
  count <- 0
  
  
  for(i in 1:n){
    hand_one[(lenghts+1):(5+p*2)] <- sample(left_deck, ((5+p*2)-lenghts), replace=F)
    
    # matrix befüllen
    
    for(j in 1:(5+p*2)){
      m[1,j] <- hand_one[j] %/% 4 + 2
      m[2, j] <- hand_one[j]%%4
    }
    
    
    # final hand player 1
    hand_num_1 <- c(m[1,1:7])
    hand_col_1 <- c(m[2,1:7])
    
    
   # final hand player 2
    hand_num_2 <- c(m[1,3:9])
    hand_col_2 <- c(m[2,3:9])
    
    # final hand player 3
    if(p>2){
    hand_num_3 <- c(m[1,3:7], m[1,10:11])
    hand_col_3 <- c(m[2,3:7], m[2,10:11])
    }
    
    # final hand player 4
    if(p>3){
      hand_num_4 <- c(m[1,3:7], m[1,12:13])
      hand_col_4 <- c(m[2,3:7], m[2,12:13])
    }
    
    # final hand player 5
    if(p>4){
      hand_num_5 <- c(m[1,3:7], m[1,14:15])
      hand_col_5 <- c(m[2,3:7], m[2,14:15])
    }
    
    # final hand player 6
    if(p>5){
      hand_num_6 <- c(m[1,3:7], m[1,16:17])
      hand_col_6 <- c(m[2,3:7], m[2,16:17])
    }
    
   # scoer <- cal_scores(hand_num_1, hand_col_1)
    #return(scoer)
    # compare hands
    if(p==2){
    compare <- c(cal_scores(hand_num_1, hand_col_1),cal_scores(hand_num_2, hand_col_2))
            
    } else   if(p==3){
      compare <- c(cal_scores(hand_num_1, hand_col_1),cal_scores(hand_num_2, hand_col_2),
                   cal_scores(hand_num_3, hand_col_3))
    } else   if(p==4){
      compare <- c(cal_scores(hand_num_1, hand_col_1),cal_scores(hand_num_2, hand_col_2),
                   cal_scores(hand_num_3, hand_col_3),cal_scores(hand_num_4, hand_col_4))
                  
    } else   if(p==5){
      compare <- c(cal_scores(hand_num_1, hand_col_1),cal_scores(hand_num_2, hand_col_2),
                   cal_scores(hand_num_3, hand_col_3),cal_scores(hand_num_4, hand_col_4),
                   cal_scores(hand_num_5, hand_col_5))
    } else   if(p==6){
      compare <- c(cal_scores(hand_num_1, hand_col_1),cal_scores(hand_num_2, hand_col_2),
                   cal_scores(hand_num_3, hand_col_3),cal_scores(hand_num_4, hand_col_4),
                   cal_scores(hand_num_5, hand_col_5),cal_scores(hand_num_6, hand_col_6))
    }
  
    
    
    if(compare[1]==max(compare)){
      win <- win+1
    }
    count <- count +1
  }
  prob <- win/count  
  return(prob)
}





cal_scores<- function(v, col){
  
  
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



cal_scores_new <- function(v1){
  v <- convertion_number(v1)
  col <- convertion_col(v1)
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










