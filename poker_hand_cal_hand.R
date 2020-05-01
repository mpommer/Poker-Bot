# Project euler poker hand

# first save as matrix



# highest card


highest_card <- function(v){
  score <- 0
 one <- max(v)
 v[which.max(v)]<- 0
 two <- max(v)
 v[which.max(v)]<- 0
 three <- max(v)
 v[which.max(v)]<- 0
 four <- max(v)
 v[which.max(v)]<- 0
 five <- max(v)
 
 score <- one*10 + two + three*0.01 + four*0.0001 + five*0.000001
 
 
 
 return(score)
}





# one pair
  
one_pair <-  function(v){
  score <- 0
  if(sum(duplicated(v))==1){
 for(i in 1:5){
  if(duplicated(v)[i]==TRUE){
    pair <- v[i]
    score <- pair*1000
    break
  } 
 }
v[which(v==pair)] <- 0    
 score <- score + highest_card(v)
  }
 return(score)
 
} 
  


# two pair
two_pair <- function(v){
  test <- 1
  score <- 0
  if(sum(duplicated(v))==2){
    for(i in 1:5){
      if(duplicated(v)[i]==TRUE){
        pair_one <- v[i]
       if(sum(v==pair_one)!=2){
         test <- 0
         }
      
       v[which(v==pair_one)] <- 0
   break
      }
      }
      for(i in 1:5){
        if(duplicated(v)[i]==TRUE && v[i]!=0){
          pair_two <- v[i]
          v[which(v==pair_two)] <- 0
          break
        }
      }
    score <- max(pair_one,pair_two)*10000 + min(pair_one,pair_two)*10 + max(v)

    }
  if(test == 0){
    score <- 0
  }
  
  return(score)
  
}
      




# three of a kind

three <- function(v){
  test <- 1
  score <- 0
  if(sum(duplicated(v))==2){
    for(i in 1:5){
    if(duplicated(v)[i]==TRUE){
      three <- v[i]
      if(sum(v==three)!=3){
        test <- 0
      }
      v[which(v==three)] <- 0
      break
    }
    }
    score <- three*100000 + highest_card(v)
  }
 
  if(test == 0){
    score <- 0
  }
  return(score)
}


   


# straight

straight <- function(v){
  score <- 0
 v1 <- sort(v)
  if(v1[1]+1==v1[2] && v1[2]+1==v1[3] && v1[3]+1==v1[4] && v1[4]+1==v1[5]){
    score <- v1[1]*1000000
  }
  return(score)
}




# flush

flush <- function(v){
  score <- 0
  if(v[1]==v[2] && v[2]==v[3] && v[3]== v[4] && v[4]==v[5]){
    score <- 10000001
  }
  return(score)
}



# Full House

full_house <- function(v){
  score <- 0
  if(max(table(v))==3 && min(table(v))==2){
    if(sum(v==v[1])==2){
      pair <- v[1]
      v[which(v==pair)] <- 0
    score <- 10000000 + max(v) + pair*0.01
    } else {
      three <- v[1]
      v[which(v==three)] <- 0
      score <- 10000000 + three + max(v)*0.01
    }
  }
  return(score)
}



# four of a kind

four_of_kind <- function(v){
  score <- 0
  if(sum(duplicated(v))==3){
    if(sum(v==v[1])!=2 && sum(v==v[1])!=3){
      for(i in 1:5){
        if(duplicated(v)[i]==TRUE){
          four <- v[i]
          v[which(v==four)] <- 0
          break
        }
      }
      score <- 10000100 + four + max(v)*0.01
    }
  }
  return(score)
}





# straight flush and royal flush

straight_flush <- function(v, v2){
  score <- 0
  if(straight(v)!=0 && flush(v2)!=0){
    score <- 10001000 + max(v)
  }
  return(score)
}



  

  
# calculate scores
  
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


  
  
  
  
  
# calculate scores for player 1
score_one <- c()
  
  
for(i in seq(1,2000, by=2)){
  v <- as.numeric(m[i,1:5])
  col <- m[i+1,1:5]
  score <- cal_scores(v,col)

  score_one <- append(score_one, score)
}  
  

  
  
  

# calculate scores for player 2
score_two <- c()

for(i in seq(1,2000, by=2)){
  v <- as.numeric(m[i,6:10])
  col <- m[i+1,6:10]
  score <- cal_scores(v,col)
  
  score_two <- append(score_two, score)
}  





# compare vectors
count <- 0
for(i in 1:1000){
  if(score_one[i]>score_two[i]){
   
    count <- count +1
  }
}
print(count)








  
  
  
  
  