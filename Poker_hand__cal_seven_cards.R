# poker hand first seven cards


#test hand




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
    for(i in 1:7){
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
  pair_three <- 1
  score <- 0
  if(max(table(v))==2 && (length(table(v))==5 || length(table(v))==4)){
    for(i in 1:7){
      if(duplicated(v)[i]==TRUE){
        pair_one <- v[i]
        v[which(v==pair_one)] <- 0
        break
      }
    }
    for(i in 1:7){
      if(duplicated(v)[i]==TRUE && v[i]!=0){
        pair_two <- v[i]
        v[which(v==pair_two)] <- 0
        break
      }
      }
      for(i in 1:7){
        if(duplicated(v)[i]==TRUE && v[i]!=0){
          pair_three <- v[i]
          v[which(v==pair_three)] <- 0
          break
        }
      }
      vector <- c(pair_one, pair_two, pair_three)
      vector <- sort(vector)
    score <- vector[3]*10000 + vector[2]*10 + max(v,vector[1])*0.1
    
  }

  return(score)
}





# three of a kind

three <- function(v){
  test <- 1
  score <- 0
  if(sum(duplicated(v))==2){
    for(i in 1:7){
      if(duplicated(v)[i]==TRUE){
        three <- v[i]
        if(sum(v==three)!=3){
          test <- 0
        }
        v[which(v==three)] <- 0
        break
      }
    }
    max <- max(v)
    v[which(v==max)] <- 0
    max_2 <- max(v)
    score <- three*100000 + max + max*0.01
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
  v1 <- unique(v1)
  if(length(v1)==2){
    v1 <- append(v1, c(0,0,0,0,0))
  } else if(length(v1)==3){
    v1 <- append(v1, c(0,0,0,0))
  } else if(length(v1)==4){
    v1 <- append(v1, c(0,0,0))
 } else if(length(v1)==5){
   v1 <- append(v1, c(0,0))
  } else if (length(v1)==6){
   v1 <- append(v1, 0)
  }
  v1 <- sort(v1)

  if(v1[1]+1==v1[2] && v1[2]+1==v1[3] && v1[3]+1==v1[4] && v1[4]+1==v1[5]){
    score <- v1[1]*1000000
    
  } else if (v1[2]+1==v1[3] && v1[3]+1==v1[4] && v1[4]+1==v1[5] && v1[5]+1==v1[6]){
    score <- v1[2]*1000000
    
  }else if (v1[3]+1==v1[4] && v1[4]+1==v1[5] && v1[5]+1==v1[6] && v1[6]+1==v1[7]){
    score <- v1[3]*1000000
  }
  return(score)
}


# flush

flush <- function(v){
  score <- 0
  if(sum(v==0)>4){
    score <- 10000001
  } else if (sum(v==1)>4){
    score <- 10000001
  } else if (sum(v==2)>4){
    score <- 10000001
  } else if (sum(v==3)>4){
    score <- 10000001
  }
  return(score)
}





# Full House

full_house <- function(v){
  three <- 0
  pair <- 0
  score <- 0
  for(i in 1:7){
    if(sum(v==v[i])==3){
      three <- v[i]
      v[which(v==three)] <- 0
      break
    }
  }
  if(three!=0){
  for(i in 1:7){
    if((sum(v==v[i])==2 && v[i]!=0) || (sum(v==v[i])==3 && v[i]!=0) ){
      if(pair<v[i]){
      pair <- v[i]}
      v[which(v==pair)] <- 0
    
    }
  }
   if(pair != 0){
     score <- 10000000 + three + pair*0.01
   } 
    
  }
  return(score)
}



full_house(t)

# four of a kind

four_of_kind <- function(v){
  score <- 0
 for(i in 2:14){
   if(sum(v==i)==4){
     four <- i
     v[which(v==four)] <- 0
     score <- 10000100 + four + max(v)*0.01
     break
   }
 }
  return(score)
}





# straight flush and royal flush

straight_flush <- function(v, v2){
  score <- 0
  if(straight(v)!=0 && flush(v2)!=0){
    if(sum(v2==0)>4){
      col <- 0
    } else if (sum(v2==1)>4){
      col <- 1
    } else if (sum(v2==2)>4){
      col <- 2
    } else if (sum(v2==3)>4){
      col <- 3
    }
v[which(v2!=col)] <- 0

   
  
  if(straight(v)>0) {
    score <- score <- 100010000 + max(v)
  }
  }
  return(score)
}





