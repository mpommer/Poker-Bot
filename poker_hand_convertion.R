# Convert card to number in 0-51
rm(list=ls())



hands_all <- "8C TS KC 9H 4S 7D 2S 5D 3S AC"

convertion <- function(n){
  vector <- ""
  ca <- strsplit(n," ")[[1]]
  vector <- append(vector, ca)
  vector <- vector[2:length(vector)]
  
  vector1 <- ""
  for(i in 1:length(vector)){
  single <- strsplit(vector[i],"")[[1]]
  vector1 <- append(vector1, single)
  }
  vector1 <- vector1[2:length(vector1)]
  
  for(i in 1:length(vector1)){
    if(vector1[i]=="T"){
      vector1[i] <- 10
    }
    if(vector1[i]=="J"){
      vector1[i] <- 11
    }
    if(vector1[i]=="Q"){
      vector1[i] <- 12
    }
    if(vector1[i]=="K"){
      vector1[i] <- 13
    }
    if(vector1[i]=="A"){
      vector1[i] <- 14
    }
  }
  
 cards <- c()
 for(i in seq(1,length(vector1), by=2)){
   num <- 0
   if(vector1[i+1]=="H"){
   num <- (as.numeric(vector1[i])-2)*4+0
   } else if (vector1[i+1]=="S"){
    num <- (as.numeric(vector1[i])-2)*4+1
   }  else if (vector1[i+1]=="C"){
     num <- (as.numeric(vector1[i])-2)*4+2
   } else if (vector1[i+1]=="D"){
     num <- (as.numeric(vector1[i])-2)*4+3
   }
   
  cards <- append(cards, num)
    
 }
 
 
  return(cards)
}





# direcr convertion in tw vectors

convertion_number <- function(v){
  v1 <- convertion(v)
  
    number <- c()
  
  for(j in 1:7){
    number <- append(number, v1[j] %/% 4 + 2)
  
  }
  
  return(number)
}

convertion_col <- function(v){
  v1 <- convertion(v)
  
  col <- c()
  
  for(j in 1:7){
    col <- append(col, v1[j] %% 4)
    
  }
  
  return(col)
}













