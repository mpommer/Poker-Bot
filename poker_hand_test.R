# test file for scores 

# pair vs two pair
v1 <- "KH KD TS KC 3D 5D 3D"
v2 <- "3H 2T 4S 6C JD 5D 2D"


s1 <- cal_scores_new(v1)
s2 <- cal_scores_new(v2)
s1>s2


s3 <- cal_scores_new(v3)


s1>s2


v1 <- "AH AC"
v <- convertion(v1)
deck <- c(0:51)
left_deck <- setdiff(deck,v)


win <- 0
count <- 0

for(i in 1:10000){
v[3:9] <- sample(left_deck, 7, replace=F)
p1 <- v[1:7]
for(j in 1:7){
p1_num[j] <- p1[j] %/% 4 + 2
p1_col[j] <- p1[j] %% 4 
}
p2 <- v[3:9]
for(j in 1:7){
  p2_num[j] <- p2[j] %/% 4 + 2
  p2_col[j] <- p2[j] %% 4 
}



s1 <- cal_scores(p1_num,p1_col)
s2 <- cal_scores(p2_num,p2_col)

if(s1>s2){
  
  win <- win+1
 
}
count <- count +1
}

print(win/count)
win
t1 <- convertion_number(v1)


