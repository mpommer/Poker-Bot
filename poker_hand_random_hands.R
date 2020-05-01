# create random hands with 7 cards

# 1 <- H
# 2 <- K
# 3 <- P
# 0 <- R

# create matrix
m1 <- matrix(,nrow = 1000, ncol = 14)

for(i in seq(1,1000,by=2)){
v <- sample(0:51, 14, replace=F)

for(j in 1:14){
  m1[i,j] <- v[j] %/% 4 + 2
  m1[i+1, j] <- v[j]%%4
}
}
