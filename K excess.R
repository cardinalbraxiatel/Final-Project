delta1new <- matrix(nrow = nrow(my_wine),ncol = cats)
delta2new <- matrix(nrow = nrow(my_wine),ncol = cats)
delta3new <- matrix(nrow = nrow(my_wine),ncol = cats)
alldistancesold <- c()
alldistancesnew <- c()


#for(i in 1:1){
#  alldistancesold <- alldistancesnew
for(i in 1:obs){
  delta1new[i,] <- (alldata[i,] - means1)
  delta2new[i,] <- (alldata[i,] - means2)
  delta3new[i,] <- (alldata[i,] - means3)  
  
  delta1squarednew <- (apply(delta1new,c(1,2),'^',2))
  delta2squarednew <- apply(delta2new,c(1,2),'^',2)
  delta3squarednew <- apply(delta3new,c(1,2),'^',2)
  
  distances1new <- rowSums(delta1squarednew)
  distances2new <- rowSums(delta2squarednew)
  distances3new <- rowSums(delta3squarednew)
}

alldistancesnew <- cbind(distances1new,distances2new,distances3new)

Clusternew <- integer(length = 1)
Cluster1new <- matrix(nrow = obs,ncol=cats) 
Cluster2new <- matrix(nrow = obs,ncol=cats)
Cluster3new <- matrix(nrow = obs,ncol=cats)
qnew <- 1
wnew <- 1
znew <- 1

for( j in 1:nrow(alldistancesnew)){
  
  Cluster <- which.min(alldistancesnew[j,])
  
  
  if(Clusternew == 1){Cluster1new[w,] <- alldata[j,]
  wnew <- wnew + 1}
  
  else if(Clusternew == 2){Cluster2new[q,] <- alldata[j,]
  qnew <- qnew + 1}
  
  else if(Clusternew ==3) {Cluster3new[z,] <- alldata[j,]
  znew <- znew + 1}
  
}
Cluster1new <- Cluster1new[1:(w-1),]
Cluster2new <- Cluster2new[1:(q-1),]
Cluster3new <- Cluster3new[1:(z-1),]
print(Cluster3new)
means1new <- colMeans(Cluster1new)
means2new <- colMeans(Cluster2new)
means3new <- colMeans(Cluster3new)
alldata <- rbind(Cluster1new,Cluster2new,Cluster3new)
#print(alldata)

#}
print(sum(alldistancesnew - alldistancesold))
}