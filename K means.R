Kmeans <- function(){
  library(fpc)
  data(wine, package="rattle")
  set.seed(1345)
  my_wine <- as.matrix(wine[-1])
  names <- colnames(my_wine)
  kmeans(my_wine,centers = 3)
  
  obs <- nrow(my_wine)
  cats <- ncol(my_wine)
  
  rnd_numbs <- sample(1:obs, 3, replace = FALSE)
  
  center1_0 <- (my_wine[rnd_numbs[1],])
  
  center2_0 <- (my_wine[rnd_numbs[2],])
  
  center3_0 <- (my_wine[rnd_numbs[3],])
  

  delta1 <- matrix(nrow = nrow(my_wine),ncol = cats)
  delta2 <- matrix(nrow = nrow(my_wine),ncol = cats)
  delta3 <- matrix(nrow = nrow(my_wine),ncol = cats)
 
  for(its in 1:10){ 
    
  for(i in 1:obs){
    delta1[i,] <- (my_wine[i,] - center1_0)
    delta2[i,] <- (my_wine[i,] - center2_0)
    delta3[i,] <- (my_wine[i,] - center3_0)  
    
    delta1squared <- (apply(delta1,c(1,2),'^',2))
    delta2squared <- apply(delta2,c(1,2),'^',2)
    delta3squared <- apply(delta3,c(1,2),'^',2)
    
    distances1 <- rowSums(delta1squared)
    distances2 <- rowSums(delta2squared)
    distances3 <- rowSums(delta3squared)
    
  }
  

 alldistances <- cbind(distances1,distances2,distances3)
 
 Cluster <- integer(length = 1)
 Cluster1 <- matrix(nrow = obs,ncol= cats) 
 Cluster2 <- matrix(nrow = obs,ncol= cats)
 Cluster3 <- matrix(nrow = obs,ncol= cats)
 q <- 1
 w <- 1
 z <- 1
 
 for( j in 1:nrow(alldistances)){
   
     Cluster <- which.min(alldistances[j,])
     
     
      if(Cluster == 1){Cluster1[w,] <- my_wine[j,]
      w <- w + 1}
      
      else if(Cluster == 2){Cluster2[q,] <- my_wine[j,]
      q <- q + 1}
      
      else if(Cluster ==3) {Cluster3[z,] <- my_wine[j,]
      z <- z + 1}
 
 }
 Cluster1 <- Cluster1[1:(w-1),]
 Cluster2 <- Cluster2[1:(q-1),]
 Cluster3 <- Cluster3[1:(z-1),]
 
 #print(nrow(Cluster2)) 
#print(nrow(Cluster3))
 #print(Cluster2)
# print(nrow(my_wine))
 means1 <- colMeans(Cluster1)
 means2 <- colMeans(Cluster2)
 means3 <- colMeans(Cluster3)
 center1_0 <- means1
 center2_0 <- means2
 center3_0 <- means3
 
 
 my_wine <- rbind(Cluster1,Cluster2,Cluster3)
 
 #print(head(my_wine))
  }
  
}