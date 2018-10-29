Metropolis <- function(iterations = 100, c = 1){
  
  Phi_0_start <- 0.5
  
  chain <- c(rep(0,iterations + 1))
  
  chain[1] <- Phi_0_start
  
  
  for(i in 1:(iterations + 1)){
    
  Phi_0 <- chain[i]
  
  Phi_1 <- rbeta(1,c*Phi_0, c*(1-Phi_0))
  
  r_top <- (dbeta(Phi_1, 6, 4))/(dbeta(Phi_1,c*Phi_0,c*(1-Phi_0)))
  
  r_bot <- (dbeta(Phi_0, 6, 4))/(dbeta(Phi_0,c*Phi_1,c*(1-Phi_1)))
  
  r <- r_top/r_bot
  
  
  if(runif(1) < r){ 
    chain[i+1] <- Phi_1
    } else{
    chain[i+1] <- chain[i]
    }
  
    
  }
  support <- seq(0,1,0.05)
  hist(chain,freq = FALSE)
  curve(dbeta(x,6,4), add = T)
  
  
}