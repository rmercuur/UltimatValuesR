distance <- 1.2
beta = 0.1
k =0.5
getNeedDim <- function(result,satE,satT,strengthE,strengthT){
  x <-
  ((strengthE+distance)/(satE+newSat(wealthGain(result),strengthE)) +
  (strengthT+distance)/(satT+newSat(fairnessGain(result),strengthT)))
  -
  ((strengthE+distance)/(satE) +
  (strengthT+distance)/(satT))
  
  x
}

newSat <- function(gain,strength){
  beta * (gain -k*strength)
}


intention <- function(df, it, strengthE, strengthT, satE, satT, Eactions, Tactions){
  if(it <500){
    
    offers <- c(1:100)
    
    needs <- sapply(offers,getNeedDim,satE=satE,satT=satT,strengthE=strengthE,strengthT=strengthT)
    
    
    #chooses random action based on needs;
    action <- which.max(needs)
    
    
    newSatE = newSat(wealthGain(action),strengthE)
    newSatT = newSat(fairnessGain(action),strengthT)
    newSatE = max(1, min((2*distance)+1, newSatE))
    newSatT = max(1, min((2*distance)+1, newSatT))
    
    newRow <- c(it,strengthE, strengthT,satE,satT,action)
    df <- rbind(df,newRow)
    df <- intention(df,it+1, strengthE, strengthT, newSatE,newSatT, Eactions, Tactions)
  }
  df
}

offers <- c(1:100)
needs2 <- sapply(offers,getNeedDim,satE=1,satT=1,strengthE=1,strengthT=1)
needs <- sapply(offers,getNeedDim,satE=1.2,satT=1,strengthE=1,strengthT=1)
plot(needs)
# df = as.data.frame(matrix(ncol=6, nrow=1))
# names(df) = c("it","SE", "ST", "satE", "satT", "offers")
# a <- rmvnorm(n=1,mean=c(1,1),sigma=matrix(c(0.0625,-0.8*0.0625,
#                                             -0.8*0.0625,0.0625),2,2))
# # b <-intention(df,1,a[1],a[2],3,3,0,0)
# b <-intention(df,1,1,1,3,3,0,0)

