library(mvtnorm) 

#To find the best variable for max, min.
#outcome was 1.7 to my thesis


#a <- rmvnorm(n=1,mean=c(1,1),sigma=matrix(c(0.0625,-0.8*0.0625, 
#                                            -0.8*0.0625,0.625),2,2)) 

intention <- function(df, it, strengthE, strengthT, satE, satT, Eactions, Tactions, distance){
  if(it <500){
  k =0.5
  beta = 1
  
  needE = (strengthE+distance) / satE
  needT = (strengthT+distance) /satT
  
  #chooses random action based on needs;
  x1 <- runif(1, 0.0, needE+needT)
  if(x1 < needE){
    E = 1 
    Eactions= Eactions +1
    Tr =0
  }
  else{
    Tr = 1 
    Tactions = Tactions + 1
    E = 0
  }
  
 
  
    
  incrementE = tanh(beta * (E- k*strengthE)) /10
  incrementT = tanh(beta * (Tr- k*strengthT)) /10
  newSatE = satE + incrementE
  newSatT = satT + incrementT
  newSatE = max(1, min((2*distance)+1, newSatE))
  newSatT = max(1, min((2*distance)+1, newSatT))
  
  newRow <- c(it,strengthE, strengthT,satE,satT, Eactions, Tactions)
  df <- rbind(df,newRow)
  df <- intention(df,it+1, strengthE, strengthT, newSatE,newSatT, Eactions, Tactions, distance)
  }
  df
}

# b <-intention(df,1,a[1],a[2],3,3,0,0)
# b <-intention(df,1,1,1,3,3,0,0)

test <- function(distance){
  df = as.data.frame(matrix(ncol=7, nrow=1))
  names(df) = c("it","SE", "ST", "satE", "satT", "Eactions", "Tactions")
  a <- rmvnorm(n=1,mean=c(1,1),sigma=matrix(c(0.0625,-0.8*0.0625, 
                                              -0.8*0.0625,0.0625),2,2)) 
  # b <-intention(df,1,a[1],a[2],3,3,0,0)
  b <-intention(df,1,1,1,3,3,0,0,distance)
  original = b[500,2]/b[500,3]
  simulated = b[500,6]/b[500,7]
  abs(original-simulated)
}

teston <- c(1,1.3,1.5,1.6,1.7,1.9,2.0,3.0,4.0,5.0,6.0)
t<-replicate(1000,sapply(teston,test))

