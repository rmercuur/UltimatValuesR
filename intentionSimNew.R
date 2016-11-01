distance <- 0
beta = 0.1
k =0.5
getNeedDim <- function(result,satE,satT,strengthE,strengthT){
  satDifE=newSat(wealthGain(result),strengthE)
  satDifT=newSat(fairnessGain(result),strengthT)
  oldSatE =satE
  oldSatT =satT
  newSatE=oldSatE+satDifE
  newSatT=oldSatT+satDifT
  newNeed= 1.0/newSatE +1.0/newSatT 
  oldNeed= (1.0/oldSatE + 1.0/oldSatT)
  needDim = oldNeed - newNeed #how much lower is your new need, e.g. positive score means good action
  needDim
}

newSat <- function(gain,strength){
  beta * (gain -k*strength)
}


intention <- function(df, it, strengthE, strengthT, satE, satT, Eactions, Tactions){
  if(it <500){
    
    demands <- c(1:100)
    
    needs <- sapply(demands,getNeedDim,satE=satE,satT=satT,strengthE=strengthE,strengthT=strengthT)
    #now gives higher score if needs diminshes more by the action
    
    #chooses random action based on needs;
    action <- which.max(needs) #think MIN is ok
    
    
    newSatE = satE+newSat(wealthGain(action),strengthE)
    newSatT = satT+newSat(fairnessGain(action),strengthT)
#    newSatE = max(1, min((2*distance)+1, newSatE))
#    newSatT = max(1, min((2*distance)+1, newSatT))
    
    newRow <- c(it,strengthE, strengthT,satE,satT,action,needs)
    df <- rbind(df,newRow)
    df <- intention(df,it+1, strengthE, strengthT, newSatE,newSatT, Eactions, Tactions)
  }
  df
}

demands <- c(1:100)
needs2 <- sapply(demands,getNeedDim,satE=1,satT=1.0,strengthE=1,strengthT=1)
needs <- sapply(demands,getNeedDim,satE=1.2,satT=1,strengthE=1,strengthT=1)


plot(needs2)
df = as.data.frame(matrix(ncol=106, nrow=1))
names(df) = c("it","SE", "ST", "satE", "satT", "demands")
# a <- rmvnorm(n=1,mean=c(1,1),sigma=matrix(c(0.0625,-0.8*0.0625,
#                                             -0.8*0.0625,0.0625),2,2))
# b <-intention(df,1,a[1],a[2],3,3,0,0)
b <-intention(df,1,1,1,1,1,0,0)



#try nr. 2
wealthGains <- sapply(1:100, wealthGain)
fairnessGains <- sapply(1:100, fairnessGain)


satEs <- sapply(wealthGains,newSat,1.0) #strength 1.0
satTs <- sapply(fairnessGains,newSat,1.0)


getNeedDimSim <-function(satPair){
  satDifE=satPair[1]
  satDifT=satPair[2]
  oldSatE =satPair[3]
  oldSatT =satPair[4]
  newSatE=oldSatE+satDifE
  newSatT=oldSatT+satDifT
  newNeed= 1.0/newSatE +1.0/newSatT 
  oldNeed= (1.0/oldSatE + 1.0/oldSatT)
  needDim = oldNeed - newNeed #how much lower is your new need, e.g. positive score means good action
  needDim
}


oldSatE <- 1.0
oldSatT <- 1.0
df2 <- data.frame(satEs,satTs,oldSatE,oldSatT)
satGain <- apply(df2,1,sum)
needDim <- apply(df2,1,getNeedDimSim)


