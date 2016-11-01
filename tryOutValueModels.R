wealthStrength = 2.0
fairnessStrength = 1.0
pieSize = 100
pieSizeMed = pieSize/2.0

wealthGain <- function(result){
 result/100
}

fairnessGain <- function(result){
  1-abs(pieSizeMed-result)/pieSizeMed
}
range01 <- function(x){(x-min(x))/(max(x)-min(x))}

gain <- function(result){
  wealthGain(result)+ fairnessGain(result)
}

multicriteriaUtilityS <- function(result, difference){
  dx = difference/2
  (wealthStrength+dx)* wealthGain(result) +
    (fairnessStrength-dx) * fairnessGain(result)
}
multicriteriaUtility <- function(result){
  wealthStrength * wealthGain(result) +
    fairnessStrength * fairnessGain(result)
}

#For every function, make it a probability distribution function?
thresholdUtilityS <-function(result,difference){
  dx = difference/2
  -(wealthStrength+dx)/(wealthGain(result)+0.5) - (fairnessStrength-dx)/(fairnessGain(result)+0.5)
}

thresholdUtility <-function(result){
  -wealthStrength/(wealthGain(result)+0.5) -fairnessStrength/(fairnessGain(result) +0.5)
}

thresholdUtility2S <-function(result, difference){
  dx = difference/2
  wealthGain(result) -(wealthStrength+dx) +  fairnessGain(result) - (fairnessStrength-dx)
}

thresholdUtility2 <-function(result){
  wealthGain(result) -wealthStrength +  fairnessGain(result) - fairnessStrength
}

thresholdMemoryUtility <- function(difference,k){
  df = as.data.frame(matrix(ncol=106, nrow=1))
  dx = difference/2
  b <-intention(df,1,1+dx,1-dx,1,1,k)
  b <- b[-1,-c(1:6)]
  #b <- t(scale(t(b))) #scaled AND removes iteration colum, now result is colid
  unname(colMeans(b)) #hmm kan alleen van 1:100
}




#ToDo for responder
#ToDo for 

#for every offer, what is the utility of that offer
#ToDo: memory nog echt meenemen

curve(gain(x),1,pieSize)
curve(multicriteriaUtility(x),1,pieSize)
curve(thresholdUtility2(x),1,pieSize)
curve(thresholdUtility(x),1,pieSize)
plot(needDim) #need function, for satE=1, a t=1;
plot(hist(b$demands))
plot(b$demands) #je maximale needDim schuift naar 66 blijkbaar... #waarom?... tjah



