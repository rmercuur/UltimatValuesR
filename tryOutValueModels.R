wealthStrength = 2.2
fairnessStrength = 1.0
pieSize = 10
pieSizeMed = pieSize/2.0

wealthGain <- function(result){
 result/10
}

fairnessGain <- function(result){
  1-abs(pieSizeMed-result)/pieSizeMed
}

multicriteriaUtility <- function(result){
  wealthStrength * wealthGain(result) +
    fairnessStrength * fairnessGain(result)
}

#For every function, make it a probability distribution function?
thresholdUtility <-function(result){
  wealthStrength/wealthGain(result) + fairnessStrength/fairnessGain(result)
}

thresholdUtility2 <-function(result){
  wealthGain(result) -wealthStrength +  fairnessGain(result) - fairnessStrength
}

memoryTesholdUtility <- function(result){
  timesActionWasChosen(result)/totalIterations
  #hmm look into previous defnitoin of this and such...
  timesActionWasChosen(result)
}


#ToDo for responder
#ToDo for 

curve(multicriteriaUtility(x),1,pieSize)
curve(thresholdUtility(x),1,pieSize)
curve(thresholdUtility2(x),1,pieSize)

