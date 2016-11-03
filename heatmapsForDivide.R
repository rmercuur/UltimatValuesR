library(ggplot2)
library(reshape2)
library(dplyr)

dg = 0.5
fairnessStrength = 1.0

thresholdUtility <-function(result){
  -wealthStrength/(wealthGain(result)+dg) -fairnessStrength/(fairnessGain(result) +dg)
}

thresholdUtilityS <-function(result,difference){
  dx = difference/2
  -((wealthStrength+dx)/(wealthGain(result)+dg)) - ((fairnessStrength-dx)/(fairnessGain(result)+dg))
}
range01 <- function(x){(x-min(x))/(max(x)-min(x))}

#plots
demands <- c(1:100)
curve(thresholdUtility(x),1,pieSize)
rejectUtility <- -wealthStrength/(wealthGain(0)+dg) -fairnessStrength/(fairnessGain(50) +dg)

abline(h = rejectUtility, col = "gray")

strengthDifference <-seq(-1,1,by=0.05)

df <- expand.grid(demands=demands, strengthDifference=strengthDifference)
df$thresholdDivide <- with(df, thresholdUtilityS(demands,strengthDifference))
#df$thresholdDivide <- range01(df$thresholdDivide)

df.melt <- melt(df, id=c("demands","strengthDifference"))


#maximum and minimums per strengthDifference
df.melt %>%
  group_by(variable, strengthDifference) %>%
  summarise(min = min(value),
            max = max(value)) -> df.melt.2

left_join(df.melt, df.melt.2) %>%
  mutate(color = value == min | value == max) %>%
  filter(color == TRUE) -> localMax


m <- ggplot(df.melt, aes(demands, strengthDifference))
myplot <- m + geom_raster(aes(fill = value), hjust=0.5,
                          vjust=0.3, interpolate=FALSE) +
  facet_grid(.~variable,scales="free") +
  scale_fill_gradient(
    low = "red",
    high = "yellow") +
  geom_point(data=localMax,aes(x = demands, y = strengthDifference), color = "black")# +
#  geom_point(data=globalMax,aes(x = demands, y = strengthDifference), color = "blue")


plot(myplot)


#maybe we can plot the maximum utilities
m <- ggplot(df.melt, aes(demands, strengthDifference))
myplot2 <- m+geom_point(data=localMax,aes(x = demands, y = strengthDifference), color = "black")
plot(myplot2)

#seems at 0.35
curve(thresholdUtilityS(x,0.35),1,pieSize)
points(which.max(thresholdUtilityS(1:100,0.35)),max(thresholdUtilityS(1:100,0.35)))

