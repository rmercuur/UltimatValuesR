library(ggplot2)
library(reshape2)
library(dplyr)

##Example
#seals$z <- with(seals, sqrt(delta_long^2 + delta_lat^2))
#m <- ggplot(seals, aes(long, lat))
#myplot <- m + geom_raster(aes(fill = z), hjust=0.5,
#                          vjust=0.5, interpolate=FALSE)
#plot(myplot)

demands <- c(1:100)
strengthDifference <-seq(0.5,1,by=0.1)

df <- expand.grid(demands=demands, strengthDifference=strengthDifference)
df$product <- with(df, multicriteriaUtilityS(demands,strengthDifference))
df$product <- scale(df$product)
df$thresholdDifference <- with(df, thresholdUtility2S(demands,strengthDifference))
df$thresholdDifference <- scale(df$thresholdDifference)
df$thresholdDivide <- with(df, thresholdUtilityS(demands,strengthDifference))
df$thresholdDivide <- scale(df$thresholdDivide)

#memory toevoegen
#heatmap of avarage (over time) utility of memory per x
dfmem <- strengthDifference
dfmem <- sapply(strengthDifference,thresholdMemoryUtility)
colnames(dfmem) <- strengthDifference
dfmem.melt <- melt(dfmem)

df$memory <- dfmem.melt$value

#one plot
# m <- ggplot(df, aes(demands, strengthDifference))
# myplot <- m + geom_raster(aes(fill = product), hjust=0.5,
#                          vjust=0.3, interpolate=FALSE) +
   # scale_fill_gradient(
   #  low = "red",
   #  high = "yellow")
# plot(myplot)



#facet
df.melt <- melt(df, id=c("demands","strengthDifference"))
m <- ggplot(df.melt, aes(demands, strengthDifference))
myplot <- m + geom_raster(aes(fill = value), hjust=0.5,
                          vjust=0.3, interpolate=FALSE) +
            facet_grid(.~variable,scales="free") +
  scale_fill_gradient(
    low = "red",
    high = "yellow")


plot(myplot)


#maximum and minimums per strengthDifference
df.melt %>%
  group_by(variable, strengthDifference) %>%
  summarise(min = min(value),
            max = max(value)) -> df.melt.2

left_join(df.melt, df.melt.2) %>%
  mutate(color = value == min | value == max) %>%
  filter(color == TRUE) -> localMax

#maximum of function
df.melt %>%
  group_by(variable) %>%
  summarise(min = min(value),
            max = max(value)) -> df.melt.3

left_join(df.melt, df.melt.2) %>%
  mutate(color = value == min | value == max) %>%
  filter(color == TRUE) -> globalMax

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


# ##heatmap of memoryfunction with dif=0
# df = as.data.frame(matrix(ncol=106, nrow=1))
# names(df) = c("it","SE", "ST", "satE", "satT", "demands")
# b <-intention(df,1,1,1,1,1,0,0)
# b.demands <- b[-1,c("it","demands")]
# colnames(b.demands)<- c("iteration","variable")
# b <- b[-1,-c(2:6)]
# colnames(b) <- c("iteration",1:100)
# b[,-1] <- t(scale(t(b[,-1])))
# b.melt <- melt(b, id=c("iteration"))
# m <- ggplot(b.melt, aes(iteration, variable))
# myplot <- m + geom_raster(aes(fill = value), hjust=0.5,
#                           vjust=0.3, interpolate=FALSE) +
#   scale_fill_gradient(
#     low = "red",
#     high = "yellow") +
#   geom_point(data=b.demands,aes(x = iteration, y = variable), color = "black")
# 
# plot(myplot)
# 
# 
# #heatmap of avarage (over time) utility of memory per x
# df <- strengthDifference
# df <- sapply(strengthDifference,thresholdMemoryUtility)
# colnames(df) <- strengthDifference
# df.melt <- melt(df)
# colnames(df.melt) <- c("demands","strengthDifference")
# #facet
# df.melt <- melt(df, id=c("demands","strengthDifference"))
# m <- ggplot(df.melt, aes(demands, strengthDifference))
# myplot <- m + geom_raster(aes(fill = value), hjust=0.5,
#                           vjust=0.3, interpolate=FALSE) +
#   scale_fill_gradient(
#     low = "red",
#     high = "yellow")
# 
# 
# plot(myplot)
