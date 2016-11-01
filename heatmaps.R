library(ggplot2)
library(reshape2)
library(dplyr)

range01 <- function(x){(x-min(x))/(max(x)-min(x))}

##Example
#seals$z <- with(seals, sqrt(delta_long^2 + delta_lat^2))
#m <- ggplot(seals, aes(long, lat))
#myplot <- m + geom_raster(aes(fill = z), hjust=0.5,
#                          vjust=0.5, interpolate=FALSE)
#plot(myplot)

demands <- c(1:100)
strengthDifference <-seq(-1,1,by=0.1)

df <- expand.grid(demands=demands, strengthDifference=strengthDifference)
df$product <- with(df, multicriteriaUtilityS(demands,strengthDifference))
df$product <- range01(df$product)
df$thresholdDifference <- with(df, thresholdUtility2S(demands,strengthDifference))
df$thresholdDifference <- range01(df$thresholdDifference)
df$thresholdDivide <- with(df, thresholdUtilityS(demands,strengthDifference))
df$thresholdDivide <- range01(df$thresholdDivide)

#memory toevoegen
#heatmap of avarage (over time) utility of memory per x 0.5
dfmem <- strengthDifference
dfmem <- sapply(strengthDifference,thresholdMemoryUtility,0.5)
colnames(dfmem) <- strengthDifference
dfmem.melt <- melt(dfmem)
df$memory05 <- dfmem.melt$value
df$memory05 <- range01(df$memory05)

#heatmap of avarage (over time) utility of memory per x 1.0
dfmem <- strengthDifference
dfmem <- sapply(strengthDifference,thresholdMemoryUtility,1.0)
colnames(dfmem) <- strengthDifference
dfmem.melt <- melt(dfmem)
df$memory10 <- dfmem.melt$value
df$memory10 <- range01(df$memory10)
# 
# #heatmap of avarage (over time) utility of memory per x 1.5
# dfmem <- strengthDifference
# dfmem <- sapply(strengthDifference,thresholdMemoryUtility,1.5)
# colnames(dfmem) <- strengthDifference
# dfmem.melt <- melt(dfmem)
# df$memory15 <- dfmem.melt$value
# df$memory15 <- range01(df$memory15)

#one plot
# m <- ggplot(df, aes(demands, strengthDifference))
# myplot <- m + geom_raster(aes(fill = product), hjust=0.5,
#                          vjust=0.3, interpolate=FALSE) +
   # scale_fill_gradient(
   #  low = "red",
   #  high = "yellow")
# plot(myplot)




df.melt <- melt(df, id=c("demands","strengthDifference"))

# #plot without maxmin
# m <- ggplot(df.melt, aes(demands, strengthDifference))
# myplot <- m + geom_raster(aes(fill = value), hjust=0.5,
#                           vjust=0.3, interpolate=FALSE) +
#             facet_grid(.~variable,scales="free") +
#   scale_fill_gradient(
#     low = "red",
#     high = "yellow")
# 
# 
# plot(myplot)


#maximum and minimums per strengthDifference
df.melt %>%
  group_by(variable, strengthDifference) %>%
  summarise(min = min(value),
            max = max(value)) -> df.melt.2

left_join(df.melt, df.melt.2) %>%
  mutate(color = value == min | value == max) %>%
  filter(color == TRUE) -> localMax

# #maximum of function
# df.melt %>%
#   group_by(variable) %>%
#   summarise(min = min(value),
#             max = max(value)) -> df.melt.3
# 
# left_join(df.melt, df.melt.2) %>%
#   mutate(color = value == min | value == max) %>%
#   filter(color == TRUE) -> globalMax

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
# b <-intention(df,1,1,1,1,1)
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
