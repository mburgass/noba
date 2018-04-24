library(tidyverse)
library(car)

mydata1 <- subset(iris, Species == "setosa", select = c(Sepal.Length, Sepal.Width))
mydata2 <- subset(iris, Species == "virginica", select = c(Sepal.Length, Sepal.Width))

#add a grouping variable
mydata1$g <- "a"
mydata2$g <- "b"

#combine the datasets
mydata <- rbind(mydata1, mydata2)

#model without grouping variable
fit0 <- lm(Sepal.Width ~ poly(Sepal.Length, 2), data = mydata)

#model with grouping variable
fit1 <- lm(Sepal.Width ~ poly(Sepal.Length, 2) * g, data = mydata)

#compare models 
anova(fit0, fit1)

#####
read.csv("stats/lpi/lpiscores_fmsy0.csv")  -> lpi_fmsy0
read.csv("stats/lpi/lpiscores_fmsy2.csv") -> lpi_fmsy2
read.csv("stats/lpi/lpiscores_fmsy1.csv") -> lpi_fmsy1

lpi_fmsy0$g<- "1"
lpi_fmsy2$g<- "2"
lpi_fmsy1$g<- "2"


lpi<- rbind(lpi_fmsy2, lpi_fmsy1, lpi_fmsy0)
lpi$year<- as.numeric(lpi$year)

fit0 <- lm(LPI_final ~ poly(year, 2), data = lpi)


fit1 <- lm(LPI_final ~ poly(year, 3) * scenario, data = lpi)

year = seq(min(2016),max(2050),by = 1)
year$g<- 'd'
conf_interval <- predict(fit1, newdata=data.frame(x=year), interval="confidence",
                         level = 0.95)
test2<- anova(fit0, fit1)
plot(fit0)

read.csv("stats/fisheries_ecosystem/biopp.csv")  %>% gather(g, biopp, 2:4) -> biopp

fitpp0<- lm(biopp ~ year, data=biopp)
fitpp1<- lm(biopp ~ year + g, data=biopp)

test<- anova(fitpp0, fitpp1)

biopp.intervals <- predict(fitpp0,newdata = newdat,interval='confidence',
                               level=0.95)
data.frame(biopp.intervals) -> biopp.intervals 
biopp.intervals%>% mutate(year = 1981:2015) %>% select(year, everything()) -> biopp.intervals

ggplot(biopp.intervals, aes(x =year, y = fit)) +
  theme_bw() +
  geom_line() +
  geom_smooth(aes(ymin = lwr, ymax = upr), stat = "identity") +
  geom_point(data = biopp, aes(x = year, y = biopp, colour=g))


plot(biopp ~ year, data=biopp)
lines(biopp$year, predict(lm(biopp~poly(year, 2) *g ,data=biopp) ),   
      col="red") 
summary(test)

abline(fitpp0)

plot(LPI_final ~ year, data=lpi)
lines(lpi$year,predict(lm(LPI_final~poly(year, 2),data=lpi))) 


#######

x=c(1,2,3,4,5,6,7,8,9,0)

y=c(13,28,43,35,96,84,101,110,108,13)

lm.out <- lm(y ~ x)
newx = seq(min(x),max(x),by = 0.05)
conf_interval <- predict(lm.out, newdata=data.frame(x=newx), interval="confidence",
                         level = 0.95)

plot(x, y, xlab="x", ylab="y", main="Regression")
abline(lm.out, col="lightblue")
lines(newx, conf_interval[,2], col="blue", lty=2)
lines(newx, conf_interval[,3], col="blue", lty=2)

###

newdat = data.frame(year= 1981:2015, g='a')
#pred = predict(fit1, newdata = newdat, se.fit = T)

#newdat$lci <- pred$fit - 1.96 * pred$se.fit
#newdat$fit <- pred$fit
#newdat$uci <- pred$fit + 1.96 * pred$se.fit


new_intervals<- rbind(predicted.intervals, intervals.b, intervals.c)

ggplot(new_intervals, aes(x =year, y = fit)) +
  theme_bw() +
  geom_line(aes(colour=g)) +
  geom_smooth(aes(ymin = lwr, ymax = upr, colour=g), stat = "identity") +
  geom_point(data = lpi, aes(x = year, y = LPI_final, colour=g))+
  xlab("Year") +
  ylab("LPI Score") +
  ggtitle("Regressions of Living Planet Index Score for three fishing scenarios")+
  scale_fill_discrete("Fishing scenario", 
                      labels=c("no fishing", "fmsy1", "fmsy2"))

plot(fitted(fit0),residuals(fit0))
predicted.intervals <- predict(fit1,newdata = newdat,interval='confidence',
                               level=0.95)
data.frame(predicted.intervals) -> predicted.intervals 
predicted.intervals%>% mutate(year = 1981:2015) %>% select(year, everything()) %>% mutate(g='a')-> predicted.intervals

dat.b = data.frame(year= 1981:2015, g='b')
intervals.b<- predict(fit1, newdata = dat.b, interval='confidence', level=0.95)
data.frame(intervals.b) -> intervals.b
intervals.b%>% mutate(year = 1981:2015) %>% select(year, everything())%>% mutate(g='b') -> intervals.b
dat.c = data.frame(year= 1981:2015, g='c')
intervals.c<- predict(fit1, newdata = dat.c, interval='confidence', level=0.95)
data.frame(intervals.c) -> intervals.c
intervals.c%>% mutate(year = 1981:2015) %>% select(year, everything()) %>% mutate(g='c') -> intervals.c

lines(predicted.intervals[,2],col='green',lwd=3)

qqnorm(lpi$LPI_final, pch = 1, frame = FALSE)
qqline(lpi$LPI_final, col = "steelblue", lwd = 2)
qqPlot(lpi_fmsy2$LPI_final)

summary(p1 <- powerTransform(lpi_fmsy0))
