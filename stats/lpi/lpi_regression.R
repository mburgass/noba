library(mgcv)
library(tidyverse)
read.csv("stats/lpi/lpiscores_fmsy0.csv")  -> lpi_fmsy0
read.csv("stats/lpi/lpiscores_fmsy2.csv") -> lpi_fmsy2
read.csv("stats/lpi/lpiscores_fmsy1.csv") -> lpi_fmsy1

lpi<- rbind(lpi_fmsy2, lpi_fmsy1, lpi_fmsy0)

##Fitting Models
fit0 <- lm(LPI_final ~ poly(year, 2), data = lpi)

fit1 <- lm(LPI_final ~ poly(year, 3) * scenario, data = lpi)

####Plotting regressions######
dat.a = data.frame(year= 1981:2015, scenario='fmsy0')
intervals.a <- predict(fit1,newdata = dat.a,interval='confidence',
                               level=0.95)
data.frame(intervals.a) -> intervals.a
intervals.a%>% mutate(year = 1981:2015) %>% select(year, everything()) %>% mutate(scenario='fmsy0')-> intervals.a

dat.b = data.frame(year= 1981:2015, scenario='fmsy1')
intervals.b<- predict(fit1, newdata = dat.b,interval='confidence', level=0.95)
data.frame(intervals.b) -> intervals.b
intervals.b%>% mutate(year = 1981:2015) %>% select(year, everything())%>% mutate(scenario='fmsy1') -> intervals.b
dat.c = data.frame(year= 1981:2015, scenario='fmsy2')
intervals.c<- predict(fit1_gam, newdata = dat.c, interval='confidence', level=0.95)
data.frame(intervals.c) -> intervals.c
intervals.c%>% mutate(year = 1981:2015) %>% select(year, everything()) %>% mutate(scenario='fmsy2') -> intervals.c

new_intervals<- rbind(intervals.a, intervals.b, intervals.c)

ggplot(new_intervals, aes(x =year, y = fit)) +
  theme_bw() +
  geom_line(aes(colour=scenario)) +
  geom_smooth(aes(ymin = lwr, ymax = upr, colour=scenario), stat = "identity") +
  geom_point(data = lpi, aes(x = year, y = LPI_final, colour=scenario), size=1)+
  xlab("Year") +
  ylab("LPI Score") +
  ggtitle("Regressions of Living Planet Index Score for three fishing scenarios")



######TETING GAM########
lpi$year<- as.numeric(lpi$year)
fit0_gam<- gam(LPI_final ~ s(year, bs="cr"), data=lpi)
fit1_gam<- gam(LPI_final ~ s(year, bs="cr")+scenario, data=lpi)


####Plotting regressions######
dat.a = data.frame(year= 1981:2015, scenario='fmsy0')
intervals.a <- predict(fit1_gam,newdata = dat.a, type="response", se=T)
predicts = data.frame(dat.a, intervals.a) %>% 
  mutate(lower = fit - 1.96*se.fit,
         upper = fit + 1.96*se.fit)

ggplot(aes(x=year,y=fit), data=predicts) +
  geom_ribbon(aes(ymin = lower, ymax=upper), fill='gray90') +
  geom_line(color='#00aaff') #+
  #theme_trueMinimal()

dat.b = data.frame(year= 1981:2015, scenario='fmsy1')
intervals.b<- predict(fit1, newdata = dat.b,interval='confidence', level=0.95)
data.frame(intervals.b) -> intervals.b
intervals.b%>% mutate(year = 1981:2015) %>% select(year, everything())%>% mutate(scenario='fmsy1') -> intervals.b
dat.c = data.frame(year= 1981:2015, scenario='fmsy2')
intervals.c<- predict(fit1_gam, newdata = dat.c, interval='confidence', level=0.95)
data.frame(intervals.c) -> intervals.c
intervals.c%>% mutate(year = 1981:2015) %>% select(year, everything()) %>% mutate(scenario='fmsy2') -> intervals.c

new_intervals<- rbind(intervals.a, intervals.b, intervals.c)