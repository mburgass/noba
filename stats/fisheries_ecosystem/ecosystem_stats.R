library(tidyverse)
library(car)

##Explore data

read.csv("stats/fisheries_ecosystem/biopp.csv")  %>% gather(scenario, biopp, 2:4) -> biopp

fitpp0<- lm(biopp ~ poly(year,3), data=biopp)
fitpp1<- lm(biopp ~ poly(year, 3) * scenario, data=biopp)
####Plotting regressions######
dat.a = data.frame(year= 1981:2015, scenario='fmsy0')
intervals.a <- predict(fitpp1,newdata = dat.a,interval='confidence',
                       level=0.95)
data.frame(intervals.a) -> intervals.a
intervals.a%>% mutate(year = 1981:2015) %>% select(year, everything()) %>% mutate(scenario='fmsy0')-> intervals.a

dat.b = data.frame(year= 1981:2015, scenario='fmsy1')
intervals.b<- predict(fitpp1, newdata = dat.b, interval='confidence', level=0.95)
data.frame(intervals.b) -> intervals.b
intervals.b%>% mutate(year = 1981:2015) %>% select(year, everything())%>% mutate(scenario='fmsy1') -> intervals.b
dat.c = data.frame(year= 1981:2015, scenario='fmsy2')
intervals.c<- predict(fitpp1, newdata = dat.c, interval='confidence', level=0.95)
data.frame(intervals.c) -> intervals.c
intervals.c%>% mutate(year = 1981:2015) %>% select(year, everything()) %>% mutate(scenario='fmsy2') -> intervals.c

new_intervals<- rbind(intervals.a, intervals.b, intervals.c)

ggplot(new_intervals, aes(x =year, y = fit)) +
  theme_bw() +
  geom_line(aes(colour=scenario)) +
  geom_smooth(aes(ymin = lwr, ymax = upr, colour=scenario), stat = "identity") +
  geom_point(data = biopp, aes(x = year, y = biopp, colour=scenario), size=1)+
  xlab("Year") +
  ylab("BioPP") +
  ggtitle("Regressions of BioPP for three fishing scenarios")


######## DemPel#############
read.csv("stats/fisheries_ecosystem/dempel.csv")  %>% gather(scenario, dempel, 2:4) -> dempel

fitdp0<- lm(dempel ~ poly(year,3), data=dempel)
fitdp1<- lm(dempel ~ poly(year,3) * scenario, data=dempel)
####Plotting regressions######
dat.a = data.frame(year= 1981:2015, scenario='fmsy0')
intervals.a <- predict(fitdp1,newdata = dat.a,interval='confidence',
                       level=0.95)
data.frame(intervals.a) -> intervals.a
intervals.a%>% mutate(year = 1981:2015) %>% select(year, everything()) %>% mutate(scenario='fmsy0')-> intervals.a

dat.b = data.frame(year= 1981:2015, scenario='fmsy1')
intervals.b<- predict(fitdp1, newdata = dat.b, interval='confidence', level=0.95)
data.frame(intervals.b) -> intervals.b
intervals.b%>% mutate(year = 1981:2015) %>% select(year, everything())%>% mutate(scenario='fmsy1') -> intervals.b
dat.c = data.frame(year= 1981:2015, scenario='fmsy2')
intervals.c<- predict(fitdp1, newdata = dat.c, interval='confidence', level=0.95)
data.frame(intervals.c) -> intervals.c
intervals.c%>% mutate(year = 1981:2015) %>% select(year, everything()) %>% mutate(scenario='fmsy2') -> intervals.c

new_intervals<- rbind(intervals.a, intervals.b, intervals.c)

ggplot(new_intervals, aes(x =year, y = fit)) +
  theme_bw() +
  geom_line(aes(colour=scenario)) +
  geom_smooth(aes(ymin = lwr, ymax = upr, colour=scenario), stat = "identity") +
  geom_point(data = dempel, aes(x = year, y = dempel, colour=scenario), size=1)+
  xlab("Year") +
  ylab("DemPel") +
  ggtitle("Regressions of DemPel for three fishing scenarios")

#####PelBioPP######
read.csv("stats/fisheries_ecosystem/pelbiopp.csv")  %>% gather(scenario, pelbiopp, 2:4) -> pelbiopp
#pelbiopp<- pelbiopp %>% filter(scenario %in% c("fmsy0", "fmsy2"))

fitpbp0<- lm(pelbiopp ~ poly(year, 3), data=pelbiopp)
fitpbp1<- lm(pelbiopp ~ poly(year, 3) * scenario, data=pelbiopp)
####Plotting regressions######
dat.a = data.frame(year= 1981:2015, scenario='fmsy0')
intervals.a <- predict(fitpbp1,newdata = dat.a,interval='confidence',
                       level=0.95)
data.frame(intervals.a) -> intervals.a
intervals.a%>% mutate(year = 1981:2015) %>% select(year, everything()) %>% mutate(scenario='fmsy0')-> intervals.a

dat.b = data.frame(year= 1981:2015, scenario='fmsy1')
intervals.b<- predict(fitpbp1, newdata = dat.b, interval='confidence', level=0.95)
data.frame(intervals.b) -> intervals.b
intervals.b%>% mutate(year = 1981:2015) %>% select(year, everything())%>% mutate(scenario='fmsy1') -> intervals.b
dat.c = data.frame(year= 1981:2015, scenario='fmsy2')
intervals.c<- predict(fitpbp1, newdata = dat.c, interval='confidence', level=0.95)
data.frame(intervals.c) -> intervals.c
intervals.c%>% mutate(year = 1981:2015) %>% select(year, everything()) %>% mutate(scenario='fmsy2') -> intervals.c

new_intervals<- rbind(intervals.a, intervals.b, intervals.c)

ggplot(new_intervals, aes(x =year, y = fit)) +
  theme_bw() +
  geom_line(aes(colour=scenario)) +
  geom_smooth(aes(ymin = lwr, ymax = upr, colour=scenario), stat = "identity") +
  geom_point(data = pelbiopp, aes(x = year, y = pelbiopp, colour=scenario), size=1)+
  xlab("Year") +
  ylab("PelBioPP") +
  ggtitle("Regressions of PelBioPP for three fishing scenarios")