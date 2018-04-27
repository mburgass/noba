library(tidyverse)
library(car)

##Explore data

read.csv("stats/fisheries_ecosystem/biopp.csv")  %>% gather(scenario, biopp, 2:4) -> biopp




fitpp0<- lm(biopp ~ poly(year,2), data=biopp)
fitpp1<- lm(biopp ~ poly(year, 2) * scenario, data=biopp)
fitpp2<- lm(biopp ~ poly(year, 3) * scenario, data=biopp)
anova(fitpp0, fitpp1)
##Non significant difference between 2 and 3. Non significant difference w/ or w/o scenario

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
  ggtitle("Regressions of BioPP for three fishing scenarios; P<2.2e-16, R2=0.7514")


######## DemPel#############
read.csv("stats/fisheries_ecosystem/dempel.csv")  %>% gather(scenario, dempel, 2:4) -> dempel



fitdp0<- lm(dempel ~ poly(year,5), data=dempel)
fitdp1<- lm(dempel ~ poly(year,5) * scenario, data=dempel)
fitdp2<- lm(dempel ~ poly(year,6) * scenario, data=dempel)
anova(fitdp1, fitdp2)

dempel_fmsy1<- filter(dempel, scenario %in% c("fmsy1", "fmsy2")) ###These lines distinguish whether 
#there is a difference between fmsy1 and fmsy2
fitdpfm1<- lm(dempel~poly(year,3), data=dempel_fmsy1)
fitdpfm2<- lm(dempel~poly(year,3) * scenario, data=dempel_fmsy1)
anova(fitdpfm1, fitdpfm2)

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
  ggtitle("Regressions of DemPel for three fishing scenarios; P<2.2e-16, R2=0.9396")

#####PelBioPP######
read.csv("stats/fisheries_ecosystem/pelbiopp.csv")  %>% gather(scenario, pelbiopp, 2:4) -> pelbiopp
#pelbiopp<- pelbiopp %>% filter(scenario %in% c("fmsy0", "fmsy2"))

fitpbp0<- lm(pelbiopp ~ poly(year, 2), data=pelbiopp)
fitpbp1<- lm(pelbiopp ~ poly(year, 2) * scenario, data=pelbiopp)
#fitpbp2<- lm(pelbiopp ~ poly(year, 3) * scenario, data=pelbiopp)
#anova(fitpbp0, fitpbp1)
pbp_fmsy1<- filter(pelbiopp, scenario %in% c("fmsy0", "fmsy2")) ###These lines distinguish whether 
#there is a difference between fmsy1 and fmsy2
pbpfm1<- lm(pelbiopp~poly(year,2), data=pbp_fmsy1)
pbpfm2<- lm(pelbiopp~poly(year,2) * scenario, data=pbp_fmsy1)
anova(pbpfm1, pbpfm2)

##No significant difference between FMSY0, FMSY2


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
  ggtitle("Regressions of PelBioPP for three fishing scenarios; P = 0.00837, R2 = 0.1878")


#####DemPP#########
read.csv("stats/fisheries_ecosystem/dempp.csv")  -> dempp
#pelbiopp<- pelbiopp %>% filter(scenario %in% c("fmsy0", "fmsy2"))

fitdpp0<- lm(DemPP ~ poly(year, 3), data=dempp)
fitdpp1<- lm(DemPP ~ poly(year, 3) * scenario, data=dempp)
anova(fitdpp0, fitdpp1)

####Plotting regressions######
dat.a = data.frame(year= 1981:2015, scenario='fmsy0')
intervals.a <- predict(fitdpp1,newdata = dat.a,interval='confidence',
                       level=0.95)
data.frame(intervals.a) -> intervals.a
intervals.a%>% mutate(year = 1981:2015) %>% select(year, everything()) %>% mutate(scenario='fmsy0')-> intervals.a

dat.b = data.frame(year= 1981:2015, scenario='fmsy1')
intervals.b<- predict(fitdpp1, newdata = dat.b, interval='confidence', level=0.95)
data.frame(intervals.b) -> intervals.b
intervals.b%>% mutate(year = 1981:2015) %>% select(year, everything())%>% mutate(scenario='fmsy1') -> intervals.b
dat.c = data.frame(year= 1981:2015, scenario='fmsy2')
intervals.c<- predict(fitdpp1, newdata = dat.c, interval='confidence', level=0.95)
data.frame(intervals.c) -> intervals.c
intervals.c%>% mutate(year = 1981:2015) %>% select(year, everything()) %>% mutate(scenario='fmsy2') -> intervals.c

new_intervals<- rbind(intervals.a, intervals.b, intervals.c)

ggplot(new_intervals, aes(x =year, y = fit)) +
  theme_bw() +
  geom_line(aes(colour=scenario)) +
  geom_smooth(aes(ymin = lwr, ymax = upr, colour=scenario), stat = "identity") +
  geom_point(data = dempp, aes(x = year, y = DemPP, colour=scenario), size=1)+
  xlab("Year") +
  ylab("DemPP") +
  ggtitle("Regressions of DemPP for three fishing scenarios; P < 2.2e-16, R2 0.8439")

#####PropPel#########
read.csv("stats/fisheries_ecosystem/proppel.csv")  -> proppel
#pelbiopp<- pelbiopp %>% filter(scenario %in% c("fmsy0", "fmsy2"))

fitproppel0<- lm(PropPel ~ poly(year, 2), data=proppel)
fitproppel1<- lm(PropPel ~ poly(year, 2) * scenario, data=proppel)

anova(fitproppel0, fitproppel1)

proppel_fmsy1<- filter(proppel, scenario %in% c("fmsy1", "fmsy2")) ###These lines distinguish whether 
#there is a difference between fmsy1 and fmsy2
proppelfm1<- lm(PropPel~poly(year,2), data=proppel_fmsy1)
proppelfm2<- lm(PropPel~poly(year,2) * scenario, data=proppel_fmsy1)
anova(proppelfm1, proppelfm2)

##Weak significance between (FMSY0, FMSy2) + (FMSY0, FMSY1) +(FMSY1, FMSY2) P= 0.01-0.05

####Plotting regressions######
dat.a = data.frame(year= 1981:2015, scenario='fmsy0')
intervals.a <- predict(fitproppel1,newdata = dat.a,interval='confidence',
                       level=0.95)
data.frame(intervals.a) -> intervals.a
intervals.a%>% mutate(year = 1981:2015) %>% select(year, everything()) %>% mutate(scenario='fmsy0')-> intervals.a

dat.b = data.frame(year= 1981:2015, scenario='fmsy1')
intervals.b<- predict(fitproppel1, newdata = dat.b, interval='confidence', level=0.95)
data.frame(intervals.b) -> intervals.b
intervals.b%>% mutate(year = 1981:2015) %>% select(year, everything())%>% mutate(scenario='fmsy1') -> intervals.b
dat.c = data.frame(year= 1981:2015, scenario='fmsy2')
intervals.c<- predict(fitproppel1, newdata = dat.c, interval='confidence', level=0.95)
data.frame(intervals.c) -> intervals.c
intervals.c%>% mutate(year = 1981:2015) %>% select(year, everything()) %>% mutate(scenario='fmsy2') -> intervals.c

new_intervals<- rbind(intervals.a, intervals.b, intervals.c)

ggplot(new_intervals, aes(x =year, y = fit)) +
  theme_bw() +
  geom_line(aes(colour=scenario)) +
  geom_smooth(aes(ymin = lwr, ymax = upr, colour=scenario), stat = "identity") +
  geom_point(data = proppel, aes(x = year, y = PropPel, colour=scenario), size=1)+
  xlab("Year") +
  ylab("PropPel") +
  ggtitle("Regressions of PropPel for three fishing scenarios; P= 2.554e-07, R2 0.371")
