library(tidyverse)
library(car)
library(cowplot)

####SURVYED BIOMASS######
read.csv("stats/indiseas/surveyed_biomass.csv")  -> surbio

surbio0<- lm(total_biomass_surveyed ~ poly(year, 2), data=surbio)
surbio1<- lm(total_biomass_surveyed ~ poly(year, 2) * scenario, data=surbio)
#surbio2<- lm(total_biomass_surveyed ~ poly(year, 3) * scenario, data=surbio)

anova(surbio0,surbio1)
##Insignificant difference between regressions

#Plotting regressions#
dat.a = data.frame(year= 1981:2015, scenario='fmsy0')
intervals.a <- predict(surbio1,newdata = dat.a,interval='confidence',
                       level=0.95)
data.frame(intervals.a) -> intervals.a
intervals.a%>% mutate(year = 1981:2015) %>% select(year, everything()) %>% mutate(scenario='fmsy0')-> intervals.a

dat.b = data.frame(year= 1981:2015, scenario='fmsy1')
intervals.b<- predict(surbio1, newdata = dat.b, interval='confidence', level=0.95)
data.frame(intervals.b) -> intervals.b
intervals.b%>% mutate(year = 1981:2015) %>% select(year, everything())%>% mutate(scenario='fmsy1') -> intervals.b
dat.c = data.frame(year= 1981:2015, scenario='fmsy2')
intervals.c<- predict(surbio1, newdata = dat.c, interval='confidence', level=0.95)
data.frame(intervals.c) -> intervals.c
intervals.c%>% mutate(year = 1981:2015) %>% select(year, everything()) %>% mutate(scenario='fmsy2') -> intervals.c

new_intervals_surbio<- rbind(intervals.a, intervals.b, intervals.c)
legend_test<-ggplot(new_intervals_surbio, aes(x =year, y = fit)) +
  theme_bw() +
  geom_line(aes(colour=scenario)) +
  geom_smooth(aes(ymin = lwr, ymax = upr, colour=scenario), stat = "identity") +
  geom_point(data = surbio, aes(x = year, y = total_biomass_surveyed, colour=scenario), size=1)+
  xlab("Year") +
  ylab("SurBio") +
  ggtitle("Regressions of Surveyed Biomass for three fishing scenarios; P= 0.4402, R2 0.07702")

#####PROPPRED SURVEYED#########
read.csv("stats/indiseas/proppred_surveyed.csv")  -> pred

pred0<- lm(proppred ~ poly(year, 4), data=pred)
pred1<- lm(proppred ~ poly(year, 4) * scenario, data=pred)
#pred2<- lm(proppred ~ poly(year, 5) * scenario, data=pred)

anova(pred0,pred1)
##Significant difference between regressions

#Plotting regressions#
dat.a = data.frame(year= 1981:2015, scenario='fmsy0')
intervals.a <- predict(pred1,newdata = dat.a,interval='confidence',
                       level=0.95)
data.frame(intervals.a) -> intervals.a
intervals.a%>% mutate(year = 1981:2015) %>% select(year, everything()) %>% mutate(scenario='fmsy0')-> intervals.a

dat.b = data.frame(year= 1981:2015, scenario='fmsy1')
intervals.b<- predict(pred1, newdata = dat.b, interval='confidence', level=0.95)
data.frame(intervals.b) -> intervals.b
intervals.b%>% mutate(year = 1981:2015) %>% select(year, everything())%>% mutate(scenario='fmsy1') -> intervals.b
dat.c = data.frame(year= 1981:2015, scenario='fmsy2')
intervals.c<- predict(pred1, newdata = dat.c, interval='confidence', level=0.95)
data.frame(intervals.c) -> intervals.c
intervals.c%>% mutate(year = 1981:2015) %>% select(year, everything()) %>% mutate(scenario='fmsy2') -> intervals.c

new_intervals_pred<- rbind(intervals.a, intervals.b, intervals.c)
ggplot(new_intervals_pred, aes(x =year, y = fit)) +
  theme_bw() +
  geom_line(aes(colour=scenario)) +
  geom_smooth(aes(ymin = lwr, ymax = upr, colour=scenario), stat = "identity") +
  geom_point(data = pred, aes(x = year, y = proppred, colour=scenario), size=1)+
  xlab("Year") +
  ylab("PropPred") +
  ggtitle("Regressions of Proportion of Predators for three fishing scenarios; P= < 2.2e-16, R2 0.9545", size=1)

#####MEAN LIFE SPAN#########
read.csv("stats/indiseas/mean_life_span.csv")  -> life_span

life0<- lm(mean_life_span ~ poly(year, 2), data=life_span)
life1<- lm(mean_life_span ~ poly(year, 2) * scenario, data=life_span)
#life2<- lm(mean_life_span ~ poly(year, 3) * scenario, data=life_span)

anova(life0,life1)
##Significant difference between regressions

#Plotting regressions#
dat.a = data.frame(year= 1981:2015, scenario='fmsy0')
intervals.a <- predict(life1,newdata = dat.a,interval='confidence',
                       level=0.95)
data.frame(intervals.a) -> intervals.a
intervals.a%>% mutate(year = 1981:2015) %>% select(year, everything()) %>% mutate(scenario='fmsy0')-> intervals.a

dat.b = data.frame(year= 1981:2015, scenario='fmsy1')
intervals.b<- predict(life1, newdata = dat.b, interval='confidence', level=0.95)
data.frame(intervals.b) -> intervals.b
intervals.b%>% mutate(year = 1981:2015) %>% select(year, everything())%>% mutate(scenario='fmsy1') -> intervals.b
dat.c = data.frame(year= 1981:2015, scenario='fmsy2')
intervals.c<- predict(life1, newdata = dat.c, interval='confidence', level=0.95)
data.frame(intervals.c) -> intervals.c
intervals.c%>% mutate(year = 1981:2015) %>% select(year, everything()) %>% mutate(scenario='fmsy2') -> intervals.c

new_intervals_life<- rbind(intervals.a, intervals.b, intervals.c)
ggplot(new_intervals_life, aes(x =year, y = fit)) +
  theme_bw() +
  geom_line(aes(colour=scenario)) +
  geom_smooth(aes(ymin = lwr, ymax = upr, colour=scenario), stat = "identity") +
  geom_point(data = life_span, aes(x = year, y = mean_life_span, colour=scenario), size=1)+
  xlab("Year") +
  ylab("Mean Life Span") +
  ggtitle("Regressions of Mean Life Span for three fishing scenarios; P= < 2.2e-16, R2 0.7016", size=1)

#####IVI Landings#########
read.csv("stats/indiseas/ivi_landings.csv")  -> ivi

ivi0<- lm(ivi_landings ~ poly(year, 2), data=ivi
          )
ivi1<- lm(ivi_landings ~ poly(year, 2) * scenario, data=ivi)
#ivi2<- lm(ivi_landings ~ poly(year, 3) * scenario, data=ivi)

anova(ivi0,ivi1)
##Significant difference between regressions

#Plotting regressions#
dat.a = data.frame(year= 1981:2015, scenario='fmsy05')
intervals.a <- predict(ivi1,newdata = dat.a,interval='confidence',
                       level=0.95)
data.frame(intervals.a) -> intervals.a
intervals.a%>% mutate(year = 1981:2015) %>% select(year, everything()) %>% mutate(scenario='fmsy05')-> intervals.a

dat.b = data.frame(year= 1981:2015, scenario='fmsy1')
intervals.b<- predict(ivi1, newdata = dat.b, interval='confidence', level=0.95)
data.frame(intervals.b) -> intervals.b
intervals.b%>% mutate(year = 1981:2015) %>% select(year, everything())%>% mutate(scenario='fmsy1') -> intervals.b
dat.c = data.frame(year= 1981:2015, scenario='fmsy2')
intervals.c<- predict(ivi1, newdata = dat.c, interval='confidence', level=0.95)
data.frame(intervals.c) -> intervals.c
intervals.c%>% mutate(year = 1981:2015) %>% select(year, everything()) %>% mutate(scenario='fmsy2') -> intervals.c

new_intervals_ivi<- rbind(intervals.a, intervals.b, intervals.c)
ggplot(new_intervals_ivi, aes(x =year, y = fit)) +
  theme_bw() +
  geom_line(aes(colour=scenario)) +
  geom_smooth(aes(ymin = lwr, ymax = upr, colour=scenario), stat = "identity") +
  geom_point(data = ivi, aes(x = year, y = ivi_landings, colour=scenario), size=1)+
  xlab("Year") +
  ylab("IVI Landings") +
  ggtitle("Regressions of IVI Landings for three fishing scenarios; P= 2.258e-08, R2 0.4051", size=1)

#ivi_test<- filter(ivi, scenario %in% c("fmsy05", "fmsy2"))
##When filtering out fmsy1 and comparing anova with and without scenario, there is no significant difference between fmsy2
#and fmsy05

###INVERSE PRESSURE#######
read.csv("stats/indiseas/inverse_pressure_nocapelin.csv")  -> invp

invp0<- lm(inversepressure ~ poly(year, 6), data=invp)
invp1<- lm(inversepressure ~ poly(year, 6) * scenario, data=invp)
#invp2<- lm(inversepressure ~ poly(year, 7) * scenario, data=invp)

anova(invp0,invp1)
##Significant difference between regressions

#Plotting regressions#
dat.a = data.frame(year= 1981:2015, scenario='fmsy05')
intervals.a <- predict(invp1,newdata = dat.a,interval='confidence',
                       level=0.95)
data.frame(intervals.a) -> intervals.a
intervals.a%>% mutate(year = 1981:2015) %>% select(year, everything()) %>% mutate(scenario='fmsy05')-> intervals.a

dat.b = data.frame(year= 1981:2015, scenario='fmsy1')
intervals.b<- predict(invp1, newdata = dat.b, interval='confidence', level=0.95)
data.frame(intervals.b) -> intervals.b
intervals.b%>% mutate(year = 1981:2015) %>% select(year, everything())%>% mutate(scenario='fmsy1') -> intervals.b
dat.c = data.frame(year= 1981:2015, scenario='fmsy2')
intervals.c<- predict(invp1, newdata = dat.c, interval='confidence', level=0.95)
data.frame(intervals.c) -> intervals.c
intervals.c%>% mutate(year = 1981:2015) %>% select(year, everything()) %>% mutate(scenario='fmsy2') -> intervals.c

new_intervals_invp_ncap<- rbind(intervals.a, intervals.b, intervals.c)
ggplot(new_intervals_invp_ncap, aes(x =year, y = fit)) +
  theme_bw() +
  geom_line(aes(colour=scenario)) +
  geom_smooth(aes(ymin = lwr, ymax = upr, colour=scenario), stat = "identity") +
  geom_point(data = invp, aes(x = year, y = inversepressure, colour=scenario), size=1)+
  xlab("Year") +
  ylab("Inverse Pressure") +
  ggtitle("Regressions of Inverse Fishing Pressure (no capelin) for three fishing scenarios; P= <2.2e-16, R2 0.6125")
#####INVERSE PRESSURE#####
read.csv("stats/indiseas/inverse_pressure.csv")  -> invp

invp0<- lm(inversepressure ~ poly(year, 2), data=invp)
invp1<- lm(inversepressure ~ poly(year, 2) * scenario, data=invp)
#invp2<- lm(inversepressure ~ poly(year, 3) * scenario, data=invp)

anova(invp0,invp1)
##Significant difference between regressions

#Plotting regressions#
dat.a = data.frame(year= 1981:2015, scenario='fmsy05')
intervals.a <- predict(invp1,newdata = dat.a,interval='confidence',
                       level=0.95)
data.frame(intervals.a) -> intervals.a
intervals.a%>% mutate(year = 1981:2015) %>% select(year, everything()) %>% mutate(scenario='fmsy05')-> intervals.a

dat.b = data.frame(year= 1981:2015, scenario='fmsy1')
intervals.b<- predict(invp1, newdata = dat.b, interval='confidence', level=0.95)
data.frame(intervals.b) -> intervals.b
intervals.b%>% mutate(year = 1981:2015) %>% select(year, everything())%>% mutate(scenario='fmsy1') -> intervals.b
dat.c = data.frame(year= 1981:2015, scenario='fmsy2')
intervals.c<- predict(invp1, newdata = dat.c, interval='confidence', level=0.95)
data.frame(intervals.c) -> intervals.c
intervals.c%>% mutate(year = 1981:2015) %>% select(year, everything()) %>% mutate(scenario='fmsy2') -> intervals.c

new_intervals_invp<- rbind(intervals.a, intervals.b, intervals.c)
ggplot(new_intervals_invp, aes(x =year, y = fit)) +
  theme_bw() +
  geom_line(aes(colour=scenario)) +
  geom_smooth(aes(ymin = lwr, ymax = upr, colour=scenario), stat = "identity") +
  geom_point(data = invp, aes(x = year, y = inversepressure, colour=scenario), size=1)+
  xlab("Year") +
  ylab("Inverse Pressure") +
  ggtitle("Regressions of Inverse Fishing Pressure for three fishing scenarios; P= <2.2e-16, R2 0.9996")

####TL LANDINGS######
read.csv("stats/indiseas/tl_landings.csv")  -> tll

tll0<- lm(trophic_level_landings ~ poly(year, 2), data=tll)
tll1<- lm(trophic_level_landings ~ poly(year, 2) * scenario, data=tll)
#tll2<- lm(trophic_level_landings ~ poly(year, 3) * scenario, data=tll)

anova(tll0,tll1)
##Significant difference between regressions

#Plotting regressions#
dat.a = data.frame(year= 1981:2015, scenario='fmsy05')
intervals.a <- predict(tll1,newdata = dat.a,interval='confidence',
                       level=0.95)
data.frame(intervals.a) -> intervals.a
intervals.a%>% mutate(year = 1981:2015) %>% select(year, everything()) %>% mutate(scenario='fmsy05')-> intervals.a

dat.b = data.frame(year= 1981:2015, scenario='fmsy1')
intervals.b<- predict(tll1, newdata = dat.b, interval='confidence', level=0.95)
data.frame(intervals.b) -> intervals.b
intervals.b%>% mutate(year = 1981:2015) %>% select(year, everything())%>% mutate(scenario='fmsy1') -> intervals.b
dat.c = data.frame(year= 1981:2015, scenario='fmsy2')
intervals.c<- predict(tll1, newdata = dat.c, interval='confidence', level=0.95)
data.frame(intervals.c) -> intervals.c
intervals.c%>% mutate(year = 1981:2015) %>% select(year, everything()) %>% mutate(scenario='fmsy2') -> intervals.c

new_intervals_tl<- rbind(intervals.a, intervals.b, intervals.c)
ggplot(new_intervals_tl, aes(x =year, y = fit)) +
  theme_bw() +
  geom_line(aes(colour=scenario)) +
  geom_smooth(aes(ymin = lwr, ymax = upr, colour=scenario), stat = "identity") +
  geom_point(data = tll, aes(x = year, y = trophic_level_landings, colour=scenario), size=1)+
  xlab("Year") +
  ylab("Trophic level") +
  ggtitle("Regressions of Trophic Level of Landings for three fishing scenarios; P= 3.58e-08, R2 0.399")+ 
  theme(plot.title = element_text(size = 10, face = "bold"))

####TL LANDINGS NO CAPELINE######
read.csv("stats/indiseas/tl_landings_nocapelin.csv")  -> tll

tll0<- lm(trophic_level_landings ~ poly(year, 6), data=tll)
tll1<- lm(trophic_level_landings ~ poly(year, 6) * scenario, data=tll)
#tll2<- lm(trophic_level_landings ~ poly(year, 7) * scenario, data=tll)

anova(tll0,tll1)
##Significant difference between regressions

#Plotting regressions#
dat.a = data.frame(year= 1981:2015, scenario='fmsy05')
intervals.a <- predict(tll1,newdata = dat.a,interval='confidence',
                       level=0.95)
data.frame(intervals.a) -> intervals.a
intervals.a%>% mutate(year = 1981:2015) %>% select(year, everything()) %>% mutate(scenario='fmsy05')-> intervals.a

dat.b = data.frame(year= 1981:2015, scenario='fmsy1')
intervals.b<- predict(tll1, newdata = dat.b, interval='confidence', level=0.95)
data.frame(intervals.b) -> intervals.b
intervals.b%>% mutate(year = 1981:2015) %>% select(year, everything())%>% mutate(scenario='fmsy1') -> intervals.b
dat.c = data.frame(year= 1981:2015, scenario='fmsy2')
intervals.c<- predict(tll1, newdata = dat.c, interval='confidence', level=0.95)
data.frame(intervals.c) -> intervals.c
intervals.c%>% mutate(year = 1981:2015) %>% select(year, everything()) %>% mutate(scenario='fmsy2') -> intervals.c

new_intervals_tl_ncap<- rbind(intervals.a, intervals.b, intervals.c)
legend_test2<- ggplot(new_intervals_tl_ncap, aes(x =year, y = fit)) +
  theme_bw() +
  geom_line(aes(colour=scenario)) +
  geom_smooth(aes(ymin = lwr, ymax = upr, colour=scenario), stat = "identity") +
  geom_point(data = tll, aes(x = year, y = trophic_level_landings, colour=scenario), size=1)+
  xlab("Year") +
  ylab("Trophic level") +
  ggtitle("Regressions of Trophic Level of Landings (no capelin) for three fishing scenarios; P= <2.2e-16, R2 0.9732")


##########PLOTS############
surbio_plot<- ggplot(new_intervals_surbio, aes(x =year, y = fit)) +
  theme_bw() +
  geom_line(aes(colour=scenario)) +
  geom_smooth(aes(ymin = lwr, ymax = upr, colour=scenario), stat = "identity") +
  geom_point(data = surbio, aes(x = year, y = total_biomass_surveyed, colour=scenario), size=1)+
  xlab("Year") +
  ylab("Surveyed Biomass")+ theme(legend.position="none")

tl_plot<- ggplot(new_intervals_tl, aes(x =year, y = fit)) +
  theme_bw() +
  geom_line(aes(colour=scenario)) +
  geom_smooth(aes(ymin = lwr, ymax = upr, colour=scenario), stat = "identity") +
  geom_point(data = tll, aes(x = year, y = trophic_level_landings, colour=scenario), size=1)+
  xlab("Year") +
  ylab("Trophic level")+ theme(legend.position="none")

life_plot<- ggplot(new_intervals_life, aes(x =year, y = fit)) +
  theme_bw() +
  geom_line(aes(colour=scenario)) +
  geom_smooth(aes(ymin = lwr, ymax = upr, colour=scenario), stat = "identity") +
  geom_point(data = life_span, aes(x = year, y = mean_life_span, colour=scenario), size=1)+
  xlab("Year") +
  ylab("Mean Life Span")+ theme(legend.position="none")

ivi_plot<- ggplot(new_intervals_ivi, aes(x =year, y = fit)) +
  theme_bw() +
  geom_line(aes(colour=scenario)) +
  geom_smooth(aes(ymin = lwr, ymax = upr, colour=scenario), stat = "identity") +
  geom_point(data = ivi, aes(x = year, y = ivi_landings, colour=scenario), size=1)+
  xlab("Year") +
  ylab("IVI Landings")+ theme(legend.position="none")

invp_plot<- ggplot(new_intervals_invp, aes(x =year, y = fit)) +
  theme_bw() +
  geom_line(aes(colour=scenario)) +
  geom_smooth(aes(ymin = lwr, ymax = upr, colour=scenario), stat = "identity") +
  geom_point(data = invp, aes(x = year, y = inversepressure, colour=scenario), size=1)+
  xlab("Year") +
  ylab("Inverse Pressure")+ theme(legend.position="none")
pred_plot<- ggplot(new_intervals_pred, aes(x =year, y = fit)) +
  theme_bw() +
  geom_line(aes(colour=scenario)) +
  geom_smooth(aes(ymin = lwr, ymax = upr, colour=scenario), stat = "identity") +
  geom_point(data = pred, aes(x = year, y = proppred, colour=scenario), size=1)+
  xlab("Year") +
  ylab("Proportion Predators")+ theme(legend.position="none")

legend<- get_legend(legend_test)
legend2<- get_legend(legend_test2)
prow<- plot_grid(life_plot, pred_plot, surbio_plot,labels = c("A", "B", "C"), align = "h", ncol=3)

prow_test<- plot_grid(prow, legend, rel_widths = c(3, .3))

prow2<- plot_grid(ivi_plot, invp_plot, labels = c("D", "E"), align = "h", ncol=2)

final<- plot_grid(prow2, legend2, rel_widths = c(3, .3))

plot_grid(prow_test, final, nrow=2)
