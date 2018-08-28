read.csv("stats/nature_index/ni_scores.csv") -> ni_scores
ni_scores$scenario<- as.character(ni_scores$scenario)
fit0 <- lm(score ~ poly(year, 2), data = ni_scores)

fit1 <- lm(score ~ poly(year, 2) * scenario, data = ni_scores)
#fit2<- lm(score ~ poly(year, 3) * scenario, data = ni_scores)
anova(fit0, fit1)

dat.a = data.frame(year= 1981:2015, scenario='fmsy0')
intervals.a <- predict(fit1,newdata = dat.a,interval='confidence',
                       level=0.95)
data.frame(intervals.a) -> intervals.a
intervals.a%>% mutate(year = 1981:2015) %>% select(year, everything()) %>% mutate(scenario='fmsy0')-> intervals.a

dat.b = data.frame(year= 1981:2015, scenario='fmsy1')
intervals.b<- predict(fit1, newdata = dat.b, interval='confidence', level=0.95)
data.frame(intervals.b) -> intervals.b
intervals.b%>% mutate(year = 1981:2015) %>% select(year, everything())%>% mutate(scenario='fmsy1') -> intervals.b
dat.c = data.frame(year= 1981:2015, scenario='fmsy2')
intervals.c<- predict(fit1, newdata = dat.c, interval='confidence', level=0.95)
data.frame(intervals.c) -> intervals.c
intervals.c%>% mutate(year = 1981:2015) %>% select(year, everything()) %>% mutate(scenario='fmsy2') -> intervals.c

new_intervals_ni<- rbind(intervals.a, intervals.b, intervals.c)

legend_test<- ggplot(new_intervals_ni, aes(x =year, y = fit)) +
  theme_bw() +
  geom_line(aes(colour=scenario)) +
  geom_smooth(aes(ymin = lwr, ymax = upr, colour=scenario), stat = "identity") +
  geom_point(data = ni_scores, aes(x = year, y = score, colour=scenario), size=1)+
  xlab("Year") +
  ylab("Norway Nature Index Score") +
  ggtitle("Regressions of Norway Nature Index for three fishing scenarios")

######PELAGIC NNI###########

read.csv("stats/nature_index/ni_pelagic.csv") -> ni_pelagic
ni_pelagic$scenario<- as.character(ni_pelagic$scenario)
ni_scores<- ni_pelagic
fit0 <- lm(score ~ poly(year, 2), data = ni_scores)

fit1 <- lm(score ~ poly(year, 2) * scenario, data = ni_scores)
#fit2<- lm(score ~ poly(year, 3) * scenario, data = ni_scores)
anova(fit2, fit1)

dat.a = data.frame(year= 1981:2015, scenario='fmsy0')
intervals.a <- predict(fit1,newdata = dat.a,interval='confidence',
                       level=0.95)
data.frame(intervals.a) -> intervals.a
intervals.a%>% mutate(year = 1981:2015) %>% select(year, everything()) %>% mutate(scenario='fmsy0')-> intervals.a

dat.b = data.frame(year= 1981:2015, scenario='fmsy1')
intervals.b<- predict(fit1, newdata = dat.b, interval='confidence', level=0.95)
data.frame(intervals.b) -> intervals.b
intervals.b%>% mutate(year = 1981:2015) %>% select(year, everything())%>% mutate(scenario='fmsy1') -> intervals.b
dat.c = data.frame(year= 1981:2015, scenario='fmsy2')
intervals.c<- predict(fit1, newdata = dat.c, interval='confidence', level=0.95)
data.frame(intervals.c) -> intervals.c
intervals.c%>% mutate(year = 1981:2015) %>% select(year, everything()) %>% mutate(scenario='fmsy2') -> intervals.c

new_intervals_nipelagic<- rbind(intervals.a, intervals.b, intervals.c)

ggplot(new_intervals_nipelagic, aes(x =year, y = fit)) +
  theme_bw() +
  geom_line(aes(colour=scenario)) +
  geom_smooth(aes(ymin = lwr, ymax = upr, colour=scenario), stat = "identity") +
  geom_point(data = ni_scores, aes(x = year, y = score, colour=scenario), size=1)+
  xlab("Year") +
  ylab("Norway Nature Index Score") +
  ggtitle("Regressions of Pelagic Norway Nature Index for three fishing scenarios")


#####BENTHIC NNI#######
read.csv("stats/nature_index/ni_benthic.csv") -> ni_benthic
ni_benthic$scenario<- as.character(ni_benthic$scenario)
ni_scores<- ni_benthic
fit0 <- lm(score ~ poly(year, 3), data = ni_scores)

fit1 <- lm(score ~ poly(year, 3) * scenario, data = ni_scores)
#fit2<- lm(score ~ poly(year, 8) * scenario, data = ni_scores)
anova(fit0, fit1)

dat.a = data.frame(year= 1981:2015, scenario='fmsy0')
intervals.a <- predict(fit1,newdata = dat.a,interval='confidence',
                       level=0.95)
data.frame(intervals.a) -> intervals.a
intervals.a%>% mutate(year = 1981:2015) %>% select(year, everything()) %>% mutate(scenario='fmsy0')-> intervals.a

dat.b = data.frame(year= 1981:2015, scenario='fmsy1')
intervals.b<- predict(fit1, newdata = dat.b, interval='confidence', level=0.95)
data.frame(intervals.b) -> intervals.b
intervals.b%>% mutate(year = 1981:2015) %>% select(year, everything())%>% mutate(scenario='fmsy1') -> intervals.b
dat.c = data.frame(year= 1981:2015, scenario='fmsy2')
intervals.c<- predict(fit1, newdata = dat.c, interval='confidence', level=0.95)
data.frame(intervals.c) -> intervals.c
intervals.c%>% mutate(year = 1981:2015) %>% select(year, everything()) %>% mutate(scenario='fmsy2') -> intervals.c

new_intervals_nibenthic<- rbind(intervals.a, intervals.b, intervals.c)

ggplot(new_intervals_nibenthic, aes(x =year, y = fit)) +
  theme_bw() +
  geom_line(aes(colour=scenario)) +
  geom_smooth(aes(ymin = lwr, ymax = upr, colour=scenario), stat = "identity") +
  geom_point(data = ni_scores, aes(x = year, y = score, colour=scenario), size=1)+
  xlab("Year") +
  ylab("Norway Nature Index Score") +
  ggtitle("Regressions of Pelagic Norway Nature Index for three fishing scenarios")


#####PLOTTING NNI########

read.csv("stats/nature_index/ni_scores.csv") -> ni_scores
ni_scores$scenario<- as.character(ni_scores$scenario)
ni_plot<- ggplot(new_intervals_ni, aes(x =year, y = fit)) +
  theme_bw() +
  geom_line(aes(colour=scenario)) +
  geom_smooth(aes(ymin = lwr, ymax = upr, colour=scenario), stat = "identity") +
  geom_point(data = ni_scores, aes(x = year, y = score, colour=scenario), size=1)+
  xlab("Year") +
  ylab("Norway Nature Index Score")+ theme(legend.position="none")+
  ggtitle("Overall")


pelagic_plot<- ggplot(new_intervals_nipelagic, aes(x =year, y = fit)) +
  theme_bw() +
  geom_line(aes(colour=scenario)) +
  geom_smooth(aes(ymin = lwr, ymax = upr, colour=scenario), stat = "identity") +
  geom_point(data = ni_pelagic, aes(x = year, y = score, colour=scenario), size=1)+
  xlab("Year") +
  ylab("Norway Nature Index Score")+ theme(legend.position="none")+
  ggtitle("Pelagic")


benthic_plot<- ggplot(new_intervals_nibenthic, aes(x =year, y = fit)) +
  theme_bw() +
  geom_line(aes(colour=scenario)) +
  geom_smooth(aes(ymin = lwr, ymax = upr, colour=scenario), stat = "identity") +
  geom_point(data = ni_benthic, aes(x = year, y = score, colour=scenario), size=1)+
  xlab("Year") +
  ylab("Norway Nature Index Score")+ theme(legend.position="none")+
  ggtitle("Benthic")

legend<- get_legend(legend_test)
prow<- plot_grid(ni_plot, benthic_plot, pelagic_plot, labels = c("A", "B", "C"), align = "h", ncol=3)

plot_grid(prow, legend, rel_widths = c(3, .3))
