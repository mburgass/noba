read.csv("stats/ohi/fmsy05_status.csv") -> ohi_fmsy05
read.csv("stats/ohi/fmsy2_status.csv") -> ohi_fmsy2
read.csv("stats/ohi/fmsy1_status.csv") -> ohi_fmsy1

ohi<- rbind(ohi_fmsy05, ohi_fmsy1, ohi_fmsy2)
ohi<- filter(ohi, year >1984)
ohi$scenario<- as.character(ohi$scenario)
fit0 <- lm(status ~ poly(year, 2), data = ohi)

fit1 <- lm(status ~ poly(year, 2) * scenario, data = ohi)
#fit2<- lm(status ~ poly(year, 11) * scenario, data = ohi)
anova(fit0, fit0)
####Plotting regressions######
dat.a = data.frame(year= 1985:2015, scenario='fmsy05')
intervals.a <- predict(fit1,newdata = dat.a,interval='confidence',
                       level=0.95)
data.frame(intervals.a) -> intervals.a
intervals.a%>% mutate(year = 1985:2015) %>% select(year, everything()) %>% mutate(scenario='fmsy05')-> intervals.a

dat.b = data.frame(year= 1985:2015, scenario='fmsy1')
intervals.b<- predict(fit1, newdata = dat.b, interval='confidence', level=0.95)
data.frame(intervals.b) -> intervals.b
intervals.b%>% mutate(year = 1985:2015) %>% select(year, everything())%>% mutate(scenario='fmsy1') -> intervals.b
dat.c = data.frame(year= 1985:2015, scenario='fmsy2')
intervals.c<- predict(fit1, newdata = dat.c, interval='confidence', level=0.95)
data.frame(intervals.c) -> intervals.c
intervals.c%>% mutate(year = 1985:2015) %>% select(year, everything()) %>% mutate(scenario='fmsy2') -> intervals.c

new_intervals<- rbind(intervals.a, intervals.b, intervals.c)

ggplot(new_intervals, aes(x =year, y = fit)) +
  theme_bw() +
  geom_line(aes(colour=scenario)) +
  geom_smooth(aes(ymin = lwr, ymax = upr, colour=scenario), stat = "identity") +
  geom_point(data = ohi, aes(x = year, y = status, colour=scenario), size=1)+
  xlab("Year") +
  ylab("OHI Food Provision Status Score") +
  ggtitle("Regressions of Ocean Health Index Score for three fishing scenarios")