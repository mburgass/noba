read.csv("stats/lpi/adjusted/81/lpiscores_fmsy0_adjusted81.csv")  -> lpi_fmsy0
read.csv("stats/lpi/adjusted/81/lpiscores_fmsy2_adjusted81.csv") -> lpi_fmsy2
read.csv("stats/lpi/adjusted/81/lpiscores_fmsy1_adjusted81.csv") -> lpi_fmsy1

lpi<- rbind(lpi_fmsy2, lpi_fmsy1, lpi_fmsy0)

##Fitting Models
fit0 <- lm(LPI_final ~ poly(year, 2), data = lpi)

fit1 <- lm(LPI_final ~ poly(year, 2) * scenario, data = lpi)

anova(fit0, fit1)
####Plotting regressions######
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

new_intervals<- rbind(intervals.a, intervals.b, intervals.c)

ggplot(new_intervals, aes(x =year, y = fit)) +
  theme_bw() +
  geom_line(aes(colour=scenario)) +
  geom_smooth(aes(ymin = lwr, ymax = upr, colour=scenario), stat = "identity") +
  geom_point(data = lpi, aes(x = year, y = LPI_final, colour=scenario), size=1)+
  xlab("Year") +
  ylab("LPI Score") +
  ggtitle("Regressions of Living Planet Index Score for three fishing scenarios")