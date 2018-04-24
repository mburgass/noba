library(readtext)
library(tidyverse)
library(EnvStats)
library(GGally)
closeAllConnections()

indicators<- read.csv("nature_index/scenarios/indicator_list.csv")
virgin_biomass<- read.csv("nature_index/scenarios/virgin_biomass.csv")
fmsy0<- read.csv("lpi_final/biomass_new/fmsy0_biomass.csv", check.names = F)
fmsy1<- read.csv("lpi_final/biomass_new/fmsy1_biomass.csv", check.names = F)
fmsy2<- read.csv("lpi_final/biomass_new/fmsy2_biomass.csv", check.names = F)


indicators<- rename(indicators, Binomial=model_group)
indicators %>% left_join(fmsy2, by="Binomial") %>% select(-ID) %>% rename(model_group=Binomial)%>%
gather(year, biomass, 4:156) %>% select(year, model_group, biomass) %>% unique() -> test %>% #spread(indicator_name)
  

test$year<- as.integer(test$year)
test<- filter(test, year>1980)
test<- filter(test, year<2016)

test %>% spread(model_group,biomass) ->test_spread
read.csv("catch/fmsy2_catch.csv")%>% select(Year, REO, GRH, HAD, SAI, RED, NCO) %>% 
  gather("species", "catch", 2:7) %>% mutate(scenario="fmsy2") %>% filter(Year<2016)-> fmsy2_catch
fmsy2_catch %>% group_by(Year) %>% summarise(catch= sum(catch)) %>% rename(year=Year) ->fmsy2_catch

data<- test_spread%>% left_join(fmsy2_catch, by='year')

GGally::ggcorr(data)
check<- round(cor(data),2)
plot(check)
fit = lm( catch ~ REO, data=data)
summary( fit )


write.csv(test, "nature_index/scenarios/fmsy2_natureindex.csv")

indicators %>% left_join(virgin_biomass, by="model_group") -> virgin_biomass2

write.csv(virgin_biomass2, "nature_index/scenarios/virgin_biomass_natureindex.csv")
