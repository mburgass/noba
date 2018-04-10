library(readtext)
library(tidyverse)
closeAllConnections()

indicators<- read.csv("nature_index/scenarios/indicator_list.csv")
virgin_biomass<- read.csv("nature_index/scenarios/virgin_biomass.csv")
fmsy0<- read.csv("lpi_final/biomass_new/fmsy0_biomass.csv", check.names = F)
fmsy1<- read.csv("lpi_final/biomass_new/fmsy1_biomass.csv", check.names = F)
fmsy2<- read.csv("lpi_final/biomass_new/fmsy2_biomass.csv", check.names = F)


indicators<- rename(indicators, Binomial=model_group)
indicators %>% left_join(fmsy2, by="Binomial") %>% select(-ID) %>% rename(model_group=Binomial) -> test

write.csv(test, "nature_index/scenarios/fmsy2_natureindex.csv")

indicators %>% left_join(virgin_biomass, by="model_group") -> virgin_biomass2

write.csv(virgin_biomass2, "nature_index/scenarios/virgin_biomass_natureindex.csv")
