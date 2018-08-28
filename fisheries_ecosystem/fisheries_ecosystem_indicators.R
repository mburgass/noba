library(tidyverse)
closeAllConnections()

##Scripts to run the following indicators: 
#o	Pel bio/PP: Ratio of pelagic biomass to primary production
#o	Bio/PP: Ratio of total biomass to primary production
#o	Dem/pel fish: Ratio of demersal to pelagic fish biomass
#o	Dem bio/PP: Ratio of demersal biomass to primary production
#o	PropOF:theproportionof species that are overfished(proportion of species with biomass below 50% of the current[single-species] estimate of BMSY), 
#o	PropPel: the proportion of total biomass that is made up of pelagic species, 
#o	PropPred: the proportion of total biomass that is comprised by predatory species, 
#o	MTLbio: mean trophic level of the community (MTL weighted by the system species biomass composition)

#PelBio/PP

read.csv("lpi_final/biomass_new/fmsy1_biomass.csv", check.names=F) %>%
  gather("year", "biomass", 3:153)%>%
  filter(year<2016, year>1980)-> fmsy1_biomass
read.csv("lpi_final/biomass_new/fmsy0_biomass.csv", check.names=F) %>%
  gather("year", "biomass", 3:153)%>%
  filter(year<2016, year>1980)-> fmsy0_biomass
read.csv("lpi_final/biomass_new/fmsy2_biomass.csv", check.names=F) %>%
  gather("year", "biomass", 3:153) %>%
  filter(year<2016, year>1980)-> fmsy2_biomass

fmsy1_biomass %>% filter(Binomial %in% c('PEL','PES', 'MES', 'MAC', 'SAI', 'BWH', 'SSH')) %>% group_by(year) %>% mutate(total_biomass_pel= sum(biomass)) %>% ungroup() %>%
  select(year, total_biomass_pel) %>% unique() %>% mutate(scenario="fmsy1")->fmsy1_pel

fmsy1_biomass %>% filter(Binomial %in% c('DF', 'PS', 'PL'))%>%group_by(year)%>%
   mutate(total_biomass_pp= sum(biomass)) %>% ungroup() %>%
  select(year, total_biomass_pp) %>% unique() %>% mutate(scenario="fmsy1") ->fmsy1_pp

fmsy0_biomass%>%
  filter(Binomial %in% c('PEL','PES', 'MES', 'MAC', 'SAI', 'BWH', 'SSH'))%>%
   group_by(year) %>% mutate(total_biomass_pel= sum(biomass)) %>% ungroup() %>%
  select(year, total_biomass_pel) %>% unique() %>% mutate(scenario="fmsy0")->fmsy0_pel
fmsy0_biomass %>%
  filter(Binomial %in% c('DF', 'PS', 'PL'))%>%group_by(year)%>%
  mutate(total_biomass_pp= sum(biomass)) %>% ungroup() %>%
  select(year, total_biomass_pp) %>% unique() %>% mutate(scenario="fmsy0") ->fmsy0_pp

fmsy2_biomass%>%
  filter(Binomial %in% c('PEL','PES', 'MES', 'MAC', 'SAI', 'BWH', 'SSH'))%>%
  group_by(year) %>% mutate(total_biomass_pel= sum(biomass)) %>% ungroup() %>%
  select(year, total_biomass_pel) %>% unique() %>% mutate(scenario="fmsy2")->fmsy2_pel
fmsy2_biomass%>%
  filter(Binomial %in% c('DF', 'PS', 'PL'))%>%group_by(year)%>%
  mutate(total_biomass_pp= sum(biomass)) %>% ungroup() %>%
  select(year, total_biomass_pp) %>% unique() %>% mutate(scenario="fmsy2") ->fmsy2_pp

fmsy_pel<- rbind(fmsy0_pel, fmsy1_pel, fmsy2_pel)
fmsy_pp<- rbind(fmsy0_pp, fmsy1_pp, fmsy2_pp)
fmsy_pel%>% left_join(fmsy_pp) %>%
  mutate(PelBioPP= total_biomass_pel/total_biomass_pp) -> pelbiopp

pelbiopp$year<- as.integer(pelbiopp$year)

ggplot(pelbiopp, aes(year, PelBioPP)) +geom_line(aes(colour=scenario))

pelbiopp<- select(pelbiopp, year, PelBioPP, scenario)
write.csv(pelbiopp, "stats/fisheries_ecosystem/pelbiopp.csv", row.names = F)

##Bio/PP
fmsy1_biomass %>% filter(!Binomial %in% c('DF', 'PS', 'PL', 'BB', 'BC', 'BD', 'PB', 'DL', 'DIN', 'DR')) %>% group_by(year) %>% mutate(total_biomass= sum(biomass)) %>% ungroup() %>%
  select(year, total_biomass) %>% unique() %>% mutate(scenario="fmsy1")->fmsy1_bio
fmsy0_biomass %>% filter(!Binomial %in% c('DF', 'PS', 'PL','BB','BC', 'BD', 'PB', 'DL', 'DIN', 'DR')) %>% group_by(year) %>% mutate(total_biomass= sum(biomass)) %>% ungroup() %>%
  select(year, total_biomass) %>% unique() %>% mutate(scenario="fmsy0")->fmsy0_bio
fmsy2_biomass %>% filter(!Binomial %in% c('DF', 'PS', 'PL','BB','BC', 'BD', 'PB', 'DL', 'DIN', 'DR')) %>% group_by(year) %>% mutate(total_biomass= sum(biomass)) %>% ungroup() %>%
  select(year, total_biomass) %>% unique() %>% mutate(scenario="fmsy2")->fmsy2_bio

fmsy_bio<- rbind(fmsy0_bio, fmsy1_bio, fmsy2_bio)
fmsy_bio %>% left_join(fmsy_pp) %>%
  mutate(BioPP=total_biomass/total_biomass_pp) ->biopp

biopp$year<- as.integer(biopp$year)
ggplot(biopp, aes(year, BioPP)) +geom_line(aes(colour=scenario))
biopp<- select(biopp, year, BioPP, scenario)
write.csv(biopp, "stats/fisheries_ecosystem/biopp.csv", row.names = F)

##Dem/Pel
fmsy0_biomass%>%
  filter(Binomial %in% c('DEO', 'REO', 'DEL', 'FLA', 'LRD', 'SSK', 'GRH', 'HAD', 'RED','NCO', 'PCO'))%>%
  group_by(year) %>% mutate(total_biomass_dem= sum(biomass)) %>% ungroup() %>%
  select(year, total_biomass_dem) %>% unique() %>% mutate(scenario="fmsy0")->fmsy0_dem

fmsy1_biomass%>%
  filter(Binomial %in% c('DEO', 'REO', 'DEL', 'FLA', 'LRD', 'SSK', 'GRH', 'HAD', 'RED','NCO', 'PCO'))%>%
  group_by(year) %>% mutate(total_biomass_dem= sum(biomass)) %>% ungroup() %>%
  select(year, total_biomass_dem) %>% unique() %>% mutate(scenario="fmsy1")->fmsy1_dem

fmsy2_biomass%>%
  filter(Binomial %in% c('DEO', 'REO', 'DEL', 'FLA', 'LRD', 'SSK', 'GRH', 'HAD', 'RED','NCO', 'PCO'))%>%
  group_by(year) %>% mutate(total_biomass_dem= sum(biomass)) %>% ungroup() %>%
  select(year, total_biomass_dem) %>% unique() %>% mutate(scenario="fmsy2")->fmsy2_dem

fmsy_dem<- rbind(fmsy0_dem, fmsy1_dem, fmsy2_dem)
fmsy_pel%>% left_join(fmsy_dem) %>%
  mutate(DemPel= total_biomass_dem/total_biomass_pel) -> DemPel
DemPel$year<- as.integer(DemPel$year)
ggplot(DemPel, aes(year, DemPel)) +geom_line(aes(colour=scenario))

dempel<- select(DemPel, year, DemPel, scenario)
write.csv(dempel, "stats/fisheries_ecosystem/dempel.csv", row.names = F)
#Dem bio/PP
fmsy_dem %>% left_join(fmsy_pp) %>% mutate(DemPP=total_biomass_dem/total_biomass_pp)-> DemPP
DemPP$year<- as.integer(DemPP$year)
ggplot(DemPP, aes(year, DemPP)) +geom_line(aes(colour=scenario))

dempp<- select(DemPP, year, DemPP, scenario)
write.csv(dempp, "stats/fisheries_ecosystem/dempp.csv", row.names = F)

#PropPel
fmsy0_biomass%>%filter(!Binomial %in% c('BB', 'PB', 'DIN', 'DF')) %>%
  group_by(year) %>% mutate(total_biomass= sum(biomass)) %>% ungroup()%>%
  select(year, total_biomass) %>% unique() %>% mutate(scenario="fmsy0")->fmsy0_total

fmsy1_biomass%>%filter(!Binomial %in% c('BB', 'PB', 'DIN', 'DF')) %>%
  group_by(year) %>% mutate(total_biomass= sum(biomass)) %>% ungroup() %>%
  select(year, total_biomass) %>% unique() %>% mutate(scenario="fmsy1")->fmsy1_total

fmsy2_biomass%>%filter(!Binomial %in% c('BB', 'PB', 'DIN', 'DF')) %>%
  group_by(year) %>% mutate(total_biomass= sum(biomass)) %>% ungroup() %>%
  select(year, total_biomass) %>% unique() %>% mutate(scenario="fmsy2")->fmsy2_total

fmsy_total<- rbind(fmsy0_total, fmsy1_total, fmsy2_total)

fmsy_pel%>% left_join(fmsy_total) %>%
  mutate(PropPel= total_biomass_pel/total_biomass) -> PropPelCommunity

PropPelCommunity$year<- as.integer(PropPelCommunity$year)
ggplot(PropPelCommunity, aes(year, PropPel)) +geom_line(aes(colour=scenario))

proppel<- select(PropPelCommunity, year, PropPel, scenario)
write.csv(proppel, "stats/fisheries_ecosystem/proppel.csv", row.names = F)

##PropPredCommunity
