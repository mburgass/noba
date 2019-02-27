library(tidyverse)
library(cowplot) 
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

read.csv("chapter_4/biomass/fmsy1_biomass.csv", check.names=F) %>%
  gather("Binomial", "biomass", 2:55) %>% rename(year=Year) %>% mutate(scenario="fmsy1") -> fmsy1_biomass
read.csv("chapter_4/biomass/fmsy11_biomass.csv", check.names=F) %>%
  gather("Binomial", "biomass", 2:55) %>% rename(year=Year) %>% mutate(scenario="fmsy11") -> fmsy11_biomass
read.csv("chapter_4/biomass/fmsy08_biomass.csv", check.names=F) %>%
  gather("Binomial", "biomass", 2:55) %>% rename(year=Year) %>% mutate(scenario="fmsy08")-> fmsy08_biomass
read.csv("chapter_4/biomass/fmsy06_biomass.csv", check.names=F) %>%
  gather("Binomial", "biomass", 2:55) %>% rename(year=Year)%>% mutate(scenario="fmsy06") -> fmsy06_biomass
read.csv("chapter_4/biomass/fmsy0_biomass.csv", check.names=F) %>%
  gather("Binomial", "biomass", 2:55) %>% rename(year=Year)%>% mutate(scenario="fmsy0") -> fmsy0_biomass
rbind(fmsy1_biomass, fmsy06_biomass, fmsy0_biomass) %>% filter(year>2014) ->fmsy_biomass

fmsy_biomass$scenario[fmsy_biomass$scenario=="fmsy1"]<- "Global Sustainability" 
fmsy_biomass$scenario[fmsy_biomass$scenario=="fmsy0"]<- "Strict Conservation"
fmsy_biomass$scenario[fmsy_biomass$scenario=="fmsy06"]<- "Precautionary Fishing" 



fmsy_biomass %>% filter(Binomial %in% c('PEL','PES', 'MES', 'MAC', 'SAI', 'BWH', 'SSH', 'CAP')) %>% group_by(year, scenario) %>% mutate(total_biomass_pel= sum(biomass)) %>% ungroup() %>%
  select(year, scenario, total_biomass_pel) %>% unique() ->fmsy_pel

fmsy_biomass %>% filter(Binomial %in% c('DF', 'PS', 'PL'))%>%group_by(year, scenario)%>%
   mutate(total_biomass_pp= sum(biomass)) %>% ungroup() %>%
  select(year, scenario, total_biomass_pp) %>% unique()  ->fmsy_pp


fmsy_pel%>% left_join(fmsy_pp) %>%
  mutate(PelBioPP= total_biomass_pel/total_biomass_pp) %>% filter(year>2014) -> pelbiopp

pelbiopp$year<- as.integer(pelbiopp$year)



pelbiopp<- select(pelbiopp, year, PelBioPP, scenario)
#write.csv(pelbiopp, "chapter_4/fisheries_ecosystem/pelbiopp.csv", row.names = F)

##Bio/PP
fmsy_biomass %>% filter(!Binomial %in% c('DF', 'PS', 'PL', 'BB', 'BC', 'BD', 'PB', 'DL', 'DIN', 'DR')) %>% group_by(year, scenario) %>% mutate(total_biomass= sum(biomass)) %>% ungroup() %>%
  select(year, scenario, total_biomass) %>% unique() -> fmsy_bio

fmsy_bio %>% left_join(fmsy_pp) %>%
  mutate(BioPP=total_biomass/total_biomass_pp) %>% filter(year>2014) ->biopp

biopp$year<- as.integer(biopp$year)
biopp<- select(biopp, year, BioPP, scenario)
#write.csv(biopp, "chapter_4/fisheries_ecosystem/biopp.csv", row.names = F)

##Dem/Pel
fmsy_biomass%>%
  filter(Binomial %in% c('DEO', 'REO', 'DEL', 'FLA', 'LRD', 'SSK', 'GRH', 'HAD', 'RED','NCO', 'PCO'))%>%
  group_by(year, scenario) %>% mutate(total_biomass_dem= sum(biomass)) %>% ungroup() %>%
  select(year, scenario, total_biomass_dem) %>% unique() ->fmsy_dem


fmsy_pel%>% left_join(fmsy_dem) %>%
  mutate(DemPel= total_biomass_dem/total_biomass_pel) -> DemPel
DemPel$year<- as.integer(DemPel$year)

dempel<- select(DemPel, year, DemPel, scenario)
#write.csv(dempel, "chapter_4/fisheries_ecosystem/dempel.csv", row.names = F)
#Dem bio/PP
fmsy_dem %>% left_join(fmsy_pp) %>% mutate(DemPP=total_biomass_dem/total_biomass_pp)-> DemPP
DemPP$year<- as.integer(DemPP$year)

dempp<- select(DemPP, year, DemPP, scenario)
#write.csv(dempp, "chapter_4/fisheries_ecosystem/dempp.csv", row.names = F)

#PropPel
fmsy_biomass%>%filter(!Binomial %in% c('BB', 'PB', 'DIN', 'DF')) %>%
  group_by(year, scenario) %>% mutate(total_biomass= sum(biomass)) %>% ungroup()%>%
  select(year, scenario, total_biomass) %>% unique() ->fmsy_total



fmsy_pel%>% left_join(fmsy_total) %>%
  mutate(PropPel= total_biomass_pel/total_biomass) -> PropPelCommunity

PropPelCommunity$year<- as.integer(PropPelCommunity$year)

proppel<- select(PropPelCommunity, year, PropPel, scenario)
#write.csv(proppel, "chapter_4/fisheries_ecosystem/proppel.csv", row.names = F)

##PropPredCommunity
fmsy_biomass%>%filter(!Binomial %in% c('PES', 'FLA', 'MES', 'MAC', 'RED', 'BWH', 'SSH', 'CAP', 'PWN', 'CEP', 'KCR', 'SCR', 'ZG', 'ZL', 'ZS', 'ZM')) %>%
  group_by(year, scenario) %>% mutate(total_biomass_pred= sum(biomass)) %>% ungroup()%>%
  select(year, scenario, total_biomass_pred) %>% unique() ->fmsy_pred

fmsy_pred%>% left_join(fmsy_total) %>%
  mutate(PropPred= total_biomass_pred/total_biomass) -> PropPred

PropPred$year<- as.integer(PropPred$year)

proppred<- select(PropPred, year, PropPred, scenario)
#write.csv(proppel, "chapter_4/fisheries_ecosystem/proppred.csv", row.names = F)


###### plot together

a<- ggplot(pelbiopp, aes(year, PelBioPP)) +geom_line(aes(colour=scenario), lwd=1.5)+
  theme(legend.position="none")+ scale_color_brewer(palette = "Dark2")+ylab("Ratio of Pelagic Biomass to Primary Production")+
  theme(axis.title.y = element_text(size=12))
b<- ggplot(biopp, aes(year, BioPP)) +geom_line(aes(colour=scenario), lwd=1.5)+
  theme(legend.position="none")+ scale_color_brewer(palette = "Dark2")+ylab("Ratio of Total Biomass to Primary Production")+
  theme(axis.title.y = element_text(size=12))
c<- ggplot(DemPel, aes(year, DemPel)) +geom_line(aes(colour=scenario), lwd=1.5)+
  theme(legend.position="none")+ scale_color_brewer(palette = "Dark2")+ylab("Ratio of Demersal to Pelagic Fish Biomass")+
  theme(axis.title.y = element_text(size=12))
d<- ggplot(DemPP, aes(year, DemPP)) +geom_line(aes(colour=scenario), lwd=1.5)+
  theme(legend.position="none")+ scale_color_brewer(palette = "Dark2")+ylab("Ratio of Demersal Biomass to Primary Production")+
  theme(axis.title.y = element_text(size=12))
e<- ggplot(PropPelCommunity, aes(year, PropPel)) +geom_line(aes(colour=scenario), lwd=1.5)+
  theme(legend.position="none")+ scale_color_brewer(palette = "Dark2")+ylab("Proportion of Pelagics in the Community")+
  theme(axis.title.y = element_text(size=12))
f<-ggplot(PropPred, aes(year, PropPred)) +geom_line(aes(colour=scenario), lwd=1.5)+
  theme(legend.position="none")+ scale_color_brewer(palette = "Dark2")+ylab("Proportion of Predators in the Community")+
  theme(axis.title.y = element_text(size=12))
legend<-ggplot(PropPred, aes(year, PropPred)) +geom_line(aes(colour=scenario), lwd=1.5)+
  scale_color_brewer(palette = "Dark2")
legend<- get_legend(legend)



prow<- plot_grid(a, b, c, d, e, f, labels = c("A", "B", "C", "D", "E", "F"), align = "v", ncol=3)
plot_grid(prow, legend, rel_widths = c(3, .4), label_size = 8)


######% Calcs####

fish_eco2<- pelbiopp %>% left_join(biopp) %>% left_join(dempel) %>% left_join(dempp) %>% left_join(proppel) %>% left_join(proppred)
fish_eco<- fish_eco2 %>% select(year, scenario, PelBioPP, BioPP, DemPel, DemPP, PropPel, PropPred)

fish_eco_2030<- fish_eco %>% filter(between(year, 2025, 2029)) %>% group_by(scenario) %>% summarise_all(mean) %>% select(-year)
fish_eco_2030$value<- "2030"

fish_eco_2050<- fish_eco %>% filter(between(year, 2045, 2049)) %>% group_by(scenario) %>% summarise_all(mean) %>% select(-year)
fish_eco_2050$value<- "2050"

fish_eco_2068<- fish_eco %>% filter(between(year, 2063, 2067)) %>% group_by(scenario) %>% summarise_all(mean) %>% select(-year)
fish_eco_2068$value<- "2068"

final<- rbind(fish_eco3, fish_eco_2030, fish_eco_2050, fish_eco_2068)

final<- final %>% select(value, scenario, PelBioPP, BioPP, DemPel, DemPP, PropPel, PropPred)

write.csv(final, "chapter_4/fisheries_ecosystem/final_fish_eco.csv", row.names=F)

#write.csv(fish_eco, "chapter_4/fisheries_ecosystem/fish_eco.csv", row.names = F)