library(readtext)
library(tidyverse)
closeAllConnections()
##IndiSeas Indicators:
#Total Biomass of surveyed species (PEL, REO, GRH, MAC, HAD,SAI, RED,BWH, SSH, NCO, PCO, CAP, PWN, KCR, SCR, ZL, ZM, ZS)

#Read base case
read.csv("lpi_final/biomass_new/fmsy1_biomass.csv", check.names=F) %>% gather("year", "biomass", 3:155) %>%
  filter(Binomial %in% c('CAP','PEL', 'REO', 'GRH', 'MAC', 'HAD','SAI', 'RED','BWH', 'SSH', 'NCO', 'PCO', 'PWN', 'KCR', 'SCR', 'ZL', 'ZM', 'ZS'))%>%
  filter(year<2016, year>1980) %>% group_by(year) %>% mutate(total_biomass_surveyed= mean(biomass)) %>% ungroup() %>%
  select(year, total_biomass_surveyed) %>% unique() %>% mutate(scenario="fmsy1") ->bc_data

#Read fisheries scenarios
read.csv("lpi_final/biomass_new/fmsy2_biomass.csv", check.names=F) %>% gather("year", "biomass", 3:155) %>%
  filter(Binomial %in% c('CAP','PEL', 'REO', 'GRH', 'MAC', 'HAD','SAI', 'RED','BWH', 'SSH', 'NCO', 'PCO', 'PWN', 'KCR', 'SCR', 'ZL', 'ZM', 'ZS'))%>%
  filter(year<2016, year>1980) %>% group_by(year) %>% mutate(total_biomass_surveyed= mean(biomass)) %>% ungroup() %>%
  select(year, total_biomass_surveyed) %>% unique() %>% mutate(scenario="fmsy2") ->fmsy2_data

read.csv("lpi_final/biomass_new/fmsy0_biomass.csv", check.names=F) %>% gather("year", "biomass", 3:155) %>%
  filter(Binomial %in% c('CAP','PEL', 'REO', 'GRH', 'MAC', 'HAD','SAI', 'RED','BWH', 'SSH', 'NCO', 'PCO', 'PWN', 'KCR', 'SCR', 'ZL', 'ZM', 'ZS'))%>%
  filter(year<2016, year>1980) %>% group_by(year) %>% mutate(total_biomass_surveyed= mean(biomass)) %>% ungroup() %>%
  select(year, total_biomass_surveyed) %>% unique() %>% mutate(scenario="fmsy0") ->fmsy0_data






surveyed_biomass<- rbind(bc_data, fmsy0_data, fmsy2_data)

surveyed_biomass$year<- as.integer(surveyed_biomass$year)  

ggplot(surveyed_biomass, aes(year, total_biomass_surveyed, group=1)) +geom_line()+
  facet_wrap(~scenario)

##Inverse fishing pressure 1/(landings/biomass) retained species
read.csv("catch/bc_catch.csv") %>%select(Year, REO, GRH, HAD, SAI, RED, NCO, CAP) %>% 
  gather("species", "catch", 2:8) %>% group_by(Year) %>%
  mutate(total_catch=sum(catch)) %>% ungroup() %>% select(Year, total_catch) %>%
  unique()%>% mutate(scenario="fmsy1") %>% filter(Year<2016) -> fmsy1_catch  



read.csv("catch/fmsy2_catch.csv") %>%select(Year, REO, GRH, HAD, SAI, RED, NCO, CAP) %>% 
  gather("species", "catch", 2:8) %>% group_by(Year) %>%
  mutate(total_catch=sum(catch)) %>% ungroup() %>% select(Year, total_catch) %>%
  unique()%>% mutate(scenario="fmsy2") %>% filter(Year<2016) -> fmsy2_catch  


read.csv("lpi_final/biomass_new/fmsy1_biomass.csv", check.names=F) %>% gather("Year", "biomass", 3:155) %>%
  filter(Binomial %in% c('REO', 'GRH', 'HAD', 'SAI', 'RED', 'NCO', 'CAP'))%>%
  filter(Year<2016, Year>1980) %>% group_by(Year) %>% mutate(total_biomass_landings= sum(biomass)) %>% ungroup() %>%
  select(Year, total_biomass_landings) %>% unique() %>% mutate(scenario="fmsy1") ->fmsy1_data

read.csv("lpi_final/biomass_new/fmsy2_biomass.csv", check.names=F) %>% gather("Year", "biomass", 3:155) %>%
  filter(Binomial %in% c('REO', 'GRH', 'HAD', 'SAI', 'RED', 'NCO', 'CAP'))%>%
  filter(Year<2016, Year>1980) %>% group_by(Year) %>% mutate(total_biomass_landings= sum(biomass)) %>% ungroup() %>%
  select(Year, total_biomass_landings) %>% unique() %>% mutate(scenario="fmsy2") ->fmsy2_data

fmsy1_data$Year<- as.integer(fmsy1_data$Year)
fmsy2_data$Year<- as.integer(fmsy2_data$Year)

fish_pressure_catch<- rbind(fmsy1_catch, fmsy2_catch)
fish_pressure_biomass<- rbind(fmsy1_data, fmsy2_data)

fish_pressure_catch%>% left_join(fish_pressure_biomass) %>%
  mutate(inversepressure= 1/(total_catch/total_biomass_landings)) %>% filter(Year>1983)-> fish_pressure

ggplot(fish_pressure, aes(Year, inversepressure)) +geom_line(aes(colour=scenario))

##Proportion Predatory Fish (classified as predatory and fish)
#Read base case
read.csv("lpi_final/biomass_new/fmsy1_biomass.csv", check.names=F) %>% gather("year", "biomass", 3:155) %>%
  filter(Binomial %in% c('DEO','PEL', 'REO','DEL', 'LRD', 'SSK', 'GRH','HAD', 'SAI', 'NCO'))%>%
  filter(year<2016, year>1980) %>% group_by(year) %>% mutate(total_biomass_pred= mean(biomass)) %>% ungroup() %>%
  select(year, total_biomass_pred) %>% unique() %>% mutate(scenario="fmsy1") ->fmsy1_pred

#Read fisheries scenarios
read.csv("lpi_final/biomass_new/fmsy2_biomass.csv", check.names=F) %>% gather("year", "biomass", 3:155) %>%
  filter(Binomial %in% c('DEO','PEL', 'REO','DEL', 'LRD', 'SSK', 'GRH','HAD', 'SAI', 'NCO'))%>%
  filter(year<2016, year>1980) %>% group_by(year) %>% mutate(total_biomass_pred= mean(biomass)) %>% ungroup() %>%
  select(year, total_biomass_pred) %>% unique() %>% mutate(scenario="fmsy2") ->fmsy2_pred

read.csv("lpi_final/biomass_new/fmsy0_biomass.csv", check.names=F) %>% gather("year", "biomass", 3:155) %>%
  filter(Binomial %in% c('DEO','PEL', 'REO','DEL', 'LRD', 'SSK', 'GRH','HAD', 'SAI', 'NCO'))%>%
  filter(year<2016, year>1980) %>% group_by(year) %>% mutate(total_biomass_pred= mean(biomass)) %>% ungroup() %>%
  select(year, total_biomass_pred) %>% unique() %>% mutate(scenario="fmsy0") ->fmsy0_pred

pred_fish<- rbind(fmsy1_pred, fmsy2_pred, fmsy0_pred)
pred_fish$year<- as.integer(pred_fish$year)
pred_surveyed<- filter(surveyed_biomass, scenario %in% c('fmsy1', 'fmsy0', 'fmsy2'))

pred_fish<- pred_fish %>% left_join(pred_surveyed)
pred_fish<- pred_fish %>% group_by(year, scenario) %>% summarise(proppred= total_biomass_pred/total_biomass_surveyed)

ggplot(pred_fish, aes(year, proppred)) +geom_line(aes(colour=scenario))

###MEAN LIFESPAN##### All species with mean lifespan data
#sum species(Max Age Species x Species Biomass) / Sum species (Species biomass) for each year

max_age<- read.csv("species_max_age.csv")

read.csv("lpi_final/biomass_new/fmsy1_biomass.csv", check.names=F) %>% gather("year", "biomass", 3:155) %>%
  filter(year<2016, year>1980) %>%
  left_join(max_age, by='Binomial') %>% filter(!is.na(Max_age)) %>%
  group_by(Binomial, year) %>% mutate(max_age_biomass= Max_age*biomass)%>% ungroup() %>%
  group_by(year) %>% mutate(max_age_biomass_year= sum(max_age_biomass)) %>%
  mutate(biomass_year= sum(biomass)) %>% ungroup() %>% group_by(year) %>%
  mutate(mean_life_span= max_age_biomass_year/biomass_year) %>% select(year, mean_life_span) %>% 
  unique()%>% mutate(scenario="fmsy1")->life_span_fmsy1

read.csv("lpi_final/biomass_new/fmsy0_biomass.csv", check.names=F) %>% gather("year", "biomass", 3:155) %>%
  filter(year<2016, year>1980) %>%
  left_join(max_age, by='Binomial') %>% filter(!is.na(Max_age)) %>%
  group_by(Binomial, year) %>% mutate(max_age_biomass= Max_age*biomass)%>% ungroup() %>%
  group_by(year) %>% mutate(max_age_biomass_year= sum(max_age_biomass)) %>%
  mutate(biomass_year= sum(biomass)) %>% ungroup() %>% group_by(year) %>%
  mutate(mean_life_span= max_age_biomass_year/biomass_year) %>% select(year, mean_life_span) %>% 
  unique()%>% mutate(scenario="fmsy0")->life_span_fmsy0

read.csv("lpi_final/biomass_new/fmsy2_biomass.csv", check.names=F) %>% gather("year", "biomass", 3:155) %>%
  filter(year<2016, year>1980) %>%
  left_join(max_age, by='Binomial') %>% filter(!is.na(Max_age)) %>%
  group_by(Binomial, year) %>% mutate(max_age_biomass= Max_age*biomass)%>% ungroup() %>%
  group_by(year) %>% mutate(max_age_biomass_year= sum(max_age_biomass)) %>%
  mutate(biomass_year= sum(biomass)) %>% ungroup() %>% group_by(year) %>%
  mutate(mean_life_span= max_age_biomass_year/biomass_year) %>% select(year, mean_life_span) %>% 
  unique()%>% mutate(scenario="fmsy2")->life_span_fmsy2

mean_lifespan<- rbind(life_span_fmsy0, life_span_fmsy1, life_span_fmsy2)
mean_lifespan$year<- as.integer(mean_lifespan$year)  

ggplot(mean_lifespan, aes(year, mean_life_span)) +geom_line(aes(colour=scenario))

## TROPHIC LEVEL LANDINGS#####

## sum of species(trophic level of species * catch species)/sum(catch species) for each year

trophic_level<- read.csv("species_trophic_level.csv")

read.csv("catch/bc_catch.csv") %>%select(Year, REO, GRH, HAD, SAI, RED, NCO, CAP) %>% 
  gather("species", "catch", 2:8)%>%
  filter(Year<2016, Year>1980) %>% left_join(trophic_level, by='species') %>%
  group_by(species, Year) %>% mutate(tl_catch=TL*catch) %>% ungroup() %>% group_by(Year) %>%
  mutate(catch_year=sum(catch)) %>% mutate(tl_catch_year= sum(tl_catch)) %>%
           mutate(trophic_level_landings= tl_catch_year/catch_year) %>% ungroup() %>% select(Year, trophic_level_landings) %>%
           unique() %>% mutate(scenario='fmsy1')-> tl_landings_fmsy1

read.csv("catch/fmsy2_catch.csv") %>%select(Year, REO, GRH, HAD, SAI, RED, NCO, CAP) %>% 
  gather("species", "catch", 2:8)%>%
  filter(Year<2016, Year>1980) %>% left_join(trophic_level, by='species') %>%
  group_by(species, Year) %>% mutate(tl_catch=TL*catch) %>% ungroup() %>% group_by(Year) %>%
  mutate(catch_year=sum(catch)) %>% mutate(tl_catch_year= sum(tl_catch)) %>%
  mutate(trophic_level_landings= tl_catch_year/catch_year) %>% ungroup() %>% select(Year, trophic_level_landings) %>%
  unique() %>% mutate(scenario='fmsy2')-> tl_landings_fmsy2

tl_landings<- rbind(tl_landings_fmsy1, tl_landings_fmsy2) %>% filter(Year>1983)
tl_landings$Year<- as.integer(tl_landings$Year)  

ggplot(tl_landings, aes(Year, trophic_level_landings)) +geom_line(aes(colour=scenario))


## TROPHIC LEVEL COMMUNITY#####
read.csv("species_trophic_level.csv") %>% rename(Binomial=species)-> trophic_level

read.csv("lpi_final/biomass_new/fmsy1_biomass.csv", check.names=F) %>% gather("year", "biomass", 3:155) %>%
  filter(year<2016, year>1980) %>% left_join(trophic_level, by='Binomial')%>% filter(!is.na(TL)) %>%
  group_by(Binomial, year) %>% mutate(tl_community=TL*biomass) %>% ungroup() %>% group_by(year) %>%
  mutate(biomass_year=sum(biomass)) %>% mutate(TL_biomass_year= sum(tl_community)) %>%
  mutate(TL_all= TL_biomass_year/biomass_year) %>% ungroup() %>% select(year, TL_all) %>%
  unique() %>% mutate(scenario='fmsy1')-> TL_community_fmsy1

read.csv("lpi_final/biomass_new/fmsy0_biomass.csv", check.names=F) %>% gather("year", "biomass", 3:155) %>%
  filter(year<2016, year>1980) %>% left_join(trophic_level, by='Binomial')%>% filter(!is.na(TL)) %>%
  group_by(Binomial, year) %>% mutate(tl_community=TL*biomass) %>% ungroup() %>% group_by(year) %>%
  mutate(biomass_year=sum(biomass)) %>% mutate(TL_biomass_year= sum(tl_community)) %>%
  mutate(TL_all= TL_biomass_year/biomass_year) %>% ungroup() %>% select(year, TL_all) %>%
  unique() %>% mutate(scenario='fmsy0')-> TL_community_fmsy0

read.csv("lpi_final/biomass_new/fmsy2_biomass.csv", check.names=F) %>% gather("year", "biomass", 3:155) %>%
  filter(year<2016, year>1980) %>% left_join(trophic_level, by='Binomial')%>% filter(!is.na(TL)) %>%
  group_by(Binomial, year) %>% mutate(tl_community=TL*biomass) %>% ungroup() %>% group_by(year) %>%
  mutate(biomass_year=sum(biomass)) %>% mutate(TL_biomass_year= sum(tl_community)) %>%
  mutate(TL_all= TL_biomass_year/biomass_year) %>% ungroup() %>% select(year, TL_all) %>%
  unique() %>% mutate(scenario='fmsy2')-> TL_community_fmsy2

tl_community<- rbind(TL_community_fmsy0, TL_community_fmsy1, TL_community_fmsy2) %>% filter(year>1983)
tl_community$year<- as.integer(tl_community$year)  

ggplot(tl_community, aes(year, TL_all)) +geom_line(aes(colour=scenario))
###IVI Landings#####
## sum of species(IVI of species * catch species)/sum(catch species) for each year

ivi<- read.csv("species_ivi.csv")

read.csv("catch/bc_catch.csv") %>%select(Year, REO, GRH, HAD, SAI, RED, NCO, CAP) %>% 
  gather("species", "catch", 2:8)%>%
  filter(Year<2016, Year>1980) %>% left_join(ivi, by='species') %>%
  group_by(species, Year) %>% mutate(ivi_catch=IVI*catch) %>% ungroup() %>% group_by(Year) %>%
  mutate(catch_year=sum(catch)) %>% mutate(ivi_catch_year= sum(ivi_catch)) %>%
  mutate(ivi_landings= ivi_catch_year/catch_year) %>% ungroup() %>% select(Year, ivi_landings) %>%
  unique() %>% mutate(scenario='fmsy1')-> ivi_landings_fmsy1

read.csv("catch/fmsy2_catch.csv") %>%select(Year, REO, GRH, HAD, SAI, RED, NCO, CAP) %>% 
  gather("species", "catch", 2:8)%>%
  filter(Year<2016, Year>1980) %>% left_join(ivi, by='species') %>%
  group_by(species, Year) %>% mutate(ivi_catch=IVI*catch) %>% ungroup() %>% group_by(Year) %>%
  mutate(catch_year=sum(catch)) %>% mutate(ivi_catch_year= sum(ivi_catch)) %>%
  mutate(ivi_landings= ivi_catch_year/catch_year) %>% ungroup() %>% select(Year, ivi_landings) %>%
  unique() %>% mutate(scenario='fmsy2')-> ivi_landings_fmsy2

ivi_landings<- rbind(ivi_landings_fmsy1, ivi_landings_fmsy2) %>% filter(Year>1983)
ivi_landings$Year<- as.integer(ivi_landings$Year)  

ggplot(ivi_landings, aes(Year, ivi_landings)) +geom_line(aes(colour=scenario))

#####OLD CODE######

fmsy05_data<- read.csv("biomass/lpi_files/fmsy05_lpi.csv", check.names=F) %>% gather("year", "biomass", 3:155) %>%
  filter(Binomial %in% c('CAP','PEL', 'REO', 'GRH', 'MAC', 'HAD','SAI', 'RED','BWH', 'SSH', 'NCO', 'PCO', 'PWN', 'KCR', 'SCR', 'ZL', 'ZM', 'ZS'))%>%
  filter(year<2101, year>1980) %>% group_by(year) %>% mutate(total_biomass_surveyed= mean(biomass)) %>% ungroup() %>%
  select(year, total_biomass_surveyed) %>% unique() %>% mutate(scenario="fmsy05") ->fmsy05_data
#Read MPA
mpa10_data<- read.csv("biomass/lpi_files/mpa10_lpi.csv", check.names=F) %>% gather("year", "biomass", 3:155) %>%
  filter(Binomial %in% c('CAP','PEL', 'REO', 'GRH', 'MAC', 'HAD','SAI', 'RED','BWH', 'SSH', 'NCO', 'PCO', 'PWN', 'KCR', 'SCR', 'ZL', 'ZM', 'ZS'))%>%
  filter(year<2101, year>1980) %>% group_by(year) %>% mutate(total_biomass_surveyed= mean(biomass)) %>% ungroup() %>%
  select(year, total_biomass_surveyed) %>% unique() %>% mutate(scenario="mpa10") ->mpa10_data

mpa25_data<- read.csv("biomass/lpi_files/mpa25_lpi.csv", check.names=F) %>% gather("year", "biomass", 3:155) %>%
  filter(Binomial %in% c('CAP','PEL', 'REO', 'GRH', 'MAC', 'HAD','SAI', 'RED','BWH', 'SSH', 'NCO', 'PCO', 'PWN', 'KCR', 'SCR', 'ZL', 'ZM', 'ZS'))%>%
  filter(year<2101, year>1980) %>% group_by(year) %>% mutate(total_biomass_surveyed= mean(biomass)) %>% ungroup() %>%
  select(year, total_biomass_surveyed) %>% unique() %>% mutate(scenario="mpa25") ->mpa25_data

mpa50_data<- read.csv("biomass/lpi_files/mpa50_lpi.csv", check.names=F) %>% gather("year", "biomass", 3:155) %>%
  filter(Binomial %in% c('CAP','PEL', 'REO', 'GRH', 'MAC', 'HAD','SAI', 'RED','BWH', 'SSH', 'NCO', 'PCO', 'PWN', 'KCR', 'SCR', 'ZL', 'ZM', 'ZS'))%>%
  filter(year<2101, year>1980) %>% group_by(year) %>% mutate(total_biomass_surveyed= mean(biomass)) %>% ungroup() %>%
  select(year, total_biomass_surveyed) %>% unique() %>% mutate(scenario="mpa50") ->mpa50_data

#Read Ocean acidification
oa01_data<- read.csv("biomass/lpi_files/oa01_lpi.csv", check.names=F) %>% gather("year", "biomass", 3:155) %>%
  filter(Binomial %in% c('CAP','PEL', 'REO', 'GRH', 'MAC', 'HAD','SAI', 'RED','BWH', 'SSH', 'NCO', 'PCO', 'PWN', 'KCR', 'SCR', 'ZL', 'ZM', 'ZS'))%>%
  filter(year<2101, year>1980) %>% group_by(year) %>% mutate(total_biomass_surveyed= mean(biomass)) %>% ungroup() %>%
  select(year, total_biomass_surveyed) %>% unique() %>% mutate(scenario="oa01") ->oa01_data
oa005_data<- read.csv("biomass/lpi_files/oa005_lpi.csv", check.names=F) %>% gather("year", "biomass", 3:155) %>%
  filter(Binomial %in% c('CAP','PEL', 'REO', 'GRH', 'MAC', 'HAD','SAI', 'RED','BWH', 'SSH', 'NCO', 'PCO', 'PWN', 'KCR', 'SCR', 'ZL', 'ZM', 'ZS'))%>%
  filter(year<2101, year>1980) %>% group_by(year) %>% mutate(total_biomass_surveyed= mean(biomass)) %>% ungroup() %>%
  select(year, total_biomass_surveyed) %>% unique() %>% mutate(scenario="oa005") ->oa005_data

#Read Climate change
cc2_data<- read.csv("biomass/lpi_files/cc2_lpi.csv", check.names=F) %>% gather("year", "biomass", 3:155) %>%
  filter(Binomial %in% c('CAP','PEL', 'REO', 'GRH', 'MAC', 'HAD','SAI', 'RED','BWH', 'SSH', 'NCO', 'PCO', 'PWN', 'KCR', 'SCR', 'ZL', 'ZM', 'ZS'))%>%
  filter(year<2101, year>1980) %>% group_by(year) %>% mutate(total_biomass_surveyed= mean(biomass)) %>% ungroup() %>%
  select(year, total_biomass_surveyed) %>% unique() %>% mutate(scenario="cc2") ->cc2_data
cc3_data<- read.csv("biomass/lpi_files/cc3_lpi.csv", check.names=F) %>% gather("year", "biomass", 3:155) %>%
  filter(Binomial %in% c('CAP','PEL', 'REO', 'GRH', 'MAC', 'HAD','SAI', 'RED','BWH', 'SSH', 'NCO', 'PCO', 'PWN', 'KCR', 'SCR', 'ZL', 'ZM', 'ZS'))%>%
  filter(year<2101, year>1980) %>% group_by(year) %>% mutate(total_biomass_surveyed= mean(biomass)) %>% ungroup() %>%
  select(year, total_biomass_surveyed) %>% unique() %>% mutate(scenario="cc3") ->cc3_data