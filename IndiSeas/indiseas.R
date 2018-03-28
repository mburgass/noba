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

##Proportion Predatory Fish
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