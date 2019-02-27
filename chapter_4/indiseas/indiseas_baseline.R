library(readtext)
library(tidyverse)
closeAllConnections()
##IndiSeas Indicators:

#Total Biomass of surveyed species (PEL, REO, GRH, MAC, HAD,SAI, RED,BWH, SSH, NCO, PCO, CAP, PWN, KCR, SCR, ZL, ZM, ZS)

#Read base case
read.csv("chapter_4/biomass/fmsy1_biomass.csv", check.names=F)  %>% gather("Binomial", "biomass", 3:55)%>%
  filter(Binomial %in% c('CAP','PEL', 'REO', 'GRH', 'MAC', 'HAD','SAI', 'RED','BWH', 'SSH', 'NCO', 'PCO', 'PWN', 'KCR', 'SCR', 'ZL', 'ZM', 'ZS'))%>%
  group_by(Year) %>% mutate(total_biomass_surveyed= mean(biomass)) %>% ungroup() %>%
  select(Year, total_biomass_surveyed) %>% unique() %>% mutate(scenario="Global Sustainability") ->fmsy1_data

#Read fisheries scenarios
read.csv("chapter_4/biomass/fmsy11_biomass.csv", check.names=F) %>% gather("Binomial", "biomass", 3:55) %>%
  filter(Binomial %in% c('CAP','PEL', 'REO', 'GRH', 'MAC', 'HAD','SAI', 'RED','BWH', 'SSH', 'NCO', 'PCO', 'PWN', 'KCR', 'SCR', 'ZL', 'ZM', 'ZS'))%>%
  group_by(Year) %>% mutate(total_biomass_surveyed= mean(biomass)) %>% ungroup() %>%
  select(Year, total_biomass_surveyed) %>% unique() %>% mutate(scenario="fmsy11") ->fmsy11_data

read.csv("chapter_4/biomass/fmsy0_biomass.csv", check.names=F) %>% gather("Binomial", "biomass", 3:55) %>%
  filter(Binomial %in% c('CAP','PEL', 'REO', 'GRH', 'MAC', 'HAD','SAI', 'RED','BWH', 'SSH', 'NCO', 'PCO', 'PWN', 'KCR', 'SCR', 'ZL', 'ZM', 'ZS'))%>%
  group_by(Year) %>% mutate(total_biomass_surveyed= mean(biomass)) %>% ungroup() %>%
  select(Year, total_biomass_surveyed) %>% unique() %>% mutate(scenario="Strict Conservation") ->fmsy0_data


read.csv("chapter_4/biomass/fmsy06_biomass.csv", check.names=F) %>% gather("Binomial", "biomass", 3:55) %>%
  filter(Binomial %in% c('CAP','PEL', 'REO', 'GRH', 'MAC', 'HAD','SAI', 'RED','BWH', 'SSH', 'NCO', 'PCO', 'PWN', 'KCR', 'SCR', 'ZL', 'ZM', 'ZS'))%>%
  group_by(Year) %>% mutate(total_biomass_surveyed= mean(biomass)) %>% ungroup() %>%
  select(Year, total_biomass_surveyed) %>% unique() %>% mutate(scenario="Precautionary Fishing") -> fmsy06_data

read.csv("chapter_4/biomass/fmsy08_biomass.csv", check.names=F) %>% gather("Binomial", "biomass", 3:55) %>%
  filter(Binomial %in% c('CAP','PEL', 'REO', 'GRH', 'MAC', 'HAD','SAI', 'RED','BWH', 'SSH', 'NCO', 'PCO', 'PWN', 'KCR', 'SCR', 'ZL', 'ZM', 'ZS'))%>%
 group_by(Year) %>% mutate(total_biomass_surveyed= mean(biomass)) %>% ungroup() %>%
  select(Year, total_biomass_surveyed) %>% unique() %>% mutate(scenario="fmsy08") ->fmsy08_data

surveyed_biomass<- rbind(fmsy0_data, fmsy06_data, fmsy1_data)

surveyed_biomass$Year<- as.integer(surveyed_biomass$Year)  

#write.csv(surveyed_biomass, "chapter_4/indiseas/surveyed_biomass.csv", row.names = F)

#surveyed_biomass<- read.csv("Chapter_4/indiseas/surveyed_biomass.csv", check.names = F)
ggplot(surveyed_biomass, aes(Year, total_biomass_surveyed)) +geom_line(aes(colour=scenario))

######Inverse fishing pressure 1/(landings/biomass) retained species
read.csv("chapter_4/catch/fmsy_1_catch.csv") %>%select(Year, BWH, MAC, PWN, SSH, REO, GRH, HAD, SAI, RED, NCO, CAP) %>% 
  gather("species", "catch", 2:8) %>% group_by(Year) %>%
  mutate(total_catch=sum(catch)) %>% ungroup() %>% select(Year, total_catch) %>%
  unique()%>% mutate(scenario="Global Sustainability")-> fmsy1_catch  

read.csv("chapter_4/catch/fmsy_0.8_catch.csv") %>%select(Year, BWH, MAC, PWN, SSH, REO, GRH, HAD, SAI, RED, NCO, CAP) %>%  
  gather("species", "catch", 2:8) %>% group_by(Year) %>%
  mutate(total_catch=sum(catch)) %>% ungroup() %>% select(Year, total_catch) %>%
  unique()%>% mutate(scenario="fmsy08")-> fmsy08_catch  

read.csv("chapter_4/catch/fmsy_0.6_catch.csv") %>%select(Year, BWH, MAC, PWN, SSH, REO, GRH, HAD, SAI, RED, NCO, CAP) %>% 
  gather("species", "catch", 2:8) %>% group_by(Year) %>%
  mutate(total_catch=sum(catch)) %>% ungroup() %>% select(Year, total_catch) %>%
  unique()%>% mutate(scenario="Precautionary Fishing")-> fmsy06_catch 

read.csv("chapter_4/catch/fmsy_1.1_catch.csv") %>%select(Year, BWH, MAC, PWN, SSH, REO, GRH, HAD, SAI, RED, NCO, CAP) %>% 
  gather("species", "catch", 2:8) %>% group_by(Year) %>%
  mutate(total_catch=sum(catch)) %>% ungroup() %>% select(Year, total_catch) %>%
  unique()%>% mutate(scenario="fmsy11") -> fmsy11_catch  


read.csv("chapter_4/biomass/fmsy1_biomass.csv", check.names=F)  %>% gather("Binomial", "biomass", 3:55)%>%
  filter(Binomial %in% c('REO', 'GRH', 'HAD', 'SAI', 'RED', 'NCO', 'BWH', 'MAC', 'PWN', 'SSH', 'CAP'))%>%
  group_by(Year) %>% mutate(total_biomass_landings= sum(biomass)) %>% ungroup() %>%
  select(Year, total_biomass_landings) %>% unique() %>% mutate(scenario="Global Sustainability") ->fmsy1_data

read.csv("chapter_4/biomass/fmsy11_biomass.csv", check.names=F)  %>% gather("Binomial", "biomass", 3:55)%>%
  filter(Binomial %in% c('REO', 'GRH', 'HAD', 'SAI', 'RED', 'NCO', 'BWH', 'MAC', 'PWN', 'SSH', 'CAP'))%>%
  group_by(Year) %>% mutate(total_biomass_landings= sum(biomass)) %>% ungroup() %>%
  select(Year, total_biomass_landings) %>% unique() %>% mutate(scenario="fmsy11") ->fmsy11_data

read.csv("chapter_4/biomass/fmsy08_biomass.csv", check.names=F)  %>% gather("Binomial", "biomass", 3:55)%>%
  filter(Binomial %in% c('REO', 'GRH', 'HAD', 'SAI', 'RED', 'NCO', 'BWH', 'MAC', 'PWN', 'SSH', 'CAP'))%>%
  group_by(Year) %>% mutate(total_biomass_landings= sum(biomass)) %>% ungroup() %>%
  select(Year, total_biomass_landings) %>% unique() %>% mutate(scenario="fmsy08") ->fmsy08_data

read.csv("chapter_4/biomass/fmsy06_biomass.csv", check.names=F)  %>% gather("Binomial", "biomass", 3:55)%>%
  filter(Binomial %in% c('REO', 'GRH', 'HAD', 'SAI', 'RED', 'NCO', 'BWH', 'MAC', 'PWN', 'SSH', 'CAP'))%>%
  group_by(Year) %>% mutate(total_biomass_landings= sum(biomass)) %>% ungroup() %>%
  select(Year, total_biomass_landings) %>% unique() %>% mutate(scenario="Precautionary Fishing") ->fmsy06_data

fmsy1_data$Year<- as.integer(fmsy1_data$Year)
fmsy11_data$Year<- as.integer(fmsy11_data$Year)
fmsy08_data$Year<- as.integer(fmsy08_data$Year)
fmsy06_data$Year<- as.integer(fmsy06_data$Year)

fish_pressure_catch<- rbind(fmsy06_catch, fmsy1_catch)
fish_pressure_biomass<- rbind(fmsy06_data,fmsy1_data)
ggplot(fish_pressure_catch, aes(Year, total_catch))+geom_line(aes(colour=scenario), lwd=1.5)
#write.csv(fish_pressure_catch, "chapter_4/catch/total_catch.csv", row.names = F)

fish_pressure_catch%>% group_by(scenario) %>% mutate(catch=sum(total_catch)) %>% select(scenario, catch) %>% unique() -> scenario_catch

ggplot(scenario_catch, aes(scenario, catch))+geom_bar(aes(fill=scenario), stat="identity")

fish_pressure_catch%>% left_join(fish_pressure_biomass) %>%
  mutate(inversepressure= 1/(total_catch/total_biomass_landings)) -> fish_pressure

ggplot(fish_pressure, aes(Year, inversepressure)) +geom_line(aes(colour=scenario), lwd=1.5)
#write.csv(fish_pressure, "chapter_4/indiseas/inverse_pressure.csv", row.names = F)

###PROPORTION PREDATORY FISH / Biomass surveyed####
read.csv("chapter_4/biomass/fmsy1_biomass.csv", check.names=F) %>% gather("Binomial", "biomass", 3:55) %>%
  filter(Binomial %in% c('DEO','PEL', 'REO','DEL', 'LRD', 'SSK', 'GRH','HAD', 'SAI', 'NCO'))%>%
  group_by(Year) %>% mutate(total_biomass_pred= mean(biomass)) %>% ungroup() %>%
  select(Year, total_biomass_pred) %>% unique() %>% mutate(scenario="Global Sustainability") ->fmsy1_pred

read.csv("chapter_4/biomass/fmsy11_biomass.csv", check.names=F) %>% gather("Binomial", "biomass", 3:55) %>%
  filter(Binomial %in% c('DEO','PEL', 'REO','DEL', 'LRD', 'SSK', 'GRH','HAD', 'SAI', 'NCO'))%>%
  group_by(Year) %>% mutate(total_biomass_pred= mean(biomass)) %>% ungroup() %>%
  select(Year, total_biomass_pred) %>% unique() %>% mutate(scenario="fmsy11") ->fmsy11_pred

read.csv("chapter_4/biomass/fmsy06_biomass.csv", check.names=F) %>% gather("Binomial", "biomass", 3:55) %>%
  filter(Binomial %in% c('DEO','PEL', 'REO','DEL', 'LRD', 'SSK', 'GRH','HAD', 'SAI', 'NCO'))%>%
  group_by(Year) %>% mutate(total_biomass_pred= mean(biomass)) %>% ungroup() %>%
  select(Year, total_biomass_pred) %>% unique() %>% mutate(scenario="Precautionary Fishing") ->fmsy06_pred

read.csv("chapter_4/biomass/fmsy08_biomass.csv", check.names=F) %>% gather("Binomial", "biomass", 3:55) %>%
  filter(Binomial %in% c('DEO','PEL', 'REO','DEL', 'LRD', 'SSK', 'GRH','HAD', 'SAI', 'NCO'))%>%
 group_by(Year) %>% mutate(total_biomass_pred= mean(biomass)) %>% ungroup() %>%
  select(Year, total_biomass_pred) %>% unique() %>% mutate(scenario="fmsy08") ->fmsy08_pred

read.csv("chapter_4/biomass/fmsy0_biomass.csv", check.names=F) %>% gather("Binomial", "biomass", 3:55) %>%
  filter(Binomial %in% c('DEO','PEL', 'REO','DEL', 'LRD', 'SSK', 'GRH','HAD', 'SAI', 'NCO'))%>%
  group_by(Year) %>% mutate(total_biomass_pred= mean(biomass)) %>% ungroup() %>%
  select(Year, total_biomass_pred) %>% unique() %>% mutate(scenario="Strict Conservation") ->fmsy0_pred

pred_fish<- rbind(fmsy0_pred, fmsy06_pred, fmsy1_pred)
pred_fish$Year<- as.integer(pred_fish$Year)
pred_surveyed<- filter(surveyed_biomass, scenario %in% c('Strict Conservation', 'Precautionary Fishing', 'Global Sustainability'))

pred_fish<- pred_fish %>% left_join(pred_surveyed)
pred_fish<- pred_fish %>% group_by(Year, scenario) %>% summarise(proppred= total_biomass_pred/total_biomass_surveyed)

ggplot(pred_fish, aes(Year, proppred)) +geom_line(aes(colour=scenario), lwd=1.5)

#write.csv(pred_fish, "chapter_4/indiseas/proppred_surveyed.csv", row.names = F)

###MEAN LIFESPAN##### All species with mean lifespan data
#sum species(Max Age Species x Species Biomass) / Sum species (Species biomass) for each year

max_age<- read.csv("species_max_age.csv")

read.csv("chapter_4/biomass/fmsy1_biomass.csv", check.names=F) %>% gather("Binomial", "biomass", 3:55) %>%
  left_join(max_age, by='Binomial') %>% filter(!is.na(Max_age)) %>%
  group_by(Binomial, Year) %>% mutate(max_age_biomass= Max_age*biomass)%>% ungroup() %>%
  group_by(Year) %>% mutate(max_age_biomass_year= sum(max_age_biomass)) %>%
  mutate(biomass_year= sum(biomass)) %>% ungroup() %>% group_by(Year) %>%
  mutate(mean_life_span= max_age_biomass_year/biomass_year) %>% select(Year, mean_life_span) %>% 
  unique()%>% mutate(scenario="Global Sustainability")->life_span_fmsy1

read.csv("chapter_4/biomass/fmsy11_biomass.csv", check.names=F) %>% gather("Binomial", "biomass", 3:55) %>%
  filter(Year>2014) %>%
  left_join(max_age, by='Binomial') %>% filter(!is.na(Max_age)) %>%
  group_by(Binomial, Year) %>% mutate(max_age_biomass= Max_age*biomass)%>% ungroup() %>%
  group_by(Year) %>% mutate(max_age_biomass_year= sum(max_age_biomass)) %>%
  mutate(biomass_year= sum(biomass)) %>% ungroup() %>% group_by(Year) %>%
  mutate(mean_life_span= max_age_biomass_year/biomass_year) %>% select(Year, mean_life_span) %>% 
  unique()%>% mutate(scenario="fmsy11")->life_span_fmsy11

read.csv("chapter_4/biomass/fmsy08_biomass.csv", check.names=F) %>% gather("Binomial", "biomass", 3:55) %>%
  left_join(max_age, by='Binomial') %>% filter(!is.na(Max_age)) %>%
  group_by(Binomial, Year) %>% mutate(max_age_biomass= Max_age*biomass)%>% ungroup() %>%
  group_by(Year) %>% mutate(max_age_biomass_year= sum(max_age_biomass)) %>%
  mutate(biomass_year= sum(biomass)) %>% ungroup() %>% group_by(Year) %>%
  mutate(mean_life_span= max_age_biomass_year/biomass_year) %>% select(Year, mean_life_span) %>% 
  unique()%>% mutate(scenario="fmsy08")->life_span_fmsy08

read.csv("chapter_4/biomass/fmsy06_biomass.csv", check.names=F) %>% gather("Binomial", "biomass", 3:55) %>%
  left_join(max_age, by='Binomial') %>% filter(!is.na(Max_age)) %>%
  group_by(Binomial, Year) %>% mutate(max_age_biomass= Max_age*biomass)%>% ungroup() %>%
  group_by(Year) %>% mutate(max_age_biomass_year= sum(max_age_biomass)) %>%
  mutate(biomass_year= sum(biomass)) %>% ungroup() %>% group_by(Year) %>%
  mutate(mean_life_span= max_age_biomass_year/biomass_year) %>% select(Year, mean_life_span) %>% 
  unique()%>% mutate(scenario="Precautionary Fishing")->life_span_fmsy06

read.csv("chapter_4/biomass/fmsy0_biomass.csv", check.names=F) %>% gather("Binomial", "biomass", 3:55) %>%
  left_join(max_age, by='Binomial') %>% filter(!is.na(Max_age)) %>%
  group_by(Binomial, Year) %>% mutate(max_age_biomass= Max_age*biomass)%>% ungroup() %>%
  group_by(Year) %>% mutate(max_age_biomass_year= sum(max_age_biomass)) %>%
  mutate(biomass_year= sum(biomass)) %>% ungroup() %>% group_by(Year) %>%
  mutate(mean_life_span= max_age_biomass_year/biomass_year) %>% select(Year, mean_life_span) %>% 
  unique()%>% mutate(scenario="Strict Conservation")->life_span_fmsy0

mean_lifespan<- rbind(life_span_fmsy0, life_span_fmsy06, life_span_fmsy1)
mean_lifespan$Year<- as.integer(mean_lifespan$Year)  

ggplot(mean_lifespan, aes(Year, mean_life_span)) +geom_line(aes(colour=scenario))

#write.csv(mean_lifespan, "chapter_4/indiseas/mean_life_span.csv", row.names = F)

trophic_level<- read.csv("species_trophic_level.csv")

read.csv("chapter_4/catch/fmsy_1_catch.csv") %>%select(Year,REO, GRH, HAD, SAI, RED, NCO, BWH, MAC, PWN, SSH, CAP) %>% 
  gather("species", "catch", 2:12)%>%
  left_join(trophic_level, by='species') %>%
  group_by(species, Year) %>% mutate(tl_catch=TL*catch) %>% ungroup() %>% group_by(Year) %>%
  mutate(catch_year=sum(catch)) %>% mutate(tl_catch_year= sum(tl_catch)) %>%
  mutate(trophic_level_landings= tl_catch_year/catch_year) %>% ungroup() %>% select(Year, trophic_level_landings) %>%
  unique() %>% mutate(scenario='Global Sustainability')-> tl_landings_fmsy1

read.csv("chapter_4/catch/fmsy_0.8_catch.csv") %>%select(Year,REO, GRH, HAD, SAI, RED, NCO, BWH, MAC, PWN, SSH, CAP) %>% 
  gather("species", "catch", 2:12)%>%
  left_join(trophic_level, by='species') %>%
  group_by(species, Year) %>% mutate(tl_catch=TL*catch) %>% ungroup() %>% group_by(Year) %>%
  mutate(catch_year=sum(catch)) %>% mutate(tl_catch_year= sum(tl_catch)) %>%
  mutate(trophic_level_landings= tl_catch_year/catch_year) %>% ungroup() %>% select(Year, trophic_level_landings) %>%
  unique() %>% mutate(scenario='fmsy08')-> tl_landings_fmsy08

read.csv("chapter_4/catch/fmsy_0.6_catch.csv") %>%select(Year,REO, GRH, HAD, SAI, RED, NCO, BWH, MAC, PWN, SSH, CAP) %>% 
  gather("species", "catch", 2:12)%>%
  left_join(trophic_level, by='species') %>%
  group_by(species, Year) %>% mutate(tl_catch=TL*catch) %>% ungroup() %>% group_by(Year) %>%
  mutate(catch_year=sum(catch)) %>% mutate(tl_catch_year= sum(tl_catch)) %>%
  mutate(trophic_level_landings= tl_catch_year/catch_year) %>% ungroup() %>% select(Year, trophic_level_landings) %>%
  unique() %>% mutate(scenario='Precautionary Fishing')-> tl_landings_fmsy06

read.csv("chapter_4/catch/fmsy_1.1_catch.csv") %>%select(Year,REO, GRH, HAD, SAI, RED, NCO, BWH, MAC, PWN, SSH, CAP) %>% 
  gather("species", "catch", 2:12)%>%
   left_join(trophic_level, by='species') %>%
  group_by(species, Year) %>% mutate(tl_catch=TL*catch) %>% ungroup() %>% group_by(Year) %>%
  mutate(catch_year=sum(catch)) %>% mutate(tl_catch_year= sum(tl_catch)) %>%
  mutate(trophic_level_landings= tl_catch_year/catch_year) %>% ungroup() %>% select(Year, trophic_level_landings) %>%
  unique() %>% mutate(scenario='fmsy11')-> tl_landings_fmsy11

tl_landings<- rbind(tl_landings_fmsy06, tl_landings_fmsy1)
tl_landings$Year<- as.integer(tl_landings$Year)  

ggplot(tl_landings, aes(Year, trophic_level_landings)) +geom_line(aes(colour=scenario))

#write.csv(tl_landings, "chapter_4/indiseas/tl_landings.csv", row.names = F)

####TROPHIC LEVEL COMMUNITY#######

read.csv("species_trophic_level.csv") %>% rename(Binomial=species)-> trophic_level

read.csv("chapter_4/biomass/fmsy1_biomass.csv", check.names=F) %>% gather("Binomial", "biomass", 3:55) %>%
  filter(Year>2014) %>% left_join(trophic_level, by='Binomial')%>% filter(!is.na(TL)) %>%
  group_by(Binomial, Year) %>% mutate(tl_community=TL*biomass) %>% ungroup() %>% group_by(Year) %>%
  mutate(biomass_year=sum(biomass)) %>% mutate(TL_biomass_year= sum(tl_community)) %>%
  mutate(TL_all= TL_biomass_year/biomass_year) %>% ungroup() %>% select(Year, TL_all) %>%
  unique() %>% mutate(scenario='Global Sustainability')-> TL_community_fmsy1

read.csv("chapter_4/biomass/fmsy11_biomass.csv", check.names=F) %>% gather("Binomial", "biomass", 3:55) %>% left_join(trophic_level, by='Binomial')%>% filter(!is.na(TL)) %>%
  group_by(Binomial, Year) %>% mutate(tl_community=TL*biomass) %>% ungroup() %>% group_by(Year) %>%
  mutate(biomass_year=sum(biomass)) %>% mutate(TL_biomass_year= sum(tl_community)) %>%
  mutate(TL_all= TL_biomass_year/biomass_year) %>% ungroup() %>% select(Year, TL_all) %>%
  unique() %>% mutate(scenario='fmsy11')-> TL_community_fmsy11

read.csv("chapter_4/biomass/fmsy0_biomass.csv", check.names=F) %>% gather("Binomial", "biomass", 3:55) %>%
left_join(trophic_level, by='Binomial')%>% filter(!is.na(TL)) %>%
  group_by(Binomial, Year) %>% mutate(tl_community=TL*biomass) %>% ungroup() %>% group_by(Year) %>%
  mutate(biomass_year=sum(biomass)) %>% mutate(TL_biomass_year= sum(tl_community)) %>%
  mutate(TL_all= TL_biomass_year/biomass_year) %>% ungroup() %>% select(Year, TL_all) %>%
  unique() %>% mutate(scenario='Strict Conservation')-> TL_community_fmsy0

read.csv("chapter_4/biomass/fmsy06_biomass.csv", check.names=F) %>% gather("Binomial", "biomass", 3:55) %>%
  left_join(trophic_level, by='Binomial')%>% filter(!is.na(TL)) %>%
  group_by(Binomial, Year) %>% mutate(tl_community=TL*biomass) %>% ungroup() %>% group_by(Year) %>%
  mutate(biomass_year=sum(biomass)) %>% mutate(TL_biomass_year= sum(tl_community)) %>%
  mutate(TL_all= TL_biomass_year/biomass_year) %>% ungroup() %>% select(Year, TL_all) %>%
  unique() %>% mutate(scenario='Precautionary Fishing')-> TL_community_fmsy06

read.csv("chapter_4/biomass/fmsy08_biomass.csv", check.names=F) %>% gather("Binomial", "biomass", 3:55) %>%
  left_join(trophic_level, by='Binomial')%>% filter(!is.na(TL)) %>%
  group_by(Binomial, Year) %>% mutate(tl_community=TL*biomass) %>% ungroup() %>% group_by(Year) %>%
  mutate(biomass_year=sum(biomass)) %>% mutate(TL_biomass_year= sum(tl_community)) %>%
  mutate(TL_all= TL_biomass_year/biomass_year) %>% ungroup() %>% select(Year, TL_all) %>%
  unique() %>% mutate(scenario='fmsy08')-> TL_community_fmsy08

tl_community<- rbind(TL_community_fmsy0, TL_community_fmsy06, TL_community_fmsy1) #%>% filter(year>1983)
tl_community$Year<- as.integer(tl_community$Year)

ggplot(tl_community, aes(Year, TL_all)) +geom_line(aes(colour=scenario))


######% Calcs####

indiseas_baseline<- surveyed_biomass  %>% left_join(pred_fish) %>% left_join(mean_lifespan) %>% left_join(tl_community)
indiseas_baseline2<- indiseas_baseline %>% filter(Year<2015) %>% filter(Year>2009)

#write.csv(fish_eco, "chapter_4/fisheries_ecosystem/fish_eco_baseline.csv", row.names = F)

indiseas_baseline3<- indiseas_baseline2 %>% group_by(scenario) %>% summarise_all(mean) %>% select(-Year)
indiseas_baseline3$value<- "2015"

fish_pressure<- fish_pressure %>% select(Year, scenario, inversepressure)


indiseasfish_baseline<- fish_pressure %>% left_join(tl_landings)
indiseasfish_baseline2<- indiseasfish_baseline %>% filter(Year<2015) %>% filter(Year>2009)

#write.csv(fish_eco, "chapter_4/fisheries_ecosystem/fish_eco_baseline.csv", row.names = F)

indiseasfish_baseline3<- indiseasfish_baseline2 %>% group_by(scenario) %>% summarise_all(mean) %>% select(-Year)
indiseasfish_baseline3$value<- "2015"