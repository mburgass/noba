library(readtext)
library(tidyverse)
closeAllConnections()
##IndiSeas Indicators:

#Total Biomass of surveyed species (PEL, REO, GRH, MAC, HAD,SAI, RED,BWH, SSH, NCO, PCO, CAP, PWN, KCR, SCR, ZL, ZM, ZS)

#Read base case
read.csv("chapter_4/biomass/fmsy1_biomass.csv", check.names=F)  %>% gather("Binomial", "biomass", 3:55)%>%
  filter(Binomial %in% c('CAP','PEL', 'REO', 'GRH', 'MAC', 'HAD','SAI', 'RED','BWH', 'SSH', 'NCO', 'PCO', 'PWN', 'KCR', 'SCR'))%>%
  filter(Year>2014) %>% group_by(Year) %>% mutate(total_biomass_surveyed= mean(biomass)) %>% ungroup() %>%
  select(Year, total_biomass_surveyed) %>% unique() %>% mutate(scenario="Global Sustainability") ->fmsy1_data

#Read fisheries scenarios
read.csv("chapter_4/biomass/fmsy11_biomass.csv", check.names=F) %>% gather("Binomial", "biomass", 3:55) %>%
  filter(Binomial %in% c('CAP','PEL', 'REO', 'GRH', 'MAC', 'HAD','SAI', 'RED','BWH', 'SSH', 'NCO', 'PCO', 'PWN', 'KCR', 'SCR'))%>%
  filter(Year>2014) %>% group_by(Year) %>% mutate(total_biomass_surveyed= mean(biomass)) %>% ungroup() %>%
  select(Year, total_biomass_surveyed) %>% unique() %>% mutate(scenario="fmsy11") ->fmsy11_data

read.csv("chapter_4/biomass/fmsy0_biomass.csv", check.names=F) %>% gather("Binomial", "biomass", 3:55) %>%
  filter(Binomial %in% c('CAP','PEL', 'REO', 'GRH', 'MAC', 'HAD','SAI', 'RED','BWH', 'SSH', 'NCO', 'PCO', 'PWN', 'KCR', 'SCR'))%>%
  filter(Year>2014) %>% group_by(Year) %>% mutate(total_biomass_surveyed= mean(biomass)) %>% ungroup() %>%
  select(Year, total_biomass_surveyed) %>% unique() %>% mutate(scenario="Strict Conservation") ->fmsy0_data


read.csv("chapter_4/biomass/fmsy06_biomass.csv", check.names=F) %>% gather("Binomial", "biomass", 3:55) %>%
  filter(Binomial %in% c('CAP','PEL', 'REO', 'GRH', 'MAC', 'HAD','SAI', 'RED','BWH', 'SSH', 'NCO', 'PCO', 'PWN', 'KCR', 'SCR'))%>%
  filter(Year>2014) %>% group_by(Year) %>% mutate(total_biomass_surveyed= mean(biomass)) %>% ungroup() %>%
  select(Year, total_biomass_surveyed) %>% unique() %>% mutate(scenario="Precautionary Fishing") -> fmsy06_data

read.csv("chapter_4/biomass/fmsy08_biomass.csv", check.names=F) %>% gather("Binomial", "biomass", 3:55) %>%
  filter(Binomial %in% c('CAP','PEL', 'REO', 'GRH', 'MAC', 'HAD','SAI', 'RED','BWH', 'SSH', 'NCO', 'PCO', 'PWN', 'KCR', 'SCR'))%>%
  filter(Year>2014) %>% group_by(Year) %>% mutate(total_biomass_surveyed= mean(biomass)) %>% ungroup() %>%
  select(Year, total_biomass_surveyed) %>% unique() %>% mutate(scenario="fmsy08") ->fmsy08_data

surveyed_biomass_no_zoo<- rbind(fmsy0_data, fmsy06_data, fmsy1_data)

surveyed_biomass_no_zoo$Year<- as.integer(surveyed_biomass_no_zoo$Year)
surveyed_biomass_no_zoo$title<- "Excluding Zooplankton"

surveyed_biomass<- rbind(fmsy0_data, fmsy06_data, fmsy1_data)

surveyed_biomass$Year<- as.integer(surveyed_biomass$Year)
#surveyed_biomass$title<- "Including Zooplankton"

#biomass<- rbind(surveyed_biomass, surveyed_biomass_no_zoo)

ggplot(biomass, aes(Year, total_biomass_surveyed)) +geom_line(aes(colour=scenario), lwd=1)+ scale_color_brewer(palette = "Dark2") +
  theme(legend.position="none")+
  ylab("Total Surveyed Biomass (tonnes)")+theme_bw()+theme(axis.text=element_text(size=20))+theme(axis.title.x=element_text(size=20))+
  theme(axis.title.y=element_text(size=20))+ theme(legend.text=element_text(size=20))+
  theme(legend.title=element_text(size=20))+theme(strip.text.x = element_text(size = 20))+
  facet_wrap(~title, scales="free", ncol = 1)

a<- ggplot(surveyed_biomass, aes(Year, total_biomass_surveyed)) +geom_line(aes(colour=scenario), lwd=1)+ scale_color_brewer(palette = "Dark2") +
  theme(legend.position="none")+
  ylab("Total Surveyed Biomass (tonnes)")+theme_bw()+theme(axis.text=element_text(size=20))+theme(axis.title.x=element_text(size=20))+
  theme(axis.title.y=element_text(size=15))+ theme(legend.text=element_text(size=20))+
  theme(legend.title=element_text(size=20))+theme(strip.text.x = element_text(size = 20))+ 
  theme(legend.position="none")

b<- ggplot(surveyed_biomass_no_zoo, aes(Year, total_biomass_surveyed)) +geom_line(aes(colour=scenario), lwd=1)+ scale_color_brewer(palette = "Dark2") +
  theme(legend.position="none")+
  ylab("Total Surveyed Biomass (tonnes)")+theme_bw()+theme(axis.text=element_text(size=20))+theme(axis.title.x=element_text(size=20))+
  theme(axis.title.y=element_text(size=15))+ theme(legend.text=element_text(size=20))+
  theme(legend.title=element_text(size=20))+theme(strip.text.x = element_text(size = 20))+ 
  theme(legend.position="none")

legend_catch<-ggplot(surveyed_biomass_no_zoo, aes(Year, total_biomass_surveyed)) +geom_line(aes(colour=scenario), lwd=1)+ scale_color_brewer(palette = "Dark2") +
  ylab("Total Surveyed Biomass (tonnes)")+theme_bw()+theme(axis.text=element_text(size=20))+theme(axis.title.x=element_text(size=20))+
  theme(axis.title.y=element_text(size=15))+ theme(legend.text=element_text(size=20))+
  theme(legend.title=element_text(size=20))+theme(strip.text.x = element_text(size = 20))+ theme(legend.position="bottom")

legend_catch<- get_legend(legend_catch)

prow<- plot_grid(a, b, labels = c("A", "B"), align = "v", ncol=1, rel_widths = c(1, 0.8))
plot_grid(prow, legend_catch, rel_heights = c(1, .2), ncol=1, rel_widths = c(1, 0.8))

#write.csv(surveyed_biomass, "chapter_4/indiseas/surveyed_biomass.csv", row.names = F)

#surveyed_biomass<- read.csv("Chapter_4/indiseas/surveyed_biomass.csv", check.names = F)
ggplot(surveyed_biomass, aes(Year, total_biomass_surveyed)) +geom_line(aes(colour=scenario))

######Inverse fishing pressure 1/(landings/biomass) retained species
read.csv("chapter_4/catch/fmsy_1_catch.csv") %>%select(Year, BWH, MAC, PWN, SSH, REO, GRH, HAD, SAI, RED, NCO, CAP) %>% 
  gather("species", "catch", 2:8) %>% group_by(Year) %>%
  mutate(total_catch=sum(catch)) %>% ungroup() %>% select(Year, total_catch) %>%
  unique()%>% mutate(scenario="Global Sustainability") %>% filter(Year>2014) -> fmsy1_catch  

read.csv("chapter_4/catch/fmsy_0.8_catch.csv") %>%select(Year, BWH, MAC, PWN, SSH, REO, GRH, HAD, SAI, RED, NCO, CAP) %>%  
  gather("species", "catch", 2:8) %>% group_by(Year) %>%
  mutate(total_catch=sum(catch)) %>% ungroup() %>% select(Year, total_catch) %>%
  unique()%>% mutate(scenario="fmsy08") %>% filter(Year>2014) -> fmsy08_catch  

read.csv("chapter_4/catch/fmsy_0.6_catch.csv") %>%select(Year, BWH, MAC, PWN, SSH, REO, GRH, HAD, SAI, RED, NCO, CAP) %>% 
  gather("species", "catch", 2:8) %>% group_by(Year) %>%
  mutate(total_catch=sum(catch)) %>% ungroup() %>% select(Year, total_catch) %>%
  unique()%>% mutate(scenario="Precautionary Fishing") %>% filter(Year>2014) -> fmsy06_catch 

read.csv("chapter_4/catch/fmsy_1.1_catch.csv") %>%select(Year, BWH, MAC, PWN, SSH, REO, GRH, HAD, SAI, RED, NCO, CAP) %>% 
  gather("species", "catch", 2:8) %>% group_by(Year) %>%
  mutate(total_catch=sum(catch)) %>% ungroup() %>% select(Year, total_catch) %>%
  unique()%>% mutate(scenario="fmsy11") %>% filter(Year>2014) -> fmsy11_catch  


read.csv("chapter_4/biomass/fmsy1_biomass.csv", check.names=F)  %>% gather("Binomial", "biomass", 3:55)%>%
  filter(Binomial %in% c('REO', 'GRH', 'HAD', 'SAI', 'RED', 'NCO', 'BWH', 'MAC', 'PWN', 'SSH', 'CAP'))%>%
  filter(Year>2014) %>% group_by(Year) %>% mutate(total_biomass_landings= sum(biomass)) %>% ungroup() %>%
  select(Year, total_biomass_landings) %>% unique() %>% mutate(scenario="Global Sustainability") ->fmsy1_data

read.csv("chapter_4/biomass/fmsy11_biomass.csv", check.names=F)  %>% gather("Binomial", "biomass", 3:55)%>%
  filter(Binomial %in% c('REO', 'GRH', 'HAD', 'SAI', 'RED', 'NCO', 'BWH', 'MAC', 'PWN', 'SSH', 'CAP'))%>%
  filter(Year>2014) %>% group_by(Year) %>% mutate(total_biomass_landings= sum(biomass)) %>% ungroup() %>%
  select(Year, total_biomass_landings) %>% unique() %>% mutate(scenario="fmsy11") ->fmsy11_data

read.csv("chapter_4/biomass/fmsy08_biomass.csv", check.names=F)  %>% gather("Binomial", "biomass", 3:55)%>%
  filter(Binomial %in% c('REO', 'GRH', 'HAD', 'SAI', 'RED', 'NCO', 'BWH', 'MAC', 'PWN', 'SSH', 'CAP'))%>%
  filter(Year>2014) %>% group_by(Year) %>% mutate(total_biomass_landings= sum(biomass)) %>% ungroup() %>%
  select(Year, total_biomass_landings) %>% unique() %>% mutate(scenario="fmsy08") ->fmsy08_data

read.csv("chapter_4/biomass/fmsy06_biomass.csv", check.names=F)  %>% gather("Binomial", "biomass", 3:55)%>%
  filter(Binomial %in% c('REO', 'GRH', 'HAD', 'SAI', 'RED', 'NCO', 'BWH', 'MAC', 'PWN', 'SSH', 'CAP'))%>%
  filter(Year>2014) %>% group_by(Year) %>% mutate(total_biomass_landings= sum(biomass)) %>% ungroup() %>%
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
  filter(Year>2014) %>% group_by(Year) %>% mutate(total_biomass_pred= mean(biomass)) %>% ungroup() %>%
  select(Year, total_biomass_pred) %>% unique() %>% mutate(scenario="Global Sustainability") ->fmsy1_pred

read.csv("chapter_4/biomass/fmsy11_biomass.csv", check.names=F) %>% gather("Binomial", "biomass", 3:55) %>%
  filter(Binomial %in% c('DEO','PEL', 'REO','DEL', 'LRD', 'SSK', 'GRH','HAD', 'SAI', 'NCO'))%>%
  filter(Year>2014) %>% group_by(Year) %>% mutate(total_biomass_pred= mean(biomass)) %>% ungroup() %>%
  select(Year, total_biomass_pred) %>% unique() %>% mutate(scenario="fmsy11") ->fmsy11_pred

read.csv("chapter_4/biomass/fmsy06_biomass.csv", check.names=F) %>% gather("Binomial", "biomass", 3:55) %>%
  filter(Binomial %in% c('DEO','PEL', 'REO','DEL', 'LRD', 'SSK', 'GRH','HAD', 'SAI', 'NCO'))%>%
  filter(Year>2014) %>% group_by(Year) %>% mutate(total_biomass_pred= mean(biomass)) %>% ungroup() %>%
  select(Year, total_biomass_pred) %>% unique() %>% mutate(scenario="Precautionary Fishing") ->fmsy06_pred

read.csv("chapter_4/biomass/fmsy08_biomass.csv", check.names=F) %>% gather("Binomial", "biomass", 3:55) %>%
  filter(Binomial %in% c('DEO','PEL', 'REO','DEL', 'LRD', 'SSK', 'GRH','HAD', 'SAI', 'NCO'))%>%
  filter(Year>2014) %>% group_by(Year) %>% mutate(total_biomass_pred= mean(biomass)) %>% ungroup() %>%
  select(Year, total_biomass_pred) %>% unique() %>% mutate(scenario="fmsy08") ->fmsy08_pred

read.csv("chapter_4/biomass/fmsy0_biomass.csv", check.names=F) %>% gather("Binomial", "biomass", 3:55) %>%
  filter(Binomial %in% c('DEO','PEL', 'REO','DEL', 'LRD', 'SSK', 'GRH','HAD', 'SAI', 'NCO'))%>%
  filter(Year>2014) %>% group_by(Year) %>% mutate(total_biomass_pred= mean(biomass)) %>% ungroup() %>%
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
  filter(Year>2014) %>%
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
  filter(Year>2014) %>%
  left_join(max_age, by='Binomial') %>% filter(!is.na(Max_age)) %>%
  group_by(Binomial, Year) %>% mutate(max_age_biomass= Max_age*biomass)%>% ungroup() %>%
  group_by(Year) %>% mutate(max_age_biomass_year= sum(max_age_biomass)) %>%
  mutate(biomass_year= sum(biomass)) %>% ungroup() %>% group_by(Year) %>%
  mutate(mean_life_span= max_age_biomass_year/biomass_year) %>% select(Year, mean_life_span) %>% 
  unique()%>% mutate(scenario="fmsy08")->life_span_fmsy08

read.csv("chapter_4/biomass/fmsy06_biomass.csv", check.names=F) %>% gather("Binomial", "biomass", 3:55) %>%
  filter(Year>2014) %>%
  left_join(max_age, by='Binomial') %>% filter(!is.na(Max_age)) %>%
  group_by(Binomial, Year) %>% mutate(max_age_biomass= Max_age*biomass)%>% ungroup() %>%
  group_by(Year) %>% mutate(max_age_biomass_year= sum(max_age_biomass)) %>%
  mutate(biomass_year= sum(biomass)) %>% ungroup() %>% group_by(Year) %>%
  mutate(mean_life_span= max_age_biomass_year/biomass_year) %>% select(Year, mean_life_span) %>% 
  unique()%>% mutate(scenario="Precautionary Fishing")->life_span_fmsy06

read.csv("chapter_4/biomass/fmsy0_biomass.csv", check.names=F) %>% gather("Binomial", "biomass", 3:55) %>%
  filter(Year>2014) %>%
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
  filter(Year>2014) %>% left_join(trophic_level, by='species') %>%
  group_by(species, Year) %>% mutate(tl_catch=TL*catch) %>% ungroup() %>% group_by(Year) %>%
  mutate(catch_year=sum(catch)) %>% mutate(tl_catch_year= sum(tl_catch)) %>%
  mutate(trophic_level_landings= tl_catch_year/catch_year) %>% ungroup() %>% select(Year, trophic_level_landings) %>%
  unique() %>% mutate(scenario='Global Sustainability')-> tl_landings_fmsy1

read.csv("chapter_4/catch/fmsy_0.8_catch.csv") %>%select(Year,REO, GRH, HAD, SAI, RED, NCO, BWH, MAC, PWN, SSH, CAP) %>% 
  gather("species", "catch", 2:12)%>%
  filter(Year>2014) %>% left_join(trophic_level, by='species') %>%
  group_by(species, Year) %>% mutate(tl_catch=TL*catch) %>% ungroup() %>% group_by(Year) %>%
  mutate(catch_year=sum(catch)) %>% mutate(tl_catch_year= sum(tl_catch)) %>%
  mutate(trophic_level_landings= tl_catch_year/catch_year) %>% ungroup() %>% select(Year, trophic_level_landings) %>%
  unique() %>% mutate(scenario='fmsy08')-> tl_landings_fmsy08

read.csv("chapter_4/catch/fmsy_0.6_catch.csv") %>%select(Year,REO, GRH, HAD, SAI, RED, NCO, BWH, MAC, PWN, SSH, CAP) %>% 
  gather("species", "catch", 2:12)%>%
  filter(Year>2014) %>% left_join(trophic_level, by='species') %>%
  group_by(species, Year) %>% mutate(tl_catch=TL*catch) %>% ungroup() %>% group_by(Year) %>%
  mutate(catch_year=sum(catch)) %>% mutate(tl_catch_year= sum(tl_catch)) %>%
  mutate(trophic_level_landings= tl_catch_year/catch_year) %>% ungroup() %>% select(Year, trophic_level_landings) %>%
  unique() %>% mutate(scenario='Precautionary Fishing')-> tl_landings_fmsy06

read.csv("chapter_4/catch/fmsy_1.1_catch.csv") %>%select(Year,REO, GRH, HAD, SAI, RED, NCO, BWH, MAC, PWN, SSH, CAP) %>% 
  gather("species", "catch", 2:12)%>%
  filter(Year>2014) %>% left_join(trophic_level, by='species') %>%
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

read.csv("chapter_4/biomass/fmsy11_biomass.csv", check.names=F) %>% gather("Binomial", "biomass", 3:55) %>%
  filter(Year>2014) %>% left_join(trophic_level, by='Binomial')%>% filter(!is.na(TL)) %>%
  group_by(Binomial, Year) %>% mutate(tl_community=TL*biomass) %>% ungroup() %>% group_by(Year) %>%
  mutate(biomass_year=sum(biomass)) %>% mutate(TL_biomass_year= sum(tl_community)) %>%
  mutate(TL_all= TL_biomass_year/biomass_year) %>% ungroup() %>% select(Year, TL_all) %>%
  unique() %>% mutate(scenario='fmsy11')-> TL_community_fmsy11

read.csv("chapter_4/biomass/fmsy0_biomass.csv", check.names=F) %>% gather("Binomial", "biomass", 3:55) %>%
  filter(Year>2014) %>% left_join(trophic_level, by='Binomial')%>% filter(!is.na(TL)) %>%
  group_by(Binomial, Year) %>% mutate(tl_community=TL*biomass) %>% ungroup() %>% group_by(Year) %>%
  mutate(biomass_year=sum(biomass)) %>% mutate(TL_biomass_year= sum(tl_community)) %>%
  mutate(TL_all= TL_biomass_year/biomass_year) %>% ungroup() %>% select(Year, TL_all) %>%
  unique() %>% mutate(scenario='Strict Conservation')-> TL_community_fmsy0

read.csv("chapter_4/biomass/fmsy06_biomass.csv", check.names=F) %>% gather("Binomial", "biomass", 3:55) %>%
  filter(Year>2014) %>% left_join(trophic_level, by='Binomial')%>% filter(!is.na(TL)) %>%
  group_by(Binomial, Year) %>% mutate(tl_community=TL*biomass) %>% ungroup() %>% group_by(Year) %>%
  mutate(biomass_year=sum(biomass)) %>% mutate(TL_biomass_year= sum(tl_community)) %>%
  mutate(TL_all= TL_biomass_year/biomass_year) %>% ungroup() %>% select(Year, TL_all) %>%
  unique() %>% mutate(scenario='Precautionary Fishing')-> TL_community_fmsy06

read.csv("chapter_4/biomass/fmsy08_biomass.csv", check.names=F) %>% gather("Binomial", "biomass", 3:55) %>%
  filter(Year>2014) %>% left_join(trophic_level, by='Binomial')%>% filter(!is.na(TL)) %>%
  group_by(Binomial, Year) %>% mutate(tl_community=TL*biomass) %>% ungroup() %>% group_by(Year) %>%
  mutate(biomass_year=sum(biomass)) %>% mutate(TL_biomass_year= sum(tl_community)) %>%
  mutate(TL_all= TL_biomass_year/biomass_year) %>% ungroup() %>% select(Year, TL_all) %>%
  unique() %>% mutate(scenario='fmsy08')-> TL_community_fmsy08

tl_community<- rbind(TL_community_fmsy0, TL_community_fmsy06, TL_community_fmsy1) #%>% filter(year>1983)
tl_community$Year<- as.integer(tl_community$Year)

ggplot(tl_community, aes(Year, TL_all)) +geom_line(aes(colour=scenario))
#write.csv(tl_community, "chapter_4/indiseas/tl_community.csv", row.names = F)



a<- ggplot(surveyed_biomass, aes(Year, total_biomass_surveyed)) +geom_line(aes(colour=scenario), lwd=1)+ scale_color_brewer(palette = "Dark2") +
  theme(legend.position="none")+
  ylab("Total Surveyed Biomass (tonnes)")
b<- ggplot(fish_pressure, aes(Year, inversepressure)) +geom_line(aes(colour=scenario), lwd=1)+ 
  theme(legend.position="none")+
  ylab("Inverse Fishing Pressure Score")+ scale_color_brewer(palette = "Dark2")
c<- ggplot(pred_fish, aes(Year, proppred)) +geom_line(aes(colour=scenario), lwd=1)+
  theme(legend.position="none")+
  ylab("Proportion of Predatory Fish")+ scale_color_brewer(palette = "Dark2")
d<- ggplot(mean_lifespan, aes(Year, mean_life_span)) +geom_line(aes(colour=scenario), lwd=1)+
  theme(legend.position="none")+
  ylab("Mean Lifespan (Years)")+ scale_color_brewer(palette = "Dark2")
e<- ggplot(tl_landings, aes(Year, trophic_level_landings)) +geom_line(aes(colour=scenario), lwd=1)+
  ylab("Trophic Level of Landings")+theme(legend.position="none")+ scale_color_brewer(palette = "Dark2")
f<- ggplot(tl_community, aes(Year, TL_all)) +geom_line(aes(colour=scenario), lwd=1)+
  ylab("Trophic Level of the Community")+theme(legend.position="none")+ scale_color_brewer(palette = "Dark2")

legend_biomass<-ggplot(tl_community, aes(Year, TL_all)) +geom_line(aes(colour=scenario), lwd=1)+ scale_color_brewer(palette = "Dark2")
legend_catch<-ggplot(fish_pressure, aes(Year, inversepressure)) +geom_line(aes(colour=scenario), lwd=1)+ scale_color_brewer(palette = "Dark2")

legend_biomass<- get_legend(legend_biomass)
legend_catch<- get_legend(legend_catch)

prow<- plot_grid(a, c, d, f, labels = c("A", "B", "C", "D"), align = "v", ncol=2)
plot_grid(prow, legend_biomass, rel_widths = c(3, .3), label_size = 8)


prow2<- plot_grid(b, e, labels = c("A", "B"), align = "v", ncol=1)
plot_grid(prow2, legend_catch, rel_widths = c(3, .3), label_size = 8)


######% Calcs####
fish_pressure<- fish_pressure %>% select(Year, scenario, inversepressure)

indiseas2<- surveyed_biomass  %>% left_join(pred_fish) %>% left_join(mean_lifespan) %>% left_join(tl_community)
indiseas_fish<- fish_pressure %>% left_join(tl_landings)
indiseas<- indiseas2 #%>% select(year, scenario, PelBioPP, BioPP, DemPel, DemPP, PropPel, PropPred)

indiseas_2030<- indiseas %>% filter(between(Year, 2025, 2029)) %>% group_by(scenario) %>% summarise_all(mean) %>% select(-Year)
indiseas_2030$value<- "2030"

indiseas_2050<- indiseas %>% filter(between(Year, 2045, 2049)) %>% group_by(scenario) %>% summarise_all(mean) %>% select(-Year)
indiseas_2050$value<- "2050"

indiseas_2068<- indiseas %>% filter(between(Year, 2063, 2067)) %>% group_by(scenario) %>% summarise_all(mean) %>% select(-Year)
indiseas_2068$value<- "2068"

final<- rbind(indiseas_baseline3, indiseas_2030, indiseas_2050, indiseas_2068)

final<- final %>% select(value, scenario, PelBioPP, BioPP, DemPel, DemPP, PropPel, PropPred)

write.csv(final, "chapter_4/fisheries_ecosystem/final_indiseas.csv", row.names=F)

indiseasfish_2030<- indiseas_fish %>% filter(between(Year, 2025, 2029)) %>% group_by(scenario) %>% summarise_all(mean) %>% select(-Year)
indiseasfish_2030$value<- "2030"

indiseasfish_2050<- indiseas_fish %>% filter(between(Year, 2045, 2049)) %>% group_by(scenario) %>% summarise_all(mean) %>% select(-Year)
indiseasfish_2050$value<- "2050"

final_fish<- rbind(indiseasfish_baseline3, indiseasfish_2030, indiseasfish_2050, indiseasfish_2068)


indiseasfish_2068<- indiseas_fish %>% filter(between(Year, 2063, 2067)) %>% group_by(scenario) %>% summarise_all(mean) %>% select(-Year)
indiseasfish_2068$value<- "2068"

final_fish<- rbind(indiseasfish_baseline3, indiseasfish_2030, indiseasfish_2050, indiseasfish_2068)

write.csv(final_fish, "chapter_4/fisheries_ecosystem/final_indiseas_fish.csv", row.names=F)

