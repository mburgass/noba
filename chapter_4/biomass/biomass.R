library(tidyverse)
fmsy0_data<- read.csv("chapter_4/biomass/fmsy0_biomass.csv")
fmsy1_data<- read.csv("chapter_4/biomass/fmsy1_biomass.csv")
fmsy11_data<- read.csv("chapter_4/biomass/fmsy11_biomass.csv")
fmsy08_data<- read.csv("chapter_4/biomass/fmsy08_biomass.csv")
fmsy06_data<- read.csv("chapter_4/biomass/fmsy06_biomass.csv")

fmsy0_data %>% gather("species", "biomass", 2:55) %>% mutate(scenario="Strict Conservation")->fmsy0_data
fmsy1_data %>% gather("species", "biomass", 2:55) %>% mutate(scenario="Global Sustainability")->fmsy1_data
fmsy11_data %>% gather("species", "biomass", 2:55) %>% mutate(scenario="fmsy11")->fmsy11_data
fmsy08_data %>% gather("species", "biomass", 2:55) %>% mutate(scenario="fmsy08")->fmsy08_data
fmsy06_data %>% gather("species", "biomass", 2:55) %>% mutate(scenario="Precautionary Fishing")->fmsy06_data


biomass<- rbind(fmsy0_data, fmsy1_data, fmsy06_data)

biomass%>% filter(Year>2014)->biomass

legend_title<- "Scenario"

biomass %>% filter(species %in% c('BWH', 'CAP', 'CEP', 'DEL', 'DEO', 'FLA', 'GRH', 'HAD', 'KWH', 'LRD', 'MAC', 'MES', 'NCO', 'PEL', 'PES', 'PL', 'RED', 'REO', 'SAI', 'SBB', 'SSH', 'ZM'))-> test

#write.csv(test, "chapter_4/biomass/rename.csv", row.names = F)

biomass<- read.csv("chapter_4/biomass/rename.csv")

ggplot(biomass, aes(Year, biomass)) +geom_line(aes(colour=scenario), lwd=1)+
  scale_color_brewer(legend_title, palette="Dark2")+
  facet_wrap(~species, scales = "free")+
  ylab("Biomass (tonnes)")+ 
  theme(strip.text.x = element_text(size = 10))+theme(axis.text=element_text(size=15))+theme(axis.title.x=element_text(size=20))+
  theme(axis.title.y=element_text(size=20))+ theme(legend.text=element_text(size=20))+
  theme(legend.title=element_text(size=20))+ 
  scale_x_continuous(breaks=scales::pretty_breaks(n = 3))
                                                              
####Iconic Species Abundance######

iconic<- biomass %>% filter(species %in% c('POB', 'KWH', 'SWH', 'HWH', 'MWH', 'FWH', 'BES', 'HOS', 'RIS', 'SBA', 'SBB')) %>%
  group_by(scenario, Year) %>% mutate(iconic_biomass=mean(biomass)) %>% select(Year, scenario, iconic_biomass) %>% unique()

ggplot(iconic, aes(Year, iconic_biomass)) +geom_line(aes(colour=scenario), lwd=1)+
  scale_color_brewer(legend_title, palette="Dark2")+
  ylab("Iconic Species Biomass (tonnes)")

write.csv(iconic, "chapter_4/biomass/iconic_species.csv")
