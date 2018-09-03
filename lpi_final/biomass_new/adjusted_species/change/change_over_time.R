library(tidyverse)
fmsy0_change<- read.csv("lpi_final/biomass_new/adjusted_species/change/fmsy0_change.csv", check.names = FALSE)
fmsy1_change<- read.csv("lpi_final/biomass_new/adjusted_species/change/fmsy1_change.csv", check.names = FALSE)
fmsy2_change<- read.csv("lpi_final/biomass_new/adjusted_species/change/fmsy2_change.csv", check.names = FALSE)

fmsy0_change %>% select(Group, 4:38) %>% gather("year", "biomass", 2:36)->fmsy0_change
fmsy0_change %>% group_by(Group, year) %>% summarize(biomass = sum(biomass, na.rm=T))%>%
  ungroup()-> fmsy0_change
fmsy0_change %>% group_by(Group) %>% mutate(pct_change = (biomass/lag(biomass) - 1) * 100) %>% ungroup() ->fmsy0_change
fmsy0_change$scenario<- "fmsy0"
fmsy0_change$Group<- as.character(fmsy0_change$Group)
fmsy0_change$year<- as.numeric(fmsy0_change$year)
fmsy0_change$biomass<- as.numeric(fmsy0_change$biomass)
fmsy0_change$pct_change<- as.numeric(fmsy0_change$pct_change)
fmsy0_change[is.na(fmsy0_change)] <- 0

ggplot(fmsy0_change, aes(year,pct_change)) + 
  geom_point()+
  geom_line(aes(colour=Group))
