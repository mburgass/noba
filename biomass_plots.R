read.csv("lpi_final/biomass_new/fmsy1_biomass.csv", check.names=F) %>%
  gather("year", "biomass", 3:155)%>%
  filter(year<2016, year>1980)-> fmsy1_biomass
fmsy1_biomass$scenario<- "fmsy1"
read.csv("lpi_final/biomass_new/fmsy0_biomass.csv", check.names=F) %>%
  gather("year", "biomass", 3:155)%>%
  filter(year<2016, year>1980)-> fmsy0_biomass
fmsy0_biomass$scenario<- "fmsy0"

read.csv("lpi_final/biomass_new/fmsy2_biomass.csv", check.names=F) %>%
  gather("year", "biomass", 3:155) %>%
  filter(year<2016, year>1980)-> fmsy2_biomass
fmsy2_biomass$scenario<- "fmsy2"

rbind(fmsy1_biomass, fmsy2_biomass, fmsy0_biomass) %>% select(-ID) -> fmsy_biomass

fmsy_biomass$Binomial<- as.character(fmsy_biomass$Binomial)
fmsy_biomass$year<- as.integer(fmsy_biomass$year)

ggplot(fmsy_biomass, aes(year, biomass)) +geom_line(aes(colour=scenario))+facet_wrap(~Binomial, scales="free")


#####OCean acidification#####
read.csv("lpi_final/biomass_new/oa005_biomass.csv", check.names=F) %>%
  gather("year", "biomass", 3:155)%>%
  filter(year<2016, year>1980)-> oa005_biomass
oa005_biomass$scenario<- "oa005"

read.csv("lpi_final/biomass_new/oa01_biomass.csv", check.names=F) %>%
  gather("year", "biomass", 3:155) %>%
  filter(year<2016, year>1980)-> oa01_biomass
oa01_biomass$scenario<- "oa01"

rbind(fmsy1_biomass, oa005_biomass, oa01_biomass) %>% select(-ID) -> oa_biomass
oa_biomass$Binomial<- as.character(oa_biomass$Binomial)
oa_biomass$year<- as.integer(oa_biomass$year)
ggplot(oa_biomass, aes(year, biomass)) +geom_line(aes(colour=scenario))+facet_wrap(~Binomial, scales="free")