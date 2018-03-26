closeAllConnections()
library(tidyverse)
library(rlpi)

#Base Case
read.csv("biomass/lpi_files/bc_lpi.csv", check.names = FALSE) %>% select(-ID) %>% gather ("year", "biomass", 2:154) %>%
  filter(year>1980, year<2101) %>% filter(Binomial %in% c('DEL', 'FWH', 'HAD', 'HOS', 'NCO', 'POB', 'RED', 'SHO', 'SSK', 'SWH')) %>%
  rename(species=Binomial)%>% mutate(scenario="base_case") ->bc_ts
bc_ts$species<- as.character(bc_ts$species)
bc_ts$year<- as.integer(bc_ts$year)
ggplot(bc_ts, aes(year, biomass)) + geom_line() +
  facet_wrap(~species, scales="free")

#Fisheries scenarios

#FMSY0
read.csv("biomass/lpi_files/fmsy0_lpi.csv", check.names = FALSE) %>% select(-ID) %>% gather ("year", "biomass", 2:154) %>%
  filter(year>1980, year<2101) %>% filter(Binomial %in% c('DEL', 'FWH', 'HAD', 'HOS', 'NCO', 'POB', 'RED', 'SHO', 'SSK', 'SWH')) %>%
  rename(species=Binomial)%>% mutate(scenario="fmsy0") ->fmsy0_ts
fmsy0_ts$species<- as.character(fmsy0_ts$species)
fmsy0_ts$year<- as.integer(fmsy0_ts$year)
ggplot(fmsy0_ts, aes(year, biomass)) + geom_line() +
  facet_wrap(~species, scales="free")

#FMSY05
read.csv("biomass/lpi_files/fmsy05_lpi.csv", check.names = FALSE) %>% select(-ID) %>% gather ("year", "biomass", 2:154) %>%
  filter(year>1980, year<2101) %>% filter(Binomial %in% c('DEL', 'FWH', 'HAD', 'HOS', 'NCO', 'POB', 'RED', 'SHO', 'SSK', 'SWH')) %>%
  rename(species=Binomial) %>% mutate(scenario="fmsy05")->fmsy05_ts
fmsy05_ts$species<- as.character(fmsy05_ts$species)
fmsy05_ts$year<- as.integer(fmsy05_ts$year)
ggplot(fmsy05_ts, aes(year, biomass)) + geom_line() +
  facet_wrap(~species, scales="free")

#FMSY1
read.csv("biomass/lpi_files/fmsy1_lpi.csv", check.names = FALSE) %>% select(-ID) %>% gather ("year", "biomass", 2:154) %>%
  filter(year>1980, year<2101) %>% filter(Binomial %in% c('DEL', 'FWH', 'HAD', 'HOS', 'NCO', 'POB', 'RED', 'SHO', 'SSK', 'SWH')) %>%
  rename(species=Binomial) %>% mutate(scenario="fmsy1") ->fmsy1_ts
fmsy1_ts$species<- as.character(fmsy1_ts$species)
fmsy1_ts$year<- as.integer(fmsy1_ts$year)
ggplot(fmsy1_ts, aes(year, biomass)) + geom_line() +
  facet_wrap(~species, scales="free")

#FMSY2
read.csv("biomass/lpi_files/fmsy2_lpi.csv", check.names = FALSE) %>% select(-ID) %>% gather ("year", "biomass", 2:154) %>%
  filter(year>1980, year<2101) %>% filter(Binomial %in% c('DEL', 'FWH', 'HAD', 'HOS', 'NCO', 'POB', 'RED', 'SHO', 'SSK', 'SWH')) %>%
  rename(species=Binomial) %>% mutate(scenario="fmsy2")->fmsy2_ts
fmsy2_ts$species<- as.character(fmsy2_ts$species)
fmsy2_ts$year<- as.integer(fmsy2_ts$year)
ggplot(fmsy2_ts, aes(year, biomass)) + geom_line() +
  facet_wrap(~species, scales="free")

fmsy_ts<- rbind(bc_ts, fmsy0_ts, fmsy05_ts, fmsy1_ts, fmsy2_ts)

ggplot(fmsy_ts, aes(year, biomass)) +geom_line(aes(colour=species))+
  facet_wrap(~scenario)

ggplot(fmsy_ts, aes(year, biomass)) + geom_line(aes(colour=scenario)) +
  facet_wrap(~species, scales="free")

##MPA Scenarios
#MPA10
read.csv("biomass/lpi_files/mpa10_lpi.csv", check.names = FALSE) %>% select(-ID) %>% gather ("year", "biomass", 2:154) %>%
  filter(year>1980, year<2101) %>% filter(Binomial %in% c('DEL', 'FWH', 'HAD', 'HOS', 'NCO', 'POB', 'RED', 'SHO', 'SSK', 'SWH')) %>%
  rename(species=Binomial) %>% mutate(scenario="mpa10")->mpa10_ts
mpa10_ts$species<- as.character(mpa10_ts$species)
mpa10_ts$year<- as.integer(mpa10_ts$year)
ggplot(mpa10_ts, aes(year, biomass)) + geom_line() +
  facet_wrap(~species, scales="free")

#MPA25
read.csv("biomass/lpi_files/mpa25_lpi.csv", check.names = FALSE) %>% select(-ID) %>% gather ("year", "biomass", 2:154) %>%
  filter(year>1980, year<2101) %>% filter(Binomial %in% c('DEL', 'FWH', 'HAD', 'HOS', 'NCO', 'POB', 'RED', 'SHO', 'SSK', 'SWH')) %>%
  rename(species=Binomial) %>% mutate(scenario="mpa25")->mpa25_ts
mpa25_ts$species<- as.character(mpa25_ts$species)
mpa25_ts$year<- as.integer(mpa25_ts$year)
ggplot(mpa25_ts, aes(year, biomass)) + geom_line() +
  facet_wrap(~species, scales="free")

#MPA50
read.csv("biomass/lpi_files/mpa50_lpi.csv", check.names = FALSE) %>% select(-ID) %>% gather ("year", "biomass", 2:154) %>%
  filter(year>1980, year<2101) %>% filter(Binomial %in% c('DEL', 'FWH', 'HAD', 'HOS', 'NCO', 'POB', 'RED', 'SHO', 'SSK', 'SWH')) %>%
  rename(species=Binomial) %>% mutate(scenario="mpa50")->mpa50_ts
mpa50_ts$species<- as.character(mpa50_ts$species)
mpa50_ts$year<- as.integer(mpa50_ts$year)
ggplot(mpa50_ts, aes(year, biomass)) + geom_line() +
  facet_wrap(~species, scales="free")

mpa_ts<- rbind(bc_ts, mpa10_ts, mpa25_ts, mpa50_ts)

ggplot(mpa_ts, aes(year, biomass)) +geom_line(aes(colour=species))+
  facet_wrap(~scenario)

ggplot(mpa_ts, aes(year, biomass)) + geom_line(aes(colour=scenario)) +
  facet_wrap(~species, scales="free")

##Climate change scenarios
#CC2
read.csv("biomass/lpi_files/cc2_lpi.csv", check.names = FALSE) %>% select(-ID) %>% gather ("year", "biomass", 2:154) %>%
  filter(year>1980, year<2101) %>% filter(Binomial %in% c('DEL', 'FWH', 'HAD', 'HOS', 'NCO', 'POB', 'RED', 'SHO', 'SSK', 'SWH')) %>%
  rename(species=Binomial) %>% mutate(scenario="cc2")->cc2_ts
cc2_ts$species<- as.character(cc2_ts$species)
cc2_ts$year<- as.integer(cc2_ts$year)
ggplot(cc2_ts, aes(year, biomass)) + geom_line() +
  facet_wrap(~species, scales="free")

##CC3
read.csv("biomass/lpi_files/cc3_lpi.csv", check.names = FALSE) %>% select(-ID) %>% gather ("year", "biomass", 2:154) %>%
  filter(year>1980, year<2101) %>% filter(Binomial %in% c('DEL', 'FWH', 'HAD', 'HOS', 'NCO', 'POB', 'RED', 'SHO', 'SSK', 'SWH')) %>%
  rename(species=Binomial) %>% mutate(scenario="cc3")->cc3_ts
cc3_ts$species<- as.character(cc3_ts$species)
cc3_ts$year<- as.integer(cc3_ts$year)
ggplot(cc3_ts, aes(year, biomass)) + geom_line() +
  facet_wrap(~species, scales="free")

##oa005
read.csv("biomass/lpi_files/oa005_lpi.csv", check.names = FALSE) %>% select(-ID) %>% gather ("year", "biomass", 2:154) %>%
  filter(year>1980, year<2101) %>% filter(Binomial %in% c('DEL', 'FWH', 'HAD', 'HOS', 'NCO', 'POB', 'RED', 'SHO', 'SSK', 'SWH')) %>%
  rename(species=Binomial) %>% mutate(scenario="oa005")->oa005_ts
oa005_ts$species<- as.character(oa005_ts$species)
oa005_ts$year<- as.integer(oa005_ts$year)
ggplot(oa005_ts, aes(year, biomass)) + geom_line() +
  facet_wrap(~species, scales="free")


##oa01
read.csv("biomass/lpi_files/oa01_lpi.csv", check.names = FALSE) %>% select(-ID) %>% gather ("year", "biomass", 2:154) %>%
  filter(year>1980, year<2101) %>% filter(Binomial %in% c('DEL', 'FWH', 'HAD', 'HOS', 'NCO', 'POB', 'RED', 'SHO', 'SSK', 'SWH')) %>%
  rename(species=Binomial) %>% mutate(scenario="oa01")->oa01_ts
oa01_ts$species<- as.character(oa01_ts$species)
oa01_ts$year<- as.integer(oa01_ts$year)
ggplot(oa01_ts, aes(year, biomass)) + geom_line() +
  facet_wrap(~species, scales="free")

cc_ts<- rbind(bc_ts, cc2_ts, cc3_ts, oa005_ts, oa01_ts)

ggplot(cc_ts, aes(year, biomass)) +geom_line(aes(colour=species))+
  facet_wrap(~scenario)

ggplot(cc_ts, aes(year, biomass)) + geom_line(aes(colour=scenario)) +
  facet_wrap(~species, scales="free")

multi_ts<- rbind(bc_ts, oa005_ts, fmsy0_ts, fmsy2_ts, mpa50_ts)

ggplot(multi_ts, aes(year, biomass)) + geom_line(aes(colour=scenario)) +
  facet_wrap(~species, scales="free")
