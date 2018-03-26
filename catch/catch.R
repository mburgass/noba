closeAllConnections()
library(tidyverse)
#Read base case
read.csv("catch/bc_catch.csv") %>% select(Year, REO, GRH, HAD, SAI, RED, NCO, CAP) %>% 
  gather("species", "catch", 2:8) %>% mutate(scenario="base_case") %>% filter(Year<2101) -> bc_catch
#Read fisheries scenarios
read.csv("catch/fmsy2_catch.csv")%>% select(Year, REO, GRH, HAD, SAI, RED, NCO, CAP) %>% 
  gather("species", "catch", 2:8) %>% mutate(scenario="fmsy2") %>% filter(Year<2101)  -> fmsy2_catch
read.csv("catch/fmsy1_catch.csv")%>% select(Year, REO, GRH, HAD, SAI, RED, NCO, CAP) %>% 
  gather("species", "catch", 2:8) %>% mutate(scenario="fmsy1")%>% filter(Year<2101) -> fmsy1_catch
read.csv("catch/fmsy05_catch.csv")%>% select(Year, REO, GRH, HAD, SAI, RED, NCO, CAP) %>% 
  gather("species", "catch", 2:8) %>% mutate(scenario="fmsy05") %>% filter(Year<2101) -> fmsy05_catch

fmsy_catch<- rbind(bc_catch, fmsy05_catch, fmsy1_catch, fmsy2_catch)

ggplot(fmsy_catch, aes(Year, catch)) +geom_line(aes(colour=species))+
  facet_wrap(~scenario)

ggplot(fmsy_catch, aes(Year, catch)) + geom_line(aes(colour=scenario)) +
  facet_wrap(~species, scales="free")

##Read MPA Scenarios
read.csv("catch/mpa10_catch.csv")%>% select(Year, REO, GRH, HAD, SAI, RED, NCO, CAP) %>% 
  gather("species", "catch", 2:8) %>% mutate(scenario="mpa10")%>% filter(Year<2101) -> mpa10_catch
read.csv("catch/mpa25_catch.csv")%>% select(Year, REO, GRH, HAD, SAI, RED, NCO, CAP) %>% 
  gather("species", "catch", 2:8) %>% mutate(scenario="mpa25")%>% filter(Year<2101) -> mpa25_catch
read.csv("catch/mpa50_catch.csv")%>% select(Year, REO, GRH, HAD, SAI, RED, NCO, CAP) %>% 
  gather("species", "catch", 2:8) %>% mutate(scenario="mpa50")%>% filter(Year<2101)-> mpa50_catch

mpa_catch<- rbind(bc_catch, mpa10_catch, mpa25_catch, mpa50_catch)

ggplot(mpa_catch, aes(Year, catch)) +geom_line(aes(colour=species))+
  facet_wrap(~scenario)

ggplot(mpa_catch, aes(Year, catch)) +geom_line(aes(colour=scenario)) +
  facet_wrap(~species, scales="free")

##Read Climate Change Scenarios
read.csv("catch/cc2_catch.csv")%>% select(Year, REO, GRH, HAD, SAI, RED, NCO, CAP) %>% 
  gather("species", "catch", 2:8) %>% mutate(scenario="cc2")%>% filter(Year<2101) -> cc2_catch
read.csv("catch/cc3_catch.csv")%>% select(Year, REO, GRH, HAD, SAI, RED, NCO, CAP) %>% 
  gather("species", "catch", 2:8) %>% mutate(scenario="cc3")%>% filter(Year<2101) -> cc3_catch
read.csv("catch/oa005_catch.csv")%>% select(Year, REO, GRH, HAD, SAI, RED, NCO, CAP) %>% 
  gather("species", "catch", 2:8) %>% mutate(scenario="oa005")%>% filter(Year<2101)-> oa005_catch
read.csv("catch/oa01_catch.csv")%>% select(Year, REO, GRH, HAD, SAI, RED, NCO, CAP) %>% 
  gather("species", "catch", 2:8) %>% mutate(scenario="oa01")%>% filter(Year<2101)-> oa01_catch

cc_catch<- rbind(bc_catch, cc2_catch, cc3_catch, oa005_catch, oa01_catch)

ggplot(cc_catch, aes(Year, catch)) +geom_line(aes(colour=species))+
  facet_wrap(~scenario)

ggplot(cc_catch, aes(Year, catch)) +geom_line(aes(colour=scenario)) +
  facet_wrap(~species, scales="free")

all_catch<- rbind(bc_catch, fmsy05_catch, fmsy1_catch, fmsy2_catch, mpa10_catch, mpa25_catch, mpa50_catch, cc2_catch, cc3_catch, oa005_catch, oa01_catch)

ggplot(all_catch, aes(Year, catch)) +geom_line(aes(colour=species))+
  facet_wrap(~scenario)

ggplot(all_catch, aes(Year, catch)) +geom_line(aes(colour=scenario)) +
  facet_wrap(~species, scales="free") 


