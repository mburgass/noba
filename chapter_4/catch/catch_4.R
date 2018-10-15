closeAllConnections()
library(tidyverse)
#Read base case
read.csv("chapter_4/catch/fmsy_0.6_catch.csv") %>% select(Year, REO, GRH, MAC, HAD, SAI, RED, BWH, SSH, NCO, CAP, PWN) %>% 
  gather("species", "catch", 2:12) %>% mutate(scenario="fmsy0.6") -> catch06
#Read fisheries scenarios
read.csv("chapter_4/catch/fmsy_0.8_catch.csv")%>% select(Year, REO, GRH, MAC, HAD, SAI, RED, BWH, SSH, NCO, CAP, PWN) %>% 
  gather("species", "catch", 2:12) %>% mutate(scenario="fmsy0.8")  -> catch08
read.csv("chapter_4/catch/fmsy_1_catch.csv")%>% select(Year, REO, GRH, MAC, HAD, SAI, RED, BWH, SSH, NCO, CAP, PWN) %>% 
  gather("species", "catch", 2:12) %>% mutate(scenario="fmsy1") -> catch1
read.csv("chapter_4/catch/fmsy_1.1_catch.csv")%>% select(Year, REO, GRH, MAC, HAD, SAI, RED, BWH, SSH, NCO, CAP, PWN) %>% 
  gather("species", "catch", 2:12) %>% mutate(scenario="fmsy1.1") -> catch1.1

fmsy_catch<- rbind(catch06, catch08, catch1, catch1.1)

fmsy_catch %>% filter(Year> 2014) %>% ggplot(aes(Year, catch)) +geom_line(aes(colour=species))+
  facet_wrap(~scenario)

fmsy_catch %>% filter(Year> 2014) %>% ggplot(aes(Year, catch)) + geom_line(aes(colour=scenario)) +
  facet_wrap(~species, scales="free")