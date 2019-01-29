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

write.csv(fmsy_catch, "chapter_4/catch/catch_all_years.csv", row.names = F)

fmsy_catch %>% filter(Year> 2014) %>% ggplot(aes(Year, catch)) +geom_line(aes(colour=species))+
  facet_wrap(~scenario)

fmsy_catch %>% filter(Year> 2014) %>% ggplot(aes(Year, catch)) + geom_line(aes(colour=scenario)) +
  facet_wrap(~species, scales="free")

total_catch<- fmsy_catch %>% group_by(scenario, Year) %>% summarise(total_catch=sum(catch)) %>% ungroup()%>%
  group_by(scenario) %>% summarise(total_catch=sum(total_catch))

write.csv(total_catch, "chapter_4/catch/total_catch2.csv")

####ADD IN PRICE/TON#####

usd_ton<- read.csv("usd_ton.csv")
fmsy_catch %>% left_join(usd_ton, by="species") %>% mutate(total_usd=catch*USD) %>% group_by(scenario, Year) %>%
  summarise(total_usd=sum(total_usd))->catch_price

catch_price_total<- catch_price%>% group_by(scenario) %>% summarise(total_usd=sum(total_usd2))

catch_price %>% filter(Year>2014) %>% ggplot(aes(Year, total_usd))+geom_line(aes(colour=scenario), lwd=1.5)+
  ylab("Catch Price (USD)")

write.csv(catch_price_total, "chapter_4/catch/total_price.csv", row.names = F)

write.csv(catch_price, "chapter_4/catch/price_year.csv", row.names=F)



#######Before/After######

read.csv("chapter_4/catch/before_after.csv")-> before_after

ggplot(before_after,aes(x=species,y=catch,fill=scenario))+
  geom_bar(stat="identity",position="dodge")+
  scale_fill_brewer(palette="Dark2")+
  xlab("Species")+ylab("Catch")+
  ggtitle("Average annual catch of fished species under different scenarios compared to historical catch")