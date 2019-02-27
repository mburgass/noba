closeAllConnections()
library(tidyverse)
#Read base case
read.csv("chapter_4/catch/fmsy_0.6_catch.csv") %>% select(Year, REO, GRH, MAC, HAD, SAI, RED, BWH, SSH, NCO, CAP, PWN) %>% 
  gather("species", "catch", 2:12) %>% mutate(scenario="Precautionary Fishing") -> catch06
#Read fisheries scenarios
read.csv("chapter_4/catch/fmsy_0.8_catch.csv")%>% select(Year, REO, GRH, MAC, HAD, SAI, RED, BWH, SSH, NCO, CAP, PWN) %>% 
  gather("species", "catch", 2:12) %>% mutate(scenario="fmsy0.8")  -> catch08
read.csv("chapter_4/catch/fmsy_1_catch.csv")%>% select(Year, REO, GRH, MAC, HAD, SAI, RED, BWH, SSH, NCO, CAP, PWN) %>% 
  gather("species", "catch", 2:12) %>% mutate(scenario="Global Sustainability") -> catch1
read.csv("chapter_4/catch/fmsy_1.1_catch.csv")%>% select(Year, REO, GRH, MAC, HAD, SAI, RED, BWH, SSH, NCO, CAP, PWN) %>% 
  gather("species", "catch", 2:12) %>% mutate(scenario="fmsy1.1") -> catch1.1

fmsy_catch<- rbind(catch06, catch1)

#write.csv(fmsy_catch, "chapter_4/catch/catch_all_years.csv", row.names = F)

fmsy_catch %>% filter(Year> 2014) %>% ggplot(aes(Year, catch)) +geom_line(aes(colour=species))+
  facet_wrap(~scenario)

fmsy_catch %>% filter(Year> 2014) %>% ggplot(aes(Year, catch)) + geom_line(aes(colour=scenario), lwd=1) +
  scale_color_brewer(legend_title, palette="Dark2")+
  facet_wrap(~species, scales="free")+
  ylab("Catch (tonnes)")

total_catch<- fmsy_catch %>% group_by(scenario, Year) %>% summarise(total_catch=sum(catch)) %>% ungroup() %>% filter(Year>2014)

a<- ggplot(total_catch, aes(Year, total_catch)) +geom_line(aes(colour=scenario), lwd=1)+
  ylab("Total Catch (Tonnes)")+ scale_color_brewer(palette = "Dark2")+theme(legend.position="none")

write.csv(total_catch, "chapter_4/catch/total_catch2.csv")

####ADD IN PRICE/TON#####

usd_ton<- read.csv("usd_ton.csv")
fmsy_catch %>% left_join(usd_ton, by="species") %>% mutate(total_usd=catch*USD) %>% group_by(scenario, Year) %>%
  summarise(total_usd=sum(total_usd))->catch_price

catch_price_total<- catch_price%>% group_by(scenario) %>% summarise(total_usd=sum(total_usd2))

catch_price %>% filter(Year>2014) %>% ggplot(aes(Year, total_usd))+geom_line(aes(colour=scenario), lwd=1.5)+
  ylab("Catch Price (USD)")

#write.csv(catch_price_total, "chapter_4/catch/total_price.csv", row.names = F)

write.csv(catch_price, "chapter_4/catch/price_year.csv", row.names=F)



#######Before/After######

read.csv("chapter_4/catch/before_after.csv")-> before_after

before_after %>% filter(scenario %in% c('Historical Catch', 'Precautionary Fishing', 'Global Sustainability')) -> before_after

ggplot(before_after,aes(x=species,y=catch,fill=scenario))+
  geom_bar(stat="identity",position="dodge")+
  scale_fill_brewer(legend_title, palette="Dark2")+
  xlab("Species")+ylab("Catch (tonnes)")+
  ggtitle("Average annual catch of fished species under different scenarios compared to historical catch")

####Pelagic Catch########

fmsy_catch%>% filter(species %in% c('MAC', 'SAI', 'BWH', 'SSH', 'CAP'))%>% group_by(scenario, Year) %>%
  summarise(total_catch=sum(catch)) %>% ungroup() %>% filter(Year>2014)-> pel_catch

b<- ggplot(pel_catch, aes(Year, total_catch)) +geom_line(aes(colour=scenario), lwd=1)+
         ylab("Pelagic Catch (Tonnes)")+ scale_color_brewer(palette = "Dark2")+theme(legend.position="none")

###Demersal Catch#####

fmsy_catch%>%filter(species %in% c('REO', 'GRH', 'HAD', 'RED', 'NCO', 'PWN'))%>% group_by(scenario, Year) %>%
  summarise(total_catch=sum(catch)) %>% ungroup() %>% filter(Year>2014)-> dem_catch

c<- ggplot(dem_catch, aes(Year, total_catch)) +geom_line(aes(colour=scenario), lwd=1)+
  ylab("Demersal Catch (Tonnes)")+ scale_color_brewer(palette = "Dark2")+theme(legend.position="none")

####Fish Exploitation Rate (summed catch/summed biomass)####

fish_catch<- fmsy_catch %>% group_by(scenario, Year) %>% filter(species != 'PWN')%>% summarise(total_catch=sum(catch)) %>% ungroup() %>% filter(Year>2014)

read.csv("chapter_4/biomass/fmsy1_biomass.csv", check.names=F) %>%
  gather("Binomial", "biomass", 2:55) %>% rename(year=Year) %>% mutate(scenario="fmsy1") -> fmsy1_biomass
read.csv("chapter_4/biomass/fmsy06_biomass.csv", check.names=F) %>%
  gather("Binomial", "biomass", 2:55) %>% rename(year=Year)%>% mutate(scenario="fmsy06") -> fmsy06_biomass
read.csv("chapter_4/biomass/fmsy0_biomass.csv", check.names=F) %>%
  gather("Binomial", "biomass", 2:55) %>% rename(year=Year)%>% mutate(scenario="fmsy0") -> fmsy0_biomass
rbind(fmsy1_biomass, fmsy06_biomass) %>% filter(year>2014) ->fmsy_biomass

fmsy_biomass$scenario[fmsy_biomass$scenario=="fmsy1"]<- "Global Sustainability" 
fmsy_biomass$scenario[fmsy_biomass$scenario=="fmsy0"]<- "Strict Conservation"
fmsy_biomass$scenario[fmsy_biomass$scenario=="fmsy06"]<- "Precautionary Fishing" 

fish_biomass<- fmsy_biomass %>% filter(Binomial %in% c('DEO', 'PEL', 'PES', 'REO', 'DEL', 'FLA', 'LRD', 'SSK', 'MES', 'GRH', 'MAC', 'HAD', 'SAI', 'BWH', 'RED', 'SSH', 'NCO', 'PCO', 'CAP'))%>%
  group_by(scenario, year) %>% summarise(total_biomass=sum(biomass)) %>% ungroup() %>% rename(Year=year)

fish_catch %>% left_join(fish_biomass) %>%
  mutate(FER= total_catch/total_biomass) -> fishexp

fishexp$Year<- as.integer(fishexp$Year)

fishexp<- fishexp %>% select(scenario, Year, FER)

d<- ggplot(fishexp, aes(Year, FER))+ geom_line(aes(colour=scenario), lwd=1)+
         ylab("Fish Exploitation Rate")+ scale_color_brewer(palette = "Dark2")+theme(legend.position="none")

e<- ggplot(fish_catch, aes(Year, total_catch))+ geom_line(aes(colour=scenario), lwd=1)+
  ylab("Fish Catch (Tonnes)")+ scale_color_brewer(palette = "Dark2")+theme(legend.position="none")

###Target Species Exploitation Rate####

target_biomass<- fmsy_biomass %>% filter(Binomial %in% c('REO', 'GRH', 'MAC', 'HAD','SAI', 'RED', 'BWH', 'SSH', 'NCO', 'CAP', 'PWN')) %>%
  rename (species=Binomial, Year=year)%>%
  group_by(scenario, Year) %>% summarise(total_biomass=sum(biomass)) %>% ungroup()

target_biomass %>% left_join(total_catch) %>%
  mutate(ER=total_catch/total_biomass)-> exp_rate

exp_rate<- exp_rate %>% select(scenario, Year, ER)

f<- ggplot(exp_rate, aes(Year, ER))+ geom_line(aes(colour=scenario), lwd=1)+
  ylab("Exploitation Rate of Target Species")+ scale_color_brewer(palette = "Dark2")+theme(legend.position="none")

legend<- ggplot(exp_rate, aes(Year, ER))+ geom_line(aes(colour=scenario), lwd=1)+
  ylab("Exploitation Rate of Target Species")+ scale_color_brewer(palette = "Dark2")
legend<- get_legend(legend)


prow<- plot_grid(a, b, c, e, d, f, labels = c("A", "B", "C", "D", "E", "F"), align = "v", ncol=3)
plot_grid(prow, legend, rel_widths = c(3, .4), label_size = 8)

