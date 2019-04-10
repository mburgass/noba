library(tidyverse)
fmsy0_change<- read.csv("chapter_4/biomass_change/fmsy0_change.csv")
fmsy1_change<- read.csv("chapter_4/biomass_change/fmsy1_change.csv")
fmsy06_change<- read.csv("chapter_4/biomass_change/fmsy06_change.csv")

fmsy0_change%>% select(Group, X2010:X2014, X2025:X2029, X2045:X2049, X2063:X2067) %>% gather("year", "biomass", 2:21)%>%
  separate(year,c("X","year"),remove=T,sep="X") %>% group_by(Group, year) %>% summarize(biomass = sum(biomass, na.rm=T))%>%
  ungroup()-> fmsy0_change

fmsy0_change %>% filter(year %in% c(2010:2014)) %>% group_by(Group) %>% summarize(biomass= mean(biomass, na.rm=T)) -> fmsy0_2015

fmsy0_2015$year<- "2015"

fmsy0_change %>% filter(year %in% c(2025:2029)) %>% group_by(Group) %>% summarize(biomass= mean(biomass, na.rm=T)) -> fmsy0_2030

fmsy0_2030$year<- "2030"

fmsy0_change %>% filter(year %in% c(2045:2049)) %>% group_by(Group) %>% summarize(biomass= mean(biomass, na.rm=T)) -> fmsy0_2050

fmsy0_2050$year<- "2050"

fmsy0_change %>% filter(year %in% c(2063:2067)) %>% group_by(Group) %>% summarize(biomass= mean(biomass, na.rm=T)) -> fmsy0_2068

fmsy0_2068$year<- "2068"

fmsy0_change<- rbind(fmsy0_2015, fmsy0_2030, fmsy0_2050, fmsy0_2068)

fmsy0_change$Group<- as.character(fmsy0_change$Group)
fmsy0_change$year<- as.factor(fmsy0_change$year)
fmsy0_change$biomass<- as.numeric(fmsy0_change$biomass)

#write.csv(fmsy0_change, "chapter_4/biomass_change/fmsy0_tweak.csv", row.names = F)

read.csv("chapter_4/biomass_change/fmsy0_tweak.csv")-> fmsy0_change

fmsy1_change%>% select(Group, X2010:X2014, X2025:X2029, X2045:X2049, X2063:X2067) %>% gather("year", "biomass", 2:21)%>%
  separate(year,c("X","year"),remove=T,sep="X") %>% group_by(Group, year) %>% summarize(biomass = sum(biomass, na.rm=T))%>%
  ungroup()-> fmsy1_change

fmsy1_change %>% filter(year %in% c(2010:2014)) %>% group_by(Group) %>% summarize(biomass= mean(biomass, na.rm=T)) -> fmsy1_2015

fmsy1_2015$year<- "2015"

fmsy1_change %>% filter(year %in% c(2025:2029)) %>% group_by(Group) %>% summarize(biomass= mean(biomass, na.rm=T)) -> fmsy1_2030

fmsy1_2030$year<- "2030"

fmsy1_change %>% filter(year %in% c(2045:2049)) %>% group_by(Group) %>% summarize(biomass= mean(biomass, na.rm=T)) -> fmsy1_2050

fmsy1_2050$year<- "2050"

fmsy1_change %>% filter(year %in% c(2063:2067)) %>% group_by(Group) %>% summarize(biomass= mean(biomass, na.rm=T)) -> fmsy1_2068

fmsy1_2068$year<- "2068"

fmsy1_change<- rbind(fmsy1_2015, fmsy1_2030, fmsy1_2050, fmsy1_2068)


fmsy1_change$Group<- as.character(fmsy1_change$Group)
fmsy1_change$year<- as.factor(fmsy1_change$year)
fmsy1_change$biomass<- as.numeric(fmsy1_change$biomass)

#write.csv(fmsy1_change, "chapter_4/biomass_change/fmsy1_tweak.csv", row.names = F)

read.csv("chapter_4/biomass_change/fmsy1_tweak.csv")-> fmsy1_change

fmsy06_change%>% select(Group, X2010:X2014, X2025:X2029, X2045:X2049, X2063:X2067) %>% gather("year", "biomass", 2:21)%>%
  separate(year,c("X","year"),remove=T,sep="X") %>% group_by(Group, year) %>% summarize(biomass = sum(biomass, na.rm=T))%>%
  ungroup()-> fmsy06_change

fmsy06_change %>% filter(year %in% c(2010:2014)) %>% group_by(Group) %>% summarize(biomass= mean(biomass, na.rm=T)) -> fmsy06_2015

fmsy06_2015$year<- "2015"

fmsy06_change %>% filter(year %in% c(2025:2029)) %>% group_by(Group) %>% summarize(biomass= mean(biomass, na.rm=T)) -> fmsy06_2030

fmsy06_2030$year<- "2030"

fmsy06_change %>% filter(year %in% c(2045:2049)) %>% group_by(Group) %>% summarize(biomass= mean(biomass, na.rm=T)) -> fmsy06_2050

fmsy06_2050$year<- "2050"

fmsy06_change %>% filter(year %in% c(2063:2067)) %>% group_by(Group) %>% summarize(biomass= mean(biomass, na.rm=T)) -> fmsy06_2068

fmsy06_2068$year<- "2068"

fmsy06_change<- rbind(fmsy06_2015, fmsy06_2030, fmsy06_2050, fmsy06_2068)




fmsy06_change$Group<- as.character(fmsy06_change$Group)
fmsy06_change$year<- as.factor(fmsy06_change$year)
fmsy06_change$biomass<- as.numeric(fmsy06_change$biomass)

#write.csv(fmsy06_change, "chapter_4/biomass_change/fmsy06_tweak.csv", row.names = F)

read.csv("chapter_4/biomass_change/fmsy06_tweak.csv")-> fmsy06_change

fmsy_change<- rbind(fmsy0_change, fmsy06_change, fmsy1_change)

fmsy_change %>% select(-biomass) ->fmsy_change
fmsy_change$pct_change<- as.numeric(fmsy_change$pct_change)
fmsy_change %>% filter(!year=="2015") ->fmsy_change
fmsy_change$year<- as.character(fmsy_change$year)
fmsy_change %>% filter(!Group=="Epibenthos") ->fmsy_change


ggplot(fmsy_change, aes(x=Group, y=pct_change))+
  geom_bar(aes(fill = year),position = "dodge", stat="identity")+
  theme_bw()+
  xlab("Group") +
  ylab("% Change of Biomass")+theme(axis.text=element_text(size=15))+theme(axis.title.x=element_text(size=15))+
  theme(axis.title.y=element_text(size=15))+ theme(legend.text=element_text(size=15))+
  theme(legend.title=element_text(size=15))+
  facet_wrap(~scenario, ncol=1)+ 
  theme(strip.text.x = element_text(size = 15))

fmsy06_noinvert$scenario<- "fmsy06"
fmsy1_noinvert$scenario<- "fmsy1"
fmsy2_noinvert$scenario<- "fmsy2"

fmsy_noinvert<- rbind(fmsy06_noinvert, fmsy1_noinvert, fmsy2_noinvert)

fmsy_noinvert %>% filter(Group!="Invertebrate") %>% group_by(year, scenario) %>%
  transmute(biomass=sum(biomass, na.rm=T))%>% distinct()->fmsy_noinvert

fmsy_noinvert %>% group_by(scenario) %>% mutate(pct_change=(biomass/lag(biomass) - 1) * 100)
