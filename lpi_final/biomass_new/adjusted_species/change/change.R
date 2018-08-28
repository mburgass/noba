library(tidyverse)
fmsy0_change<- read.csv("lpi_final/biomass_new/adjusted_species/change/fmsy0_change.csv")
fmsy1_change<- read.csv("lpi_final/biomass_new/adjusted_species/change/fmsy1_change.csv")
fmsy2_change<- read.csv("lpi_final/biomass_new/adjusted_species/change/fmsy2_change.csv")

fmsy0_change%>% select(Group, X1981, X2015) %>% gather("year", "biomass", 2:3)%>%
  separate(year,c("X","year"),remove=T,sep="X")%>%
  group_by(Group, year) %>% summarize(biomass = sum(biomass, na.rm=T))%>%
  ungroup()-> fmsy0_change
fmsy0_change$Group<- as.character(fmsy0_change$Group)
fmsy0_change$year<- as.factor(fmsy0_change$year)
fmsy0_change$biomass<- as.numeric(fmsy0_change$biomass)

fmsy0_noinvert<-fmsy0_change
fmsy0_change %>% group_by(Group) %>% mutate(pct_change = (biomass/lag(biomass) - 1) * 100) %>% filter(year==2015)->fmsy0_change
fmsy0_change$scenario<- "fmsy0"

fmsy1_change%>% select(Group, X1981, X2015) %>% gather("year", "biomass", 2:3)%>%
  separate(year,c("X","year"),remove=T,sep="X")%>%
  group_by(Group, year) %>% summarize(biomass = sum(biomass, na.rm=T))%>%
  ungroup()-> fmsy1_change
fmsy1_change$Group<- as.character(fmsy1_change$Group)
fmsy1_change$year<- as.factor(fmsy1_change$year)
fmsy1_change$biomass<- as.numeric(fmsy1_change$biomass)
fmsy1_noinvert<-fmsy1_change
fmsy1_change %>% group_by(Group) %>% mutate(pct_change = (biomass/lag(biomass) - 1) * 100) %>% filter(year==2015)->fmsy1_change
fmsy1_change$scenario<- "fmsy1"



fmsy2_change%>% select(Group, X1981, X2015) %>% gather("year", "biomass", 2:3)%>%
  separate(year,c("X","year"),remove=T,sep="X")%>%
  group_by(Group, year) %>% summarize(biomass = sum(biomass, na.rm=T))%>%
  ungroup()-> fmsy2_change
fmsy2_change$Group<- as.character(fmsy2_change$Group)
fmsy2_change$year<- as.factor(fmsy2_change$year)
fmsy2_change$biomass<- as.numeric(fmsy2_change$biomass)
fmsy2_noinvert<-fmsy2_change
fmsy2_change %>% group_by(Group) %>% mutate(pct_change = (biomass/lag(biomass) - 1) * 100) %>% filter(year==2015)->fmsy2_change
fmsy2_change$scenario<- "fmsy2"

fmsy_change<- rbind(fmsy0_change, fmsy1_change, fmsy2_change)

fmsy_change %>% select(-year, -biomass) ->fmsy_change
fmsy_change$pct_change<- as.numeric(fmsy_change$pct_change)

barplot<- ggplot(fmsy_change, aes(x=Group, y=pct_change))+
  geom_bar(aes(fill = scenario),position = "dodge", stat="identity")+
  theme_bw()+
  xlab("Group") +
  ylab("% Change of Biomass")+theme(axis.text=element_text(size=20))+theme(axis.title.x=element_text(size=20))+
  theme(axis.title.y=element_text(size=15))+ theme(legend.text=element_text(size=15))+
  theme(legend.title=element_text(size=15))

fmsy0_noinvert$scenario<- "fmsy0"
fmsy1_noinvert$scenario<- "fmsy1"
fmsy2_noinvert$scenario<- "fmsy2"

fmsy_noinvert<- rbind(fmsy0_noinvert, fmsy1_noinvert, fmsy2_noinvert)

fmsy_noinvert %>% filter(Group!="Invertebrate") %>% group_by(year, scenario) %>%
  transmute(biomass=sum(biomass, na.rm=T))%>% distinct()->fmsy_noinvert

fmsy_noinvert %>% group_by(scenario) %>% mutate(pct_change=(biomass/lag(biomass) - 1) * 100)
