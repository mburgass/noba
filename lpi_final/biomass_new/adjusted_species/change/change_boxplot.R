library(tidyverse)
library(cowplot)
fmsy0_change<- read.csv("lpi_final/biomass_new/adjusted_species/change/fmsy0_change.csv")
fmsy1_change<- read.csv("lpi_final/biomass_new/adjusted_species/change/fmsy1_change.csv")
fmsy2_change<- read.csv("lpi_final/biomass_new/adjusted_species/change/fmsy2_change.csv")

fmsy0_change%>% select(Binomial, Group, X1981, X2015) %>% gather("year", "biomass", 3:4)%>%
  separate(year,c("X","year"),remove=T,sep="X")->fmsy0_change
fmsy0_change$scenario<- "fmsy0"
fmsy0_change %>% group_by(Binomial) %>% mutate(pct_change = (biomass/lag(biomass) - 1) * 100) %>%
  filter(year==2015)->fmsy0_change

fmsy1_change%>% select(Binomial,Group, X1981, X2015) %>% gather("year", "biomass", 3:4)%>%
  separate(year,c("X","year"),remove=T,sep="X")->fmsy1_change
fmsy1_change$scenario<- "fmsy1"
fmsy1_change %>% group_by(Binomial) %>% mutate(pct_change = (biomass/lag(biomass) - 1) * 100) %>%
  filter(year==2015)->fmsy1_change

fmsy2_change%>% select(Binomial,Group, X1981, X2015) %>% gather("year", "biomass", 3:4)%>%
  separate(year,c("X","year"),remove=T,sep="X")->fmsy2_change
fmsy2_change$scenario<- "fmsy2"
fmsy2_change %>% group_by(Binomial) %>% mutate(pct_change = (biomass/lag(biomass) - 1) * 100) %>%
  filter(year==2015)->fmsy2_change

fmsy_box<- rbind(fmsy0_change, fmsy1_change, fmsy2_change)
fmsy_box<- select(fmsy_box, -X, -year, -biomass)

boxplot<- ggplot(fmsy_box, aes(x=Group, y=pct_change))+
  geom_boxplot(aes(fill = scenario),position = "dodge")+
  theme_bw()+
  ylab("% Change Biomass for \n Functional Groups")+theme(axis.text=element_text(size=20))+
  theme(axis.title.x=element_text(size=20))+
  theme(axis.title.y=element_text(size=15))+ theme(legend.text=element_text(size=15))+
  theme(legend.title=element_text(size=15))

plot_grid(barplot, boxplot, labels = c("A", "B"), align = "h", ncol=1)
