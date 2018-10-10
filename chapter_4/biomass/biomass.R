local_data<- read.csv("chapter_4/biomass/local_stewardship_total.csv")
national_data<- read.csv("chapter_4/biomass/national_enterprise_total.csv")
world_data<- read.csv("chapter_4/biomass/world_market_total.csv")
global_data<- read.csv("chapter_4/biomass/global_sustainability_total.csv")

local_data %>% gather("species", "biomass", 2:55) %>% mutate(scenario="local")->local_data
national_data %>% gather("species", "biomass", 2:55) %>% mutate(scenario="national")->national_data
world_data %>% gather("species", "biomass", 2:55) %>% mutate(scenario="world")->world_data
global_data %>% gather("species", "biomass", 2:55) %>% mutate(scenario="global")->global_data


biomass<- rbind(local_data, national_data, world_data, global_data)

ggplot(biomass, aes(Year, biomass)) +geom_line(aes(colour=scenario))+
  facet_wrap(~species, scales = "free")