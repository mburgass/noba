fmsy1_data<- read.csv("chapter_4/biomass/fmsy1_biomass.csv")
fmsy11_data<- read.csv("chapter_4/biomass/fmsy11_biomass.csv")
fmsy08_data<- read.csv("chapter_4/biomass/fmsy08_biomass.csv")
fmsy06_data<- read.csv("chapter_4/biomass/fmsy06_biomass.csv")

fmsy1_data %>% gather("species", "biomass", 2:55) %>% mutate(scenario="fmsy1")->fmsy1_data
fmsy11_data %>% gather("species", "biomass", 2:55) %>% mutate(scenario="fmsy11")->fmsy11_data
fmsy08_data %>% gather("species", "biomass", 2:55) %>% mutate(scenario="fmsy08")->fmsy08_data
fmsy06_data %>% gather("species", "biomass", 2:55) %>% mutate(scenario="fmsy06")->fmsy06_data


biomass<- rbind(fmsy1_data, fmsy11_data, fmsy08_data, fmsy06_data)

ggplot(biomass, aes(Year, biomass)) +geom_line(aes(colour=scenario))+
  facet_wrap(~species, scales = "free")