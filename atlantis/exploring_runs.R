library(ncdf4)
library(readtext)
library(tidyverse)
library(rlpi)
test<- read.table("unzip/nordic_runresults_01BoxBiomass.txt")
colnames(test) <- as.character(unlist(test[1,]))
test = test[-1, ]   
##sum all boxes
test2<- select(test, -Box)
test3<- gather(test2, "species", "biomass", 2:54)
test3$biomass<- as.numeric(test3$biomass)
test4<- test3 %>% group_by(Time, species)%>%
  dplyr::summarise(biomass = sum(biomass, na.rm=T)) #this gives a summary of each species over each time period
test5<- filter(test4, species=='SAI')
ggplot(test5, aes(x = Time, y = biomass)) +
  geom_point()

#### Biomass across all boxes #####

biomass<- read.table("unzip/nordic_runresults_01BiomIndx.txt")
colnames(biomass) <- as.character(unlist(biomass[1,]))
biomass = biomass[-1, ]  
biomass_time<- slice(biomass, 122:221) #first time step should be 121 but row numbers start at 2 so this is why it is 122. 
write.csv(biomass_time, 'biomass/biomass_time.csv', row.names = FALSE)
biomass_time<- read.csv("biomass/biomass_time.csv")
period<- read.csv("biomass/biomass_period.csv") # check this works OK - might have to change the type of numbers in cells
biomass_time2<- period %>% left_join(biomass_time, by='Time')%>%
  select(year, period, 2:37)

#simulation_step=seq(1,100,by=5)
#biomass_year<- read.csv('biomass/biomass_year.csv')
#biomass_year<- select(biomass_year, 1:38) 

biomass_year<- gather(biomass_time2, "species", "biomass", 3:36)
#biomass_step<- filter(biomass_year, Period %in% c(1,3))


biomass_step<- biomass_year %>% group_by(year, species)
vert_biomass<- biomass_step %>% summarise(biomass=mean(biomass)) %>%
  ungroup()
  #filter(!species %in% c('CEP', 'PWN', 'KCR', 'SCR')) #take out inverts for LPI
ggplot(vert_biomass, aes(x=year, y=biomass, color=species, group=species)) +
  geom_line()
biomass_final<- spread(vert_biomass, year, biomass) %>%
  rename(Binomial=species)
write.csv(biomass_final, 'biomass/biomass_final.csv', row.names=FALSE)

blank<- read.csv("biomass/biomass_1950-2015.csv", na.strings= "NULL", check.names = FALSE)
biomass_lpi<- read.csv("biomass/biomass_final.csv", check.names=FALSE)

biomass_lpi<- biomass_lpi %>% left_join(blank, by='Binomial')
biomass_lpi<- biomass_lpi[ , order(names(biomass_lpi))]
biomass_lpi <- biomass_lpi[c(68, 67, 1:66)]
lpi_data<- biomass_lpi %>% filter(!Binomial %in% c('PWN', 'CEP','KCR', 'SCR')) #filter out inverts
write.csv(lpi_data, "biomass/biomass_lpi.csv", row.names = FALSE)
lpi_data <- read.csv("biomass/biomass_lpi.csv", na.strings = "NULL")

# Create an infile from all the data. All the population data in the 'lpi_data' table will be converted and stored in a file called 'example_data_pops.txt' 
#and a file called 'example_data_infile.txt' will be created that references the first file 
#(the infile name will also be stored in the returned variable 'example_infile_name')

# Here we select the first 100 populations by creating an index vector that's FALSE for all rows, then setting the first 100 rows to TRUE
index_vector = rep(FALSE, nrow(lpi_data)) #next row reliant on how many rows - need to change if not working
index_vector[1:32] = TRUE #change to number of rows as above

example_infile_name <- create_infile(lpi_data, index_vector=index_vector, name="example_data")
# An index can be created using this infile, for the period 1970 to 2014 with 100 bootstraps.
arctic_lpi <- LPIMain(example_infile_name, REF_YEAR = 1970, PLOT_MAX = 2001, BOOT_STRAP_SIZE = 1000, VERBOSE=FALSE)
# Remove NAs (trailing years with no data)
arctic_lpi <- arctic_lpi[complete.cases(arctic_lpi), ]
# Plot the resulting index
ggplot_lpi(arctic_lpi, title = "arctic_lpi", xlims=c(1970, 2001), ylim=c(0, 2))
