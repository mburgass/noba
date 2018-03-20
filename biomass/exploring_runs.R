library(ncdf4)
library(readtext)
library(tidyverse)
library(rlpi)
test<- read.table("unzip/nordic_runresults_01BoxBiomass.txt")
colnames(test) <- as.character(unlist(test[1,]))
test = test[-1, ]   
##########sum all boxes###########
test2<- select(test, -Box)
test3<- gather(test2, "species", "biomass", 2:54)
test3$biomass<- as.numeric(test3$biomass)
test4<- test3 %>% group_by(Time, species)%>%
  dplyr::summarise(biomass = sum(biomass, na.rm=T)) #this gives a summary of each species over each time period
test5<- filter(test4, species=='SAI')
ggplot(test5, aes(x = Time, y = biomass)) +
  geom_point()
numextract <- function(string){ 
  str_extract(string, "\\-*\\d+\\.*\\d*")
} 
time_test<- numextract(test5$Time)
options(scipen=999)
time2<- as.POSIXct(as.numeric(as.character(time_test)), origin='1980-01-02',tz="GMT", format= '%d')

#### Biomass across all boxes #####

biomass<- read.table("unzip/nordic_runresults_01BiomIndx.txt")
colnames(biomass) <- as.character(unlist(biomass[1,]))
biomass = biomass[-1, ]  
#biomass_time<- slice(biomass, 122:221) #first time step should be 121 but row numbers start at 2 so this is why it is 122. 
write.csv(biomass, 'biomass/biomass_time.csv', row.names = FALSE)

###ADDED IN PROPER TIME PERIODS HERE#######
biomass_time<- read.csv("biomass/biomass_time.csv")
#period<- read.csv("biomass/biomass_period.csv") # check this works OK - might have to change the type of numbers in cells
#biomass_time2<- period %>% left_join(biomass_time, by='Time')%>%
  #select(-Time) 

#simulation_step=seq(1,100,by=5)
#biomass_year<- read.csv('biomass/biomass_year.csv')
biomass_year<- select(biomass_time, 1:38) 

biomass_year<- gather(biomass_year, "species", "biomass", 3:38)
#biomass_step<- filter(biomass_year, Period %in% c(1,3))


biomass_step<- biomass_year %>% group_by(Year, species)
vert_biomass<- biomass_step %>% summarise(biomass=mean(biomass)) %>%
  ungroup() #%>% 
  #filter(!species %in% c('CEP', 'PWN', 'MES')) #take out big groups for better graphs
ggplot(vert_biomass, aes(x=Year, y=biomass, color=species, group=species)) +
  geom_line()
biomass_final<- spread(vert_biomass, Year, biomass) %>%
  rename(Binomial=species)
biomass_final<- select(biomass_final, Binomial, num_range("19", 80:99), "2000")
write.csv(biomass_final, 'biomass/biomass_final.csv', row.names=FALSE)

blank<- read.csv("biomass/biomass_1950-2015.csv", na.strings= "NULL", check.names = FALSE)
biomass_lpi<- read.csv("biomass/biomass_final.csv", check.names=FALSE)

biomass_lpi<- biomass_lpi %>% left_join(blank, by='Binomial')
biomass_lpi<- biomass_lpi[ , order(names(biomass_lpi))]
biomass_lpi <- biomass_lpi[c(68, 69, 1:67)]
lpi_data<- biomass_lpi %>% filter(!Binomial %in% c('PWN', 'CEP','KCR', 'SCR')) #filter out inverts
write.csv(lpi_data, "biomass/biomass_lpi.csv", row.names = FALSE)
lpi_data <- read.csv("biomass/biomass_lpi.csv", na.strings = "NULL")

lpi_data<- read.csv("biomass/new_runs/lpi_files/2msy_lpi.csv")

#lpi_data<- read.csv("biomass/new_runs/lpi_files/oa_lpi_nocap.csv")

lpi_data<- lpi_data %>% filter(ID<31)

# Create an infile from all the data. All the population data in the 'lpi_data' table will be converted and stored in a file called 'example_data_pops.txt' 
#and a file called 'example_data_infile.txt' will be created that references the first file 
#(the infile name will also be stored in the returned variable 'example_infile_name')

# Here we select the first 100 populations by creating an index vector that's FALSE for all rows, then setting the first 100 rows to TRUE
index_vector = rep(FALSE, nrow(lpi_data)) #next row reliant on how many rows - need to change if not working
index_vector[1:155] = TRUE #change to number of rows as above

example_infile_name <- create_infile(lpi_data, index_vector=index_vector, name="example_data")
# An index can be created using this infile, for the period 1970 to 2014 with 100 bootstraps.
arctic_lpi <- LPIMain(example_infile_name, use_weightings = 0, REF_YEAR = 1970, PLOT_MAX = 2101, BOOT_STRAP_SIZE = 100, VERBOSE=FALSE)
# Remove NAs (trailing years with no data)
arctic_lpi <- arctic_lpi[complete.cases(arctic_lpi), ]
# Plot the resulting index
ggplot_lpi(arctic_lpi, title = "mpa50_nocap_lpi", xlims=c(1970, 2101), ylim=c(0, 2))


####TRY AND DO SEASONAL RUNS RATHER THAN ANNUAL AVERAGE##########


biomass_step_winter<- biomass_year%>% filter(Month %in% c("January"))%>%
  select(-Month)
ggplot(biomass_step_winter, aes(x=Year, y=biomass, color=species, group=species)) +
  geom_line()
biomass_winter<- spread(biomass_step_winter, Year, biomass) %>%
  rename(Binomial=species)
biomass_winter<- select(biomass_winter, Binomial, num_range("19", 80:99), "2000", "2001", "2002")
write.csv(biomass_winter, 'biomass/biomass_winter.csv', row.names=FALSE)

blank<- read.csv("biomass/biomass_1950-2015.csv", na.strings= "NULL", check.names = FALSE)
winter_lpi<- read.csv("biomass/biomass_winter.csv", check.names=FALSE)

winter_lpi<- winter_lpi %>% left_join(blank, by='Binomial')
winter_lpi<- winter_lpi[ , order(names(winter_lpi))]
winter_lpi <- winter_lpi[c(70, 71, 1:69)] ###THIS BIT NEEDS TO BE DOUBLE CHECKED TO MAKE SURE COLUMNS FIT
lpiwinter_data<- winter_lpi %>% filter(!Binomial %in% c('PWN', 'CEP','KCR', 'SCR')) #filter out inverts
write.csv(lpiwinter_data, "biomass/winter_lpi.csv", row.names = FALSE)
lpi_winter <- read.csv("biomass/winter_lpi.csv", na.strings = "NULL")

# Create an infile from all the data. All the population data in the 'lpi_data' table will be converted and stored in a file called 'example_data_pops.txt' 
#and a file called 'example_data_infile.txt' will be created that references the first file 
#(the infile name will also be stored in the returned variable 'example_infile_name')

# Here we select the first 100 populations by creating an index vector that's FALSE for all rows, then setting the first 100 rows to TRUE
index_vector = rep(FALSE, nrow(lpi_winter)) #next row reliant on how many rows - need to change if not working
index_vector[1:32] = TRUE #change to number of rows as above

example_infile_name_winter <- create_infile(lpi_winter, index_vector=index_vector, name="example_data")
# An index can be created using this infile, for the period 1970 to 2014 with 100 bootstraps.
arctic_lpiwinter <- LPIMain(example_infile_name_winter, REF_YEAR = 1970, PLOT_MAX = 2001, BOOT_STRAP_SIZE = 1000, VERBOSE=FALSE)
# Remove NAs (trailing years with no data)
arctic_lpiwinter <- arctic_lpiwinter[complete.cases(arctic_lpiwinter), ]
# Plot the resulting index
ggplot_lpi(arctic_lpiwinter, title = "arctic_lpiwinter", xlims=c(1970, 2001), ylim=c(0, 2))


####SUMMER RUN##########

biomass_step_summer<- biomass_year%>% filter(Month %in% c("August"))%>%
  select(-Month)
ggplot(biomass_step_summer, aes(x=Year, y=biomass, color=species, group=species)) +
  geom_line()
biomass_summer<- spread(biomass_step_summer, Year, biomass) %>%
  rename(Binomial=species)
biomass_summer<- select(biomass_summer, Binomial, num_range("19", 80:99), "2000", "2001")
write.csv(biomass_summer, 'biomass/biomass_summer.csv', row.names=FALSE)

blank<- read.csv("biomass/biomass_1950-2015.csv", na.strings= "NULL", check.names = FALSE)
summer_lpi<- read.csv("biomass/biomass_summer.csv", check.names=FALSE)

summer_lpi<- summer_lpi %>% left_join(blank, by='Binomial')
summer_lpi<- summer_lpi[ , order(names(summer_lpi))]
summer_lpi <- summer_lpi[c(69, 70, 1:68)] ###THIS BIT NEEDS TO BE DOUBLE CHECKED TO MAKE SURE COLUMNS FIT
lpisummer_data<- winter_lpi %>% filter(!Binomial %in% c('PWN', 'CEP','KCR', 'SCR')) #filter out inverts
write.csv(lpisummer_data, "biomass/summer_lpi.csv", row.names = FALSE)
lpi_summer <- read.csv("biomass/summer_lpi.csv", na.strings = "NULL")

# Create an infile from all the data. All the population data in the 'lpi_data' table will be converted and stored in a file called 'example_data_pops.txt' 
#and a file called 'example_data_infile.txt' will be created that references the first file 
#(the infile name will also be stored in the returned variable 'example_infile_name')

# Here we select the first 100 populations by creating an index vector that's FALSE for all rows, then setting the first 100 rows to TRUE
index_vector = rep(FALSE, nrow(lpi_summer)) #next row reliant on how many rows - need to change if not working
index_vector[1:32] = TRUE #change to number of rows as above

example_infile_name <- create_infile(lpi_summer, index_vector=index_vector, name="example_data")
# An index can be created using this infile, for the period 1970 to 2014 with 100 bootstraps.
arctic_lpisummer <- LPIMain(example_infile_name, REF_YEAR = 1970, PLOT_MAX = 2001, BOOT_STRAP_SIZE = 100, VERBOSE=FALSE)
# Remove NAs (trailing years with no data)
arctic_lpisummer <- arctic_lpisummer[complete.cases(arctic_lpisummer), ]
# Plot the resulting index
ggplot_lpi(arctic_lpisummer, title = "arctic_lpisummer", xlims=c(1970, 2001), ylim=c(0.8, 1.2))


####PLAY AROUND WITH SPAWNING STOCK BIOMASSS############

ssb<- read.table("unzip/nordic_runresults_01SSB.txt")
colnames(ssb) <- as.character(unlist(ssb[1,]))
ssb = ssb[-1, ]  
#biomass_time<- slice(biomass, 122:221) #first time step should be 121 but row numbers start at 2 so this is why it is 122. 
write.csv(ssb, 'biomass/ssb/ssb_time.csv', row.names = FALSE)

ssb_time<- read.csv("biomass/ssb/ssb_time.csv")
#period<- read.csv("biomass/biomass_period.csv") # check this works OK - might have to change the type of numbers in cells
#biomass_time2<- period %>% left_join(biomass_time, by='Time')%>%
#select(-Time) 

#simulation_step=seq(1,100,by=5)
#biomass_year<- read.csv('biomass/biomass_year.csv')
#ssb_year<- select(ssb_time, 1:38) 

ssb_year<- gather(ssb_time, "species", "biomass", 3:35)
#biomass_step<- filter(biomass_year, Period %in% c(1,3))

ssb_biomass<- ssb_year %>% group_by(Year, species) %>% summarise(biomass=mean(biomass)) %>%
  ungroup() #%>% 
#filter(!species %in% c('CEP', 'PWN', 'MES')) #take out big groups for better graphs
ggplot(ssb_biomass, aes(x=Year, y=biomass, color=species, group=species)) +
  geom_line()
ssb_final<- spread(ssb_biomass, Year, biomass) %>%
  rename(Binomial=species)
ssb_final<- select(ssb_final, Binomial, num_range("19", 80:99), "2000")
write.csv(ssb_final, 'biomass/ssb/ssb_final.csv', row.names=FALSE)

blank<- read.csv("biomass/biomass_1950-2015.csv", na.strings= "NULL", check.names = FALSE)
ssb_lpi<- read.csv("biomass/ssb/ssb_final.csv", check.names=FALSE)

ssb_lpi<- ssb_lpi %>% left_join(blank, by='Binomial')
ssb_lpi<- ssb_lpi[ , order(names(ssb_lpi))]
ssb_lpi <- ssb_lpi[c(68, 69, 1:67)]
ssb_data<- ssb_lpi %>% filter(!Binomial %in% c('PWN', 'CEP','KCR', 'SCR')) #filter out inverts
write.csv(ssb_data, "biomass/ssb/ssb_lpi.csv", row.names = FALSE)
ssb_data <- read.csv("biomass/ssb/ssb_lpi2.csv", na.strings = "NULL")

# Create an infile from all the data. All the population data in the 'lpi_data' table will be converted and stored in a file called 'example_data_pops.txt' 
#and a file called 'example_data_infile.txt' will be created that references the first file 
#(the infile name will also be stored in the returned variable 'example_infile_name')

# Here we select the first 100 populations by creating an index vector that's FALSE for all rows, then setting the first 100 rows to TRUE
index_vector = rep(FALSE, nrow(ssb_data)) #next row reliant on how many rows - need to change if not working
index_vector[1:40] = TRUE #change to number of rows as above

example_infile_name2 <- create_infile(ssb_data, index_vector=index_vector, name="example_data")
# An index can be created using this infile, for the period 1970 to 2014 with 100 bootstraps.
ssb_lpi <- LPIMain(example_infile_name, REF_YEAR = 1970, PLOT_MAX = 2001, BOOT_STRAP_SIZE = 1000, VERBOSE=FALSE)
# Remove NAs (trailing years with no data)
arctic_lpissb2 <- ssb_lpi[complete.cases(ssb_lpi), ]
# Plot the resulting index
ggplot_lpi(arctic_lpissb2, title = "ssb_lpi", xlims=c(1970, 2001), ylim=c(0.8, 1.2))
