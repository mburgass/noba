library(ncdf4)
library(readtext)
library(tidyverse)
library(rlpi)

#Read base case
bc_data<- read.csv("biomass/new_runs/lpi_files/bc_lpi.csv")

#Read fisheries scenarios
fmsy2_data<- read.csv("biomass/new_runs/lpi_files/fmsy2_lpi.csv")
fmsy1_data<- read.csv("biomass/new_runs/lpi_files/fmsy1_lpi.csv")
fmsy05_data<- read.csv("biomass/new_runs/lpi_files/fmsy05_lpi.csv")
fmsy0_data<- read.csv("biomass/new_runs/lpi_files/fmsy0_lpi.csv")

#Read MPA
mpa10_data<- read.csv("biomass/new_runs/lpi_files/mpa10_lpi.csv")
mpa25_data<- read.csv("biomass/new_runs/lpi_files/mpa25_lpi.csv")
mpa50_data<- read.csv("biomass/new_runs/lpi_files/mpa50_lpi.csv")

#Read Ocean acidification
oa01_data<- read.csv("biomass/new_runs/lpi_files/oa01_lpi.csv")
oa005_data<- read.csv("biomass/new_runs/lpi_files/oa005_lpi.csv")

#Read Climate change
cc2_data<- read.csv("biomass/new_runs/lpi_files/cc2_lpi.csv")
cc3_data<- read.csv("biomass/new_runs/lpi_files/cc3_lpi.csv")

#Read Seabird Mortality
sb3_data<- read.csv("biomass/new_runs/lpi_files/sb3_lpi.csv")





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