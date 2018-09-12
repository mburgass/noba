library(devtools)
# Install from main ZSL repository online
install_github("mburgass/rlpi", dependencies=TRUE)
library(ncdf4)
library(readtext)
library(tidyverse)
library(rlpi)
closeAllConnections()

#Read base case
fmsy1_data<- read.csv("lpi_final/biomass_new/adjusted_species/fmsy1_biomass_adjusted2.csv")

#Read fisheries scenarios
fmsy2_data<- read.csv("lpi_final/biomass_new/adjusted_species/fmsy2_biomass_adjusted2.csv")
#fmsy05_data<- read.csv("biomass/lpi_files/fmsy05_lpi.csv")
fmsy0_data<- read.csv("lpi_final/biomass_new/adjusted_species/fmsy0_biomass_adjusted2.csv")

#BC
df<- fmsy1_data %>% dplyr::filter(ID<48)
index_vector = rep(FALSE, nrow(df)) #next row reliant on how many rows - need to change if not working
index_vector[1:153] = TRUE #change to number of rows as above

example_infile_name <- create_infile(df, index_vector=index_vector, name="example_data")
# An index can be created using this infile, for the period 1970 to 2014 with 100 bootstraps.
arctic_lpi <- LPIMain(example_infile_name, use_weightings = 0, REF_YEAR = 1981, PLOT_MAX = 2015, BOOT_STRAP_SIZE = 10000, VERBOSE=FALSE)
# Remove NAs (trailing years with no data)
fmsy1_lpi <- arctic_lpi[complete.cases(arctic_lpi), ]

df<- fmsy0_data %>% dplyr::filter(ID<48)
index_vector = rep(FALSE, nrow(df)) #next row reliant on how many rows - need to change if not working
index_vector[1:153] = TRUE #change to number of rows as above

example_infile_name <- create_infile(df, index_vector=index_vector, name="example_data")
# An index can be created using this infile, for the period 1970 to 2014 with 100 bootstraps.
arctic_lpi <- LPIMain(example_infile_name, use_weightings = 0, REF_YEAR = 1981, PLOT_MAX = 2015, BOOT_STRAP_SIZE = 10000, VERBOSE=FALSE)
# Remove NAs (trailing years with no data)
fmsy0_lpi <- arctic_lpi[complete.cases(arctic_lpi), ]

df<- fmsy2_data %>% dplyr::filter(ID<48)
index_vector = rep(FALSE, nrow(df)) #next row reliant on how many rows - need to change if not working
index_vector[1:153] = TRUE #change to number of rows as above

example_infile_name <- create_infile(df, index_vector=index_vector, name="example_data")
# An index can be created using this infile, for the period 1970 to 2014 with 100 bootstraps.
arctic_lpi <- LPIMain(example_infile_name, use_weightings = 0, REF_YEAR = 1981, PLOT_MAX = 2015, BOOT_STRAP_SIZE = 10000, VERBOSE=FALSE)
# Remove NAs (trailing years with no data)
fmsy2_lpi <- arctic_lpi[complete.cases(arctic_lpi), ]  


fmsy_lpis<- list(fmsy0_lpi,fmsy1_lpi,fmsy2_lpi)
ggplot_multi_lpi(fmsy_lpis, names=c("FMSY0","FMSY1", "FMSY2"), xlims=c(1981, 2015), ylims=c(0.4, 1.4), facet=T)

write.csv(fmsy0_lpi, "lpi_final/biomass_new/adjusted_species/lpiscores_fmsy0_no_invert.csv")
write.csv(fmsy1_lpi, "lpi_final/biomass_new/adjusted_species/lpiscores_fmsy1_no_invert.csv")
write.csv(fmsy2_lpi, "lpi_final/biomass_new/adjusted_species/lpiscores_fmsy2_no_invert.csv")

fmsy2_lpi$scenario<- "fmsy2"
fmsy1_lpi$scenario<- "fmsy1"
fmsy0_lpi$scenario<- "fmsy0"
fmsy_lpis2<- rbind(fmsy0_lpi,fmsy1_lpi,fmsy2_lpi)
fmsy_lpis2<- select(fmsy_lpis2, LPI_final, scenario)
write.csv(fmsy_lpis2, "lpi_final/biomass_new/adjusted_species/lpiscores_no_invert.csv")
###############ADJUSTED ACIDIFICATION##############
#Read base case
fmsy1_data<- read.csv("lpi_final/biomass_new/adjusted_species/fmsy1_biomass_adjusted.csv")
##Read OA
oa005_data<- read.csv("lpi_final/acidification/oa005_lpi.csv")
oa01_data<- read.csv("lpi_final/acidification/oa01_lpi.csv")

#BC
df<- fmsy1_data %>% dplyr::filter(ID<52)
index_vector = rep(FALSE, nrow(df)) #next row reliant on how many rows - need to change if not working
index_vector[1:51] = TRUE #change to number of rows as above

example_infile_name <- create_infile(df, index_vector=index_vector, name="example_data")
# An index can be created using this infile, for the period 1970 to 2014 with 100 bootstraps.
arctic_lpi <- LPIMain(example_infile_name, use_weightings = 0, REF_YEAR = 1981, PLOT_MAX = 2015, BOOT_STRAP_SIZE = 100, VERBOSE=FALSE)
# Remove NAs (trailing years with no data)
fmsy1_lpi <- arctic_lpi[complete.cases(arctic_lpi), ]

df<- oa005_data %>% dplyr::filter(ID<52)
index_vector = rep(FALSE, nrow(df)) #next row reliant on how many rows - need to change if not working
index_vector[1:51] = TRUE #change to number of rows as above

example_infile_name <- create_infile(df, index_vector=index_vector, name="example_data")
# An index can be created using this infile, for the period 1970 to 2014 with 100 bootstraps.
arctic_lpi <- LPIMain(example_infile_name, use_weightings = 0, REF_YEAR = 1981, PLOT_MAX = 2015, BOOT_STRAP_SIZE = 100, VERBOSE=FALSE)
# Remove NAs (trailing years with no data)
oa005_lpi <- arctic_lpi[complete.cases(arctic_lpi), ]

df<- oa01_data %>% dplyr::filter(ID<51)
index_vector = rep(FALSE, nrow(df)) #next row reliant on how many rows - need to change if not working
index_vector[1:51] = TRUE #change to number of rows as above

example_infile_name <- create_infile(df, index_vector=index_vector, name="example_data")
# An index can be created using this infile, for the period 1970 to 2014 with 100 bootstraps.
arctic_lpi <- LPIMain(example_infile_name, use_weightings = 0, REF_YEAR = 1981, PLOT_MAX = 2015, BOOT_STRAP_SIZE = 100, VERBOSE=FALSE)
# Remove NAs (trailing years with no data)
oa01_lpi <- arctic_lpi[complete.cases(arctic_lpi), ]  

oa_lpis<- list(fmsy1_lpi,oa005_lpi,oa01_lpi)
ggplot_multi_lpi(oa_lpis, names=c("BC","OA005", "OA01"), xlims=c(1981, 2015), ylims=c(0, 2))
