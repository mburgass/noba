library(ncdf4)
library(tidyverse)
library(devtools)
# Install from main ZSL repository online
#install_github("mburgass/rlpi", dependencies=TRUE)
library(rlpi)
closeAllConnections()

#Read base case
bc_data<- read.csv("biomass/lpi_files/bc_lpi.csv")

#Read fisheries scenarios
fmsy2_data<- read.csv("biomass/lpi_files/fmsy2_lpi.csv")
#fmsy05_data<- read.csv("biomass/lpi_files/fmsy05_lpi.csv")
fmsy0_data<- read.csv("biomass/lpi_files/fmsy0_lpi.csv")

#Read MPA
mpa10_data<- read.csv("biomass/lpi_files/mpa10_lpi.csv")
mpa25_data<- read.csv("biomass/lpi_files/mpa25_lpi.csv")
mpa50_data<- read.csv("biomass/lpi_files/mpa50_lpi.csv")

#Read Ocean acidification
oa01_data<- read.csv("biomass/lpi_files/oa01_lpi.csv")
oa005_data<- read.csv("biomass/lpi_files/oa005_lpi.csv")

#Read Climate change
cc2_data<- read.csv("biomass/lpi_files/cc2_lpi.csv")
cc3_data<- read.csv("biomass/lpi_files/cc3_lpi.csv")

#Read Seabird Mortality
sb3_data<- read.csv("biomass/lpi_files/sb3_lpi.csv")







#BC
  df<- bc_data %>% dplyr::filter(ID<31)
  rm(bc_data)
  index_vector = rep(FALSE, nrow(df)) #next row reliant on how many rows - need to change if not working
  index_vector[1:155] = TRUE #change to number of rows as above
  
  example_infile_name <- create_infile(df, index_vector=index_vector, name="example_data")
  # An index can be created using this infile, for the period 1970 to 2014 with 100 bootstraps.
  arctic_lpi <- LPIMain(example_infile_name, use_weightings = 0, REF_YEAR = 1970, PLOT_MAX = 2101, BOOT_STRAP_SIZE = 100, VERBOSE=FALSE)
  # Remove NAs (trailing years with no data)
  bc_lpi <- arctic_lpi[complete.cases(arctic_lpi), ]
  # Plot the resulting index
  #bc_plot<- ggplot_lpi(bc_lpi, title = 'base_case', xlims=c(1970, 2101), ylim=c(0, 2))
  #print(bc_plot)
  
#FMSY0

  df<- fmsy0_data %>% dplyr::filter(ID<31)
  rm(fmsy0_data)
  index_vector = rep(FALSE, nrow(df)) #next row reliant on how many rows - need to change if not working
  index_vector[1:155] = TRUE #change to number of rows as above
  
  example_infile_name <- create_infile(df, index_vector=index_vector, name="example_data")
  # An index can be created using this infile, for the period 1970 to 2014 with 100 bootstraps.
  arctic_lpi <- LPIMain(example_infile_name, use_weightings = 0, REF_YEAR = 1970, PLOT_MAX = 2101, BOOT_STRAP_SIZE = 100, VERBOSE=FALSE)
  # Remove NAs (trailing years with no data)
  fmsy0_lpi <- arctic_lpi[complete.cases(arctic_lpi), ]
  # Plot the resulting index
  #fmsy0_plot<- ggplot_lpi(fmsy0_lpi, title = 'fmsy0', xlims=c(1970, 2101), ylim=c(0, 2))
  #print(fmsy0_plot)
  
#FMSY05  
  df<- fmsy05_data %>% dplyr::filter(ID<31)
  rm(fmsy05_data)
  index_vector = rep(FALSE, nrow(df)) #next row reliant on how many rows - need to change if not working
  index_vector[1:155] = TRUE #change to number of rows as above
  
  example_infile_name <- create_infile(df, index_vector=index_vector, name="example_data")
  # An index can be created using this infile, for the period 1970 to 2014 with 100 bootstraps.
  arctic_lpi <- LPIMain(example_infile_name, use_weightings = 0, REF_YEAR = 1970, PLOT_MAX = 2101, BOOT_STRAP_SIZE = 100, VERBOSE=FALSE)
  # Remove NAs (trailing years with no data)
  fmsy05_lpi <- arctic_lpi[complete.cases(arctic_lpi), ]
  # Plot the resulting index
  #fmsy05_plot<- ggplot_lpi(fmsy05_lpi, title = 'fmsy05', xlims=c(1970, 2101), ylim=c(0, 2))
  #print(fmsy05_plot)
  
#FMSY1  
  df<- fmsy1_data %>% dplyr::filter(ID<31)
  rm(fmsy1_data)
  index_vector = rep(FALSE, nrow(df)) #next row reliant on how many rows - need to change if not working
  index_vector[1:155] = TRUE #change to number of rows as above
  
  example_infile_name <- create_infile(df, index_vector=index_vector, name="example_data")
  # An index can be created using this infile, for the period 1970 to 2014 with 100 bootstraps.
  arctic_lpi <- LPIMain(example_infile_name, use_weightings = 0, REF_YEAR = 1970, PLOT_MAX = 2101, BOOT_STRAP_SIZE = 100, VERBOSE=FALSE)
  # Remove NAs (trailing years with no data)
  fmsy1_lpi <- arctic_lpi[complete.cases(arctic_lpi), ]  
  
#FMSY2
  df<- fmsy2_data %>% dplyr::filter(ID<31)
  rm(fmsy2_data)
  index_vector = rep(FALSE, nrow(df)) #next row reliant on how many rows - need to change if not working
  index_vector[1:155] = TRUE #change to number of rows as above
  
  example_infile_name <- create_infile(df, index_vector=index_vector, name="example_data")
  # An index can be created using this infile, for the period 1970 to 2014 with 100 bootstraps.
  arctic_lpi <- LPIMain(example_infile_name, use_weightings = 0, REF_YEAR = 1970, PLOT_MAX = 2101, BOOT_STRAP_SIZE = 100, VERBOSE=FALSE)
  # Remove NAs (trailing years with no data)
  fmsy2_lpi <- arctic_lpi[complete.cases(arctic_lpi), ]  

fmsy_lpis<- list(bc_lpi, fmsy0_lpi, fmsy05_lpi, fmsy1_lpi, fmsy2_lpi)
ggplot_multi_lpi(fmsy_lpis, names=c("Base Case","FMSY0", "FMSY05", "FMSY1", "FMSY2"), xlims=c(1970, 2101), ylims=c(0, 2), facet=TRUE)

#MPA10
df<- mpa10_data %>% dplyr::filter(ID<31)
rm(mpa10_data)
index_vector = rep(FALSE, nrow(df)) #next row reliant on how many rows - need to change if not working
index_vector[1:155] = TRUE #change to number of rows as above

example_infile_name <- create_infile(df, index_vector=index_vector, name="example_data")
# An index can be created using this infile, for the period 1970 to 2014 with 100 bootstraps.
arctic_lpi <- LPIMain(example_infile_name, use_weightings = 0, REF_YEAR = 1970, PLOT_MAX = 2101, BOOT_STRAP_SIZE = 100, VERBOSE=FALSE)
# Remove NAs (trailing years with no data)
mpa10_lpi <- arctic_lpi[complete.cases(arctic_lpi), ]  

#MPA25
df<- mpa25_data %>% dplyr::filter(ID<31)
rm(mpa25_data)
index_vector = rep(FALSE, nrow(df)) #next row reliant on how many rows - need to change if not working
index_vector[1:155] = TRUE #change to number of rows as above

example_infile_name <- create_infile(df, index_vector=index_vector, name="example_data")
# An index can be created using this infile, for the period 1970 to 2014 with 100 bootstraps.
arctic_lpi <- LPIMain(example_infile_name, use_weightings = 0, REF_YEAR = 1970, PLOT_MAX = 2101, BOOT_STRAP_SIZE = 100, VERBOSE=FALSE)
# Remove NAs (trailing years with no data)
mpa25_lpi <- arctic_lpi[complete.cases(arctic_lpi), ]  

#MPA50
df<- mpa50_data %>% dplyr::filter(ID<31)
rm(mpa50_data)
index_vector = rep(FALSE, nrow(df)) #next row reliant on how many rows - need to change if not working
index_vector[1:155] = TRUE #change to number of rows as above

example_infile_name <- create_infile(df, index_vector=index_vector, name="example_data")
# An index can be created using this infile, for the period 1970 to 2014 with 100 bootstraps.
arctic_lpi <- LPIMain(example_infile_name, use_weightings = 0, REF_YEAR = 1970, PLOT_MAX = 2101, BOOT_STRAP_SIZE = 100, VERBOSE=FALSE)
# Remove NAs (trailing years with no data)
mpa50_lpi <- arctic_lpi[complete.cases(arctic_lpi), ]  

mpa_lpis<- list(bc_lpi, mpa10_lpi, mpa25_lpi, mpa50_lpi)
ggplot_multi_lpi(mpa_lpis, names=c("Base Case","MPA10", "MPA25", "MPA50"), xlims=c(1970, 2101), ylims=c(0, 2), facet=TRUE)

#cc2
df<- cc2_data %>% dplyr::filter(ID<31)
rm(cc2_data)
index_vector = rep(FALSE, nrow(df)) #next row reliant on how many rows - need to change if not working
index_vector[1:155] = TRUE #change to number of rows as above

example_infile_name <- create_infile(df, index_vector=index_vector, name="example_data")
# An index can be created using this infile, for the period 1970 to 2014 with 100 bootstraps.
arctic_lpi <- LPIMain(example_infile_name, use_weightings = 0, REF_YEAR = 1970, PLOT_MAX = 2101, BOOT_STRAP_SIZE = 100, VERBOSE=FALSE)
# Remove NAs (trailing years with no data)
cc2_lpi <- arctic_lpi[complete.cases(arctic_lpi), ]  

#CC3
df<- cc3_data %>% dplyr::filter(ID<31)
rm(cc3_data)
index_vector = rep(FALSE, nrow(df)) #next row reliant on how many rows - need to change if not working
index_vector[1:155] = TRUE #change to number of rows as above

example_infile_name <- create_infile(df, index_vector=index_vector, name="example_data")
# An index can be created using this infile, for the period 1970 to 2014 with 100 bootstraps.
arctic_lpi <- LPIMain(example_infile_name, use_weightings = 0, REF_YEAR = 1970, PLOT_MAX = 2101, BOOT_STRAP_SIZE = 100, VERBOSE=FALSE)
# Remove NAs (trailing years with no data)
cc3_lpi <- arctic_lpi[complete.cases(arctic_lpi), ]  

#oa005
df<- oa005_data %>% dplyr::filter(ID<31)
rm(oa005_data)
index_vector = rep(FALSE, nrow(df)) #next row reliant on how many rows - need to change if not working
index_vector[1:155] = TRUE #change to number of rows as above

example_infile_name <- create_infile(df, index_vector=index_vector, name="example_data")
# An index can be created using this infile, for the period 1970 to 2014 with 100 bootstraps.
arctic_lpi <- LPIMain(example_infile_name, use_weightings = 0, REF_YEAR = 1970, PLOT_MAX = 2101, BOOT_STRAP_SIZE = 100, VERBOSE=FALSE)
# Remove NAs (trailing years with no data)
oa005_lpi <- arctic_lpi[complete.cases(arctic_lpi), ] 

#oa01
df<- oa01_data %>% dplyr::filter(ID<31)
rm(oa01_data)
index_vector = rep(FALSE, nrow(df)) #next row reliant on how many rows - need to change if not working
index_vector[1:155] = TRUE #change to number of rows as above

example_infile_name <- create_infile(df, index_vector=index_vector, name="example_data")
# An index can be created using this infile, for the period 1970 to 2014 with 100 bootstraps.
arctic_lpi <- LPIMain(example_infile_name, use_weightings = 0, REF_YEAR = 1970, PLOT_MAX = 2101, BOOT_STRAP_SIZE = 100, VERBOSE=FALSE)
# Remove NAs (trailing years with no data)
oa01_lpi <- arctic_lpi[complete.cases(arctic_lpi), ] 

cc_lpis<- list(bc_lpi, cc2_lpi, cc3_lpi, oa005_lpi, oa01_lpi)
ggplot_multi_lpi(cc_lpis, names=c("Base Case","CC2", "CC3", "OA005", "OA01"), xlims=c(1970, 2101), ylims=c(0, 2), facet=TRUE)
