library(devtools)
# Install from main ZSL repository online
install_github("mburgass/rlpi", ref='chapter4', dependencies=TRUE)
library(ncdf4)
library(readtext)
library(tidyverse)
library(rlpi)
closeAllConnections()


#Read base case
local_data<- read.csv("chapter_4/lpi/local_stewardship_lpi.csv")
national_data<- read.csv("chapter_4/lpi/national_enterprise_lpi.csv")
world_data<- read.csv("chapter_4/lpi/world_market_lpi.csv")
global_data<- read.csv("chapter_4/lpi/global_sustainability_lpi.csv")

df<- local_data %>% dplyr::filter(ID<50)
index_vector = rep(FALSE, nrow(df)) #next row reliant on how many rows - need to change if not working
index_vector[1:121] = TRUE #change to number of rows as above

example_infile_name <- create_infile(df, index_vector=index_vector, name="example_data")
# An index can be created using this infile, for the period 1970 to 2014 with 100 bootstraps.
arctic_lpi <- LPIMain(example_infile_name, use_weightings = 0, REF_YEAR = 2017, PLOT_MAX = 2068, BOOT_STRAP_SIZE = 100, VERBOSE=FALSE)
# Remove NAs (trailing years with no data)
local_lpi <- arctic_lpi[complete.cases(arctic_lpi), ]

df<- national_data %>% dplyr::filter(ID<50)
index_vector = rep(FALSE, nrow(df)) #next row reliant on how many rows - need to change if not working
index_vector[1:121] = TRUE #change to number of rows as above

example_infile_name <- create_infile(df, index_vector=index_vector, name="example_data")
# An index can be created using this infile, for the period 1970 to 2014 with 100 bootstraps.
arctic_lpi <- LPIMain(example_infile_name, use_weightings = 0, REF_YEAR = 2017, PLOT_MAX = 2068, BOOT_STRAP_SIZE = 1000, VERBOSE=FALSE)
# Remove NAs (trailing years with no data)
national_lpi <- arctic_lpi[complete.cases(arctic_lpi), ]

df<- world_data %>% dplyr::filter(ID<50)
index_vector = rep(FALSE, nrow(df)) #next row reliant on how many rows - need to change if not working
index_vector[1:121] = TRUE #change to number of rows as above

example_infile_name <- create_infile(df, index_vector=index_vector, name="example_data")
# An index can be created using this infile, for the period 1970 to 2014 with 100 bootstraps.
arctic_lpi <- LPIMain(example_infile_name, use_weightings = 0, REF_YEAR = 2017, PLOT_MAX = 2068, BOOT_STRAP_SIZE = 1000, VERBOSE=FALSE)
# Remove NAs (trailing years with no data)
world_lpi <- arctic_lpi[complete.cases(arctic_lpi), ]

df<- global_data %>% dplyr::filter(ID<50)
index_vector = rep(FALSE, nrow(df)) #next row reliant on how many rows - need to change if not working
index_vector[1:121] = TRUE #change to number of rows as above

example_infile_name <- create_infile(df, index_vector=index_vector, name="example_data")
# An index can be created using this infile, for the period 1970 to 2014 with 100 bootstraps.
arctic_lpi <- LPIMain(example_infile_name, use_weightings = 0, REF_YEAR = 2017, PLOT_MAX = 2068, BOOT_STRAP_SIZE = 1000, VERBOSE=FALSE)
# Remove NAs (trailing years with no data)
global_lpi <- arctic_lpi[complete.cases(arctic_lpi), ]

#########

lpis<- list(local_lpi,national_lpi,world_lpi, global_lpi)
ggplot_multi_lpi(lpis, names=c("local","national", "world", "global"), xlims=c(2017, 2068), ylims=c(0.7, 1.1), facet = T)
