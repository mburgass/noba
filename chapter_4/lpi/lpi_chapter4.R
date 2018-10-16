library(devtools)
# Install from main ZSL repository online
install_github("mburgass/rlpi", ref='chapter4', dependencies=TRUE)
library(ncdf4)
library(readtext)
library(tidyverse)
library(rlpi)
closeAllConnections()


#Read base case
fmsy1_data<- read.csv("chapter_4/lpi/fmsy1_lpi.csv")
fmsy11_data<- read.csv("chapter_4/lpi/fmsy11_lpi.csv")
fmsy08_data<- read.csv("chapter_4/lpi/fmsy08_lpi.csv")
fmsy06_data<- read.csv("chapter_4/lpi/fmsy06_lpi.csv")

df<- fmsy1_data %>% dplyr::filter(ID<50)
index_vector = rep(FALSE, nrow(df)) #next row reliant on how many rows - need to change if not working
index_vector[1:121] = TRUE #change to number of rows as above

example_infile_name <- create_infile(df, index_vector=index_vector, name="example_data")
# An index can be created using this infile, for the period 1970 to 2014 with 100 bootstraps.
arctic_lpi <- LPIMain(example_infile_name, use_weightings = 0, REF_YEAR = 2017, PLOT_MAX = 2068, BOOT_STRAP_SIZE = 10000, VERBOSE=FALSE)
# Remove NAs (trailing years with no data)
fmsy1_lpi <- arctic_lpi[complete.cases(arctic_lpi), ]

df<- fmsy11_data %>% dplyr::filter(ID<50)
index_vector = rep(FALSE, nrow(df)) #next row reliant on how many rows - need to change if not working
index_vector[1:121] = TRUE #change to number of rows as above

example_infile_name <- create_infile(df, index_vector=index_vector, name="example_data")
# An index can be created using this infile, for the period 1970 to 2014 with 100 bootstraps.
arctic_lpi <- LPIMain(example_infile_name, use_weightings = 0, REF_YEAR = 2017, PLOT_MAX = 2068, BOOT_STRAP_SIZE = 10000, VERBOSE=FALSE)
# Remove NAs (trailing years with no data)
fmsy11_lpi <- arctic_lpi[complete.cases(arctic_lpi), ]

df<- fmsy08_data %>% dplyr::filter(ID<50)
index_vector = rep(FALSE, nrow(df)) #next row reliant on how many rows - need to change if not working
index_vector[1:121] = TRUE #change to number of rows as above

example_infile_name <- create_infile(df, index_vector=index_vector, name="example_data")
# An index can be created using this infile, for the period 1970 to 2014 with 100 bootstraps.
arctic_lpi <- LPIMain(example_infile_name, use_weightings = 0, REF_YEAR = 2017, PLOT_MAX = 2068, BOOT_STRAP_SIZE = 10000, VERBOSE=FALSE)
# Remove NAs (trailing years with no data)
fmsy08_lpi <- arctic_lpi[complete.cases(arctic_lpi), ]

df<- fmsy06_data %>% dplyr::filter(ID<50)
index_vector = rep(FALSE, nrow(df)) #next row reliant on how many rows - need to change if not working
index_vector[1:121] = TRUE #change to number of rows as above

example_infile_name <- create_infile(df, index_vector=index_vector, name="example_data")
# An index can be created using this infile, for the period 1970 to 2014 with 100 bootstraps.
arctic_lpi <- LPIMain(example_infile_name, use_weightings = 0, REF_YEAR = 2017, PLOT_MAX = 2068, BOOT_STRAP_SIZE = 10000, VERBOSE=FALSE)
# Remove NAs (trailing years with no data)
fmsy06_lpi <- arctic_lpi[complete.cases(arctic_lpi), ]

#########

lpis<- list(fmsy1_lpi,fmsy11_lpi,fmsy08_lpi, fmsy06_lpi)
ggplot_multi_lpi(lpis, names=c("fmsy06","fmsy08", "fmsy1", "fmsy11"), xlims=c(2017, 2068), ylims=c(0.7, 1.1), facet = T)
