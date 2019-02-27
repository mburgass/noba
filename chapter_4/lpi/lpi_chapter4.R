library(devtools)
# Install from main ZSL repository online
install_github("mburgass/rlpi", ref='chapter4', dependencies=TRUE)
library(ncdf4)
library(readtext)
library(tidyverse)
library(rlpi)
closeAllConnections()


#Read base case
fmsy0_data<- read.csv("chapter_4/lpi/fmsy0_lpi.csv")
fmsy1_data<- read.csv("chapter_4/lpi/fmsy1_lpi.csv")
fmsy11_data<- read.csv("chapter_4/lpi/fmsy11_lpi.csv")
fmsy08_data<- read.csv("chapter_4/lpi/fmsy08_lpi.csv")
fmsy06_data<- read.csv("chapter_4/lpi/fmsy06_lpi.csv")

df<- fmsy0_data %>% dplyr::filter(ID<50)
index_vector = rep(FALSE, nrow(df)) #next row reliant on how many rows - need to change if not working
index_vector[1:121] = TRUE #change to number of rows as above

example_infile_name <- create_infile(df, index_vector=index_vector, name="example_data")
# An index can be created using this infile, for the period 1970 to 2014 with 100 bootstraps.
arctic_lpi <- LPIMain(example_infile_name, use_weightings = 0, REF_YEAR = 2015, PLOT_MAX = 2068, BOOT_STRAP_SIZE = 10000, VERBOSE=FALSE)
# Remove NAs (trailing years with no data)
fmsy0_lpi <- arctic_lpi[complete.cases(arctic_lpi), ]

df<- fmsy1_data %>% dplyr::filter(ID<50)
index_vector = rep(FALSE, nrow(df)) #next row reliant on how many rows - need to change if not working
index_vector[1:121] = TRUE #change to number of rows as above

example_infile_name <- create_infile(df, index_vector=index_vector, name="example_data")
# An index can be created using this infile, for the period 1970 to 2014 with 100 bootstraps.
arctic_lpi <- LPIMain(example_infile_name, use_weightings = 0, REF_YEAR = 2015, PLOT_MAX = 2068, BOOT_STRAP_SIZE = 10000, VERBOSE=FALSE)
# Remove NAs (trailing years with no data)
fmsy1_lpi <- arctic_lpi[complete.cases(arctic_lpi), ]

####hide##############

df<- fmsy11_data %>% dplyr::filter(ID<50)
index_vector = rep(FALSE, nrow(df)) #next row reliant on how many rows - need to change if not working
index_vector[1:121] = TRUE #change to number of rows as above

example_infile_name <- create_infile(df, index_vector=index_vector, name="example_data")
# An index can be created using this infile, for the period 1970 to 2014 with 100 bootstraps.
arctic_lpi <- LPIMain(example_infile_name, use_weightings = 0, REF_YEAR = 2015, PLOT_MAX = 2068, BOOT_STRAP_SIZE = 10000, VERBOSE=FALSE)
# Remove NAs (trailing years with no data)
fmsy11_lpi <- arctic_lpi[complete.cases(arctic_lpi), ]

df<- fmsy08_data %>% dplyr::filter(ID<50)
index_vector = rep(FALSE, nrow(df)) #next row reliant on how many rows - need to change if not working
index_vector[1:121] = TRUE #change to number of rows as above

example_infile_name <- create_infile(df, index_vector=index_vector, name="example_data")
# An index can be created using this infile, for the period 1970 to 2014 with 100 bootstraps.
arctic_lpi <- LPIMain(example_infile_name, use_weightings = 0, REF_YEAR = 2015, PLOT_MAX = 2068, BOOT_STRAP_SIZE = 10000, VERBOSE=FALSE)
# Remove NAs (trailing years with no data)
fmsy08_lpi <- arctic_lpi[complete.cases(arctic_lpi), ]

######hide##########

df<- fmsy06_data %>% dplyr::filter(ID<50)
index_vector = rep(FALSE, nrow(df)) #next row reliant on how many rows - need to change if not working
index_vector[1:121] = TRUE #change to number of rows as above

example_infile_name <- create_infile(df, index_vector=index_vector, name="example_data")
# An index can be created using this infile, for the period 1970 to 2014 with 100 bootstraps.
arctic_lpi <- LPIMain(example_infile_name, use_weightings = 0, REF_YEAR = 2015, PLOT_MAX = 2068, BOOT_STRAP_SIZE = 10000, VERBOSE=FALSE)
# Remove NAs (trailing years with no data)
fmsy06_lpi <- arctic_lpi[complete.cases(arctic_lpi), ]

#########

lpis<- list(fmsy1_lpi,fmsy06_lpi, fmsy0_lpi)
ggplot_multi_lpi(lpis, names=c("Global Sustainability","Precautionary Fishing", "Strict Conservation"), xlims=c(2015, 2068), ylims=c(0.7, 1.2), facet = T)






fmsy06_lpi$scenario<- "Precautionary Fishing"
fmsy1_lpi$scenario<- "Global Sustainability"
fmsy0_lpi$scenario<- "Strict Conservation"
fmsy_lpis2<- rbind(fmsy0_lpi,fmsy1_lpi,fmsy06_lpi)
fmsy_lpis2<- select(fmsy_lpis2, LPI_final, scenario)
write.csv(fmsy_lpis2, "chapter_4/lpi/lpi_scores.csv")

