###LPI Arctic#####
library(rlpi)
# Constructing infiles from a populations table...

# First read the population table (this is the Living Planet Database excluding confidential records and filtering for marine Arctic populations)
lpi_data <- read.csv("LPI_files/arctic_marine_lpi_2.csv", na.strings = "NULL")

# Create an infile from all the data. All the population data in the 'lpi_data' table will be converted and stored in a file called 'example_data_pops.txt' 
#and a file called 'example_data_infile.txt' will be created that references the first file 
#(the infile name will also be stored in the returned variable 'example_infile_name')

# Here we select the first 100 populations by creating an index vector that's FALSE for all rows, then setting the first 100 rows to TRUE
index_vector = rep(FALSE, nrow(lpi_data))
index_vector[1:118] = TRUE

example_infile_name <- create_infile(lpi_data, index_vector=index_vector, name="example_data")
# An index can be created using this infile, for the period 1970 to 2014 with 100 bootstraps.
arctic_lpi <- LPIMain(example_infile_name, REF_YEAR = 1970, PLOT_MAX = 2014, BOOT_STRAP_SIZE = 100, VERBOSE=FALSE)
# Remove NAs (trailing years with no data)
arctic_lpi <- arctic_lpi[complete.cases(arctic_lpi), ]
# Plot the resulting index
ggplot_lpi(arctic_lpi, title = "arctic_lpi", xlims=c(1970, 2014), ylim=c(0, 5))
# Plot the resulting index with logged y-axis (note the use of a +ive ymin)
ggplot_lpi(arctic_lpi, title = "arctic_lpi_log", xlims=c(1970, 2014), ylim=c(0.3, 2), trans="log")

###Regional LPIs#####
alaska_lpi <- read.csv("LPI_files/alaska_lpi.csv", na.strings = "NULL")
beaufort_lpi <- read.csv("LPI_files/beaufort_lpi.csv", na.strings = "NULL")
egreenland_lpi <- read.csv("LPI_files/east_greenland_lpi.csv", na.strings = "NULL")
wgreenland_lpi <- read.csv("LPI_files/west_greenland_lpi.csv", na.strings = "NULL")
janmayen_lpi <- read.csv("LPI_files/jan_mayen_lpi.csv", na.strings = "NULL")
nunavut_lpi <- read.csv("LPI_files/nunavut_lpi.csv", na.strings = "NULL")
russia_lpi <- read.csv("LPI_files/russia_lpi.csv", na.strings = "NULL")
svalbard_lpi <- read.csv("LPI_files/svalbard_lpi.csv", na.strings = "NULL")
norway_lpi <- read.csv("LPI_files/norway_lpi.csv", na.strings = "NULL")

alaska_vector = rep(TRUE, nrow(alaska_lpi))
beaufort_vector = rep(TRUE, nrow(beaufort_lpi))
egreenland_vector = rep(TRUE, nrow(egreenland_lpi))
wgreenland_vector = rep(TRUE, nrow(wgreenland_lpi))
janmayen_vector = rep(TRUE, nrow(janmayen_lpi))
nunavut_vector = rep(TRUE, nrow(nunavut_lpi))
russia_vector = rep(TRUE, nrow(russia_lpi))
svalbard_vector = rep(TRUE, nrow(svalbard_lpi))
norway_vector = rep(TRUE, nrow(norway_lpi))



alaska_infile_name <- create_infile(alaska_lpi, index_vector=alaska_vector, name="alaska_data")
alaska_lpi <- LPIMain(alaska_infile_name, REF_YEAR = 1970, PLOT_MAX = 2012, BOOT_STRAP_SIZE = 100, VERBOSE=FALSE)
ggplot_lpi(alaska_lpi, title = "alaska_lpi", xlims=c(1970, 2012), ylim=c(0, 10))

beaufort_infile_name <- create_infile(beaufort_lpi, index_vector=beaufort_vector, name="beaufort_data")
beaufort_lpi <- LPIMain(beaufort_infile_name, REF_YEAR = 1970, PLOT_MAX = 2012, BOOT_STRAP_SIZE = 100, VERBOSE=FALSE)
ggplot_lpi(beaufort_lpi, title = "beaufort_lpi", xlims=c(1970, 2012), ylim=c(0, 20))

egreenland_infile_name <- create_infile(egreenland_lpi, index_vector=egreenland_vector, name="egreenland_data")
egreenland_lpi <- LPIMain(egreenland_infile_name, REF_YEAR = 1970, PLOT_MAX = 2012, BOOT_STRAP_SIZE = 100, VERBOSE=FALSE)
ggplot_lpi(egreenland_lpi, title = "egreenland_lpi", xlims=c(1970, 2012), ylim=c(0, 10))

wgreenland_infile_name <- create_infile(wgreenland_lpi, index_vector=wgreenland_vector, name="wgreenland_data")
wgreenland_lpi <- LPIMain(wgreenland_infile_name, REF_YEAR = 1970, PLOT_MAX = 2012, BOOT_STRAP_SIZE = 100, VERBOSE=FALSE)
ggplot_lpi(wgreenland_lpi, title = "wgreenland_lpi", xlims=c(1970, 2012), ylim=c(0, 6))

janmayen_infile_name <- create_infile(janmayen_lpi, index_vector=janmayen_vector, name="janmayen_data")
janmayen_lpi <- LPIMain(janmayen_infile_name, REF_YEAR = 1970, PLOT_MAX = 2012, BOOT_STRAP_SIZE = 100, VERBOSE=FALSE)
ggplot_lpi(janmayen_lpi, title = "janmayen_lpi", xlims=c(1970, 2012), ylim=c(0, 2))

nunavut_infile_name <- create_infile(nunavut_lpi, index_vector=nunavut_vector, name="nunavut_data")
nunavut_lpi <- LPIMain(nunavut_infile_name, REF_YEAR = 1970, PLOT_MAX = 2012, BOOT_STRAP_SIZE = 100, VERBOSE=FALSE)
ggplot_lpi(nunavut_lpi, title = "nunavut_lpi", xlims=c(1970, 2012), ylim=c(0, 20))

russia_infile_name <- create_infile(russia_lpi, index_vector=russia_vector, name="russia_data")
russia_lpi <- LPIMain(russia_infile_name, REF_YEAR = 1970, PLOT_MAX = 2012, BOOT_STRAP_SIZE = 100, VERBOSE=FALSE)
ggplot_lpi(russia_lpi, title = "russia_lpi", xlims=c(1970, 2012), ylim=c(0, 2))

svalbard_infile_name <- create_infile(svalbard_lpi, index_vector=index_vector, name="svalbard_data")
svalbard_lpi <- LPIMain(svalbard_infile_name, REF_YEAR = 1970, PLOT_MAX = 2012, BOOT_STRAP_SIZE = 100, VERBOSE=FALSE)
ggplot_lpi(svalbard_lpi, title = "svalbard_lpi", xlims=c(1970, 2012), ylim=c(0, 2))

norway_infile_name <- create_infile(norway_lpi, index_vector=norway_vector, name="norway_data")
norway_lpi <- LPIMain(norway_infile_name, REF_YEAR = 1970, PLOT_MAX = 2012, BOOT_STRAP_SIZE = 100, VERBOSE=FALSE)
ggplot_lpi(norway_lpi, title = "norway_lpi", xlims=c(1970, 2012), ylim=c(0, 2))
