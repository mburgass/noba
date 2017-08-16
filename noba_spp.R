library(foreign)
library(data.table) # for fread()
library(sp)
library(rgdal)
library(raster)
library(maptools)
library(readr)
##############################################################################=
### Generate half-degree raster using am_cells to assign LOICZID to CenterLong x CenterLat.
### * Template and output in <dir_anx>/rgns
### * rasterize takes about 10 seconds...
##############################################################################=
source('/home/burgass/github/ohiprep/src/R/common.R')
dir_anx<- '/home/shares/ohi/git-annex/globalprep/spp_ico/rgns'
dir_data_am<- '/home/shares/ohi/git-annex/globalprep/_raw_data/aquamaps/d2015'
loiczid_raster_file  <- file.path(dir_anx, 'loiczid_raster.grd') #had to change file location as structure changed.
raster_template_file <- file.path(dir_anx, 'spp_ico/rgns/am_cells_template.tif')

loiczid_raster <- raster::raster(loiczid_raster_file)


##############################################################################=
ogr_location = file.path('~/shapefiles/seas')
rgn_layer    = 'NS_BS_aggregated_areas_NoBa'
ohi_type     = 'global'  # ohi_type = 'HS'    ohi_type = 'AQ'
noba_rgn<- readOGR(dsn= '/home/burgass/github/noba/shapefiles/seas', layer = 'NS_BS_aggregated_areas_NoBa', stringsAsFactors = FALSE)
### Determines proportional area of each cell covered by region polygons.  Returns data frame
### of rgn_id, loiczid, csq, and proportional area of loiczid cell covered by the rgn_id region.
### * reload: re-extract region IDs to cell IDs from indicated shape file?
### * ogr_location: where is the region vector layer information (without layer name)
### * rgn_layer:    which vector layer to use (no file extension, e.g. .shp)
### * ohi_type:     what type of assessment: global, HS, AQ
###
### Should not be year-specific, so leave prepped files in SpeciesDiversity/rgns, or change reload to TRUE.
### ??? TO DO: compare loiczid regions <-> CenterLong and CenterLat to last year's table, to make sure consistent from year to year.
##############################################################################=


message(sprintf('Reading regions shape file %s - come back in about 4 minutes.\n  %s/%s', rgn_layer, ogr_location, noba_rgn))
regions        <- noba_rgn
# slow command... ~ 4 minutes

# regions <- switch(ohi_type,
#global = regions[regions@data$rgn_typ %in% c('eez', 'eez-disputed', 'eez-inland'), ],
#HS     = regions[regions@data$rgn_typ %in% c('fao'), ],
#AQ     = regions[regions@data$ant_typ %in% c('eez-ccamlr'), ],
#regions[regions@data$rgn_typ %in% c('eez', 'eez-disputed', 'eez-inland'), ]) #default to same as global

raster_file    <- file.path(dir_anx, 'spp_ico/rgns/loiczid_raster')
#loiczid_raster <- get_loiczid_raster(reload = FALSE)

message('Extracting proportional area of LOICZID cells per region polygon.  Come back in 15-20 minutes.')
region_prop <- raster::extract(loiczid_raster,  regions, weights = TRUE, normalizeWeights = FALSE, progress = 'text')
# small = TRUE returns 1 for rgn_id 232, not what we want.
# slow command... ~15 minutes (even with the small = TRUE)


### assign rgn_id and rgn_name identifiers (from `regions`) to region_prop, convert to data.frame

rgn_id_name <- data.frame(regions@data$AreaName, regions@data$cat) %>%
  unite(combo, regions.data.cat, regions.data.AreaName, sep = '_')


names(region_prop) <- rgn_id_name$combo
region_prop_df     <- plyr::ldply(region_prop, rbind) # ??? still a plyr function.
# length(unique(region_prop_df$.id))
#   WAS: less than 254 due to repeats of Canada and one small region (232: Bosnia/Herzegovina) with no rasters identified
#   IS:  278, including a cell for #232.
region_prop_df <- region_prop_df %>%
  separate(.id, c('rgn_id', 'rgn_name'), sep = '_') %>%
  rename(loiczid = value,
         proportionArea = weight)


### ??? add in this region -  Bosnia/Herzegovina (BIH), which appears to be too small to catch using this method (<1% of area)
### ??? SKIPPING THIS FOR NOW!!!
# cells_2013[cells_2013$rgn_id==232, ]
# cells_2013[cells_2013$csq=='1401:227:4', ]
# am_cells[am_cells$CsquareCode == '1401:227:4', ]
# 6.034664/2269.83
# bih <- data.frame(rgn_id=232, LOICZID=68076, proportionArea=0.002658641)
# region_prop_df <- rbind(region_prop_df, bih)

file_loc <- file.path(dir_data_am, 'csv/hcaf_truncated.csv')
message(sprintf('Loading AquaMaps half-degree cell authority file.  Less than 1 minute.\n  %s ', file_loc))
am_cells <- fread(file_loc, header = TRUE, stringsAsFactors = FALSE) %>%
  as.data.frame() %>%
  dplyr::select(csquarecode, loiczid, cellarea)
stopifnot(sum(duplicated(am_cells$csq)) == 0)

message('Joining csq values and cell areas to loiczid values.')
region_prop_df <- region_prop_df %>%
  left_join(am_cells, by = 'loiczid')

message(sprintf('Writing loiczid/csq/cell proportions/cell areas by region to: \n  %s', rgn_prop_file))
write_csv(region_prop_df, 'noba_prop_file.csv')



#####Working out what species are in each regions#############
iucn_cells_spp<- read_csv('/home/shares/ohi/git-annex/globalprep/spp_ico/v2016/int/iucn_cells_spp.csv')
am_cells_spp<- read_csv('/home/shares/ohi/git-annex/globalprep/spp_ico/v2016/int/am_cells_spp_prob0.csv')
am_namecheck<-read_csv('/home/shares/ohi/git-annex/globalprep/spp_ico/v2016/int/namecheck_am.csv')
region_prop_df<- read_csv('noba_prop_file.csv')
sp_summary<- iucn_cells_spp %>%
  left_join(region_prop_df, by = 'loiczid')
sp_summary2<-sp_summary %>% drop_na(rgn_id)
sp_summary2<- sp_summary2 %>%
  group_by(rgn_id) %>%
  dplyr::select(sciname, iucn_sid, rgn_id, rgn_name) %>%
  ungroup()
sp_summary3<- sp_summary2 %>% distinct()
sp_summary3<- arrange(sp_summary3, rgn_id)
write_csv(sp_summary3, 'iucn_spp_summary.csv')

am_sp_summary<- am_cells_spp %>%
  left_join(region_prop_df, by = 'loiczid')
am_summary<-am_sp_summary %>% drop_na(rgn_id)
am_summary2<- am_summary %>%
  group_by(rgn_id) %>%
  dplyr::select(am_sid, rgn_id, rgn_name) %>%
  ungroup()
am_summary3<- am_summary2 %>% distinct()
am_summary3<- arrange(am_summary3, rgn_id)
am_summary4<- am_summary3 %>%
  left_join(am_namecheck, by='am_sid') %>%
  select(-sciname2)
am_summary5<- am_summary4 %>%
  dplyr::select(am_sid, rgn_id, rgn_name, sciname)
write_csv(am_summary5, 'am_spp_summary.csv')

am_spp<-read_csv('/home/burgass/github/noba/am_spp_summary.csv')
iucn_spp<- read_csv('/home/burgass/github/noba/iucn_spp_summary.csv')
all_spp<- read_csv('/home/shares/ohi/git-annex/globalprep/spp_ico/v2016/int/spp_all_cleaned.csv')
am_iucn<- full_join(am_spp, iucn_spp, by=c('sciname', 'rgn_id', 'rgn_name'))# this doesn't work - means IUCN ID doesn't align
test<- left_join(all_spp, am_iucn, by='sciname') %>%
  select(-am_sid.y, -iucn_sid.y)
#all_spp_iucn<- full_join(all_spp, iucn_spp, by=c('iucn_sid', 'sciname'))
#all_spp_iucn_am<- semi_join(all_spp_iucn, am_spp, by=c('am_sid', 'sciname', 'rgn_id', 'rgn_name'))
all_spp3<- test %>% drop_na(rgn_id)
write_csv(all_spp3, file.path('spp_summary_final_rgns.csv'))

spp_noba<- select(all_spp3, -rgn_id, -rgn_name)
spp_noba<- subset(spp_noba,!duplicated(spp_noba$sciname))
write_csv(spp_noba, 'spp_noba_summary.csv')
