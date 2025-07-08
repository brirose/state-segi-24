####Fragstats - to get patch sizes in 3 largest fires

library(terra)
library(sf)
library(here)
library(landscapemetrics)
library(dplyr)


#read in groves for later clipping
#bring in all raster in a list
groves <- st_read(here("data/spatial_data/inputs/forest/FINAL_SEGI_AnalysisArea_wOwnerships_GroveNames_noBuff/FINAL_SEGI_AnalysisArea_wOwnerships_GroveNames_noBuff.shp"))


#read in the three large fire rasters

cast = rast(here("data/spatial_data/inputs/fires_needed_rdnbr/proj_classified/_2020CASTLE_rdnbr_proj_to_grvs_tif_CBI4.tif"))
wind = rast(here("data/spatial_data/inputs/fires_needed_rdnbr/proj_classified/_2021WINDY_rdnbr_proj_to_grvs_tif_CBI4.tif"))
knp = rast(here("data/spatial_data/inputs/fires_needed_rdnbr/proj_classified/_2021KNP_Comp_rdnbr_proj_to_grvs_tif_CBI4.tif"))

cls.rast.dir = here("data/spatial_data/inputs/fires_needed_rdnbr/classified_rasters_r")
cls.rast.list <- list.files(cls.rast.dir, pattern=glob2rx('2020|2021*.tif'), full.names = T)
cls.rast.list.names <- list.files(cls.rast.dir, pattern=glob2rx('2020|2021*.tif'), full.names = F)

#
reclass_matrix <- tibble(
from = c(1, 2, 3, 4),
to = c(2, 3, 4, 6),
becomes = c(1, 2, 3, 7)
)

datalist = list()
for(i in 1:length(cls.rast.list)) {
  rast.1 = rast(cls.rast.list[i])
  names(rast.1) = "rdnbr"
  rast.1.hi <- rast.1 %>% 
    classify(reclass_matrix,
             right = T) # include
  for.patch <- get_patches(rast.1.hi, class = 7, directions = 8, 
                           return_raster =  T) 
  patch.rast = rast(for.patch[[1]])
  patch.to.poly = patch.rast %>%
    as.polygons() %>%
    st_as_sf()  %>%
    st_transform(crs(groves)) %>%
    st_intersection(groves) %>%
    mutate(area_ha_grv = round(as.numeric(st_area(.)*0.0001),2),
           ) %>%
    st_drop_geometry() %>%
    group_by(lyr.1) %>%
    summarise(area_ha = sum(area_ha_grv)) %>%
    mutate(fireyr = paste(gsub('.{10}$', '', as.character(cls.rast.list.names[i])))
    )
   datalist[[i]] <- patch.to.poly # add it to your list
  
}

all_2020_2021 = do.call(rbind, datalist)
max(all_2020_2021$area_ha)

all20_21 = all_2020_2021 %>%
  filter(area_ha>=10)
nrow(all20_21)
