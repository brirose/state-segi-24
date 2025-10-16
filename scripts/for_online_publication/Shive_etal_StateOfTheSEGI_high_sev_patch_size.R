####Fragstats - extract high severity patch sizes
library(terra)
library(sf)
library(here)
library(landscapemetrics)
library(dplyr)
library(ggplot2)

#read in groves shapefile
groves <- st_read("2023_GS_Groves_OTS_public.shp")

#read in the 26 fires with high severity
cls.rast.dir = here("C:/Users/kshive/Documents/UCB/Projects/In Progress/State of the SEGI/2024/GIT-state-segi-24/data/spatial_data/inputs/fires_needed_rdnbr/classified_rasters_r/fires_w_high_sev")
cls.rast.list <- list.files(cls.rast.dir,  pattern='tif$', full.names = T)
cls.rast.list.names <- list.files(cls.rast.dir, pattern=glob2rx('*.tif'), full.names = F)
# 

#reclass matrix to change our CBI vals (which includes 6 classes, 
#since high severity was split into 3 classes for regen analysis) to
#just high severity (4-6) and not (1-3)
reclass_matrix <- tibble(
  from = c(1, 4),
  to = c(3, 7),
  becomes = c(0, 7))

#create list for generated polygons
datalist = list()

#for loop for processing
for(i in 1:length(cls.rast.list)) {
  #read in as raster
  rast.1 = rast(cls.rast.list[i])
  #ensure value name is consistent
  names(rast.1) = "rdnbr"
  #reclassify raster
  rast.1.hi <- rast.1 %>%
    classify(reclass_matrix,
             right = F) # include
  #get patches of high severity (7)
  for.patch <- get_patches(rast.1.hi, directions = 4, class = 7,
                           return_raster =  T)
  #convert to raster  
  patch.rast = rast(for.patch[[1]])
  #convert to sf polygon
  patch.to.poly = patch.rast %>%
    as.polygons() %>%
    st_as_sf()  %>%
    #transform crs and clip to grove shapefile
    st_transform(crs(groves)) %>%
    st_intersection(groves) %>%
    #calculate area in ha
    mutate(area_ha_grv = round(as.numeric(st_area(.)*0.0001),2)) %>%
    #group by patch name and calculate area
    group_by(lyr.1) %>%
    summarise(area_ha = sum(area_ha_grv)) %>%
    #add fire year and name
    mutate(fireyr = paste(gsub('.{10}$', '', as.character(cls.rast.list.names[i])))
    ) %>%
    #convernt multipolygons to polygons
    st_collection_extract("POLYGON") %>%
    st_cast("POLYGON") %>%
    #recalculate ha for all individual polygons
    mutate(ind_area_ha = round(as.numeric(st_area(.)*0.0001),2)) %>%
    #re-intersect groves to get grove name
    st_intersection(groves) %>%
    #trim output
    select(-area_ha,-Acres,-ORIG_FID,-Grove_Grou,-Land_Stewa,
           -Land_Ste_1,-Grove_Elem,-Comments,-Shape_Leng,
           -Shape_Area,-geometry)
  #add to list
  datalist[[i]] <- patch.to.poly # add it to your list
  
}

#bind to create one large file
all_fires_w_high_sev = do.call(rbind, datalist)

#get rid of sliver that converted to point
all_fires_w_high_sev = all_fires_w_high_sev %>% st_collection_extract("POLYGON")

#get summary info on patches >10 ha
high.sevs = all_fires_w_high_sev %>%
  st_drop_geometry() %>%
  filter(ind_area_ha>=10) %>%
  mutate(patch_name = paste(lyr.1,fireyr,sep="-"))
summary(high.sevs$ind_area_ha)

