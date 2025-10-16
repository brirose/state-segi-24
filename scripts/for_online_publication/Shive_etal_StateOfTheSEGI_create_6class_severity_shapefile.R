library(terra)
library(here)
library(dplyr)
library(sf)
library(tidyverse)

##This code classifies all of the RdNBR rasters using thresholds from Miller et al. (2007),
##and further subdivides the high severity class into regeneration-relevant thresholdss. It
##then converts these to a single shapefile for manipulation in other analyses.
##This approach writes each step out to a new raster/shapefile because we had 
##other needs for these files.

#bring in groves shapefile
groves <- st_read(here("data/spatial_data/inputs/forest/2023_GS_Groves_OTS_public.shp")) %>%
  clean_names() %>%
  st_make_valid() %>%
  st_cast("MULTIPOLYGON") %>% 
  st_cast("POLYGON")

#bring in all rasters in a list
rast.dir = here("folder where rasters area stored")
rast.list <- list.files(rast.dir, pattern='tif$', full.names = T)
#get the names of the rasters only
rast.list.names <- list.files(rast.dir, pattern='tif$', full.names = F)


#check all same crs
for(i in 1:length(rast.list)) {
  rast.i = rast(rast.list[i])
  j = i+1
  rast.j = rast(rast.list[12])
  print(same.crs(rast.i,rast.j))
}

#################################################################
#reclassification matrix
reclass_matrix <- tibble(
  from = c(-32769, 69, 315, 640, 800, 1100),
  to = c(69, 315, 640, 800, 1100, 9992),
  becomes = c(1, 2, 3, 4,5,6)
)

##first make classified rasters and write them out
for(i in 1:length(rast.list)){
  rast.1 = rast(rast.list[i])
  names(rast.1) = "rdnbr"
  imf <- rast.1 %>% 
    classify(reclass_matrix,
             right = T) # include "from" value in category
  writeRaster(imf, here(paste("file path for rasters/",gsub('.{10}$', '', as.character(rast.list.names[i])),"_CBI6_r.tif",sep = '')), overwrite = T)
  
}

#get list of the now classified rasters
cls.rast.dir = here("folder with classified rasters")
cls.rast.list <- list.files(cls.rast.dir, pattern='tif$', full.names = T)
#get the names of the rasters only
cls.rast.list.names <- list.files(cls.rast.dir, pattern='tif$', full.names = F)


##convert all rasters to sf polygons, adding text burn 
##severity column, year & fire name
for(i in 1:length(cls.rast.list)){
  rast.1 = rast(cls.rast.list[i])
  names(rast.1) = "rdnbr"
  imf <- rast.1 %>%
    as.polygons() %>% 
    st_as_sf() %>%
    mutate(burnsev = rdnbr,
           burnsev_text = case_when(burnsev == 1 ~ "Undetected change",
                                    burnsev == 2 ~ "Low",
                                    burnsev == 3 ~ "Moderate",
                                    burnsev == 4 ~ "High (may have regen)",
                                    burnsev == 5 ~ "High (mod regen risk)",
                                    burnsev == 6 ~ "High (high regen risk)",
                                    .default = "oopsy"),
           year = paste(substr(as.character(rast.list.names[i]),1,4)),
           fireyr = paste(gsub('.{10}$', '', as.character(rast.list.names[i])))
    )
  write_sf(imf, here(paste("file path/",gsub('.{10}$', '', as.character(rast.list.names[i])),"_CBI5_polys_r.shp",sep = '')))
  
}

#create a list of the new shapefiles and bind them
shapes.dir <- (here("file path with shapefiles"))
shapes.list <- list.files(shapes.dir, pattern="\\.shp$", full.names=TRUE)
shapes.all = lapply(shapes.list,st_read)

cbi_all <- do.call(rbind, shapes.all)
#convert the file to the same file as the grove projection
cbi = cbi_all %>%
  st_transform(crs(groves))

write_sf(here("file path/cbi_all_r.shp"))