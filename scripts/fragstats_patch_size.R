####Fragstats - to get patch sizes in 3 largest fires

library(terra)
library(sf)
library(here)
library(landscapemetrics)
library(dplyr)
library(ggplot2)

#read in groves for later clipping
groves <- st_read(here("C:/Users/kshive/Documents/UCB/Projects/In Progress/State of the SEGI/2024/GIT-state-segi-24/data/spatial_data/inputs/forest/2023_GS_Groves_OTS_public.shp"))


#read in the three large fire rasters
cls.rast.dir = here("C:/Users/kshive/Documents/UCB/Projects/In Progress/State of the SEGI/2024/GIT-state-segi-24/data/spatial_data/inputs/fires_needed_rdnbr/classified_rasters_r/fires_w_high_sev")
cls.rast.list <- list.files(cls.rast.dir,  pattern='tif$', full.names = T)
cls.rast.list.names <- list.files(cls.rast.dir, pattern=glob2rx('*.tif'), full.names = F)
# 
##which have no high severity
# cbi = st_read(here("data/spatial_data/inputs/fires_needed_rdnbr/cbi_all_r_6class.shp")) # 
# fires.use = cbi %>%
#   filter(burnsev %in% c(4,5,6))
# subset.fires = list(unique(fires.use$fireyr))

reclass_matrix <- tibble(
  from = c(1, 4),
  to = c(3, 7),
  becomes = c(0, 7)
)# # rclmat <- matrix(m, ncol=3, byrow=TRUE)
# # rc1 <- classify(r, rclmat, include.lowest=TRUE)
# # 
# 
datalist = list()
for(i in 1:length(cls.rast.list)) {
  rast.1 = rast(cls.rast.list[i])
  names(rast.1) = "rdnbr"
  rast.1.hi <- rast.1 %>%
    classify(reclass_matrix,
             right = F) # include
# writeRaster(rast.1.hi, paste("here(data/spatial_data/outputs/fragstats/",gsub('.{10}$', '', as.character(cls.rast.list.names[i])),"_reclass.tif)",sep = ''),
#             datatype = 'INT2S')

    for.patch <- get_patches(rast.1.hi, directions = 4, class = 7,
                           return_raster =  T)

    patch.rast = rast(for.patch[[1]])
# writeRaster(patch.rast$class_7, paste("here(data/spatial_data/outputs/fragstats/",gsub('.{10}$', '', as.character(cls.rast.list.names[i])),"_patches.tif)",sep = ''),
#             datatype = 'INT2S')

  patch.to.poly = patch.rast %>%
      as.polygons() %>%
      st_as_sf()  %>%
      st_transform(crs(groves)) %>%
      st_intersection(groves) %>%
      mutate(area_ha_grv = round(as.numeric(st_area(.)*0.0001),2)) %>%
      # st_drop_geometry() %>%
      group_by(lyr.1) %>%
      summarise(area_ha = sum(area_ha_grv)) %>%
      mutate(fireyr = paste(gsub('.{10}$', '', as.character(cls.rast.list.names[i])))
      ) %>%
      st_collection_extract("POLYGON") %>%
        st_cast("POLYGON") %>%
        mutate(ind_area_ha = round(as.numeric(st_area(.)*0.0001),2)) %>%
    st_intersection(groves) %>%
    select(-area_ha,-Acres,-ORIG_FID,-Grove_Grou,-Land_Stewa,
            -Land_Ste_1,-Grove_Elem,-Comments,-Shape_Leng,
            -Shape_Area,-geometry)
    
   datalist[[i]] <- patch.to.poly # add it to your list

}

all_fires_w_high_sev = do.call(rbind, datalist)
max(all_fires_w_high_sev$ind_area_ha)
View(all_fires_w_high_sev)

#need to fix to get rid of sliver as points
all_fires_w_high_sev = all_fires_w_high_sev %>% st_collection_extract("POLYGON")

write_sf(all_fires_w_high_sev, "C:/Users/kshive/Documents/UCB/Projects/In Progress/State of the SEGI/2024/GIT-state-segi-24/data/spatial_data/outputs/fragstats/subset26fires_high_sev_patches_8sqr.shp")

prnt = st_drop_geometry(all_fires_w_high_sev)
write.csv(prnt, "C:/Users/kshive/Documents/UCB/Projects/In Progress/State of the SEGI/2024/GIT-state-segi-24/data/spatial_data/outputs/fragstats/SEGI_ind_high_sev_patches_subset26fires.csv")  

high.sevs = all_fires_w_high_sev %>%
  st_drop_geometry() %>%
  filter(ind_area_ha>=10) %>%
  mutate(patch_name = paste(lyr.1,fireyr,sep="-"))
summary(high.sevs$ind_area_ha)

hist(high.sevs$ind_area_ha, breaks = seq(from = min(high.sevs$ind_area_ha), to = max(high.sevs$ind_area_ha), by = 10))

high.sevs. = gghistplot(high.sevs, aes(ind_area_ha)) +
  geom_histogram(binwidth = 10) +
  ggtitle("High severity patches >10ha (10-265) for 26 fires 1984-2024")
high.sevs.hist

