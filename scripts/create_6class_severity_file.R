library(terra)
library(here)
library(dplyr)
library(sf)
library(tidyverse)

#bring in all raster in a list
groves <- st_read(here("data/spatial_data/inputs/forest/FINAL_SEGI_AnalysisArea_wOwnerships_GroveNames_noBuff/FINAL_SEGI_AnalysisArea_wOwnerships_GroveNames_noBuff.shp"))

rast.dir = here("data/spatial_data/inputs/fires_needed_rdnbr")
rast.list <- list.files(rast.dir, pattern='tif$', full.names = T)
rast.list.names <- list.files(rast.dir, pattern='tif$', full.names = F)

#check all same crs
for(i in 1:length(rast.list)) {
  rast.i = rast(rast.list[i])
  j = i+1
  rast.j = rast(rast.list[12])
  print(same.crs(rast.i,rast.j))
}

#create mosaic to get max value - but don't use cuz the non GEE one is messed up
rast.a = rast(rast.list[1])
rast.b = rast(rast.list[2])
rast.c = rast(rast.list[3])
rast.d = rast(rast.list[4])
rast.e = rast(rast.list[4])
rast.f = rast(rast.list[5])
rast.g = rast(rast.list[6])
rast.h = rast(rast.list[7])
rast.i = rast(rast.list[8])
rast.j = rast(rast.list[9])
rast.k = rast(rast.list[10])
rast.l = rast(rast.list[11])
rast.m = rast(rast.list[12])
rast.n = rast(rast.list[13])
rast.o = rast(rast.list[14])
rast.p = rast(rast.list[15])
rast.q = rast(rast.list[16])
rast.r = rast(rast.list[17])
rast.s = rast(rast.list[18])
rast.t = rast(rast.list[19])
rast.u = rast(rast.list[20])
rast.v = rast(rast.list[21])
rast.w = rast(rast.list[22])
rast.x = rast(rast.list[23])
rast.y = rast(rast.list[24])
rast.z = rast(rast.list[25])
rast.aa = rast(rast.list[26])
rast.bb = rast(rast.list[27])
rast.cc = rast(rast.list[28])
rast.dd = rast(rast.list[29])
rast.ee = rast(rast.list[30])
rast.ff = rast(rast.list[31])
rast.gg = rast(rast.list[32])
rast.hh = rast(rast.list[33])
rast.ii = rast(rast.list[34])
rast.jj = rast(rast.list[35])
rast.kk = rast(rast.list[36])
rast.ll = rast(rast.list[37])

allfires = mosaic(rast.a,rast.b,rast.c,rast.d,rast.e,rast.f,rast.g,rast.h,
           rast.i,rast.j,rast.k,rast.l,rast.m,rast.n,rast.o,rast.p,
           rast.q,rast.r,rast.s,rast.t,rast.u,rast.v,rast.w,rast.x,
           rast.y,rast.z,rast.aa,rast.bb,rast.cc,rast.dd,rast.ee,rast.ff,
           rast.gg,rast.hh,rast.ii,rast.jj,rast.kk,rast.ll, fun = max)
           
plot(allfires)
class(allfires)

# writeRaster(allfires, here("data/spatial_data/inputs/sos_rdnbr_max_value_r_compiled.tif"))

#################################################################
##creating cbi layer with high severity extra split
#classify and turn each raster into polys
reclass_matrix <- tibble(
  from = c(-32769, 69, 315, 640, 800, 1100),
  to = c(69, 315, 640, 800, 1100, 9992),
  becomes = c(1, 2, 3, 4,5,6)
)

##first make classified rasters
for(i in 1:length(rast.list)){
  rast.1 = rast(rast.list[i])
  names(rast.1) = "rdnbr"
  imf <- rast.1 %>% 
    classify(reclass_matrix,
             right = T) # include "from" value in category
  writeRaster(imf, here(paste("data/spatial_data/inputs/fires_needed_rdnbr/classified_rasters_r/",gsub('.{10}$', '', as.character(rast.list.names[i])),"_CBI4_r.tif",sep = '')), overwrite = T)
  
}

#get list of now classified rasters
cls.rast.dir = here("data/spatial_data/inputs/fires_needed_rdnbr/classified_rasters_r")
cls.rast.list <- list.files(cls.rast.dir, pattern='tif$', full.names = T)
cls.rast.list.names <- list.files(cls.rast.dir, pattern='tif$', full.names = F)


##convert to polys
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
                                    burnsev == 4 ~ "High (low end - regen ok)",
                                    burnsev == 5 ~ "High (mod regen risk)",
                                    burnsev == 6 ~ "High (high regen risk)",
                                    .default = "oopsy"),
           year = paste(substr(as.character(rast.list.names[i]),1,4)),
           fireyr = paste(gsub('.{10}$', '', as.character(rast.list.names[i])))
    )
  write_sf(imf, here(paste("data/spatial_data/inputs/fires_needed_rdnbr/polys_r/",gsub('.{10}$', '', as.character(rast.list.names[i])),"_CBI4_polys_r.shp",sep = '')))
  
}


#merge polys
shapes.dir <- (here("data/spatial_data/inputs/fires_needed_rdnbr/polys_r"))
shapes.list <- list.files(shapes.dir, pattern="\\.shp$", full.names=TRUE)
shapes.all = lapply(shapes.list,st_read)

cbi_all_ks <- do.call(rbind, shapes.all)
st_write(cbi_all_ks, here("data/spatial_data/inputs/fires_needed_rdnbr/cbi_all_r_ks.shp"))


##compare mine and Bri's - the were the same! so its happening with the max value raster
cbi = st_read(here("data/spatial_data/inputs/fires_needed_rdnbr/cbi_all_r_ks.shp")) %>% 
  st_make_valid() %>%
  st_transform(crs(groves))
cbi_bri <- st_read(here("data/spatial_data/outputs/cbi_all_recalc.shp")) %>%
  st_make_valid() %>%
  st_transform(crs(groves))


options(scipen = 999)

cbi_grvs = cbi %>%
  st_intersection(groves) %>%
  mutate(area_ha = round(as.numeric(st_area(.)*0.0001),5)) %>%
  # select(-Acres,-Hectares)
  filter(burnsev %in% c(4,5,6)) 

cbi_bri_grvs = cbi_bri %>%
  st_intersection(groves) %>%
  mutate(area_ha = round(as.numeric(st_area(.)*0.0001),5)) %>%
  # select(-Acres,-Hectares)
  filter(burnsev %in% c(4,5,6)) 


# combine where there is buffer overlap and poorly drawn geometries
rgn_groves <- cbi_grvs %>% 
  mutate(dist_year = year, dist_type = brnsv_t) %>%
  summarise(.by = c(dist_year, dist_type, burnsev), 
            geometry = st_union(st_combine(geometry))) %>% 
  st_make_valid() %>% 
  mutate(dist_id = 1:length(dist_year))

rgn_groves$Hectares = as.numeric(st_area(rgn_groves)*0.0001)

rgn_groves_lookup <- rgn_groves %>% 
  st_drop_geometry()
head(rgn_groves_lookup)
# write.csv(dist_groves_lookup, here("outputs/fires_treatments_summarised_22May2025.csv"))

#total area of groves and buffers 
total_groveHa <- sum(as.numeric(st_area(groves)*0.0001))

##create disturbance history files
rgn_hist <- st_intersection(rgn_groves) %>% 
  mutate(resist_id = 1:length(origins)) %>% 
  select(resist_id, origins) %>% 
  st_collection_extract(type = c("POLYGON"), warn = FALSE) %>% #keep only polys
  #recombine multipolygons
  summarise(.by = c(resist_id, origins), geometry = st_combine(geometry)) %>% 
  mutate(area_ha = as.numeric(st_area(.))*0.0001)
#This removal makes sense, but there are also defo areas 
#with shifting boundaries, so we will keep them all
# %>% 
#   # remove areas less than 1 acre (likely "overlap" due to poorly mapped edges)
#   filter(area_ha >= 0.04) # NOTE: split this out and find hectares represented
head(rgn_hist)

rgn_hist_long_prep <- rgn_hist %>% 
  # st_drop_geometry() %>%  # attributes only
  unnest(origins) %>%  # make long dataframe
  rename(dist_id = origins) %>% 
  left_join(rgn_groves_lookup, by = join_by(dist_id)) %>% # connect with disturbance attributes
  select(-dist_id,-Hectares) %>%
  st_make_valid()
head(rgn_hist_long_prep)
# View(rgn_hist_long_prep)
st_write(rgn_hist_long_prep, here("data/spatial_data/inputs/fires_needed_rdnbr/cbi_unioned_for_repair.shp"))
####don't forget to repair it in arcpro!!

#repaired:
cbi_union_repair = st_read(here("data/spatial_data/inputs/fires_needed_rdnbr/cbi_unioned_for_repair.shp"))
head(cbi_union_repair)

n.occur = data.frame(table(cbi_union_repair$resist_id))
n.occur[n.occur$Freq > 1,]
n.occur2 = n.occur %>%
  filter(Freq > 1)
nrow(n.occur2)

rgn_cls1 <- cbi_union_repair %>% 
  # select(-unit_name,-grove_name) %>%
  arrange(dist_year) %>%
  rename(
    recent_dist_yr = dist_year,
    recent_dist_type = dist_type
  ) %>% 
  group_by(resist_id) %>%
  # identify next treatment where relevant
  # note: use lead if year = character, lag if year = numeric
  mutate(
    recent_dist_type = tolower(recent_dist_type),
    recent_dist_code = burnsev,
    prior1_dist_yr = lag(recent_dist_yr, 1),
    prior1_dist_type = lag(recent_dist_type, 1),
    prior1_dist_code = lag(burnsev, 1),
    max_dist_type = case_when(
      recent_dist_code > prior1_dist_code ~ recent_dist_type,
      recent_dist_code < prior1_dist_code ~ prior1_dist_type,
      recent_dist_code == prior1_dist_code ~ prior1_dist_type,
      is.na(prior1_dist_code) ~ recent_dist_type,
      .default = "oopsy")
    
  ) %>%
  ungroup() %>%
  arrange(resist_id)
rgn_cls1$area_ha2 = round(as.numeric(st_area(rgn_cls1)*0.0001),5)
head(rgn_cls1)
# View(rgn_cls1)

rgn_order_slice = rgn_cls1 %>%
  arrange(resist_id,recent_dist_yr) %>%
  group_by(resist_id) %>%
  slice_max(recent_dist_yr)

rgn_cls = rgn_order_slice %>%
  group_by(max_dist_type) %>%
  summarise(area_ha = sum(area_ha))
rgn_cls
sum(rgn_cls$area_ha)

#see if the deleted ones plus the repeat are the diff
sum(cbi_grvs$area_ha) - sum(rgn_cls$area_ha)
sum(cbi_bri_grvs$area_ha) - sum(cbi_grvs$area_ha)
head(rgn_order_slice)

#so the repeated high sev is 6.05 ha, which explains the diffs between the all high severity vs footprint of high severity
#also note that this is still 9 ha less than Bri, can only assume its because of the geometry issues + projection issues
#could triple check that by summing the area that was likely discarded, <0.9 or so??? - need to go back to before writing the shapefile and slice it
diff = rgn_order_slice %>% 
  filter(!is.na(prior1_dist_type))
sum(diff$area_ha)  
sum(cbi_grvs$area_ha) + sum(diff$area_ha)  

