library(tidyverse)
library(terra)
library(sf)
library(here)
library(janitor)
library(scales)

options(scipen = 999)

#Input and clean treatment data

#fire history in CBI
fire <- st_read(here("data/spatial_data/outputs/cbi_all_updated_wRedwoodFire_02Feb25/cbi_all.shp")) %>% 
  st_make_valid()


#full, cleaned treatment layer (will this be the case?)
treatment <- st_read(here("data/spatial_data/outputs/SEGI_Trtdata/SEGI_Trtdata_12Apr25c.shp")) %>% 
  st_transform(crs(fire)) %>% 
  st_make_valid() %>%
  mutate(area_ha = round(as.numeric(st_area(.)*0.0001)))
sum(treatment$area_ha)

#fixed groves, layer made by Kristen NO BUFFER
groves <- st_read(here("data/spatial_data/inputs/forest/FINAL_SEGI_AnalysisArea_wOwnerships_GroveNames_noBuff/FINAL_SEGI_AnalysisArea_wOwnerships_GroveNames_noBuff.shp"))%>%
  clean_names() %>% 
  st_transform(crs(fire)) %>%  #uniform crs
  st_make_valid()

##
trt_owners = treatment %>%
  mutate(landowner = case_when(
    source == "blm_treatmentfiles_2020_2024" ~ "Bureau of Land Management",
    source == "calaveras_all_treatments_2013_2024" ~ "California State Parks",
    source == "calfire_thp_all_sosegi" ~ "Private",
    source == "mhdsf_1951_2019_sosegi" ~ "Mountain Home Demonstration State Forest",
    source == "nps_1966_2024" ~ "National Park Service",
    source == "srl_alderonly_trts" ~ "Nonprofit",
    source == "ucb_sosegi_2001_2024" ~ "UC Berkeley Center for Forestry",
    source == "usfs_alltrtsources_thru2024" ~ "US Forest Service",
    source == "usfs_bigstumpaddition_2024" ~ "US Forest Service",
    .default = "oopsy"))

trt_clean <- trt_owners %>% 
  clean_names() %>%
  rename(dist_year = year) %>% 
  # make categories fast to code
  mutate(
    dist_type = case_when(
      treatment == "Fire-related treatment" ~ "rx_pile",
      treatment == "Mechanical treatment" ~ "mech_under",
      T ~ "fix"),
  ) %>% 
  select(dist_year, dist_type, landowner)
trt_clean$hectares = as.numeric(st_area(trt_clean)*0.0001)
head(trt_clean)

trt_clean_groves = trt_clean %>%
  st_intersection(groves) %>%
  select(-acres,-hectares.1) %>% 
  mutate(hectares = round(as.numeric(st_area(.)*0.0001),1)) %>%
  st_drop_geometry() %>%
  select(dist_year,dist_type,grove_name,unit_name,landowner,hectares)

head(trt_clean_groves)
names(trt_clean_groves)
# write.csv()

# create clean fire layer
groves = groves %>%
  mutate(landowner = case_when(
    unit_name %in% c("Sequoia and Kings Canyon National Parks",
                     "Yosemite National Park")
    ~ "National Park Service",
    unit_name == "Save the Redwoods League" ~ "Nonprofit",
    unit_name == "Whittaker's Forest" ~ "UC Berkeley Center for Forestry",
    unit_name %in% c("Giant Sequoia National Monument",
                     "Sierra National Forest","Tahoe National Forest")
    ~ "US Forest Service",
    unit_name == "Tulare County" ~ "County",
    .default = unit_name))

fire_groves1 <- st_intersection(fire, groves)#%>%
# combine where there is buffer overlap and poorly drawn geometries

fire_groves <- fire_groves1 %>%
  summarise(.by = c(fire_yr, id, burnsev, unit_name, grove_name),
            geometry = st_union(st_combine(geometry))) %>%
  st_make_valid() %>%
  mutate(dist_id = 1:length(fire_yr))

fire_groves$hectares = round(as.numeric((st_area(fire_groves)/10000)),1)
head(fire_groves)
# fire_groves_lookup <- fire_groves %>%
#   st_drop_geometry()

# fire_groves = fire_groves %>%
#   mutate(landowner = case_when(
#     unit_name %in% c("Sequoia and Kings Canyon National Parks",
#                      "Yosemite National Park")
#     ~ "National Park Service",
#     unit_name == "Save the Redwoods League" ~ "Nonprofit",
#     unit_name == "Whittaker's Forest" ~ "UC Berkeley Center for Forestry",
#     unit_name %in% c("Giant Sequoia National Monument",
#                      "Sierra National Forest","Tahoe National Forest")
#     ~ "US Forest Service",
#     unit_name == "Tulare County" ~ "County",
#     .default = unit_name))
# 
fire_clean_groves <- fire_groves %>%
  clean_names() %>%
  st_drop_geometry() %>%
  # filter(burnsev != 1) %>%
  #replace burnsev with class names, create cols to combine with trt
  mutate(
    dist_type = case_when(
      burnsev == 1 ~ "undetected_change",
      burnsev == 2 ~ "lowsev_fire",
      burnsev == 3 ~ "modsev_fire",
      burnsev == 4 ~ "highsev_fire"),
    dist_year = as.numeric(fire_yr)) %>%
  group_by(dist_year, dist_type, landowner, unit_name, grove_name) %>% #only needed columns
  summarise(hectares = round(sum(hectares),1)) %>%
  ungroup() %>%
  select(dist_year,dist_type,grove_name,unit_name,landowner,hectares)

head(fire_clean_groves)
names(fire_clean_groves)

all_disturb_groves = fire_clean_groves %>%
  bind_rows(trt_clean_groves) %>%
  mutate(dist_group = case_when(
    dist_type %in% c("mech_under","rx_pile") ~ "Active treatment",
                     .default = "Wildfire"))
head(all_disturb_groves)

write.csv(all_disturb_groves, here("outputs/fire_and_treatment_wGroves_Owners_12May2025.csv"))

#################################
##redo to get FOOTPRINT of area treated thru time
#need to redo this to keep geometry
trt_clean_groves_foot = trt_clean %>%
  st_intersection(groves) %>%
  select(-acres,-hectares.1) %>% 
  mutate(hectares = round(as.numeric(st_area(.)*0.0001),1)) %>%
  select(dist_year,dist_type,grove_name,unit_name,landowner,hectares)

#first dissolve to one footprint of treated area
trt_clean_groves_footprint_diss = trt_clean_groves_foot %>%
  st_union() 
#intersect it with groves fil to get ownership
trt_clean_groves_footprint_all1 = groves %>%
  st_intersection(trt_clean_groves_footprint_diss)
trt_clean_groves_footprint_all1$area_ha = as.numeric(st_area(trt_clean_groves_footprint_all1)*0.0001)

#summarise by owner
trt_clean_groves_footprint_all = trt_clean_groves_footprint_all1 %>%
  group_by(landowner) %>%
  summarise(area_ha = sum(area_ha))
trt_clean_groves_footprint_all
sum(trt_clean_groves_footprint_all$area_ha)

###############################################
#summarise by owner and disturbance type
#first dissolve to one footprint of treated area
##fire
trt_clean_groves_footprint_diss_fire = trt_clean_groves_foot %>%
  filter(dist_type == "rx_pile") %>%
  st_union() 
#intersect it with groves fil to get ownership
trt_clean_groves_footprint_all_fire1 = groves %>%
  st_intersection(trt_clean_groves_footprint_diss_fire) 
trt_clean_groves_footprint_all_fire1$area_ha = as.numeric(st_area(trt_clean_groves_footprint_all_fire1)*0.0001)
trt_clean_groves_footprint_all_fire = trt_clean_groves_footprint_all_fire1 %>%
  group_by(landowner) %>%
  summarise(area_ha = sum(area_ha))

##mech
trt_clean_groves_footprint_diss_mech = trt_clean_groves_foot %>%
  filter(dist_type == "mech_under" & dist_year >=1966) %>%
  st_union() 
#intersect it with groves fil to get ownership
trt_clean_groves_footprint_all_mech1 = groves %>%
  st_intersection(trt_clean_groves_footprint_diss_mech) 
trt_clean_groves_footprint_all_mech1$area_ha = as.numeric(st_area(trt_clean_groves_footprint_all_mech1)*0.0001)
trt_clean_groves_footprint_all_mech = trt_clean_groves_footprint_all_mech1 %>%
  group_by(landowner) %>%
  summarise(area_ha = sum(area_ha))
trt_clean_groves_footprint_all_mech

###########################################
##calc area managed by diff owners
groves_lo = groves %>%
  mutate(landowner = case_when(
    unit_name %in% c("Sequoia and Kings Canyon National Parks",
                     "Yosemite National Park")
    ~ "National Park Service",
    unit_name == "Save the Redwoods League" ~ "Nonprofit",
    unit_name == "Whittaker's Forest" ~ "UC Berkeley Center for Forestry",
    unit_name %in% c("Giant Sequoia National Monument",
                     "Sierra National Forest","Tahoe National Forest")
    ~ "US Forest Service",
    unit_name == "Tulare County" ~ "County",
    .default = unit_name),
    area_ha = round(as.numeric(st_area(.)*0.0001),1)) %>%
  st_drop_geometry() %>%
  select(landowner, area_ha) %>%
  group_by(landowner) %>%
  summarise(hectares = sum(area_ha))
groves_lo

write.csv(groves_lo,here("outputs/grove_total_area_managed_by_agency.csv"))
