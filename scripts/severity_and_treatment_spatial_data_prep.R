library(tidyverse)
library(terra)
library(sf)
library(here)
library(janitor)
library(scales)

options(scipen = 999)

#Input and clean treatment data

#fixed groves, layer made by Kristen NO BUFFER
groves <- st_read(here("data/spatial_data/inputs/forest/FINAL_SEGI_AnalysisArea_wOwnerships_GroveNames_noBuff/FINAL_SEGI_AnalysisArea_wOwnerships_GroveNames_noBuff.shp"))%>%
  clean_names() %>% 
  st_make_valid()

#fire perimeters
perimeters <- st_read("data/spatial_data/inputs/fire/fire23_1.gdb", layer = "firep23_1") %>% 
  clean_names()  %>%
  st_transform(crs(groves))%>%
  st_cast("MULTIPOLYGON")

#fire history in CBI - from KS with the 6 classes for regen too
fire <- st_read(here("data/spatial_data/inputs/fires_needed_rdnbr/cbi_all_r_ks.shp")) %>% 
  st_transform(crs(groves)) %>%  #uniform crs
  st_make_valid() %>%
  mutate(id = fireyr)
head(fire)

#Bri version
# fire <- st_read(here("data/spatial_data/outputs/cbi_all_recalc.shp")) %>% 
#   st_transform(crs(groves)) %>%  #uniform crs
#   st_make_valid()


#full, cleaned treatment layer (will this be the case?)
treatment <- st_read(here("data/spatial_data/outputs/SEGI_Trtdata/SEGI_Trtdata_12Apr25c.shp")) %>% 
  st_transform(crs(groves)) %>% 
  st_make_valid() %>%
  mutate(area_ha = round(as.numeric(st_area(.)*0.0001)))
sum(treatment$area_ha)


##change landowner names
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
  select(dist_year,dist_type,grove_name,unit_name,landowner,hectares)

trt_clean_groves_tbl = trt_clean_groves %>%
  st_drop_geometry()
head(trt_clean_groves_tbl)
tapply(trt_clean_groves_tbl$hectares,trt_clean_groves_tbl$year,)

trt_clean_groves_tbl_sum = trt_clean_groves_tbl %>%
  group_by(dist_year,dist_type) %>%
  summarise(area_ha = sum(hectares))
# View(trt_clean_groves_tbl_sum)

names(trt_clean_groves)
# write.csv()

########################################################
########################################################
##FIRE

#calfire perimeters, 1910-2024
perims = perimeters %>%
  st_intersection(groves) %>%
  mutate(area_ha = round(as.numeric(st_area(.)*0.0001),1)) %>%
  select(year,fire_name,unit_name,grove_name,area_ha)
perims

#get footprint area
perims.foot = perims %>%
  st_cast("MULTIPOLYGON") %>%
  st_union()
perims.foot$area_ha = round(as.numeric(st_area(perims.foot)*0.0001),1)
perims.foot


#create area burned through time table
perims_table = st_drop_geometry(perims) %>%
  mutate(fire_name = case_when(
    fire_name == " " ~ "unnamed",
    .default = fire_name
  )) %>%
  group_by(year, fire_name) %>%
  summarise(area_ha = sum(area_ha))
# write.csv(perims_table, here("outputs/calfire_perims_area_burned_by_fire_1910_2024.csv"))

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
  summarise(.by = c(year, id, burnsev, unit_name, grove_name),
            geometry = st_union(st_combine(geometry))) %>%
  st_make_valid() %>%
  mutate(dist_id = 1:length(year))

fire_groves$hectares = round(as.numeric((st_area(fire_groves)/10000)),1)
head(fire_groves)
fire_groves_lookup <- fire_groves %>%
  st_drop_geometry()

fire_groves = fire_groves %>%
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
#for later processing
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
      #high severity is divided into three classes for the regen assessment, here is treated the same
      burnsev == 4 ~ "highsev_fire",
      burnsev == 5 ~ "highsev_fire",
      burnsev == 6 ~ "highsev_fire"),
    dist_year = as.numeric(year)) %>%
  group_by(dist_year, dist_type, landowner, unit_name, grove_name) %>% #only needed columns
  summarise(hectares = round(sum(hectares),1)) %>%
  ungroup() %>%
  select(dist_year,dist_type,grove_name,unit_name,landowner,hectares)

head(fire_clean_groves)
names(fire_clean_groves)

#to make appendix table
fire_clean_groves_tbl <- fire_groves %>%
  clean_names() %>%
  st_drop_geometry() %>%
  # filter(burnsev != 1) %>%
  #replace burnsev with class names, create cols to combine with trt
  mutate(
    dist_type = case_when(
      burnsev == 1 ~ "undetected_change",
      burnsev == 2 ~ "lowsev_fire",
      burnsev == 3 ~ "modsev_fire",
      #high severity is divided into three classes for the regen assessment, here is treated the same
      burnsev == 4 ~ "highsev_fire",
      burnsev == 5 ~ "highsev_fire",
      burnsev == 6 ~ "highsev_fire"),
    dist_year = as.numeric(year)) %>%
  group_by(dist_year, dist_type, grove_name, id) %>% #only needed columns
  summarise(hectares = round(sum(hectares),1)) %>%
  ungroup() %>%
  mutate(fire.name = str_to_sentence(gsub("^.{0,4}", "", id))) %>%
  select(dist_year,dist_type,grove_name,hectares,id,fire.name) %>%
  mutate(severity = case_when(dist_type == "undetected_change" ~ "Undetected change",
                              dist_type == "lowsev_fire" ~ "Low",
                              dist_type == "modsev_fire" ~ "Moderate",
                              dist_type == "highsev_fire" ~ "High")) %>%
  select(-dist_type,-id) %>%
  pivot_wider(names_from = severity, values_from = hectares, values_fill = 0)
fire_clean_groves_tbl

write.csv(fire_clean_groves_tbl, here("outputs/wildfire_severity_by_grove_firename_24June2025.csv"))

#get the groves with the most percents of high severity in 2020 and 2021
groves$hectares = round(as.numeric(st_area(groves)*0.0001))
grv.size = groves %>%
  group_by(grove_name) %>%
  summarise(grv.ha = sum(hectares))

grvs.sev.2020.2021 = fire_clean_groves_tbl %>%
  filter(dist_year %in% c(2020,2021)) %>%
  left_join(grv.size) %>%
  mutate(perc.hi = High/grv.ha) %>%
  select(dist_year,fire.name,grove_name, High, perc.hi, grv.ha)
grvs.sev.2020.2021
View(grvs.sev.2020.2021)

all_disturb_groves = fire_clean_groves %>%
  bind_rows(trt_clean_groves_tbl) %>%
  mutate(dist_group = case_when(
    dist_type %in% c("mech_under","rx_pile") ~ "Active treatment",
                     .default = "Wildfire"))
head(all_disturb_groves)

write.csv(all_disturb_groves, here("outputs/fire_and_treatment_wGroves_Owners_22May2025.csv"))

#################################
##redo to get FOOTPRINT of area treated thru time
#overall
#al ha treated
sum(trt_clean_groves$hectares)

##dissolve
trt_clean_groves_foot_all = st_intersection(trt_clean_groves) %>%
  summarise(geometry = st_union(st_combine(geometry))) %>% 
  st_make_valid()
trt_clean_groves_foot_all$Hectares = as.numeric(st_area(trt_clean_groves_foot_all)*0.0001)
trt_clean_groves_foot_all


#by type
trt_clean_groves_foot_type = trt_clean_groves %>%
  summarise(.by = c(dist_type), 
            geometry = st_union(st_combine(geometry))) %>% 
  st_make_valid() %>% 
  group_by(dist_type) %>%
  mutate(dist_id = 1:length(dist_type))
trt_clean_groves_foot_type$Hectares = as.numeric(st_area(trt_clean_groves_foot_type)*0.0001)
trt_clean_groves_foot_type

#by type and landowner
trt_clean_groves_foot_type_lo = trt_clean_groves %>%
  summarise(.by = c(dist_type, landowner), 
            geometry = st_union(st_combine(geometry))) %>% 
  st_make_valid()
trt_clean_groves_foot_type_lo$Hectares = as.numeric(st_area(trt_clean_groves_foot_type_lo)*0.0001)
View(trt_clean_groves_foot_type_lo)

#by landowner
trt_clean_groves_foot_lo = trt_clean_groves %>%
  summarise(.by = c(landowner), 
            geometry = st_union(st_combine(geometry))) %>% 
  st_make_valid()
trt_clean_groves_foot_lo$Hectares = as.numeric(st_area(trt_clean_groves_foot_lo)*0.0001)
trt_clean_groves_foot_lo


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


