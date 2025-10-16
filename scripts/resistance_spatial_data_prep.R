library(tidyverse)
library(terra)
library(sf)
library(here)
library(janitor)
library(scales)
library(dplyr)

options(scipen = 999)

#Input and clean treatment data

#fixed groves, layer made by Kristen NO BUFFER
groves <- st_read(here("data/spatial_data/inputs/forest/2023_GS_Groves_OTS_public.shp")) %>%
  clean_names() %>% 
  st_make_valid() %>%
  st_cast("MULTIPOLYGON") %>% 
  st_cast("POLYGON")

#fire history in CBI - from KS with the 6 classes for regen too
fire <- st_read(here("data/spatial_data/inputs/fires_needed_rdnbr/cbi_all_r_6class.shp")) %>% 
  st_transform(crs(groves)) %>%  #uniform crs
  st_make_valid() %>%
  mutate(id = fireyr) %>%
  st_cast("MULTIPOLYGON") %>% 
  st_cast("POLYGON")
head(fire)

# #fire history in CBI - Bri version
# fire <- st_read(here("data/spatial_data/outputs/cbi_all_recalc.shp")) %>% 
#   st_make_valid()
# 
#full, cleaned treatment layer (will this be the case?)
treatment <- st_read(here("data/spatial_data/outputs/SEGI_Trtdata/SEGI_Trtdata_12Apr25c.shp")) %>% 
  st_transform(crs(groves)) %>% 
  st_make_valid() %>%
  st_cast("MULTIPOLYGON") %>% 
  st_cast("POLYGON")
nrow(treatment)

#get rid of error in nps data - shows thin in goliath (475) 
#and burn in merced (518) that did not happen
trt.a = treatment %>%
  filter(rowid != 475&rowid != 518)
nrow(trt.a)

#get rid of duplicates where year, treatment and shape is same
trt <- trt.a %>%
  group_by(year,treatment,area_ha,geometry) %>%
  summarize(geometry = st_union(geometry)) %>%
  ungroup()
nrow(trt)

#get clean treatment layer
trt_clean <- trt %>% 
  clean_names() %>%
  rename(dist_year = year) %>% 
  # make categories fast to code
  mutate(
    dist_type = case_when(
      treatment == "Fire-related treatment" ~ "rx_pile",
      treatment == "Mechanical treatment" ~ "mech_under",
      T ~ "fix"), area_ha1 = as.numeric(st_area(.)*0.0001)
  ) %>% 
  group_by(dist_year, dist_type) %>% # select only needed columns
  summarise(area_ha = sum(area_ha1))
# %>%
#   st_cast("MULTIPOLYGON") %>% 
#   st_cast("POLYGON")


# create clean fire layer
fire_clean <- fire %>% 
  clean_names() %>% 
  #replace burnsev with class names, create cols to combine with trt
  mutate(
    dist_type = case_when(
      burnsev == 1 ~ "undetected_change",
      burnsev == 2 ~ "lowsev_fire",
      burnsev == 3 ~ "modsev_fire",
      burnsev == 4 ~ "highsev_fire",
      burnsev == 5 ~ "highsev_fire",
      burnsev == 6 ~ "highsev_fire"),
    dist_year = as.numeric(year)) %>%
  filter(dist_type != "undetected_change") %>%
  select(dist_year, dist_type)  #only needed columns 

#combine to into all disturbance layer
disturb <- fire_clean %>% 
  bind_rows(trt_clean)

# #reduce to within  groves
# grvs = st_union(groves)

#reduce disturbance to within  groves
dist_groves1 <- st_intersection(disturb, groves)#%>% 

# combine where there is buffer overlap and poorly drawn geometries
dist_groves <- dist_groves1 %>% 
  summarise(.by = c(dist_year, dist_type), 
            geometry = st_union(st_combine(geometry))) %>% 
  st_make_valid() %>% 
  mutate(dist_id = 1:length(dist_year))
# %>%
#   st_cast("MULTIPOLYGON") %>%
#   st_cast("POLYGON")
nrow(dist_groves)

dist_groves$Hectares = as.numeric(st_area(dist_groves)*0.0001)

dist_groves_lookup <- dist_groves %>% 
  st_drop_geometry()

# write.csv(dist_groves_lookup, here("outputs/fires_treatments_summarised_28May2025.csv"))

#total area of groves and buffers 
total_groveHa <- sum(as.numeric(st_area(groves)*0.0001))

##create disturbance history files
dist_hist <- st_intersection(dist_groves) %>% 
  mutate(resist_id = 1:length(origins)) %>% 
  select(resist_id, origins) %>% 
  st_collection_extract(type = c("POLYGON"), warn = FALSE) %>% #keep only polys
  #recombine multipolygons
  summarise(.by = c(resist_id, origins), geometry = st_combine(geometry)) %>%
  # st_cast("MULTIPOLYGON") %>%
  # st_cast("POLYGON") %>%
  mutate(area_ha = as.numeric(st_area(.))*0.0001)
head(dist_hist)

#weird that Hectares went away? Has been in code to remove in last line but with changes to polys seems to be gone
dist_hist_long <- dist_hist %>% 
  # st_drop_geometry() %>%  # attributes only
  unnest(origins) %>%  # make long dataframe
  rename(dist_id = origins) %>% 
  left_join(dist_groves_lookup, by = join_by(dist_id)) %>% # connect with disturbance attributes
  select(-dist_id) #%>%
  # st_cast("MULTIPOLYGON") %>%
  # st_cast("POLYGON")
  

###############################################################
###############################################################
###############################################################

#YOU HAVE TO DO THE REPAIR HERE IN ARCPRO OR THERE ARE PROBLEMS WITH DUPS LATER

###############################################################
###############################################################
###############################################################

##need to repair geometry in arcpro and bring back in
# write_sf(dist_hist_long_prep, here("data/spatial_data/outputs/dist_hist_long_8July2025_forUnion.shp"))

# dist_hist_long = read_sf(here("data/spatial_data/outputs/dist_hist_long_28May2025.shp"))

#query out polys that have high in any year (since the later summary doesn't go back thru all disturbances)
ever_high = dist_hist_long %>%
  filter(dist_type == "highsev_fire") %>%
  mutate(ever.high = "past high sev") %>%
  group_by(resist_id, ever.high) %>%
  summarise(area_ha = mean(area_ha)) %>%
  st_drop_geometry()
# View(ever_high)

n.occur = data.frame(table(ever_high$resist_id))
n.occur[n.occur$Freq > 1,]
n.occur2 = n.occur %>%
  filter(Freq > 1)
nrow(n.occur2)

dist_order <- dist_hist_long %>% 
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
    prior1_dist_yr = lag(recent_dist_yr, 1),
    prior1_dist_type = lag(recent_dist_type, 1),
    prior2_dist_yr = lag(recent_dist_yr, 2),
    prior2_dist_type = lag(recent_dist_type, 2),
    prior3_dist_yr = lag(recent_dist_yr, 3),
    prior3_dist_type = lag(recent_dist_type, 3)
  ) %>%
  ungroup() %>%
  arrange(resist_id)
# View(dist_order)
nrow(dist_order)
names(dist_order)
length(unique(dist_order$resist_id))


##get rid of the dup rows from the pivoting 
#(rows where max treat for year is not same as recent)
dist_order_slice = dist_order %>%
  arrange(resist_id,recent_dist_yr) %>%
  group_by(resist_id) %>%
  slice_max(recent_dist_yr)
nrow(dist_order_slice)
View(dist_order_slice)
length(unique(dist_order_slice$resist_id))

#explored in arcpro, these polys are bad geometry
#2204, 2244
# dist_not_flat = dist_order_slice %>%
#   filter(resist_id %in% c(2204,2244))
# dist_not_flat

n.occur = data.frame(table(dist_order_slice$resist_id
))
n.occur[n.occur$Freq > 1,]
n.occur2 = n.occur %>%
  filter(Freq > 1)
nrow(n.occur2)
# View(n.occur2)
#68 records appear 2x (136 rows extra in dist_order_slice?) -- 

#pull out the dup resist_ids into a list
dist_order_list_rpt = dist_order_slice %>%
  filter(recent_dist_yr==prior1_dist_yr) %>%
  select(resist_id) %>%
  mutate(reduce.list = 1) %>%
  # ##could not recall how to get rid of duplicate #s
  # group_by(resist_id) %>%
  # summarise(reduce.list = sum(reduce.list)) %>%
  mutate(dup = "dup") %>%
  st_drop_geometry()
# View(dist_order_list_rpt)
nrow(dist_order_list_rpt)

n.occur = data.frame(table(dist_order_list_rpt$resist_id
))
n.occur[n.occur$Freq > 1,]
n.occur2 = n.occur %>%
  filter(Freq > 1)
nrow(n.occur2)

#rejoin the list of dups to get a dataframe of just duplicates
dist_order_rpt1 = dist_order_slice %>% 
  left_join(dist_order_list_rpt) %>%
  #classify all as part of a dup or not
  mutate(dupdup = case_when(dup == "dup" ~ "dup",
                            .default = "not dup"),
         #create column to filter outthe trailing rows for the same resist_id
         keep = case_when(recent_dist_yr==prior1_dist_yr ~ "keep row",
                          recent_dist_yr!=prior1_dist_yr ~ "delete",
                          is.na(prior1_dist_yr) ~ "delete",
                          .default = "oopsy")
         ) %>%
  #get final dataframe of the duplicates and filter out trailing rows
  filter(dupdup == "dup" & keep == "keep row")

# & !is.na(prior1_dist_yr)
# View(dist_order_rpt1)
nrow(dist_order_rpt1)

dist_order_rpt = dist_order_rpt1 %>%
  # filter(recent_dist_yr==prior1_dist_yr) %>%
  mutate(recent_dist_type = case_when(
    ##they are just equal/duplicated?
    recent_dist_yr==prior1_dist_yr & recent_dist_type == prior1_dist_type  ~ recent_dist_type,
    
    ##when fire or rs is listed first and then mech_under is "prior"
    recent_dist_yr==prior1_dist_yr & (recent_dist_type %in% c("lowsev_fire", "modsev_fire","rx_pile") &
       (prior1_dist_type %in% c("mech_under"))) ~ "combo_fire_mech",
    
    ##when mech is listed first and then good fire is "prior"
    recent_dist_yr==prior1_dist_yr & (recent_dist_type %in% c("mech_under") & 
       (prior1_dist_type %in% c("lowsev_fire", "modsev_fire","rx_pile"))) 
    ~ "combo_fire_mech",
    
    #if any are high sev, use thatfire sev's then take max
    recent_dist_yr==prior1_dist_yr & (recent_dist_type %in% c("lowsev_fire", "modsev_fire","highsev_fire","rx_pile","mech_under") &
       (prior1_dist_type %in% c("highsev_fire"))) ~ "highsev_fire",
    #if fire sev's then take max
    recent_dist_yr==prior1_dist_yr & (recent_dist_type %in% c("highsev_fire") &
       (prior1_dist_type %in% c("lowsev_fire", "modsev_fire","highsev_fire","rx_pile","mech_under"))) ~ 
      "highsev_fire",
    #if fire sev's then take max - when mod is max
    recent_dist_yr==prior1_dist_yr & (recent_dist_type %in% c("lowsev_fire","rx_pile") &
       (prior1_dist_type %in% c("modsev_fire"))) ~ 
      "modsev_fire",
    recent_dist_yr==prior1_dist_yr & (recent_dist_type %in% c("modsev_fire") &
       (prior1_dist_type %in% c("lowsev_fire","rx_pile"))) ~ 
      "modsev_fire",
    recent_dist_yr==prior1_dist_yr & (recent_dist_type %in% c("lowsev_fire") &
       (prior1_dist_type %in% c("lowsev_fire","rx_pile"))) ~ 
      "lowsev_fire",
    recent_dist_yr==prior1_dist_yr & (recent_dist_type %in% c("lowsev_fire","rx_pile") &
       (prior1_dist_type %in% c("lowsev_fire"))) ~ 
      "lowsev_fire",
    recent_dist_yr==prior1_dist_yr & (recent_dist_type %in% c("modsev_fire") &
       (prior1_dist_type %in% c("lowsev_fire"))) ~ 
      "modsev_fire",
    # is.na(prior1_dist_type) ~ "disregard",
    .default = recent_dist_type),
    prior1_dist_yr = case_when(recent_dist_yr==prior1_dist_yr ~ prior2_dist_yr, .default = prior1_dist_yr),         
    prior1_dist_type = case_when(recent_dist_yr==prior1_dist_yr ~ prior2_dist_type, .default = prior1_dist_type),
    prior2_dist_yr = case_when(recent_dist_yr==prior1_dist_yr ~ prior3_dist_yr, .default = prior2_dist_yr),
    prior2_dist_type = case_when(recent_dist_yr==prior1_dist_yr ~ prior3_dist_type, .default = prior2_dist_type)) %>%
  select(-prior3_dist_yr,-prior3_dist_type) %>%
  filter(keep != "delete") %>%
  select(-dup,-dupdup,-keep)
nrow(dist_order_rpt)
# View(dist_order_rpt)

#create data frame of the non-duplicated resist_ids
dist_order_rptNO = dist_order_slice %>% 
  left_join(dist_order_list_rpt) %>%
  mutate(dupdup = case_when(dup == "dup" ~ "dup",
                            .default = "not dup")) %>%
  filter(dupdup == "not dup") %>%
  select(-dup,-dupdup,-prior3_dist_yr,-prior3_dist_type)

nrow(dist_order_rptNO)
# View(dist_order_rptNO)

#bind the cleaned up dups, non-dups and the table with ever high
all_dist_order = dist_order_rptNO %>%
  rbind(dist_order_rpt) %>%
  left_join(ever_high)
length(unique(all_dist_order$resist_id))
nrow(all_dist_order)
View(all_dist_order)

# write_sf(all_dist_order, here("data/spatial_data/outputs/all_dist_order_2320_4593.shp"))

#confirmed in arcpro that these dups are funky geometry, should be deleted
all_dist_order_cln = all_dist_order %>%
  filter(!resist_id %in% c(2289, 4568))
all_dist_order_cln
View(all_dist_order_cln)

#check for duplicates again 
n.occur = data.frame(table(all_dist_order_cln$resist_id))
n.occur[n.occur$Freq > 1,]
n.occur2 = n.occur %>%
  filter(Freq > 1)
nrow(n.occur2)

#create look up table
all_dist_order_lookup = all_dist_order_cln %>%
  st_drop_geometry()

dist_order_wide_summary_3trts = all_dist_order_cln %>%
  group_by(resist_id) %>%
  summarise(last_dist_yr = max(recent_dist_yr), last_dist_type = recent_dist_type) %>%
  left_join(all_dist_order_lookup) %>%
  mutate(prior1_dist_yr_0 = replace_na(prior1_dist_yr, 0),
         diff.1.to.2 = recent_dist_yr - prior1_dist_yr_0,
         area_ha = as.numeric(st_area(.)*0.0001)) 
# View(dist_order_wide_summary_3trts)
nrow(dist_order_wide_summary_3trts)

n.occur = data.frame(table(dist_order_wide_summary_3trts$resist_id
))
n.occur[n.occur$Freq > 1,]

write.csv(dist_order_wide_summary_3trts, here("outputs/dist_order_wide_summary_3trts_raw.csv"))

recent_only = dist_order_wide_summary_3trts %>%
  select(resist_id,recent_dist_yr,recent_dist_type,last_dist_yr) %>%
  mutate(dist_year = recent_dist_yr, dist_type = recent_dist_type) %>%
  select(-recent_dist_type,-recent_dist_yr)
head(recent_only)

second_only = dist_order_wide_summary_3trts %>%
  select(resist_id,prior1_dist_yr,prior1_dist_type,last_dist_yr) %>%
  mutate(dist_year = prior1_dist_yr, dist_type = prior1_dist_type) %>%
  select(-prior1_dist_type,-prior1_dist_yr) %>%
  filter(!is.na(dist_year))
head(second_only)

third_only = dist_order_wide_summary_3trts %>%
  select(resist_id,prior2_dist_yr,prior2_dist_type,last_dist_yr) %>%
  mutate(dist_year = prior2_dist_yr, dist_type = prior2_dist_type) %>%
  select(-prior2_dist_type,-prior2_dist_yr) %>%
  filter(!is.na(dist_year))
head(third_only)

reduced.data = dist_order_wide_summary_3trts %>%
  st_drop_geometry() %>%
  select(resist_id,last_dist_yr,area_ha,ever.high,diff.1.to.2, last_dist_type)

#
dist_hist_long_again = recent_only %>%
  bind_rows(second_only) %>% 
  bind_rows(third_only) %>%
  arrange(resist_id)
head(dist_hist_long_again)
nrow(dist_hist_long_again)
length(unique(dist_hist_long_again$resist_id))

dist_order_fix_fix = dist_hist_long_again %>%
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
    prior1_dist_yr = lag(recent_dist_yr, 1),
    prior1_dist_type = lag(recent_dist_type, 1),
    prior2_dist_yr = lag(recent_dist_yr, 2),
    prior2_dist_type = lag(recent_dist_type, 2),
    prior3_dist_yr = lag(recent_dist_yr, 3),
    prior3_dist_type = lag(recent_dist_type, 3)
  ) %>%
  ungroup() %>%
  arrange(resist_id)

dist_tallies = dist_order_fix_fix %>%
  mutate(TRUES=1) %>%
  arrange(recent_dist_type) %>%
  pivot_wider(names_from = recent_dist_type, values_from = TRUES, values_fill = 0) %>%
  group_by(resist_id) %>%
  summarise(across(c(combo_fire_mech:rx_pile), sum)) %>%
  left_join(reduced.data) %>%
  # st_cast("MULTIPOLYGON") %>%
  # st_cast("POLYGON") %>%
  mutate(ever.high = case_when(is.na(ever.high) ~ "never high sev",
                               .default = ever.high),
         area_ha = as.numeric(st_area(.)*0.0001))
head(dist_tallies)
sum(dist_tallies$area_ha)
write_sf(dist_tallies, here("data/spatial_data/outputs/dist_order_wide_summary_3trts_tallied.shp"))

dist_tallies_tbl = st_drop_geometry(dist_tallies)
head(dist_tallies_tbl)
sum(dist_tallies_tbl$area_ha)

n.occur = data.frame(table(dist_tallies$resist_id))
n.occur[n.occur$Freq > 1,]

write.csv(dist_tallies_tbl, here("outputs/dist_order_wide_summary_3trts_tallied.csv"))


###########################################################################################
###########################################################################################

# classify the resistance

###########################################################################################
###########################################################################################

snapshot.resist = dist_tallies %>%
  mutate(resist_class =
           case_when(
             #High resistance: mod or low or rx + any mech, mod or low or rx + mod or low or rx
             (combo_fire_mech >= 1 & last_dist_yr>=2010) & ever.high == "never high sev" ~
               'High resistance',
             (mech_under + rx_pile + modsev_fire + lowsev_fire) >= 2 
             & (rx_pile + modsev_fire + lowsev_fire)>=1  & ever.high == "never high sev" 
             & diff.1.to.2 <=15 & last_dist_yr>=2010 ~ 
               'High resistance',
             ###had two trts, but they are too far apart, base classification on last_dist
             (mech_under + rx_pile + modsev_fire + lowsev_fire) >= 2 
             & (rx_pile + modsev_fire + lowsev_fire)>=1  & ever.high == "never high sev" 
             & diff.1.to.2 > 15 & last_dist_yr>=2010 &
               last_dist_type %in% c("rx_pile","modsev_fire","lowsev_fire") ~ 
               'Moderate resistance',
             ###had two trts, but they are too far apart, base classification on last_dist (mech)
             (mech_under + rx_pile + modsev_fire + lowsev_fire) >= 2 
             & (rx_pile + modsev_fire + lowsev_fire)>=1  & ever.high == "never high sev" 
             & diff.1.to.2 > 15 & last_dist_yr>=2010 &
               last_dist_type == "mech_under" ~ 
               'Low resistance',
             #Mod resistance: mod or low or rx
             (rx_pile + modsev_fire + lowsev_fire == 1 | 
                (rx_pile + modsev_fire + lowsev_fire > 1 & diff.1.to.2 > 15))
             &   mech_under == 0 & ever.high == "never high sev" 
             & last_dist_yr>=2010 ~ 
               'Moderate resistance',
             (rx_pile + modsev_fire + lowsev_fire) >= 2 
             & (rx_pile + modsev_fire + lowsev_fire)>=1  & ever.high == "never high sev" 
             & mech_under == 0 & diff.1.to.2 >15 & last_dist_yr>=2010 ~ 
               'Moderate resistance',
             #Low resistance: any mech
             mech_under >= 1 
             & (rx_pile + modsev_fire + lowsev_fire == 0 |
                  (rx_pile + modsev_fire + lowsev_fire > 0 & diff.1.to.2>15))
             & last_dist_yr>=2010 & ever.high == "never high sev" 
             ~ "Low resistance",
             #Loss of mature forest: any high
             # highsev_fire >= 1 ~
             # "Loss of mature forest",
             ever.high == "past high sev" ~ 
               "Loss of mature forest",
             (ever.high == "never high sev") & last_dist_yr<2010 ~ 
               "No resistance",
             .default = "oopsy")
  ) %>%
  mutate(area_ha_p = as.numeric(st_area(.)*0.0001)) %>%
  group_by(resist_class) %>%
  summarise(area_ha = sum(area_ha_p)) %>%
  st_make_valid() %>%
  st_cast("MULTIPOLYGON") %>%
  st_cast("POLYGON") %>%
  mutate(area_ha = as.numeric(st_area(.)*0.0001))
head(snapshot.resist)
sum(snapshot.resist$area_ha)

sr = snapshot.resist %>%
  group_by(resist_class) %>%
  summarise(area_ha = sum(area_ha))
sr
####################################################

####find the area with no resistance spatially, then get percentages by grove

####################################################


no_resist <- st_difference(groves, st_union(snapshot.resist)) %>%
  st_as_sf() %>%
  mutate(area_ha = as.numeric(st_area(.)*0.0001), resist_class = "No resistance") %>%
  group_by(resist_class) %>%
  summarise(area_ha = sum(area_ha)) %>%
  mutate(area_ha = as.numeric(st_area(.)*0.0001))
no_resist

sum(no_resist$area_ha)
sum(snapshot.resist$area_ha) + sum(no_resist$area_ha)

# 
resist_polys_summarized = snapshot.resist %>%
  rbind(no_resist) %>%
  group_by(resist_class) %>%
  summarise(area_ha = sum(area_ha)) %>%
  mutate(dist_id = 1:length(resist_class))
sum(resist_polys_summarized$area_ha)


##re-intersect to get rid ov overlaps
##################################
###Roughly 3 ha of polygons did not "flatten"/combine with st_intersection as expected, leaving overlapping
###polygons. This code re-intersects them, and then chooses the most "optomistic" assignment for that polygon

resist_polys_all_lookup <-resist_polys_summarized %>% 
  st_drop_geometry()

##re-intersect to resolve few polys that did not "flatten" and overlap
resist_polys_all_2nd_int <- st_intersection(resist_polys_summarized) %>% 
  mutate(resist_id = 1:length(origins)) %>%
  # select(resist_id, origins) %>%
  st_collection_extract(type = c("POLYGON"), warn = FALSE) %>% #keep only polys
  #recombine multipolygons
  summarise(.by = c(resist_id, origins), geometry = st_combine(geometry)) %>%
  mutate(area_ha = as.numeric(st_area(.))*0.0001)
# View(resist_polys_all_2nd_int)

resist_polys_all_2nd_int_long <- resist_polys_all_2nd_int %>% 
  # st_drop_geometry() %>%  # attributes only
  unnest(origins) %>%  # make long dataframe
  rename(dist_id = origins) %>% 
  left_join(resist_polys_all_lookup, by = join_by(dist_id)) %>% # connect with resisturbance attributes
  # select(-dist_id,-area_ha.x,-area_ha.y) %>%
  mutate(area_ha = as.numeric(st_area(.))*0.0001)
# %>%
#   filter(area_ha > 0.000001)
resist_polys_all_2nd_int_long
sum(resist_polys_all_2nd_int_long$area_ha)

##Give ranks to the regen status types so that where there is conflict keep min of those when grouped by resist_id
resist_polys_all = resist_polys_all_2nd_int_long %>%
  # select(-area_ha.x,-area_ha.y) %>%
  mutate(rank = case_when(resist_class == "High resistance" ~ 5,
                          resist_class == "Moderate resistance" ~ 4,
                          resist_class == "Low resistance" ~ 3,
                          resist_class == "No resistance" ~ 2,
                          resist_class == "Loss of mature forest" ~ 1
  )) %>%
  # mutate(area_ha = as.numeric(st_area(.))*0.0001) %>%
  group_by(resist_id) %>%
  slice_min(rank) %>%
  group_by(resist_class) %>%
  summarise(area_ha = sum(area_ha))
resist_polys_all
# View(resist_polys_all)
sum(resist_polys_all$area_ha)

st_write(resist_polys_summarized,here("data/spatial_data/outputs/resistance_snapshot_15yrs_summarized.shp"))

resist_polys_summarized_tbl = st_drop_geometry(resist_polys_all) %>%
  mutate(perc_area = (area_ha/10132.88)*100,
         rank = case_when(
           resist_class == "High resistance" ~ 5, 
           resist_class == "Moderate resistance" ~ 4, 
           resist_class == "Low resistance" ~ 3, 
           resist_class == "No resistance" ~ 2, # CHANGE COLOR
           resist_class == "Loss of mature forest" ~ 1,
           .default = -999))
write.csv(resist_polys_summarized_tbl, here("outputs/resistance_snapshot_15yrs_summary_classes.csv"))

############################
#create figure
ggplot(resist_polys_summarized_tbl, aes(x = reorder(resist_class, -rank), y = perc_area, 
                                fill = factor(resist_class, 
                                              levels = c("High resistance", 
                                                         "Moderate resistance", "Low resistance", 
                                                         "No resistance", 
                                                         "Loss of mature forest")))) +
  geom_bar(stat="identity") +
  scale_fill_manual(values = c("#005557", "#09AEA9","#4ADBCC","#FF7E15","#F52605")) +
  scale_x_discrete(
    labels = c("High resistance\n(21%)", "Moderate resistance\n(43%)", 
               "Low resistance\n(3%)", 
               "No resistance\n(15%)", "Mature forest loss\n(18%)")) +     
  ylab("Percent of range") + xlab("") +
  theme_bw()+
  coord_flip()+
  theme(legend.title=element_blank(),
        legend.position = "none",
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), panel.background = element_blank(), 
        # plot.background = element_rect(color = "black", linewidth = 0.5),
        axis.line = element_line(colour = "black"),
        axis.text.y = element_text(size = 14),
        axis.text.x = element_text(size = 12)) +
  # scale_y_continuous(limits = c(0,43), expand = c(0, 0)) +
  scale_y_continuous(expand = c(0.001, 0), limits = c(0,45))

ggsave(here("outputs/figures_for_manuscript/resistance_classes_snapshot_percent_15yrs_30June2025_matchColors.jpg"),
       width = 7, height = 5)


#############################################################
###intersect with groves

groves.redu = groves %>%
  select(grove_name)
# 
# #try to bring in a repaired file for the next step?
# resist_polys_all = st_read(here("data/spatial_data/outputs/resistance_snapshot_15yrs_summarized.shp")) %>%
#   mutate(resist_class = rsst_cl)


##get the percent per grove 
resist_grvs1 = st_intersection(groves.redu,resist_polys_all) %>%
  st_make_valid() %>%
  st_collection_extract(type = c("POLYGON"), warn = FALSE) %>% #keep only polys
  # st_cast("MULTIPOLYGON") %>%
  # st_cast("POLYGON") %>%
  mutate(area_ha2 = as.numeric(st_area(.)*0.0001)) %>%
  group_by(grove_name, resist_class) %>%
  summarise(resist_ha = sum(area_ha2)) 
head(resist_grvs1)
sum(resist_grvs1$resist_ha)

#get groves size from resist polys (is slightly larger, makes the percents work)
grv.size = resist_grvs1 %>%
  group_by(grove_name) %>%
  summarise(grv_ha = sum(resist_ha)) %>%
  st_drop_geometry() %>%
  # mutate(grv_ha = Acres/2.471) %>%
  select(grove_name, grv_ha)
sum(grv.size$grv_ha)
head(grv.size)

resist_grvs2 = resist_grvs1 %>%  
  left_join(grv.size) %>%
  st_drop_geometry()
sum(resist_grvs2$resist_ha)
# View(resist_grvs2)

resist_grvs = resist_grvs2 %>%
  st_drop_geometry() %>%
  mutate(perc.grv = round(resist_ha/grv_ha,3)*100) %>%
  select(grove_name, perc.grv, resist_class) %>%
  # group_by(GroveName) %>%
  pivot_wider(names_from = resist_class, values_from = perc.grv, values_fill = 0) %>%
  left_join(grv.size) %>%
  clean_names() %>%
  select(grove_name, grv_ha, high_resistance, moderate_resistance,
         low_resistance, no_resistance, loss_of_mature_forest) %>%
  mutate(total = high_resistance + moderate_resistance +
           low_resistance + no_resistance + loss_of_mature_forest)

head(resist_grvs)

write.csv(resist_grvs, here("outputs/resistance_percentages_by_grove.csv"))
