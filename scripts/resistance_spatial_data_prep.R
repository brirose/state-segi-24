library(tidyverse)
library(terra)
library(sf)
library(here)
library(janitor)
library(scales)

options(scipen = 999)

#Input and clean treatment data

#fixed groves, layer made by Kristen NO BUFFER
groves <- st_read(here("data/spatial_data/inputs/forest/2023_GS_Groves_OTS_public.shp")) %>%
  clean_names() %>% 
  st_make_valid()

#fire history in CBI - from KS with the 6 classes for regen too
fire <- st_read(here("data/spatial_data/inputs/fires_needed_rdnbr/cbi_all_r_ks.shp")) %>% 
  st_transform(crs(groves)) %>%  #uniform crs
  st_make_valid() %>%
  mutate(id = fireyr)
head(fire)

# #fire history in CBI - Bri version
# fire <- st_read(here("data/spatial_data/outputs/cbi_all_recalc.shp")) %>% 
#   st_make_valid()
# 
#full, cleaned treatment layer (will this be the case?)
treatment <- st_read(here("data/spatial_data/outputs/SEGI_Trtdata/SEGI_Trtdata_12Apr25c.shp")) %>% 
  st_transform(crs(groves)) %>% 
  st_make_valid()
nrow(treatment)

#get rid of error in nps data
trt.a = treatment %>%
  filter(rowid != 475)
nrow(trt.a)

#get rid of duplicates where year, treatment and shape is same
trt <- trt.a %>%
  group_by(year,treatment,area_ha,geometry) %>%
  summarize(geometry = st_union(geometry)) %>%
  ungroup()
nrow(trt)

#get celan treatment layer
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

###############################
##DON'T THINK WE NEED THIS ANYMORE CUZ IT WAS DOING TH
# #reduce to within  groves
# grvs = st_union(groves)
# 
# trt_groves1 <- st_intersection(trt_clean, grvs)
# 
# trt_groves2 = trt_groves1 %>%
#   st_intersection()
# nrow(trt_groves2)
# 
# trt_groves3 <- trt_groves2 %>%
#   summarise(.by = c(dist_year, dist_type),
#             geometry = st_union(st_combine(geometry))) %>%
#   st_make_valid()
# 
# dist_hist <- st_intersection(trt_groves3) %>%
#   mutate(resist_id = 1:length(origins)) %>%
#   select(resist_id, origins) %>%
#   st_collection_extract(type = c("POLYGON"), warn = FALSE) %>% #keep only polys
#   #recombine multipolygons
#   summarise(.by = c(resist_id, origins), geometry = st_combine(geometry)) %>%
#   mutate(area_ha = as.numeric(st_area(.))*0.0001)
# 
# 
# n.occur = data.frame(table(dist_hist$resist_id
# ))
# n.occur[n.occur$Freq > 1,]
# 
# ##note: unable to make valid in R, used ArcPro to repair geometry, resulting in XX removed for NULL geometry
#
# trt_groves4 = st_collection_extract(trt_groves3) %>%
#   st_cast("POLYGON")
#
# trt_groves5 = trt_groves4 %>%
#   group_by(dist_year, dist_type) %>%
#   summarise() %>%
#   st_collection_extract("POLYGON")
# # %>%
# #   mutate(area_ha = round(as.numeric(st_area(.)*0.0001),10)) %>%
# #   group_by(dist_year,dist_type) %>%
# #   summarise(area_ha = round(sum(area_ha),3))
# View(trt_groves3)
# # %>%
# #   mutate(dist_id = 1:length(dist_year))
#
# #create a new treatment type, where mechanical/commercial and fire occur in
# #the same year on the same polygon
# trt_clean = trt_groves4 %>%
#   st_make_valid() %>%
#   # select(1,2,3,5) %>%
#   mutate(TRUES=1) %>%
#   # select(-2) %>%
#   # mutate(ID = row_number()) %>%
#   pivot_wider(names_from = dist_type, values_from = TRUES, values_fill = 0) %>%
#   group_by(dist_year, mech_under, rx_pile) %>%
#   summarise() %>%
#   mutate(new_dist_type = case_when(
#     (mech_under == 1 & rx_pile == 1) ~ "COMBO_MECH_FIRE",
#     (mech_under == 1 & rx_pile == 0) ~ "mech_under",
#     (mech_under == 0 & rx_pile == 1) ~ "rx_pile",
#     .default = "oopsy")) %>%
#     # ((mech_under + rx_pile == 2) & sum_all>1)) ~ "COMBO_MECH_FIRE",
#     # ((mech_under == 1) & sum_all==1) ~ "mech_under",
#     # ((rx_pile == 1) & sum_all==1) ~ "rx_pile",
#     # .default = "invalid") %>%
#   # select(-c(2:7)) %>%
#   # group_by(dist_year,dist_type) %>%
#   # mutate(Trt_Yr = paste(dist_year,"-",UCB_TRT_NEW, sep = '')) %>%
#   select(new_dist_type,dist_year) %>%
#   st_make_valid %>%
#   st_collection_extract("POLYGON")
# # %>%
# #   st_cast("POLYGON")
# head(trt_clean)
# View(trt_clean)
#
#
# trt_clean$HaR = st_area(trt_clean)*0.0001
# trt_clean5 = trt_clean %>%
#   group_by(dist_year,dist_type) %>%
#   summarise(HectaresR = sum(HaR))
# View(trt_clean5)
#
#

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
  select(dist_year, dist_type)#only needed columns

#combine to into all disturbance layer
disturb <- fire_clean %>% 
  bind_rows(trt_clean)



# #reduce to within  groves
grvs = st_union(groves)

#reduce disturbance to within  groves
dist_groves1 <- st_intersection(disturb, grvs)#%>% 
# combine where there is buffer overlap and poorly drawn geometries

dist_groves <- dist_groves1 %>% 
  summarise(.by = c(dist_year, dist_type), 
            geometry = st_union(st_combine(geometry))) %>% 
  st_make_valid() %>% 
  mutate(dist_id = 1:length(dist_year))

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
  mutate(area_ha = as.numeric(st_area(.))*0.0001)
#This removal makes sense, but there are also defo areas 
#with shifting boundaries, so we will keep them all
# %>% 
#   # remove areas less than 1 acre (likely "overlap" due to poorly mapped edges)
#   filter(area_ha >= 0.04) # NOTE: split this out and find hectares represented

dist_hist_long <- dist_hist %>% 
  # st_drop_geometry() %>%  # attributes only
  unnest(origins) %>%  # make long dataframe
  rename(dist_id = origins) %>% 
  left_join(dist_groves_lookup, by = join_by(dist_id)) %>% # connect with disturbance attributes
  select(-dist_id,-Hectares)

###############################################################
###############################################################
###############################################################

#YOU HAVE TO DO THE REPAIR HERE IN ARCPRO OR THERE ARE PROBLEMS WITH DUPS LATER

###############################################################
###############################################################
###############################################################

##need to repair geometry in arcpro and bring back in
# write_sf(dist_hist_long_prep, here("data/spatial_data/outputs/dist_hist_long_28May2025.shp"))

dist_hist_long = read_sf(here("data/spatial_data/outputs/dist_hist_long_28May2025.shp"))

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
# View(dist_order_slice)
length(unique(dist_order_slice$resist_id))

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
    ##the are just equal/duplicated?
    recent_dist_type == prior1_dist_type  ~ recent_dist_type,
    
    ##when fire or rs is listed first and then mech_under is "prior"
    (recent_dist_type %in% c("lowsev_fire", "modsev_fire","rx_pile") &
       (prior1_dist_type %in% c("mech_under"))) ~ "combo_fire_mech",
    
    ##when mech is listed first and then good fire is "prior"
    (recent_dist_type %in% c("mech_under") & 
       (prior1_dist_type %in% c("lowsev_fire", "modsev_fire","rx_pile"))) 
    ~ "combo_fire_mech",
    
    #if any are high sev, use thatfire sev's then take max
    (recent_dist_type %in% c("lowsev_fire", "modsev_fire","highsev_fire","rx_pile","mech_under") &
       (prior1_dist_type %in% c("highsev_fire"))) ~ "highsev_fire",
    #if fire sev's then take max
    (recent_dist_type %in% c("highsev_fire") &
       (prior1_dist_type %in% c("lowsev_fire", "modsev_fire","highsev_fire","rx_pile","mech_under"))) ~ 
      "highsev_fire",
    #if fire sev's then take max - when mod is max
    (recent_dist_type %in% c("lowsev_fire","rx_pile") &
       (prior1_dist_type %in% c("modsev_fire"))) ~ 
      "modsev_fire",
    (recent_dist_type %in% c("modsev_fire") &
       (prior1_dist_type %in% c("lowsev_fire","rx_pile"))) ~ 
      "modsev_fire",
    (recent_dist_type %in% c("lowsev_fire") &
       (prior1_dist_type %in% c("lowsev_fire","rx_pile"))) ~ 
      "lowsev_fire",
    (recent_dist_type %in% c("lowsev_fire","rx_pile") &
       (prior1_dist_type %in% c("lowsev_fire"))) ~ 
      "lowsev_fire",
    (recent_dist_type %in% c("modsev_fire") &
       (prior1_dist_type %in% c("lowsev_fire"))) ~ 
      "modsev_fire",
    # is.na(prior1_dist_type) ~ "disregard",
    .default = "oopsy"),
    prior1_dist_yr = prior2_dist_yr,         
    prior1_dist_type = prior2_dist_type,
    prior2_dist_yr = prior3_dist_yr,         
    prior2_dist_type = prior3_dist_type) %>%
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
# View(all_dist_order)

n.occur = data.frame(table(all_dist_order$resist_id))
n.occur[n.occur$Freq > 1,]
n.occur2 = n.occur %>%
  filter(Freq > 1)
nrow(n.occur2)

all_dist_order_lookup = all_dist_order %>%
  st_drop_geometry()

dist_order_wide_summary_3trts = all_dist_order %>%
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
##unclear if this really needs to be repaired?
# write_sf(dist_order_wide_summary_3trts, here("data/spatial_data/outputs/dist_order_wide_summary_3trts_raw_data_pre_REPAIR.shp"))

recent_only = dist_order_wide_summary_3trts %>%
  select(resist_id,recent_dist_yr,recent_dist_type) %>%
  mutate(dist_year = recent_dist_yr, dist_type = recent_dist_type) %>%
  select(-recent_dist_type,-recent_dist_yr)
head(recent_only)

second_only = dist_order_wide_summary_3trts %>%
  select(resist_id,prior1_dist_yr,prior1_dist_type) %>%
  mutate(dist_year = prior1_dist_yr, dist_type = prior1_dist_type) %>%
  select(-prior1_dist_type,-prior1_dist_yr) %>%
  filter(!is.na(dist_year))
head(second_only)

third_only = dist_order_wide_summary_3trts %>%
  select(resist_id,prior2_dist_yr,prior2_dist_type) %>%
  mutate(dist_year = prior2_dist_yr, dist_type = prior2_dist_type) %>%
  select(-prior2_dist_type,-prior2_dist_yr) %>%
  filter(!is.na(dist_year))
head(third_only)

reduced.data = dist_order_wide_summary_3trts %>%
  st_drop_geometry() %>%
  select(resist_id,last_dist_yr,last_dist_type,area_ha,ever.high,diff.1.to.2)

dist_hist_long_again = recent_only %>%
  bind_rows(second_only) %>% 
  bind_rows(third_only) %>%
  arrange(resist_id)
head(dist_hist_long_again)
nrow(dist_hist_long_again)
length(unique(dist_hist_long_again$resist_id))

dist_tallies = dist_hist_long_again %>%
  mutate(TRUES=1) %>%
  arrange(dist_type) %>%
  pivot_wider(names_from = dist_type, values_from = TRUES, values_fill = 0) %>%
  group_by(resist_id) %>%
  summarise(across(c(combo_fire_mech:rx_pile), sum)) %>%
  left_join(reduced.data) %>%
  mutate(ever.high = case_when(is.na(ever.high) ~ "never high sev",
                               .default = ever.high),
         area_ha = as.numeric(st_area(.)*0.0001))
head(dist_tallies)
sum(dist_tallies$area_ha)
write_sf(dist_tallies, here("data/spatial_data/outputs/dist_order_wide_summary_3trts_tallied.shp"))

dist_tallies_tbl = dist_hist_long_again %>%
  st_drop_geometry() %>%
  mutate(TRUES=1) %>%
  arrange(dist_type) %>%
  pivot_wider(names_from = dist_type, values_from = TRUES, values_fill = 0) %>%
  summarise(.by = resist_id, across(c(combo_fire_mech:rx_pile), sum)) %>%
  left_join(reduced.data) %>%
  mutate(ever.high = case_when(is.na(ever.high) ~ "never high sev",
                               .default = ever.high))
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
  st_make_valid()
  
# View(snapshot.resist)
head(snapshot.resist)
sum(snapshot.resist$area_ha)

write_sf(snapshot.resist, here("data/spatial_data/outputs/snapshot_resistance_polys-diss_7July2025.shp"))


####################################################

####find the area with no resistance spatially, then get percentages by grove

####################################################

##have to repair geometry in arcpro:
snapshot.resist.rpr = read_sf(here("data/spatial_data/outputs/snapshot_resistance_polys-diss_7July2025.shp")) %>%
  mutate(area_ha1 = as.numeric(st_area(.)*0.0001))
sum(snapshot.resist.rpr$area_ha1)  

# groves <- st_read(here("data/spatial_data/inputs/forest/FINAL_SEGI_AnalysisArea_wOwnerships_GroveNames_noBuff/FINAL_SEGI_AnalysisArea_wOwnerships_GroveNames_noBuff.shp"))%>%
#   st_make_valid() %>%
#   st_as_sf()
# groves$area_ha = as.numeric(st_area(groves)*0.0001)
# sum(groves$area_ha)
# 
# resist_polys = st_read(here("data/spatial_data/outputs/dist_order_wide_summary_3trts_tallied.shp")) %>%
#   st_transform(crs(groves)) %>%  #uniform crs
#   st_make_valid() %>%
#   mutate(resist_id = resst_d) %>%
#   mutate(area_ha2 = as.numeric(st_area(.)*0.0001)) %>%
#   select(resist_id, area_ha2) %>%
#   left_join(snapshot.resist) %>%
#   group_by(resist_class) %>%
#   summarise(area_ha = sum(area_ha2)) %>%
#   st_as_sf()
# head(resist_polys)
# sum(resist_polys$area_ha)

#get no resistance area

#****************************************
#****************************************
#****************************************
#note - when i used st_difference from sf, no resistance areas was 868 (too high - 10160)
# rmapshaper is supposed to be better at not creating artifacts and
#null geometries. When I used mapshaper, the additional no resistance is 864,
# which gives 10151 (still too high, should be around 10130).
# - also note the weird summing when you join them in the notes below
library(rmapshaper)
no_resist <- ms_erase(groves, snapshot.resist.rpr)# %>%
write_sf(no_resist, here("data/spatial_data/outputs/no_resist_7July2025.shp"))

no_resist_rpr = read_sf(here("data/spatial_data/outputs/no_resist_7July2025.shp"))

no_resist_rpr <- st_difference(groves, st_union(snapshot.resist.rpr))
# %>%
#   # mutate(geometry = x) %>%
#   # select(-x) %>%
#   st_as_sf()
no_resist
# 
no_resist_rpr$area_ha = as.numeric(st_area(no_resist_rpr)*0.0001)
no_resist_rpr = no_resist_rpr %>%
  mutate(resist_class = "No resistance") %>%
  group_by(resist_class) %>%
  summarise(area_ha = sum(area_ha))
head(no_resist_rpr)
sum(no_resist_rpr$area_ha)
sum(snapshot.resist.rpr$area_ha1) + sum(no_resist_rpr$area_ha)


resist_polys_all = snapshot.resist.rpr %>%
  rbind(no_resist_rpr)
# note that if you sum the two area_ha that you brought in from the two different shapefiles, its 10160
sum(resist_polys_all$area_ha)
#but if you re-calculate after you join the two shapefiles together, you get 10155
resist_polys_all$area_ha = as.numeric(st_area(resist_polys_all)*0.0001)
head(resist_polys_all)
sum(resist_polys_all$area_ha)


st_write(resist_polys_all,here("data/spatial_data/outputs/resistance_snapshot_15yrs_summarized.shp"), append=F)

##get the percent per grove 
grv.size = groves %>%
  mutate(grv_polys = as.numeric(st_area(groves)*0.0001)) %>%
  group_by(GroveName) %>%
  summarise(grv_ha = sum(grv_polys)) %>%
  st_drop_geometry() %>%
  # mutate(grv_ha = Acres/2.471) %>%
  select(GroveName, grv_ha)
sum(grv.size$grv_ha)

resist_grvs1 = st_intersection(groves,resist_polys_all) %>%
  mutate(area_ha2 = as.numeric(st_area(.)*0.0001)) %>%
  group_by(GroveName, resist_class) %>%
  summarise(resist_ha = sum(area_ha2)) %>%  
  left_join(grv.size)
sum(resist_grvs1$resist_ha)
View(resist_grvs1)

resist_grvs = resist_grvs1 %>%
  mutate(perc.grv = round(resist_ha/grv_ha,3)*100) %>%
  st_drop_geometry() %>%
  select(GroveName, perc.grv, resist_class) %>%
  # group_by(GroveName) %>%
  pivot_wider(names_from = resist_class, values_from = perc.grv, values_fill = 0) %>%
  left_join(grv.size) %>%
  clean_names() %>%
  select(grove_name, grv_ha, high_resistance, moderate_resistance,
         low_resistance, no_resistance, loss_of_mature_forest) %>%
  mutate(total = high_resistance + moderate_resistance +
           low_resistance + no_resistance + loss_of_mature_forest)

View(resist_grvs)

write.csv(resist_grvs, here("outputs/resistance_percentages_by_grove.csv"))
