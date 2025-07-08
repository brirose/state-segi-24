# ---
#   title: "Regen Failure"
# author: "Shive Lab (B. Baker)"
# date: "`r Sys.Date()`"
# output: html_document
# ---
  
library(here)
library(tidyverse)
library(janitor)
library(sf)
library(terra)
library(kableExtra)


# This document holds the analysis for regeneration failure areas.

groves <- st_read(here("data/spatial_data/inputs/forest/FINAL_SEGI_AnalysisArea_wOwnerships_GroveNames_noBuff/FINAL_SEGI_AnalysisArea_wOwnerships_GroveNames_noBuff.shp"))%>%
  clean_names() %>% 
  st_make_valid()

#fire history in CBI - from KS with the 6 classes for regen too
fire <- st_read(here("data/spatial_data/inputs/fires_needed_rdnbr/cbi_all_r_ks.shp")) %>% 
  st_transform(crs(groves)) %>%  #uniform crs
  st_make_valid() %>%
  mutate(id = fireyr)
f = fire %>% filter()

groves_sf <- groves %>% st_as_sf()

# groves$hectares <- expanse(groves, unit = "ha")
grove_ha <- sum(groves$hectares)

######################################
######################################
##need same classification process and resistance, but for the high severity classes (4-6) only

# create clean fire layer
rgn_clean <- fire %>% 
  clean_names() %>% 
  #replace burnsev with class names, create cols to combine with trt
  mutate(
    dist_type = case_when(
      burnsev %in% c(1:3) ~ "regen_ok",
      burnsev == 4 ~ "low_of_high_rgn_ok",
      burnsev == 5 ~ "mod_risk_rgn",
      burnsev == 6 ~ "high_risk_rgn"),
    dist_year = as.numeric(year)) %>%
  filter(dist_type != "undetected_change") %>%
  select(dist_year, dist_type)#only needed columns

#reduce disturbance to within  groves
rgn_groves1 <- st_intersection(rgn_clean, groves)#%>% 
# combine where there is buffer overlap and poorly drawn geometries

rgn_groves <- rgn_groves1 %>% 
  summarise(.by = c(dist_year, dist_type), 
            geometry = st_union(st_combine(geometry))) %>% 
  st_make_valid() %>% 
  mutate(dist_id = 1:length(dist_year))

rgn_groves$Hectares = as.numeric(st_area(rgn_groves)*0.0001)

rgn_groves_lookup <- rgn_groves %>% 
  st_drop_geometry()

# write.csv(dist_groves_lookup, here("outputs/fires_treatments_summarised_28May2025.csv"))

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

rgn_hist_long_prep <- rgn_hist %>% 
  # st_drop_geometry() %>%  # attributes only
  unnest(origins) %>%  # make long dataframe
  rename(dist_id = origins) %>% 
  left_join(rgn_groves_lookup, by = join_by(dist_id)) %>% # connect with disturbance attributes
  select(-dist_id,-Hectares)

#need to repair geometry in arcpro and bring back in
# write_sf(rgn_hist_long_prep, here("data/spatial_data/outputs/rgn_hist_long_29May2025_for_repair_low_of_high.shp"))

# rgn_hist_long = read_sf(here("data/spatial_data/outputs/rgn_hist_long_29May2025_for_repair_low_of_high.shp"))

n.occur = data.frame(table(rgn_hist_long_prep$resist_id))
n.occur[n.occur$Freq > 1,]
n.occur2 = n.occur %>%
  filter(Freq > 1)
max(n.occur2$Freq)

rgn_order <- rgn_hist_long_prep %>% 
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
    prior2_dist_type = lag(recent_dist_type, 2)
  ) %>%
  ungroup() %>%
  arrange(resist_id)
rgn_order$area_ha = round(as.numeric(st_area(rgn_order)*0.0001),2)
head(rgn_order)
nrow(rgn_order)
names(rgn_order)
length(unique(rgn_order$resist_id))

##this is only to look for dup rows from pivoting
#(rows where max treat for year is not same as recent)
# rgn_order_slice = rgn_order %>%
#   arrange(resist_id,recent_dist_yr) %>%
#   group_by(resist_id) %>%
#   slice_max(recent_dist_yr)
# nrow(rgn_order_slice)
# View(rgn_order_slice)
# length(unique(rgn_order_slice$resist_id))
# 
# n.occur = data.frame(table(rgn_order_slice$resist_id
# ))
# n.occur[n.occur$Freq > 1,]
# n.occur2 = n.occur %>%
#   filter(Freq > 1)
# nrow(n.occur2)
# # View(n.occur2)
# #68 records appear 2x (136 rows extra in dist_order_slice?) -- 
# 
# #confirm really no recent/prior same year duos
# # rgn_dup_chk = rgn_order_slice %>%
# #   filter(recent_dist_yr == prior1_dist_yr)
# # rgn_dup_chk
# 
# rgn_order_lookup = rgn_order_slice %>%
#   st_drop_geometry()

# rgn_order_wide_summary = rgn_order %>%
#   group_by(resist_id) %>%
#   summarise(last_trt = max(recent_dist_yr)) %>%
#   left_join(rgn_order_lookup) %>%
#   mutate(prior1_dist_yr_0 = replace_na(prior1_dist_yr, 0),
#          diff.1.to.2 = recent_dist_yr - prior1_dist_yr_0) 
# # View(dist_order_wide_summary_3trts)
# head(rgn_order_wide_summary)
# nrow(rgn_order_wide_summary)
# 
# n.occur = data.frame(table(rgn_order_wide_summary$resist_id))
# n.occur[n.occur$Freq > 1,]
# 
# write.csv(dist_order_wide_summary_3trts, here("outputs/rgn_order_wide_summary_raw.csv"))
##unclear if this really needs to be repaired?
# write_sf(dist_order_wide_summary_3trts, here("data/spatial_data/outputs/dist_order_wide_summary_3trts_raw_data_pre_REPAIR.shp"))

# reduced.data = dist_order_wide_summary_3trts %>%
#   st_drop_geometry() %>%
#   select(resist_id,last_trt,area_ha,ever.high,diff.1.to.2)
# 

rgn_tallies = rgn_order %>%
  # st_drop_geometry() %>%
  mutate(TRUES=1) %>% 
  pivot_wider(names_from = recent_dist_type, values_from = TRUES, values_fill = 0) %>%
  group_by(resist_id) %>%
  summarise(across(c(regen_ok:high_risk_rgn), sum),area_ha = mean(area_ha))
# %>%
#   summarise(area_ha = mean(area_ha))
# %>%
#   left_join(rgn_order_lookup)
#   mutate(ever.high = case_when(is.na(ever.high) ~ "never high sev",
#                                .default = ever.high))
head(rgn_tallies)
View(rgn_tallies)
nrow(rgn_tallies)

n.occur = data.frame(table(rgn_tallies$resist_id))
n.occur[n.occur$Freq > 1,]

# write_sf(rgn_tallies, here("data/spatial_data/outputs/rgn_order_tallied.shp"))
write.csv(rgn_tallies, here("outputs/regen_hist_tallied.csv"))

##################################################################################
###############################
##Regen totals
rgn_cls = rgn_tallies %>%
  mutate(regen_status =
           case_when(
             ##exclude burned areas that are prolly ok (low and mod sev) because we also want 
             ##the rx areas, will get this from resistance classification
                 (regen_ok >0 & low_of_high_rgn_ok == 0 & mod_risk_rgn == 0 & high_risk_rgn == 0) ~
                   'exclude',
                 (low_of_high_rgn_ok >0 & mod_risk_rgn == 0 & high_risk_rgn == 0) ~
                   'Immediate: Low end of high severity - likely ok',
                 (mod_risk_rgn > 0 & high_risk_rgn == 0) ~
                   'Immediate: Moderate risk',
                 (high_risk_rgn > 0) ~
                   'Immediate: High risk',               
                 .default = "oopsy")) %>%
  filter(regen_status!="exclude")
head(rgn_cls)

####get area that has no fire history
#total area of groves
total_groveHa <- sum(as.numeric(st_area(groves)*0.0001))
total_fireHa = sum(rgn_cls$area_ha)

##create resistance file summarized to resistance classes
rgn_tbl = rgn_cls %>%
  group_by(regen_status) %>%
  summarise(area_ha = sum(area_ha)) %>%
  # group_by(regen_status) %>%
  # summarise(area_ha = sum(area_ha)) %>%
  mutate(perc_area_imm = (area_ha/total_groveHa)*100,
         rank = case_when(
           regen_status == "Immediate: High risk" ~ 3, 
           regen_status == "Immediate: Moderate risk" ~ 2, 
           regen_status == "Immediate: Low end of high severity - likely ok" ~ 1,
           # regen_status == "Immediate: Likely ok" ~ 1,
           .default = -999)) %>%
  arrange(rank) 
rgn_tbl

rgn_tbl_grvs = st_intersection(rgn_tbl, groves) %>%
  mutate(area_ha = round(as.numeric(st_area(.)*0.0001),2)) %>%
  select(-hectares,-rank,-acres) %>%
  mutate(timeframe = "immediate")
head(rgn_tbl_grvs)

rgn_tbl_grvs_lookup = rgn_tbl_grvs %>%
  st_drop_geometry()

###################################################################
###################################################################
##Long term limitations

#create a high severity only shapefile
high_sev_grvs = rgn_tbl %>%
  filter(!regen_status %in% c("Immediate: Likely ok","No fire history")) %>%
  mutate(regen_status = "Longterm regeneration risk") %>%
  group_by(regen_status) %>%
  summarise(area_ha = sum(area_ha)) %>%
  st_make_valid()

##get by groves
longterm_fail <- st_buffer(high_sev_grvs, -36.8) %>%
  mutate(area_ha = round(as.numeric(st_area(.)*0.0001),2))

##get by groves
longterm_fail_grvs = longterm_fail %>%
  st_intersection(groves) %>%
  mutate(area_ha = round(as.numeric(st_area(.)*0.0001),2)) %>%
  select(-hectares,-acres) %>%
  mutate(timeframe = "longterm")
head(longterm_fail_grvs)

all_regen_grvs =  longterm_fail_grvs %>%
  st_drop_geometry() %>%
  add_column(perc_area_imm = NA) %>%
  rbind(rgn_tbl_grvs_lookup) 
all_regen_grvs

write_csv(all_regen_grvs, here("outputs/regen_failure_by_grove_ownership_30May2025.csv"))

##get summary by class
all_regen_summary = rgn_tbl %>%
  st_drop_geometry() %>%
  add_row(regen_status = "Longterm regeneration risk", area_ha = 632) %>%
  select(-rank) %>%
  add_row(regen_status = "No fire history",
  area_ha = total_groveHa-total_fireHa, perc_area_imm = ((total_groveHa-total_fireHa)/total_groveHa)*100)
all_regen_summary


#intersect long term with immediate
##get summary by class
all_regen_overlap = rgn_tbl %>%
  # filter(regen_status != "Immediate: Low end of high severity - likely ok") %>%
  st_intersection(longterm_fail) %>%
  mutate(area_ha = round(as.numeric(st_area(.)*0.0001),)) %>%
  select(regen_status, regen_status.1, area_ha)
all_regen_overlap

