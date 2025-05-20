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
  st_make_valid()

#fixed groves, layer made by Kristen NO BUFFER
groves <- st_read(here("data/spatial_data/inputs/forest/FINAL_SEGI_AnalysisArea_wOwnerships_GroveNames_noBuff/FINAL_SEGI_AnalysisArea_wOwnerships_GroveNames_noBuff.shp"))%>%
  clean_names() %>% 
  st_transform(crs(fire)) %>%  #uniform crs
  st_make_valid()

#get celan treatment layer
trt_clean <- treatment %>% 
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

# #reduce to within  groves
# grvs = st_union(groves)
# 
# trt_groves1 <- st_intersection(trt_clean1, grvs)
# 
# trt_groves2 = trt_groves1 %>%
#   st_intersection()
# nrow(trt_groves2)
# 
# trt_groves3 <- trt_groves2 %>% 
#   summarise(.by = c(dist_year, dist_type), 
#             geometry = st_union(st_combine(geometry))) %>% 
#   st_make_valid()

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
# #The resulting shapefile was then used in ArcPro GUI to create unique polygons
# #by disturbance history using the union tool.
# st_write(all.trts4, "C:\\Users\\kshive\\Documents\\UCB\\GIS\\Projects\\StateOfTheSierra\\shapes\\RevisionOutput_24Jan2025\\ALL_TREATMENTS_25Jan2025_diss_union_CLEAN_addCOMBO_diss.shp")
# 
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
      burnsev == 4 ~ "highsev_fire"),
    dist_year = as.numeric(fire_yr)) %>%
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

# write.csv(dist_groves_lookup, here("outputs/fires_treatments_summarised_27March2025.csv"))

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
# %>% 
#   # remove areas less than 1 acre (likely "overlap" due to poorly mapped edges)
#   filter(area_ha >= 0.04) # NOTE: split this out and find hectares represented

dist_hist_long <- dist_hist %>% 
  # st_drop_geometry() %>%  # attributes only
  unnest(origins) %>%  # make long dataframe
  rename(dist_id = origins) %>% 
  left_join(dist_groves_lookup, by = join_by(dist_id)) %>% # connect with disturbance attributes
  select(-dist_id,-Hectares)

#query out polys that have high in any year (since the later summary doesn't go back thru all disturbances)
ever_high = dist_hist_long %>%
  filter(dist_type == "highsev_fire") %>%
  mutate(ever.high = "past high sev") %>%
  group_by(resist_id, ever.high) %>%
  summarise(max_hi_yr = max(dist_year)) %>%
  st_drop_geometry()
# View(ever.high)  

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
  mutate(dup = "dup") %>%
  st_drop_geometry()
View(dist_order_list_rpt)
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
View(dist_order_rpt1)
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
    #if fire sev's then take max
    (recent_dist_type %in% c("lowsev_fire", "modsev_fire","highsev_fire") &
       (prior1_dist_type %in% c("highsev_fire"))) ~ "highsev_fire",
    #if fire sev's then take max
    (recent_dist_type %in% c("highsev_fire") &
       (prior1_dist_type %in% c("lowsev_fire", "modsev_fire","highsev_fire"))) ~ 
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
View(dist_order_rpt)

#create data frame of the non-duplicated resist_ids
dist_order_rptNO = dist_order_slice %>% 
  left_join(dist_order_list_rpt) %>%
  mutate(dupdup = case_when(dup == "dup" ~ "dup",
                            .default = "not dup")) %>%
  filter(dupdup == "not dup") %>%
  select(-dup,-dupdup,-prior3_dist_yr,-prior3_dist_type)

nrow(dist_order_rptNO)
View(dist_order_rptNO)


#bind the cleaned up dups, non-dups and the table with ever high
all_dist_order = dist_order_rptNO %>%
  rbind(dist_order_rpt) %>%
  left_join(ever_high)
length(unique(all_dist_order$resist_id))
nrow(all_dist_order)
View(all_dist_order)

n.occur = data.frame(table(all_dist_order$resist_id))
n.occur[n.occur$Freq > 1,]
n.occur2 = n.occur %>%
  filter(Freq > 1)
nrow(n.occur2)

all_dist_order_lookup = all_dist_order %>%
  st_drop_geometry()

dist_order_wide_summary_3trts = all_dist_order %>%
  group_by(resist_id) %>%
  summarise(last_trt = max(recent_dist_yr)) %>%
  left_join(all_dist_order_lookup) %>%
  mutate(prior1_dist_yr_0 = replace_na(prior1_dist_yr, 0),
         diff.1.to.2 = recent_dist_yr - prior1_dist_yr_0) 
View(dist_order_wide_summary_3trts)
nrow(dist_order_wide_summary_3trts)

n.occur = data.frame(table(dist_order_wide_summary_3trts$resist_id
))
n.occur[n.occur$Freq > 1,]

write.csv(dist_order_wide_summary_3trts, here("outputs/dist_order_wide_summary_3trts_raw.csv"))
write_sf(dist_order_wide_summary_3trts, here("data/spatial_data/outputs/dist_order_wide_summary_3trts_raw_data_pre_REPAIR.shp"))

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
  select(resist_id,last_trt,area_ha,ever.high,diff.1.to.2)

dist_hist_long_again = recent_only %>%
  bind_rows(second_only) %>% 
  bind_rows(third_only) %>%
  arrange(resist_id)
head(dist_hist_long_again)
nrow(dist_hist_long_again)
length(unique(dist_hist_long_again$resist_id))

dist_tallies = dist_hist_long_again %>%
  st_drop_geometry() %>%
  mutate(TRUES=1) %>% 
  arrange(dist_type) %>% 
  pivot_wider(names_from = dist_type, values_from = TRUES, values_fill = 0) %>%
  summarise(.by = resist_id, across(c(combo_fire_mech:rx_pile), sum)) %>%
  left_join(reduced.data)
head(dist_tallies)
View(dist_tallies)
nrow(dist_tallies)

write_sf(dist_tallies, here("data/spatial_data/outputs/dist_order_wide_summary_3trts_tallied.shp"))
write.csv(dist_tallies, here("outputs/dist_order_wide_summary_3trts_tallied.csv"))

