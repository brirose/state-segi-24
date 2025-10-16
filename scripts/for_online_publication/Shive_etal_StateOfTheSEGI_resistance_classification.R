library(tidyverse)
library(terra)
library(sf)
library(here)
library(janitor)
library(scales)
library(dplyr)

options(scipen = 999)

##this code combines treatment and severity data and manipulates them for
##resistance classification. 

#grove layer
groves <- st_read(here("file path/2023_GS_Groves_OTS_public.shp")) %>%
  clean_names() %>% 
  st_make_valid() %>%
  st_cast("MULTIPOLYGON") %>% 
  st_cast("POLYGON")

#fire history in CBI (as 6 classes to account for regen)
fire <- st_read(here("file path/cbi_all_r_6class.shp")) %>% 
  st_transform(crs(groves)) %>%  #uniform crs
  st_make_valid() %>%
  mutate(id = fireyr) %>%
  st_cast("MULTIPOLYGON") %>% 
  st_cast("POLYGON")
head(fire)
 
#full, cleaned treatment layer
treatment <- st_read(here("file path/SEGI_Trtdata_12Apr25c.shp")) %>% 
  st_transform(crs(groves)) %>% 
  st_make_valid() %>%
  st_cast("MULTIPOLYGON") %>% 
  st_cast("POLYGON")

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

#reduce disturbance to within  groves
dist_groves1 <- st_intersection(disturb, groves)

# combine where there is overlap and poorly drawn geometries
dist_groves <- dist_groves1 %>% 
  summarise(.by = c(dist_year, dist_type), 
            geometry = st_union(st_combine(geometry))) %>% 
  st_make_valid() %>% 
  mutate(dist_id = 1:length(dist_year),
         Hectares = as.numeric(st_area(dist_groves)*0.0001))

dist_groves_lookup <- dist_groves %>% 
  st_drop_geometry()

#total area of groves
total_groveHa <- sum(as.numeric(st_area(groves)*0.0001))

##create disturbance history files
dist_hist <- st_intersection(dist_groves) %>% 
  mutate(resist_id = 1:length(origins)) %>% 
  select(resist_id, origins) %>% 
  st_collection_extract(type = c("POLYGON"), warn = FALSE) %>% #keep only polys
  #recombine multipolygons
  summarise(.by = c(resist_id, origins), geometry = st_combine(geometry)) %>%
  mutate(area_ha = as.numeric(st_area(.))*0.0001)
head(dist_hist)

#make long dataset
dist_hist_long <- dist_hist %>% 
  # st_drop_geometry() %>%  # attributes only
  unnest(origins) %>%  # make long dataframe
  rename(dist_id = origins) %>% 
  left_join(dist_groves_lookup, by = join_by(dist_id)) %>% # connect with disturbance attributes
  select(-dist_id)

########################################################
########################################################
#query out polys that have high in any year (since the later summary doesn't go back thru all disturbances)
ever_high = dist_hist_long %>%
  filter(dist_type == "highsev_fire") %>%
  mutate(ever.high = "past high sev") %>%
  group_by(resist_id, ever.high) %>%
  summarise(area_ha = mean(area_ha)) %>%
  st_drop_geometry()

#get polys to one row with disurnaces in order
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

##get rid of the dup rows from the pivoting 
#(rows where max treat for year is not same as recent)
dist_order_slice = dist_order %>%
  arrange(resist_id,recent_dist_yr) %>%
  group_by(resist_id) %>%
  slice_max(recent_dist_yr)

#pull out the dup resist_ids into a list
dist_order_list_rpt = dist_order_slice %>%
  filter(recent_dist_yr==prior1_dist_yr) %>%
  select(resist_id) %>%
  mutate(reduce.list = 1) %>%
  mutate(dup = "dup") %>%
  st_drop_geometry()

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

#create new treatment type when mech and fire occur in the same year on 
#same poly
#also chooses high severity when more than one severity occurs on 
#same poly in same year (due to mappng errors, these are mostly all slivers) 
dist_order_rpt = dist_order_rpt1 %>%
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

#create data frame of the non-duplicated resist_ids
dist_order_rptNO = dist_order_slice %>% 
  left_join(dist_order_list_rpt) %>%
  mutate(dupdup = case_when(dup == "dup" ~ "dup",
                            .default = "not dup")) %>%
  filter(dupdup == "not dup") %>%
  select(-dup,-dupdup,-prior3_dist_yr,-prior3_dist_type)

#bind the cleaned up dups, non-dups and the table with ever high
all_dist_order = dist_order_rptNO %>%
  rbind(dist_order_rpt) %>%
  left_join(ever_high)

#check for duplicates
n.occur = data.frame(table(all_dist_order$resist_id))
n.occur[n.occur$Freq > 1,]
n.occur2 = n.occur %>%
  filter(Freq > 1)
nrow(n.occur2)

#confirmed in arcpro that these dups are funky geometry, should be deleted
all_dist_order_cln = all_dist_order %>%
  filter(!resist_id %in% c(2289, 4568))
all_dist_order_cln

#check for duplicates again 
n.occur = data.frame(table(all_dist_order_cln$resist_id))
n.occur[n.occur$Freq > 1,]
n.occur2 = n.occur %>%
  filter(Freq > 1)
nrow(n.occur2)

#create look up table
all_dist_order_lookup = all_dist_order_cln %>%
  st_drop_geometry()

#get data on yrs between trts, add 0s
dist_order_wide_summary_3trts = all_dist_order_cln %>%
  group_by(resist_id) %>%
  summarise(last_dist_yr = max(recent_dist_yr), last_dist_type = recent_dist_type) %>%
  left_join(all_dist_order_lookup) %>%
  mutate(prior1_dist_yr_0 = replace_na(prior1_dist_yr, 0),
         diff.1.to.2 = recent_dist_yr - prior1_dist_yr_0,
         area_ha = as.numeric(st_area(.)*0.0001)) 

#check for duplicates again....
n.occur = data.frame(table(dist_order_wide_summary_3trts$resist_id))
n.occur[n.occur$Freq > 1,]

#split the 3 disturbances out to re-assemble in long form 
#(pivot_long was too challenging with both dist_type and dist_year!)
recent_only = dist_order_wide_summary_3trts %>%
  select(resist_id,recent_dist_yr,recent_dist_type,last_dist_yr) %>%
  mutate(dist_year = recent_dist_yr, dist_type = recent_dist_type) %>%
  select(-recent_dist_type,-recent_dist_yr)

second_only = dist_order_wide_summary_3trts %>%
  select(resist_id,prior1_dist_yr,prior1_dist_type,last_dist_yr) %>%
  mutate(dist_year = prior1_dist_yr, dist_type = prior1_dist_type) %>%
  select(-prior1_dist_type,-prior1_dist_yr) %>%
  filter(!is.na(dist_year))

third_only = dist_order_wide_summary_3trts %>%
  select(resist_id,prior2_dist_yr,prior2_dist_type,last_dist_yr) %>%
  mutate(dist_year = prior2_dist_yr, dist_type = prior2_dist_type) %>%
  select(-prior2_dist_type,-prior2_dist_yr) %>%
  filter(!is.na(dist_year))

reduced.data = dist_order_wide_summary_3trts %>%
  st_drop_geometry() %>%
  select(resist_id,last_dist_yr,area_ha,ever.high,diff.1.to.2, last_dist_type)

dist_hist_long_again = recent_only %>%
  bind_rows(second_only) %>% 
  bind_rows(third_only) %>%
  arrange(resist_id)

#now that the data is fixed of the duplications, start the process over
#again to get final classification
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

#get tallies for each treatment type
dist_tallies = dist_order_fix_fix %>%
  mutate(TRUES=1) %>%
  arrange(recent_dist_type) %>%
  pivot_wider(names_from = recent_dist_type, values_from = TRUES, values_fill = 0) %>%
  group_by(resist_id) %>%
  summarise(across(c(combo_fire_mech:rx_pile), sum)) %>%
  left_join(reduced.data) %>%
  mutate(ever.high = case_when(is.na(ever.high) ~ "never high sev",
                               .default = ever.high),
         area_ha = as.numeric(st_area(.)*0.0001))

###########################################################################################
###########################################################################################

# classify the resistance

###########################################################################################
###########################################################################################

#classifies the resistance according to rules described in methods section
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

sr = snapshot.resist %>%
  group_by(resist_class) %>%
  summarise(area_ha = sum(area_ha))
sr
####################################################

####find the area with no resistance spatially, then get percentages by grove

####################################################
#gets areas outside of all fires and treatments
no_resist <- st_difference(groves, st_union(snapshot.resist)) %>%
  st_as_sf() %>%
  mutate(area_ha = as.numeric(st_area(.)*0.0001), resist_class = "No resistance") %>%
  group_by(resist_class) %>%
  summarise(area_ha = sum(area_ha)) %>%
  mutate(area_ha = as.numeric(st_area(.)*0.0001))
no_resist

#combine outside and classified areas
resist_polys_summarized = snapshot.resist %>%
  rbind(no_resist) %>%
  group_by(resist_class) %>%
  summarise(area_ha = sum(area_ha)) %>%
  mutate(dist_id = 1:length(resist_class))
sum(resist_polys_summarized$area_ha)

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

resist_polys_all_2nd_int_long <- resist_polys_all_2nd_int %>% 
  unnest(origins) %>%  # make long dataframe
  rename(dist_id = origins) %>% 
  left_join(resist_polys_all_lookup, by = join_by(dist_id)) %>% # connect with resisturbance attributes
  mutate(area_ha = as.numeric(st_area(.))*0.0001)

##Give ranks to the regen status types so that where there is conflict keep min of those when grouped by resist_id
resist_polys_all = resist_polys_all_2nd_int_long %>%
  mutate(rank = case_when(resist_class == "High resistance" ~ 5,
                          resist_class == "Moderate resistance" ~ 4,
                          resist_class == "Low resistance" ~ 3,
                          resist_class == "No resistance" ~ 2,
                          resist_class == "Loss of mature forest" ~ 1
  )) %>%
  group_by(resist_id) %>%
  slice_min(rank) %>%
  group_by(resist_class) %>%
  summarise(area_ha = sum(area_ha))

resist_polys_summarized_tbl = st_drop_geometry(resist_polys_all) %>%
  mutate(perc_area = (area_ha/10132.88)*100,
         rank = case_when(
           resist_class == "High resistance" ~ 5, 
           resist_class == "Moderate resistance" ~ 4, 
           resist_class == "Low resistance" ~ 3, 
           resist_class == "No resistance" ~ 2, # CHANGE COLOR
           resist_class == "Loss of mature forest" ~ 1,
           .default = -999))

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
        axis.line = element_line(colour = "black"),
        axis.text.y = element_text(size = 14),
        axis.text.x = element_text(size = 12)) +
  scale_y_continuous(expand = c(0.001, 0), limits = c(0,45))

#################################################################
#################################################################
##This is the same resistanc classification approach, 
##but extending back to 1984 to get a sense of how often treatments
##have been allowed to "lapse"

snapshot.resist.all = dist_tallies %>%
  mutate(resist_class = case_when(
    #High resistance: mod or low or rx + any mech, mod or low or rx + mod or low or rx
    combo_fire_mech == 1 & ever.high == "never high sev" ~
      'High resistance',               
    #High resistance: mod or low or rx + any mech, mod or low or rx + mod or low or rx
    (mech_under + rx_pile + modsev_fire + lowsev_fire >= 2 )
    & (rx_pile + modsev_fire + lowsev_fire>=1)
    & ever.high == "never high sev" & diff.1.to.2 <=15 ~ 
      'High resistance',
    ###had two trts, but they are too far apart, base classification on last_dist
    (mech_under + rx_pile + modsev_fire + lowsev_fire) >= 2 
    & (rx_pile + modsev_fire + lowsev_fire)>=1  & ever.high == "never high sev" 
    & diff.1.to.2 > 15 & 
      last_dist_type %in% c("rx_pile","modsev_fire","lowsev_fire") ~ 
      'Moderate resistance',
    ###had two trts, but they are too far apart, base classification on last_dist
    (mech_under + rx_pile + modsev_fire + lowsev_fire) >= 2 
    & (rx_pile + modsev_fire + lowsev_fire)>=1  & ever.high == "never high sev" 
    & diff.1.to.2 > 15 & 
      last_dist_type == "mech_under" ~ 
      'Low resistance',
    #Mod resistance: mod or low or rx
    (rx_pile+ modsev_fire + lowsev_fire == 1) &
      mech_under == 0 & combo_fire_mech == 0
    & ever.high == "never high sev" ~ 
      'Moderate resistance',
    #Low resistance: any mech
    mech_under >= 1 & 
      (rx_pile + modsev_fire + lowsev_fire == 0) & ever.high == "never high sev" ~ 
      "Low resistance",
    #Loss of mature forest: any high
    ever.high == "past high sev" ~ 
      "Loss of mature forest",
    .default = "oopsy"),
    area_ha_all = area_ha) %>%
  select(-area_ha)
head(snapshot.resist.all)

#summarise it to resistance classes (no need to add no resistance here)
resist_tbl_all = snapshot.resist.all %>%
  st_drop_geometry() %>%
  group_by(resist_class, last_dist_yr) %>%
  summarise(area_ha_all = sum(area_ha_all)) %>%
  select(last_dist_yr,resist_class, area_ha_all)
resist_tbl_all

##get some stats
#ha "lost" per category
resist_lost = resist_tbl_all %>%
  filter(resist_class != "Loss of mature forest") %>%
  filter(last_dist_yr>=2010&last_dist_yr<2015) %>%
  # filter(last_dist_yr<2010) %>%
  group_by(resist_class) %>%
  summarise(area_ha_all = sum(area_ha_all))
resist_lost

#get complete cases of resistance thru time since 1966
all_resist_complete = snapshot.resist.all %>%
  filter(!resist_class %in% c("No resistance","Loss of mature forest")) %>%
  filter(last_dist_yr>=1969) %>%
  select(resist_id, last_dist_yr, resist_class, area_ha_all) %>%
  complete(last_dist_yr = 1969:2024, resist_class, fill = list(area_ha_all = 0)) %>%
  group_by(last_dist_yr, resist_class) %>%
  summarise(area_ha_all = sum(area_ha_all))
all_resist_complete

resist_lost = all_resist_complete %>%
  filter(resist_class != "Loss of mature forest") %>%
  # filter(last_dist_yr<2010) %>%
  filter(last_dist_yr>=2010 & last_dist_yr <=2015) %>%
  group_by(resist_class) %>%
  summarise(area_ha_all = sum(area_ha_all))
resist_lost
sum(resist_lost$area_ha_all)

all_resist_complete$resist_class_f = factor(all_resist_complete$resist_class, levels=c("Low resistance","Moderate resistance","High resistance"))

#plot with shading to indicate faded and fading resistance
ggplot(all_resist_complete, 
       aes(last_dist_yr,area_ha_all, fill = factor(resist_class_f,
                                                   levels = c("Low resistance","Moderate resistance","High resistance")))) + 
  geom_rect(aes(xmin= -Inf,
                xmax = 2009.5,
                ymin = -Inf,
                ymax = Inf), fill = 'gray65', alpha = 0.02) +
  geom_rect(aes(xmin= 2009.5,
                xmax = 2015.5,
                ymin = -Inf,
                ymax = Inf), fill = 'gray85', alpha = 0.02) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_grid(cols = vars(resist_class_f), scales = "free_y") + 
  scale_fill_manual(
    labels = c("High resistance", "Moderate resistance", "Low resistance"),     
    values = c("#005557", "#09AEA9","#4ADBCC")) +
  theme_bw()+
  xlab("Year of last beneficial disturbance") +
  ylab("Area impacted (ha)") +
  theme(legend.title=element_blank(),
        legend.position = "none",
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), panel.background = element_blank(), 
        # plot.background = element_rect(color = "black", linewidth = 0.5),
        axis.line = element_line(colour = "black"),
        # axis.text.y = element_text(size = 14)
  )  +
  scale_y_continuous(limits = c(0,1800), expand = c(0, 0), breaks = seq(0, 1800, by = 600)) +
  # scale_y_continuous(expand = c(0, 0)) +
  scale_x_continuous(expand = c(0.01, 0))


