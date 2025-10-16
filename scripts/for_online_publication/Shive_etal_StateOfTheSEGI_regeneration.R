library(here)
library(tidyverse)
library(janitor)
library(sf)
library(terra)
library(kableExtra)

options(scipen=999)

# This document holds the analysis for the regeneration assessment.

groves <- st_read(here("data/spatial_data/inputs/forest/2023_GS_Groves_OTS_public.shp")) %>%
  clean_names() %>% 
  st_make_valid() 

#fire severity
fire <- st_read(here("data/spatial_data/inputs/fires_needed_rdnbr/cbi_all_r_6class.shp")) %>% 
  st_transform(crs(groves)) %>%  #uniform crs
  st_make_valid() %>%
  mutate(id = fireyr)

#full, cleaned treatment layer
treatment <- st_read(here("data/spatial_data/outputs/SEGI_Trtdata/SEGI_Trtdata_12Apr25c_repair.shp")) %>% 
  st_transform(crs(groves)) %>% 
  st_make_valid() %>%
  st_cast("MULTIPOLYGON") %>% 
  st_cast("POLYGON")
nrow(treatment)

grove_ha <- sum(groves$hectares)

######################################
######################################
##Classify the fire layer for regen, accounting for castle being treated differently (see Table 2 in manuscript)
#(note: this process is basically the same as that for the resistance classification but with different specifications)
rgn_fire_clean <- fire %>% 
  clean_names() %>% 
  #replace burnsev with class names, create cols to combine with trt
  mutate(new_sev_class = case_when(
    #accounts for Castle differences by reassigning all high severity (4-6) to what is hih risk for the rest of the fires (6)
        fireyr == "2020CASTLE" & burnsev >= 4 ~ 
                                     "High (high regen risk)",
                                   .default = brnsv_t),
         new_sev_code = case_when(fireyr == "2020CASTLE" & burnsev >= 4 ~ 
                                    6,
                                  .default = burnsev)) %>%
  ##assigns regen classes
  mutate(rgn_dist_class = case_when(
    new_sev_code == 1 ~ "Unburned",
    new_sev_code %in% c(2:4) ~ "regen_ok",
    new_sev_code == 5 ~ "mod_risk_to_rgn",
    new_sev_code == 6 ~ "high_risk_to_rgn",
    .default = "oopsy"),
    rgn_dist_year = as.numeric(year)) %>%
  mutate(area_ha = round(as.numeric(st_area(.)*0.0001),5)) %>%
  group_by(rgn_dist_year, rgn_dist_class) %>%
  summarise(area_ha = sum(area_ha))
rgn_fire_clean

#treatment
#get rid of error in NPS data - shows a lop and scatter in goliath (475) 
#and burn in merced (518) that did not happen
rgn_trt.a = treatment %>%
  filter(rowid != 475&rowid != 518)
nrow(rgn_trt.a)

#get rid of duplicates where year, treatment and shape is same
rgn_trt <- rgn_trt.a %>%
  group_by(year,treatment,area_ha,geometry) %>%
  summarize(geometry = st_union(geometry)) %>%
  ungroup()
nrow(rgn_trt)

#get clean treatment layer with regen assignment
rgn_trt_clean <- rgn_trt %>% 
  clean_names() %>%
  rename(rgn_dist_year = year) %>% 
  # make categories fast to code
  mutate(
    rgn_dist_class = case_when(
      treatment == "Fire-related treatment" ~ "regen_ok",
      treatment == "Mechanical treatment" ~ "mech_under",
      T ~ "fix"), area_ha1 = as.numeric(st_area(.)*0.0001)
  ) %>% 
  group_by(rgn_dist_year, rgn_dist_class) %>% # select only needed columns
  summarise(area_ha = sum(area_ha1)) %>%
  filter(rgn_dist_year >= 1984)
rgn_trt_clean 

#combine treatment and fire
rgn_disturb <- rgn_fire_clean %>% 
  bind_rows(rgn_trt_clean)

#reduce disturbance to within  groves
rgn_groves1 <- st_intersection(rgn_disturb, groves)

rgn_groves <- rgn_groves1 %>% 
  summarise(.by = c(rgn_dist_year, rgn_dist_class), 
            geometry = st_union(st_combine(geometry))) %>% 
  st_make_valid() %>% 
  mutate(dist_id = 1:length(rgn_dist_year))

rgn_groves$Hectares = as.numeric(st_area(rgn_groves)*0.0001)

rgn_groves_lookup <- rgn_groves1 %>% 
  summarise(.by = c(rgn_dist_year, rgn_dist_class), 
            geometry = st_union(st_combine(geometry))) %>% 
  st_make_valid() %>% 
  mutate(dist_id = 1:length(rgn_dist_year)) %>%
  st_drop_geometry()

##create disturbance history files
rgn_hist <- st_intersection(rgn_groves) %>% 
  mutate(resist_id = 1:length(origins)) %>% 
  select(resist_id, origins) %>% 
  st_collection_extract(type = c("POLYGON"), warn = FALSE) %>% #keep only polys
  #recombine multipolygons
  summarise(.by = c(resist_id, origins), geometry = st_combine(geometry)) %>% 
  mutate(area_ha = as.numeric(st_area(.))*0.0001)
rgn_hist

#make a long dataframe
rgn_hist_long <- rgn_hist %>% 
  unnest(origins) %>%  # make long dataframe
  rename(dist_id = origins) %>% 
  left_join(rgn_groves_lookup, by = join_by(dist_id)) %>% # connect with disturbance attributes
  select(-dist_id)

#manipulate to show disturbances in order
rgn_order <- rgn_hist_long %>% 
  # select(-unit_name,-grove_name) %>%
  arrange(rgn_dist_year) %>%
  rename(
    recent_dist_yr = rgn_dist_year,
    recent_rgn_dist_class = rgn_dist_class
  ) %>% 
  group_by(resist_id) %>%
  # identify next treatment where relevant
  # note: use lead if year = character, lag if year = numeric
  mutate(
    recent_rgn_dist_class = tolower(recent_rgn_dist_class),
    prior1_dist_yr = lag(recent_dist_yr, 1),
    prior1_rgn_dist_class = lag(recent_rgn_dist_class, 1),
    prior2_dist_yr = lag(recent_dist_yr, 2),
    prior2_rgn_dist_class = lag(recent_rgn_dist_class, 2),
    prior3_dist_yr = lag(recent_dist_yr, 3),
    prior3_rgn_dist_class = lag(recent_rgn_dist_class, 3)
  ) %>%
  ungroup() %>%
  arrange(resist_id)
rgn_order$area_ha = as.numeric(st_area(rgn_order)*0.0001)

#consolidate for disturbances that occcur in same year, prioritizing so that
#any risk class trumps "regen_ok" or unburned
rgn_order_fix = rgn_order %>%
  # filter(recent_dist_yr==prior1_dist_yr) %>%
  mutate(recent_rgn_dist_class = case_when(
    ##they are just equal/duplicated?
    recent_dist_yr==prior1_dist_yr & recent_rgn_dist_class == prior1_rgn_dist_class
    ~ recent_rgn_dist_class,
    recent_dist_yr==prior1_dist_yr &
      if_any(c(recent_rgn_dist_class,prior1_rgn_dist_class), ~ . == "regen_ok") ~ "regen_ok",
    recent_dist_yr==prior1_dist_yr &
      if_any(c(recent_rgn_dist_class,prior1_rgn_dist_class), ~ . != "regen_ok") & 
      if_any(c(recent_rgn_dist_class,prior1_rgn_dist_class), ~ . == "unburned") ~ "unburned",
    .default = recent_rgn_dist_class),
    prior1_dist_yr = case_when(recent_dist_yr==prior1_dist_yr ~ prior2_dist_yr, .default = prior1_dist_yr),        
    prior1_rgn_dist_class = case_when(recent_dist_yr==prior1_dist_yr ~ prior2_rgn_dist_class, .default = prior1_rgn_dist_class),
    prior2_dist_yr = case_when(recent_dist_yr==prior1_dist_yr ~ prior3_dist_yr, .default = prior2_dist_yr),         
    prior2_rgn_dist_class = case_when(recent_dist_yr==prior1_dist_yr ~ prior3_rgn_dist_class, .default = prior2_rgn_dist_class)) %>%
  select(-prior3_dist_yr,-prior3_rgn_dist_class)
rgn_order_fix

#create a tally for multiple (not duplicative) disturbances
rgn_tallies = rgn_order_fix %>%
  mutate(TRUES=1) %>% 
  pivot_wider(names_from = recent_rgn_dist_class, values_from = TRUES, values_fill = 0)

#get total # of disturbances by type
rgn_tallies_sum = rgn_tallies %>%
  group_by(resist_id) %>%
  summarise(across(c(mod_risk_to_rgn:high_risk_to_rgn), sum)) %>%
  mutate(area_ha = as.numeric(st_area(.))*0.0001)
rgn_tallies_sum

#check for duplicates again 
n.occur = data.frame(table(rgn_tallies_sum$resist_id))
n.occur[n.occur$Freq > 1,]
n.occur2 = n.occur %>%
  filter(Freq > 1)
nrow(n.occur2)

all_rgn_order_lookup = rgn_tallies_sum %>%
  st_drop_geometry()

##################################################################################
###############################
##Assign final classes, where 
#any risk trups regen_ok or unburned, highest risk trumps moderate
rgn_cls = rgn_tallies_sum %>%
  st_make_valid() %>%
  mutate(regen_status =
           case_when(
             ##need to decide how to add teh rx
             high_risk_to_rgn > 0 ~
                 'High risk of inadequate postfire regeneration',
             mod_risk_to_rgn > 0 &  high_risk_to_rgn == 0 ~
                  'Moderate risk of inadequate postfire regeneration',
             if_all(c(mod_risk_to_rgn,high_risk_to_rgn,regen_ok), ~. == 0) & 
                 (mech_under > 0 | unburned > 0) ~ "Unburned",
             if_all(c(mod_risk_to_rgn,high_risk_to_rgn), ~. == 0) & 
                  regen_ok > 0 ~ "Potential for recent regeneration",
             .default = "oopsy")) %>%
  select(regen_status,area_ha) %>%
  group_by(regen_status) %>%
  summarise(area_ha = sum(area_ha))
rgn_cls

#get shapfile of the area totally unburned and untreated
no_dist <- st_difference(groves, st_union(rgn_cls)) %>%
  st_as_sf() %>%
  st_make_valid() %>%
  mutate(area_ha = as.numeric(st_area(.)*0.0001), regen_status = "Unburned") %>%
  group_by(regen_status) %>%
  summarise(area_ha = sum(area_ha))
no_dist

#bind all treated/fire polys to the unburned/untreated to make comprehensive dataset
rgn_polys_all_v1 = rgn_cls %>%
  rbind(no_dist) %>%
  group_by(regen_status) %>%
  summarise(area_ha = sum(area_ha)) %>%
  mutate(dist_id = 1:length(regen_status))
rgn_polys_all_v1


##################################
###Roughly 4 ha of polygons did not "flatten"/combine with st_intersection as expected, leaving overlapping
###polygons. This code re-intersects them, and then chooses the most "conservative" assignment for that polygon
rgn_polys_all_lookup <- rgn_polys_all_v1 %>% 
  st_drop_geometry()

##re-intersect to resolve few polys that did not "flatten" and overlap
rgn_polys_all_2nd_int <- st_intersection(rgn_polys_all) %>% 
  mutate(resist_id = 1:length(origins)) %>% 
  select(resist_id, origins) %>% 
  st_collection_extract(type = c("POLYGON"), warn = FALSE) %>% #keep only polys
  #recombine multipolygons
  summarise(.by = c(resist_id, origins), geometry = st_combine(geometry)) %>%
  mutate(area_ha = as.numeric(st_area(.))*0.0001)
head(rgn_polys_all_2nd_int)

rgn_polys_all_2nd_int_long <- rgn_polys_all_2nd_int %>% 
  # st_drop_geometry() %>%  # attributes only
  unnest(origins) %>%  # make long dataframe
  rename(dist_id = origins) %>% 
  left_join(rgn_polys_all_lookup, by = join_by(dist_id)) %>% # connect with disturbance attributes
  select(-dist_id) #%>%
rgn_polys_all_2nd_int_long

##Give ranks to the regen status types so that where there is conflict keep min of those when grouped by resist_id
rgn_polys_all = rgn_polys_all_2nd_int_long %>%
  select(-area_ha.x,-area_ha.y) %>%
  mutate(rank = case_when(regen_status == "High risk of inadequate postfire regeneration" ~ 1,
                          regen_status == "Moderate risk of inadequate postfire regeneration" ~ 2,
                          regen_status == "Potential for recent regeneration" ~ 3,
                          regen_status == "Unburned" ~ 4
  )) %>%
  mutate(area_ha = as.numeric(st_area(.))*0.0001) %>%
  group_by(resist_id) %>%
  slice_min(rank) %>%
  group_by(regen_status) %>%
  summarise(area_ha = sum(area_ha))
rgn_polys_all

###################################################################
###################################################################
##Assess potential for long term limitations

#create a high severity only shapefile
high_sev <- fire %>% 
  st_intersection(groves) %>%
  clean_names() %>% 
  #replace burnsev with class names, create cols to combine with trt
  mutate(high_cls = case_when(
    burnsev %in% c(1:3) ~ "not_high_sev",
    burnsev %in% c(4:6) ~ "high_sev",
    .default = "oopsy"), area_ha = as.numeric(st_area(.)*0.0001)) %>%
  group_by(high_cls) %>%
  summarise(area_ha = sum(area_ha)) %>%
  mutate(area_ha = as.numeric(st_area(.)*0.0001)) %>%
  filter(high_cls == "high_sev")
high_sev
sum(high_sev$area_ha)

##parse in or out of dispersal area
outta_dispersal <- st_buffer(high_sev,-36.8) %>%
  # st_buffer(-36.8) %>%
  mutate(area_ha = as.numeric(st_area(.)*0.0001), dispersal = "out_of_range") %>%
  select(-high_cls)
outta_dispersal

inna_dispersal = st_difference(groves,outta_dispersal) %>%
  mutate(area_ha = as.numeric(st_area(.)*0.0001), dispersal = "in_range") %>%
  group_by(dispersal) %>%
  summarise(area_ha = sum(area_ha))
inna_dispersal

#combine areas within dispersal distance out of dispersal distance
all_disp = outta_dispersal %>%
  rbind(inna_dispersal) %>%
  mutate(area_ha = as.numeric(st_area(.)*0.0001))

###now merge recent postfire regen potential and long term to one dataset
#excludes a sliver (0.0000002 ha) that was unburned + out of dispersal range, was an artifact of the clipping/merging
combo_rgn = st_intersection(rgn_polys_all,all_disp) %>%
  select(-area_ha.1) %>%
  mutate(area_ha = as.numeric(st_area(.))*0.0001) %>%
  filter(area_ha > 1) %>%
  arrange(regen_status) %>%
  st_drop_geometry(combo_rgn)
combo_rgn

########################################################################
########################################################################
##Make the pie chart

#resummarize
rgn_pivot = rgn %>%
  pivot_wider(names_from = dispersal, values_from = area_ha, values_fill = 0) %>%
  group_by(regen_status) %>%
  summarise(across(c(out_of_range,in_range),sum)) %>%
  mutate(totes = out_of_range+in_range, perc.out = out_of_range/totes, perc.in = in_range/totes) %>%
  mutate(regen_status = case_when(regen_status == "Unburned" ~ "Unburned/Undetected change",
                                  .default = regen_status))
rgn_pivot
sum(rgn_pivot$out_of_range)

#take out the long-term seed limitation, will overlap that outside of R
rgn_imm = rgn %>%
  group_by(regen_status) %>%
  summarise(area_ha = sum(area_ha), perc = area_ha/10133) %>%
  mutate(regen_status = case_when(regen_status == "Unburned" ~ "Unburned/Undetected change",
                                  .default = regen_status))
rgn_imm

#plot
love.pies = ggplot(rgn_imm, aes(x="", y=area_ha, fill=factor(regen_status, 
                                                             levels = c("High risk of inadequate postfire regeneration",
                                                                        "Moderate risk of inadequate postfire regeneration","Unburned/Undetected change",
                                                                        "Potential for recent regeneration")))) +
  geom_bar(stat="identity", width=0.05) +
  coord_polar("y", start=0) +
  scale_fill_manual(values=c("gold3","turquoise3","sienna3","ivory3")) +
  theme(panel.grid.major = element_blank(), 
        panel.background = element_blank(),
        axis.title = element_blank(), axis.text = element_blank()) +
  theme(legend.title = element_blank(), 
        legend.text = element_text(size = 8)) +
  theme(legend.key.size = unit(0.4, 'cm'), 
        legend.text = element_text(size=8)) 
love.pies



