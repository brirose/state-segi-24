library(tidyverse)
library(terra)
library(sf)
library(here)
library(janitor)
library(scales)

options(scipen = 999)

#Input and clean treatment, fire severity and perimeter data

#import groves
groves <- st_read(here("file path"))%>%
  clean_names() %>% 
  st_make_valid()

#fire perimeters
perimeters <- st_read("file path/fire23_1.gdb", layer = "firep23_1") %>% 
  clean_names()  %>%
  st_transform(crs(groves))%>%
  st_cast("MULTIPOLYGON")

#fire history in CBI - from KS with the 6 classes for regen too
fire <- st_read(here("file path/cbi_all_r_6class.shp")) %>% 
  st_transform(crs(groves)) %>%  #uniform crs
  st_make_valid() %>%
  mutate(id = fireyr)
head(fire)

#combined and cleaned treatment layer
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

#rename for merging
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

#clip to groves
trt_clean_groves = trt_clean %>%
  st_intersection(groves) %>%
  select(-acres,-hectares.1) %>% 
  mutate(hectares = round(as.numeric(st_area(.)*0.0001),1)) %>%
  select(dist_year,dist_type,grove_name,unit_name,landowner,hectares)

#create table to join back
trt_clean_groves_tbl = trt_clean_groves %>%
  st_drop_geometry()
head(trt_clean_groves_tbl)

#summary treatment data
trt_clean_groves_tbl_sum = trt_clean_groves_tbl %>%
  group_by(dist_year,dist_type) %>%
  summarise(area_ha = sum(hectares))

########################################################
########################################################
##Wildfire perimeters from CAL FIRE (1910-2024)

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
perims = st_drop_geometry(perims) %>%
  mutate(fire_name = case_when(
    fire_name == " " ~ "unnamed",
    .default = fire_name
  )) %>%
  group_by(year, fire_name) %>%
  summarise(area_ha = sum(area_ha))

#########################################
#########################################
#Fire severity

#rename unit_name to manageing entity
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

#clip fire seveirty to groves
fire_groves1 <- st_intersection(fire, groves)#%>%

#clean and dissolve
fire_groves <- fire_groves1 %>%
  summarise(.by = c(year, id, burnsev, unit_name, grove_name),
            geometry = st_union(st_combine(geometry))) %>%
  st_make_valid() %>%
  mutate(dist_id = 1:length(year))
fire_groves$hectares = round(as.numeric((st_area(fire_groves)/10000)),1)

#create lookup table
fire_groves_lookup <- fire_groves %>%
  st_drop_geometry()

##reassign high severity class (4-6 = high)
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

#create table for supplementary materials
fire_clean_groves_tbl <- fire_groves %>%
  clean_names() %>%
  st_drop_geometry() %>%
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


#table of grove acreages
groves$hectares = round(as.numeric(st_area(groves)*0.0001))
grv.size = groves %>%
  group_by(grove_name) %>%
  summarise(grv.ha = sum(hectares))

#get the groves with the largest percentage of high severity in 2020 and 2021
grvs.sev.2020.2021 = fire_clean_groves_tbl %>%
  filter(dist_year %in% c(2020,2021)) %>%
  left_join(grv.size) %>%
  mutate(perc.hi = High/grv.ha) %>%
  select(dist_year,fire.name,grove_name, High, perc.hi, grv.ha)
grvs.sev.2020.2021
View(grvs.sev.2020.2021)

#combine treatment and severity
sev_trt = fire_clean_groves %>%
  bind_rows(trt_clean_groves_tbl) %>%
  mutate(dist_group = case_when(
    dist_type %in% c("mech_under","rx_pile") ~ "Active treatment",
                     .default = "Wildfire"))


#################################
##Get FOOTPRINT of area treated thru time

#all treatments
trt_clean_groves_foot_all = st_intersection(trt_clean_groves) %>%
  summarise(geometry = st_union(st_combine(geometry))) %>% 
  st_make_valid()
trt_clean_groves_foot_all$Hectares = as.numeric(st_area(trt_clean_groves_foot_all)*0.0001)
trt_clean_groves_foot_all


#by treatment type
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
##calculate area managed by diff owners
groves_lo = groves %>%
  mutate(area_ha = round(as.numeric(st_area(.)*0.0001),1)) %>%
  st_drop_geometry() %>%
  select(landowner, area_ha) %>%
  group_by(landowner) %>%
  summarise(hectares = sum(area_ha))
groves_lo

#############################################################################
#############################################################################
###Create stats and figures

##Area burned
all_burned_area = sum(perims$area_ha)
length(perims$fire_name)

#area burned by time period
perims_timeframes = perims %>%
  mutate(time.cat = case_when(
    year<2015 ~ "pre-2015",
    .default = "2015 and later"),
    fireid = paste(perims$year,"-",perims$fire_name, sep='')) %>%
  group_by(time.cat) %>%
  summarise(num.fires = length(fireid), area_ha_sum = sum(area_ha), med.area_ha = median(area_ha), 
            mn.area_ha = mean(area_ha), min.area_ha = min(area_ha), 
            max.area_ha = max(area_ha)) %>%
  mutate(percent.burned.area = area_ha_sum/all_burned_area,
         percent.grove.area = area_ha_sum/10130.5)
perims_timeframes

#get zero years for perims
perims_complete = perims %>% 
  group_by(year) %>%
  summarise(area_ha = sum(area_ha)) %>%
  complete(year = 1910:2024, fill = list(area_ha = 0))
perims_complete

#create total area burned through time figure
permis_plot = ggplot(perims_complete, aes(factor(year),area_ha)) +
  geom_bar(stat="identity",fill="darkblue",width = 0.8) +
  scale_x_discrete(expand = c(0.01, 0.01),
                   breaks=c(seq(1910,2025,5),2024)) +
  scale_y_continuous(labels = scales::comma, expand = c(0, 0), limits = c(0,4000),
                     sec.axis = sec_axis(
                       trans = ~ (. / 10130) * 100, # Transformation to percentage
                       # trans = ~ . / sum(df$value) * 100, # Transformation to percentage
                       name = "Percent of range (%)", # Secondary axis label
                       labels = scales::percent_format(scale = 1, accuracy = 1, expand = c(0, 0)))) +
  ylab("Wildfire area burned (ha)") +
  # scale_fill_discrete("#006666") +
  theme(axis.title.x = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 45, vjust = 0.9, hjust=0.9),
        text = element_text(size = 18)) 
permis_plot
ggsave(here("outputs/figures_for_manuscript/wildfire_area_burned_1910_2024.jpg"), width = 14, height = 5)


#########################################################
#########################################################
###Plot treatment and severity

##get annual severity
annual_activity <- sev_trt %>%
  filter(dist_year>=1984) %>%
  summarise(.by = c(dist_group, dist_year, dist_type), area_ha = sum(hectares))

#add in zeros
annual_fire_comp = annual_activity %>%
  filter(dist_group == "Wildfire") %>%
  complete(dist_year = 1984:2024,dist_type, fill = list(area_ha = 0, dist_group = "Wildfire"))
annual_fire_comp

#are area burned
sum(annual_fire_comp$area_ha)
tapply(annual_fire_comp$area_ha,annual_fire_comp$dist_type,sum)

#get some grove level stats on high severity
grv.area1 = groves %>%
  group_by(grove_name) %>%
  summarise(acres.sum = sum(acres))
grv.area1$grove_area_ha = round(as.numeric(st_area(grv.area1)*0.0001),2)

grv.area = grv.area1 %>%
  st_drop_geometry() %>%
  select(grove_name, grove_area_ha)

high_grv_2020_2021 <- sev_trt %>%
  filter(dist_type == "highsev_fire" & dist_year %in% c(2020,2021)) %>%
  group_by(grove_name) %>%
  summarise(area_ha_hi = sum(hectares)) %>%
  left_join(grv.area) %>%
  mutate(perc.hi.20.21 = area_ha_hi/grove_area_ha)
high_grv_2020_2021
# View(high_grv_2020_2021)

#get complete dataframe
annual_dist_subset = annual_activity %>%
  filter(dist_group == "Active treatment") %>%
  complete(dist_year = 1984:2024,dist_type, fill = list(area_ha = 0, dist_group = "Active treatment")) %>%
  rbind(annual_fire_comp) %>%
  filter(dist_year >=1984)
annual_dist_subset

##get compare stats trt wildfire
cmpr.sev.trt = annual_dist_subset %>%
  select(-dist_group) %>%
  pivot_wider(names_from = dist_type, values_from = area_ha, values_fill = 0) %>%
  mutate(bene.fire = lowsev_fire + modsev_fire, all.trt = mech_under + rx_pile,
         all.burn.lmh = highsev_fire + modsev_fire + lowsev_fire,
         trt.perc.bene.fire = all.trt/bene.fire, rx.perc.bene.fire = rx_pile/bene.fire,
         trt.perc.all.fire = all.trt/all.burn.lmh) 
cmpr.sev.trt

#plot severity and treatment
acs_wildfire = annual_dist_subset %>% filter(dist_type %in% c("highsev_fire",
                                                              "modsev_fire","lowsev_fire","undetected_change"))
ggplot(acs_wildfire, aes(factor(dist_year), area_ha, fill = factor(dist_type,
                                                                   levels = c("highsev_fire",
                                                                              "modsev_fire","lowsev_fire","undetected_change")))) +
  geom_col(stat="identity", width=0.7) +
  # facet_wrap(~factor(dist_group, levels = c("Wildfire","Active treatment"))) +
  ylab("Hectares") +
  scale_fill_manual(
    name = "Fire severity",
    labels = c("High severity","Moderate severity","Low severity","Undetected change"),
    # values = c("#4499CC","#2288FF","#9944CC","#525252"))+
    # values = c("#1199BB","#2244CC","#2288FF","#523252"))+
    values = c("#1199BB","grey44","orange","black"))+
  theme_bw() +
  
  scale_x_discrete(expand = c(0.01, 0.01),
                   breaks=c(seq(1985,2025,5),2024)) +
  scale_y_continuous(labels = scales::comma, expand = c(0, 0), limits = c(0,3700)) +
  theme(axis.title.x = element_blank(),
        legend.position = c(0.15,0.84),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), panel.background = element_blank(),
        # plot.background = element_rect(color = "black", linewidth = 0.5),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
        text = element_text(size = 12))


#########################################################
#########################################################
###Treatment
#re-filter for longer treatment timeframe
annual_activity <- sev_trt %>%
  filter(dist_year>=1969) %>%
  summarise(.by = c(dist_group, dist_year, dist_type), area_ha = sum(hectares))

#total area treated entire time
trt = sev_trt %>%
  filter(dist_group == "Active treatment")
nrow(trt)
sum(trt$hectares)

#get trts 1969-2024 with zeros
annual_trt_subset = annual_activity %>%
  filter(dist_group == "Active treatment") %>%
  complete(dist_year = 1969:2024,dist_type, fill = list(area_ha = 0, dist_group = "Active treatment")) %>%
  filter(dist_year >=1969)

##figure of all treats
ggplot(annual_trt_subset, aes(factor(dist_year), area_ha, fill = dist_type))+
  geom_col()+
  theme_bw()+
  scale_fill_manual(
    name = "Treatment type",
    labels = c("Mechanical/manual treatment", "Fire-related treatment"),
    values = c("#999933","#006666"))+
  labs(y = "Area treated (ha)") +
  scale_x_discrete(expand = c(0.01, 0.01),
                   breaks=c(seq(1970,2025,5),2024)) +
  scale_y_continuous(labels = scales::comma, expand = c(0, 0), limits = c(0,1100)) +
  theme(axis.title.x = element_blank(),
        legend.title = element_text(size = 12),
        legend.position = c(0.16,0.88),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), panel.background = element_blank(), 
        # plot.background = element_rect(color = "black", linewidth = 0.5),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=0.7),
        text = element_text(size = 12)) 


##treatment through time by landowner, just showing 1970-2024
annual_trt_activity_ownership <- trt %>%
  summarise(.by = c(landowner, dist_year, dist_type), area_ha = sum(hectares)) %>%
  filter(dist_year >= 1969)

tapply(annual_trt_activity_ownership$area_ha,annual_trt_activity_ownership$landowner,sum)  
tapply(annual_trt_activity_ownership$dist_year,annual_trt_activity_ownership$landowner,summary)

##pick the largest three landowners for plotting
annual_trt_activity_ownership_top3 <- annual_trt_activity_ownership %>%
  mutate(landowner.init = case_when(
    landowner == "Mountain Home Demonstration State Forest" ~ "MHDSF",
    landowner == "National Park Service" ~ "NPS",
    landowner == "US Forest Service" ~ "USFS", .default = "oopsy"
  )) %>%
  filter(landowner %in% c("Mountain Home Demonstration State Forest",
                          "National Park Service","US Forest Service")) %>%
  # select(landowner.init,dist_type,dist_year,area_ha) %>%
  complete(dist_year = 1969:2024,dist_type, landowner.init, 
           fill = list(area_ha = 0)) 
annual_trt_activity_ownership_top3

##plot treatment for 3 largest SEGI managers
ggplot(annual_trt_activity_ownership_top3, aes(factor(dist_year), area_ha, 
                                               fill = dist_type))+
  geom_col(width = 0.8)+
  facet_wrap("landowner.init") +
  theme_bw()+
  scale_fill_manual(
    name = "Treatment type",
    labels = c("Mechanical/manual treatment", "Fire-related treatment"),
    values = c("#999933","#006666"))+
  labs(y = "Area treated (ha)") +
  scale_x_discrete(expand = c(0.01, 0.01),
                   breaks=c(seq(1970,2025,5),2024)) +
  scale_y_continuous(labels = scales::comma, expand = c(0, 0), limits = c(0,650)) +
  theme(axis.title.x = element_blank(),
        legend.title = element_text(size = 12),
        legend.position = c(0.16,0.8),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), panel.background = element_blank(), 
        # plot.background = element_rect(color = "black", linewidth = 0.5),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=0.7),
        text = element_text(size = 10))
##added total acres managed outside of R

##get some basic stats
sum(annual_trt_activity$area_ha)
summary(annual_trt_activity$area_ha)
tapply(annual_trt_activity$area_ha,annual_trt_activity$dist_type,summary)

trt_activity_owners = annual_trt_activity_ownership %>%
  group_by(dist_type,landowner) %>%
  summarise(mean_ha = mean(area_ha), total_ha = sum(area_ha),
            max_yr = max(dist_year), max_ha = max(area_ha), 
            min_yr = min(dist_year), min_ha = min(area_ha))
trt_activity_owners
