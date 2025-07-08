library(tidyverse)
library(terra)
library(sf)
library(here)
library(janitor)
library(scales)

options(scipen = 999)

#grove boundaries
groves <- st_read(here("data/spatial_data/inputs/forest/FINAL_SEGI_AnalysisArea_wOwnerships_GroveNames_noBuff/FINAL_SEGI_AnalysisArea_wOwnerships_GroveNames_noBuff.shp")) %>%
  clean_names()
groves$hectares = round(as.numeric(st_area(groves)*0.0001),1)
groves_tbl = st_drop_geometry(groves) %>%
  select(-acres) %>%
  mutate(mnging.agency = case_when(
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
    mngmt.type = case_when(mnging.agency %in% c("U.S. Forest Service","National Park Service","Bureau of Land Management")
                           ~ "Federal",
                           mnging.agency %in% c("UC Berkeley Center for Forestry","Calaveras Big Trees State Park",
                                                "Mountain Home Demonstration State Forest") ~ "State",
                           .default = mnging.agency))

write.csv(groves_tbl, here("outputs/groves_with_owners_full.csv"))

#calfire perims 1910-2024
perims = read.csv(here("outputs/calfire_perims_area_burned_by_fire_1910_2024.csv"))

#bring in severity and treatment files by landowner for different figures
sev_trt = read.csv(here("outputs/fire_and_treatment_wGroves_Owners_22May2025.csv"))
names(sev_trt)

#########################################################
#########################################################
###Area burned

#totals
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
  mutate(percent.burned.area = area_ha_sum/all_burned_area)
perims_timeframes

#get zero years for perims
perims_complete = perims %>% 
  group_by(year) %>%
  summarise(area_ha = sum(area_ha)) %>%
  complete(year = 1910:2024, fill = list(area_ha = 0))
perims_complete

#create total area burned through time figure
ggplot(perims_complete, aes(factor(year),area_ha)) +
  geom_bar(stat="identity",fill="darkblue",width = 0.8) +
  scale_x_discrete(expand = c(0.01, 0.01),
                   breaks=c(seq(1910,2025,5),2024)) +
  scale_y_continuous(labels = scales::comma, expand = c(0, 0), limits = c(0,4000)) +
  ylab("Wildfire area burned (ha)") +
  # scale_fill_discrete("#006666") +
  theme(axis.title.x = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 45, vjust = 0.9, hjust=0.9),
        text = element_text(size = 20)) 

# ggsave(here("outputs/figures_for_manuscript/wildfire_area_burned_1910_2024.jpg"), width = 14, height = 5)


#########################################################
#########################################################
###Plot treatment and severity

##first get side by side annual severity
annual_activity <- sev_trt %>%
  filter(dist_year>=1984) %>%
  summarise(.by = c(dist_group, dist_year, dist_type), area_ha = sum(hectares))

annual_fire_comp = annual_activity %>%
  filter(dist_group == "Wildfire") %>%
  complete(dist_year = 1984:2024,dist_type, fill = list(area_ha = 0, dist_group = "Wildfire"))
annual_fire_comp
#are area burned
sum(annual_fire_comp$area_ha)
tapply(annual_fire_comp$area_ha,annual_fire_comp$dist_type,sum)

#get some grove level stats on high severity
grv.area1 = groves %>%
  group_by(GroveName) %>%
  summarise(acres.sum = sum(Acres))
grv.area1$grove_area_ha = round(as.numeric(st_area(grv.area1)*0.0001),2)

grv.area = grv.area1 %>%
  st_drop_geometry() %>%
  mutate(grove_name = GroveName) %>%
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
annual_comp_subset = annual_activity %>%
  filter(dist_group == "Active treatment") %>%
  complete(dist_year = 1984:2024,dist_type, fill = list(area_ha = 0, dist_group = "Active treatment")) %>%
  rbind(annual_fire_comp) %>%
  filter(dist_year >=1984)
annual_comp_subset

##get compare stats trt wildfire
cmpr.sev.trt = annual_comp_subset %>%
  select(-dist_group) %>%
  pivot_wider(names_from = dist_type, values_from = area_ha, values_fill = 0) %>%
  mutate(bene.fire = lowsev_fire + modsev_fire, all.trt = mech_under + rx_pile,
         all.burn.lmh = highsev_fire + modsev_fire + lowsev_fire,
         trt.perc.bene.fire = all.trt/bene.fire, rx.perc.bene.fire = rx_pile/bene.fire,
         trt.perc.all.fire = all.trt/all.burn.lmh) 
cmpr.sev.trt
# View(cmpr.sev.trt)
sum(cmpr.sev.trt$bene.fire)
sum(cmpr.sev.trt$all.trt)
sum(cmpr.sev.trt$rx_pile)

#plot severity and treatment
acs_wildfire = annual_comp_subset %>% filter(dist_type %in% c("highsev_fire",
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
    values = c("#1199BB","#2244CC","#2288FF","#523252"))+
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

ggsave(here("outputs/figures_for_manuscript/fire_severity_1984_2024.jpg"), width = 7, height = 5)

# #plot severity and treatment
# ggplot(annual_comp_subset, aes(factor(dist_year), area_ha, fill = factor(dist_type, 
#                     levels = c("highsev_fire",
#                            "modsev_fire","lowsev_fire","undetected_change","mech_under","rx_pile")))) +
#   geom_col(stat="identity", width=0.7) + 
#   facet_wrap(~factor(dist_group, levels = c("Wildfire","Active treatment"))) +
#   ylab("Hectares") +
#   scale_fill_manual(
#     name = "Treatment and severity classes",
#     labels = c("High severity","Moderate severity","Low severity","Undetected change","Mechanical treatment/hand thinning","Fire-related treatment"),
#     # values = c("#4499CC","#2288FF","#9944CC","#525252"))+
#     values = c("#1199BB","#2244CC","#2288FF","#523252","#999933","#006666"))+
#   theme_bw() +
#   
#     scale_x_discrete(expand = c(0.01, 0.01),
#                    breaks=c(seq(1985,2025,5),2024)) +
#   scale_y_continuous(labels = scales::comma, expand = c(0, 0), limits = c(0,4000)) +
#   theme(axis.title.x = element_blank(),
#         legend.position = c(0.12,0.76),
#         panel.grid.major = element_blank(), 
#         panel.grid.minor = element_blank(), panel.background = element_blank(), 
#         # plot.background = element_rect(color = "black", linewidth = 0.5),
#         axis.line = element_line(colour = "black"),
#         axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
#         text = element_text(size = 14)) 
# 
# ggsave(here("outputs/figures_for_manuscript/trt_fire_combo.jpg"), width = 14, height = 5)

#########################################################
#########################################################
###Treatment only

annual_activity <- sev_trt %>%
  filter(dist_year>=1970) %>%
  summarise(.by = c(dist_group, dist_year, dist_type), area_ha = sum(hectares))

#total area treated entire time
trt = sev_trt %>%
  filter(dist_group == "Active treatment")
nrow(trt)
sum(trt$hectares)

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

ggsave(here("outputs/figures_for_manuscript/trt_annual_1970_2024.jpg"), width = 8, height = 5)


##treatment through time by landowner, just showing 1970-2024
annual_trt_activity_ownership <- trt %>%
  summarise(.by = c(landowner, dist_year, dist_type), area_ha = sum(hectares)) %>%
  filter(dist_year > 1969)

tapply(annual_trt_activity_ownership$area_ha,annual_trt_activity_ownership$landowner,sum)  
tapply(annual_trt_activity_ownership$dist_year,annual_trt_activity_ownership$landowner,summary)

annual_trt_activity_ownership_top3 <- annual_trt_activity_ownership %>%
  mutate(landowner.init = case_when(
    landowner == "Mountain Home Demonstration State Forest" ~ "MHDSF",
    landowner == "National Park Service" ~ "NPS",
    landowner == "US Forest Service" ~ "USFS"
  )) %>%
  filter(landowner %in% c("Mountain Home Demonstration State Forest",
                          "National Park Service","US Forest Service")) %>%
  select(landowner.init,dist_type,dist_year,area_ha) %>%
  complete(dist_year = 1969:2024,dist_type, landowner.init, 
           fill = list(area_ha = 0)) 
annual_trt_activity_ownership_top3

##treatment by ownership
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

ggsave(here("outputs/figures_for_manuscript/trt_usfs_nps_mhdsf.jpg"), width = 8, height = 5)


##some basic stats
sum(annual_trt_activity$area_ha)
summary(annual_trt_activity$area_ha)
tapply(annual_trt_activity$area_ha,annual_trt_activity$dist_type,summary)

trt_activity_owners = annual_trt_activity_ownership %>%
  group_by(dist_type,landowner) %>%
  summarise(mean_ha = mean(area_ha), total_ha = sum(area_ha),
            max_yr = max(dist_year), max_ha = max(area_ha), 
            min_yr = min(dist_year), min_ha = min(area_ha))
trt_activity_owners



