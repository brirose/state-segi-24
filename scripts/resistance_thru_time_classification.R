library(tidyverse)
library(ggplot2)
library(scales)
library(here)
library(sf)

options(scipen = 999)

##dataset by individual polygons with disturbance history as 
#tallies and last year of treatment and # years between prior treatments
dist_wide_all15 = read.csv(here("outputs/dist_order_wide_summary_3trts_tallied.csv"))
head(dist_wide_all15)
sum(dist_wide_all15$area_ha)

###############################
##RESISTANCE SNAPSHOT (CURRENT)
#this calcs resistance based on last treatment BEING 2010-2024 
#and time between prior treatment as <15 years to be High resistance
snapshot.resist = dist_wide_all15 %>%
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
  )
# View(snapshot.resist)
head(snapshot.resist)
sum(snapshot.resist$area_ha)
# write.csv(snapshot.resist, here("outputs/snapshot_resistance_classification_polys_26June2025.csv"))


#how much area had repeated high severity?
rpt_high = snapshot.resist %>%
  filter(highsev_fire>1)
sum(rpt_high$area_ha)

# write.csv(snapshot.resist,here("outputs/resistance_snapshot_15yrs_polydata.csv"))

########################################################################################
########################################################################################
#add to shapefile to calculate the % of groves by resistance class
##note, prior to importing th shapefile, need to repair the geometry in ArcPro 
##to get rid of self intersections and rows without valid geometry
groves <- st_read(here("data/spatial_data/inputs/forest/FINAL_SEGI_AnalysisArea_wOwnerships_GroveNames_noBuff/FINAL_SEGI_AnalysisArea_wOwnerships_GroveNames_noBuff.shp"))%>%
  st_make_valid() %>%
  st_as_sf()
groves$area_ha = as.numeric(st_area(groves)*0.0001)
sum(groves$area_ha)

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

#note - when i used st_difference from sf, no resistance areas was 868 (too high - 10160)
# rmapshaper is supposed to be better at not creating artifacts and
#null geometries. When I used mapshaper, the additional no resistance is 864,
# which gives 10151 (too low, should be around 10130).
# - also note the weird summing when you join them in the notes below
library(rmapshaper)
# no_resist <- ms_erase(groves, snapshot.resist)# %>%

no_resist <- st_difference(groves, st_union(snapshot.resist))
# %>%
#   # mutate(geometry = x) %>%
#   # select(-x) %>%
#   st_as_sf()
no_resist

no_resist$area_ha = as.numeric(st_area(no_resist)*0.0001)
no_resist = no_resist %>%
  mutate(resist_class = "No resistance") %>%
  group_by(resist_class) %>%
  summarise(area_ha = sum(area_ha))
head(no_resist)

resist_polys_all = resist_polys %>%
  rbind(no_resist)
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

####Make figures
##get total area accounted not accounted for in classification
total_groveHa = 10130.4
no_resist_ha <- total_groveHa - sum(snapshot.resist$area_ha)

##create resistance file summarized to resistance classes
resist_tbl_snapshot = snapshot.resist %>%
  group_by(resist_class) %>%
  summarise(area_ha = sum(area_ha)) %>%
  add_row(
    resist_class = "No resistance",
    area_ha = no_resist_ha) %>%
  group_by(resist_class) %>%
  summarise(area_ha = sum(area_ha)) %>%
  mutate(perc_area = (area_ha/total_groveHa)*100,
         rank = case_when(
           resist_class == "High resistance" ~ 6, 
           resist_class == "Moderate resistance" ~ 5, 
           resist_class == "Low resistance" ~ 4, 
           resist_class == "Faded resistance" ~ 3, # CHANGE COLOR
           resist_class == "No resistance" ~ 2, # CHANGE COLOR
           resist_class == "Loss of mature forest" ~ 1,
           .default = -999)) %>%
  arrange(rank) 
resist_tbl_snapshot

# write.csv(resist_tbl_snapshot,here("outputs/resistance_snapshot_15yrs.csv"))

# High: #005557
#   Medium: #09AEA9
#   Low: #4ADBCC
#   None: #FF7E15
#   Forest loss: #F52605
#   Type conversion: #191919



###############################
##Resistance through time
#this calcs resistance through time based on last treatment and time between treatments
#but doesn't have a cut off for the last treatment like above
#Does not include "faded resistance" cuz that is what we want to show thru time
snapshot.resist.all = dist_wide_all15 %>%
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
View(snapshot.resist.all)
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
sum(resist_lost$area_ha_all)
# View(resist_lost)
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


write.csv(resist_tbl_all,here("outputs/resistance_thru_time_15yrs.csv"))

# all_resist_complete$resist_class_f = factor(all_resist_complete$resist_class, levels=c("Low resistance","Moderate resistance","High resistance"))

ggplot(all_resist_complete, 
           aes(last_dist_yr,area_ha_all, fill = factor(resist_class_f,
                  levels = c("Low resistance","Moderate resistance","High resistance")))) + 
  # geom_rect(aes(xmin= -Inf,
  #               xmax = 2009.5,
  #               ymin = -Inf,
  #               ymax = Inf), fill = 'gray85', alpha = 0.02) +
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

ggsave(here("outputs/figures_for_manuscript/resistance_classes_thru_time_1970_2024.jpg"),
width = 10, height = 5)



