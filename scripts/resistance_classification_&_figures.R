library(tidyverse)
library(ggplot2)
library(scales)
library(here)
library(sf)


##dataset by individual polygons with disturbance history as 
#tallies and last year of treatment and # years between prior treatments
dist_wide_all15 = read.csv(here("outputs/dist_order_wide_summary_3trts_tallied.csv"))
head(dist_wide_all15)

###############################
##RESISTANCE SNAPSHOT (CURRENT)
#this calcs resistance based on last treatment BEING 2010-2024 
#and time between prior treatment as <15 years to be High resistance
snapshot.resist = dist_wide_all15 %>%
  mutate(resist_class =
           case_when(
             #High resistance: mod or low or rx + any mech, mod or low or rx + mod or low or rx
             (combo_fire_mech >= 1 & last_trt>=2010) ~
             'High resistance',               
             ((mech_under + rx_pile + modsev_fire + lowsev_fire) >= 2 
             & (rx_pile + modsev_fire + lowsev_fire)>=1 
             & highsev_fire == 0 & diff.1.to.2 <=15 & last_trt>=2010) ~ 
               'High resistance',
             #Mod resistance: mod or low or rx
             ((rx_pile + modsev_fire + lowsev_fire) == 1 |
               ((mech_under + rx_pile + modsev_fire + lowsev_fire) > 1 & 
                   (diff.1.to.2 >15)))
                & last_trt>=2010
             & highsev_fire == 0 ~ 
               'Moderate resistance',
             #Low resistance: any mech
             mech_under >= 1 
             & (rx_pile + modsev_fire + lowsev_fire == 0 |
               (rx_pile + modsev_fire + lowsev_fire > 0 & diff.1.to.2>15))
             & highsev_fire == 0 & last_trt>=2010
                  ~ "Low resistance",
             #Loss of mature forest: any high
             highsev_fire >= 1 |ever.high == "past high sev" ~ 
               "Loss of mature forest",
             highsev_fire == 0 & last_trt<2010 ~ 
               "Faded resistance",
             .default = "oopsy"),
             area_ha = round(area_ha,2)
         # ,
         # test.high = (mech_under + rx_pile + modsev_fire + lowsev_fire),
         # test.mod = (rx_pile + modsev_fire + lowsev_fire),
         # test.low = mech_under
  )
# View(snapshot.resist)
head(snapshot.resist)

# write.csv(snapshot.resist,here("outputs/resistance_snapshot_15yrs_polydata.csv"))

########################################################################################
########################################################################################
#add to shapefile for visual inspection in ArcPro
##note, prior to importing th shapefile, need to repair the geometry in ArcPro 
##to get rid of self intersections and rows without valid geometry
# resist_polys = st_read(here("data/spatial_data/outputs/dist_order_wide_summary_3trts_raw_data_repaired.shp")) %>%
#   st_make_valid() %>%
#   mutate(resist_id = resst_d) %>% 
#   select(resist_id) %>%
#   left_join(snapshot.resist) %>%
#   group_by(resist_class) %>%
#   summarise(area_ha = sum(area_ha))
# head(resist_polys)
# 
# #get no resistance area
# groves <- st_read(here("data/spatial_data/inputs/forest/FINAL_SEGI_AnalysisArea_wOwnerships_GroveNames_noBuff/FINAL_SEGI_AnalysisArea_wOwnerships_GroveNames_noBuff.shp"))%>%
#   st_transform(crs(resist_polys)) %>%  #uniform crs
#   st_make_valid()
# 
# grvs = st_union(groves) %>%
#   st_as_sf()
# 
# no_resist <- st_difference(grvs, st_union(resist_polys)) %>%
#   summarise(area_ha = as.numeric(st_area(.)*0.0001),
#             resist_class = "No resistance")
# 
# resist_polys_all = resist_polys %>%
#   bind_rows(no_resist)
# 
# st_write(resist_polys_all,here("data/spatial_data/outputs/resistance_snapshot_15yrs_summarized.shp"))


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

#create figure
ggplot(resist_tbl_snapshot, aes(x = reorder(resist_class, -rank), y = perc_area, 
                       fill = factor(resist_class, 
                                     levels = c("High resistance", 
                                                "Moderate resistance", "Low resistance", 
                                                "Faded resistance","No resistance", 
                                                "Loss of mature forest")))) +
  geom_bar(stat="identity") +
  scale_fill_manual(
    labels = c("High resistance", "Moderate resistance", "Low resistance", 
               "Faded resistance","No resistance", "Loss of mature forest"),     
    values = c("#11aa99", "#3d405b","#f9a03f","darkgrey","grey33","#813405")) +
  scale_x_discrete(
    labels = c("High resistance\n(20%)", "Moderate resistance\n(40%)", 
               "Low resistance\n(3%)", "Fading resistance\n(8%)",
               "No resistance\n(14%)", "Loss of mature forest\n(15%)")) +     
  ylab("Percent of range") + xlab("") +
  theme_bw()+
  coord_flip()+
  theme(legend.title=element_blank(),
        legend.position = "none",
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), panel.background = element_blank(), 
        # plot.background = element_rect(color = "black", linewidth = 0.5),
        axis.line = element_line(colour = "black"),
        axis.text.y = element_text(size = 14)) 

# ggsave(here("outputs/figures_for_manuscript/resistance_classes_snapshot_percent_15yrs_20May2025.jpg"),
# width = 7, height = 5)



###############################
##Resistance through time
#this calcs resistance through time based on last treatment and time between treatments
#but doesn't have a cut off for the last treatment like above
#Does not include "faded resistance" cuz that is what we want to show thru time
snapshot.resist.all = dist_wide_all15 %>%
  mutate(resist_class =
           case_when(
             #High resistance: mod or low or rx + any mech, mod or low or rx + mod or low or rx
             combo_fire_mech >= 1 ~
               'High resistance',               
             #High resistance: mod or low or rx + any mech, mod or low or rx + mod or low or rx
                  (mech_under + rx_pile + modsev_fire + lowsev_fire) >= 2 
                   & (rx_pile + modsev_fire + lowsev_fire)>=1 
                   & highsev_fire == 0 & diff.1.to.2 <=15 ~ 
                         'High resistance',
             #Mod resistance: mod or low or rx
                  (rx_pile + modsev_fire + lowsev_fire) == 1 |
                  (((mech_under + rx_pile + modsev_fire + lowsev_fire) > 1)
                  & (diff.1.to.2 >15))
                  & highsev_fire == 0 ~ 
                       'Moderate resistance',
             #Low resistance: any mech
                  mech_under >= 1 
                   & (rx_pile + modsev_fire + lowsev_fire == 0 |
                  (rx_pile + modsev_fire + lowsev_fire > 0 & diff.1.to.2>15))
                   & highsev_fire == 0 ~ 
                       "Low resistance",
             #Loss of mature forest: any high
                 highsev_fire >= 1 ~ 
                      "Loss of mature forest",
             .default = "oopsy"),
         test.high = (mech_under + rx_pile + modsev_fire + lowsev_fire),
         test.mod = (rx_pile + modsev_fire + lowsev_fire),
         test.low = mech_under
  )
# View(snapshot.resist.all)
head(snapshot.resist.all)

#summarise it to resistance classes (no need to add no resistance here)
resist_tbl_all = snapshot.resist.all %>%
  st_drop_geometry() %>%
  group_by(resist_class, last_trt) %>%
  summarise(area_ha_all = sum(area_ha)) %>%
  select(last_trt,resist_class, area_ha_all)
resist_tbl_all

#get complete cases of resistance thru time since 1966
all_resist_complete = snapshot.resist.all %>%
  filter(!resist_class %in% c("No resistance","Loss of mature forest")) %>%
  filter(last_trt>=1966) %>%
  complete(last_trt,resist_class)

# write.csv(resist_tbl_snapshot,here("outputs/resistance_thru_time_15yrs.csv"))

all_resist_complete$resist_class_f = factor(all_resist_complete$resist_class, levels=c("Low resistance","Moderate resistance","High resistance","No resistance","Loss of mature forest"))

ggplot(all_resist_complete, 
           aes(last_trt,area_ha, fill = factor(resist_class,
                  levels = c("Low resistance","Moderate resistance","High resistance")))) + 
  # geom_rect(aes(xmin= -Inf,
  #               xmax = 2009.5,
  #               ymin = -Inf,
  #               ymax = Inf), fill = 'gray85', alpha = 0.02) +
  geom_rect(aes(xmin= -Inf,
                xmax = 2009.5,
                ymin = -Inf,
                ymax = Inf), fill = 'gray75', alpha = 0.02) +
  geom_rect(aes(xmin= 2009.5,
                xmax = 2015.5,
                ymin = -Inf,
                ymax = Inf), fill = 'gray80', alpha = 0.02) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap("resist_class_f", ncol = 1) + 
  scale_fill_manual(
    labels = c("High resistance", "Moderate resistance", "Low resistance"),     
    values = c("#11aa99", "steelblue","sienna3")) +
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
  scale_y_continuous(limits = c(0,800), expand = c(0, 0)) +
  scale_x_continuous(expand = c(0.01, 0))

# ggsave(here("outputs/figures_for_manuscript/resistance_classes_thru_time_1970_2024.jpg"),
# width = 7, height = 5)

