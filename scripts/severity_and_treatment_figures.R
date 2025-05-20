library(tidyverse)
library(terra)
library(sf)
library(here)
library(janitor)
library(scales)

options(scipen = 999)

#bring in severity and treatment files by landowner for different figures
sev_trt = read.csv(here("outputs/fire_and_treatment_wGroves_Owners_12May2025.csv"))
names(sev_trt)

##first get side by side annual severity
annual_activity <- sev_trt %>%
  summarise(.by = c(dist_group, dist_year, dist_type), area_ha = sum(hectares))

ggplot(annual_activity, aes(dist_year, area_ha, fill = factor(dist_type, 
                    levels = c("mech_under","rx_pile","highsev_fire",
                           "modsev_fire","lowsev_fire","undetected_change")))) +
  geom_col(stat="identity", width=0.7) + 
  facet_wrap(~dist_group) +
  ylab("Hectares") +
  scale_fill_manual(
    name = "Treatment and severity classes",
    labels = c("Mechanical treatment/hand thinning","Fire-related treatment", "High severity","Moderate severity","Low severity","Undetected change"),
    # values = c("#4499CC","#2288FF","#9944CC","#525252"))+
    values = c("#999933","#006666","#1199BB","#2244CC","#2288FF","#523252"))+
  theme_bw() +
  scale_x_continuous(expand = c(0.01, 0.01), limits = c(1984,2024)) +
  scale_y_continuous(labels = scales::comma, expand = c(0, 0), limits = c(0,4000)) +
  theme(axis.title.x = element_blank(),
        legend.position = c(0.12,0.76),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), panel.background = element_blank(), 
        # plot.background = element_rect(color = "black", linewidth = 0.5),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=0.7),
        text = element_text(size = 14)) 

ggsave(here("outputs/figures_for_manuscript/trt_fire_combo.jpg"), width = 14, height = 5)

#total area treated entire time
trt = sev_trt %>%
  filter(dist_group == "Active treatment")
nrow(trt)
sum(trt_all$hectares)

##figure of all treats
ggplot(trt, aes(dist_year, hectares, fill = dist_type))+
  geom_col()+
  theme_bw()+
  scale_fill_manual(
    name = "Treatment type",
    labels = c("Mechanical/manual treatment", "Fire-related treatment"),
    values = c("#999933","#006666"))+
  labs(y = "Area treated (ha)") +
  scale_x_continuous(expand = c(0.01, 0.01)) +
  scale_y_continuous(labels = scales::comma, expand = c(0, 0), limits = c(0,650)) +
  theme(axis.title.x = element_blank(),
        legend.title = element_text(size = 12),
        legend.position = c(0.16,0.88),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), panel.background = element_blank(), 
        # plot.background = element_rect(color = "black", linewidth = 0.5),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=0.7),
        text = element_text(size = 12)) 

# ggsave(here("outputs/figures_for_manuscript/trt_annual_1951_2024.jpg"), width = 8, height = 5)


###basic annual trt figure for entire period
annual_trt_activity <- trt %>%
  summarise(.by = c(dist_year, dist_type), area_ha = sum(hectares))

##treatment through time by landowner, just showing 1970-2024
annual_trt_activity_ownership <- trt %>%
  summarise(.by = c(landowner, dist_year, dist_type), area_ha = sum(hectares)) %>%
  filter(dist_year >= 1970)
tapply(annual_trt_activity_ownership$area_ha,annual_trt_activity_ownership$landowner,sum)  

annual_trt_activity_ownership_top3 <- annual_trt_activity_ownership %>%
  mutate(landowner.init = case_when(
    landowner == "Mountain Home Demonstration State Forest" ~ "MHDSF",
    landowner == "National Park Service" ~ "NPS",
    landowner == "US Forest Service" ~ "USFS"
  )) %>%
  filter(landowner %in% c("Mountain Home Demonstration State Forest",
                          "National Park Service","US Forest Service"))

##treatment by ownership
ggplot(annual_trt_activity_ownership_top3, aes(dist_year, area_ha, 
                                                   fill = dist_type))+
  geom_col()+
  facet_wrap("landowner.init") +
  theme_bw()+
  scale_fill_manual(
    name = "Treatment type",
    labels = c("Mechanical/manual treatment", "Fire-related treatment"),
    values = c("#999933","#006666"))+
  labs(y = "Area treated (ha)") +
  scale_x_continuous(expand = c(0.01, 0.01), limits = c(1970,2024)) +
  scale_y_continuous(labels = scales::comma, expand = c(0, 0), limits = c(0,650)) +
  theme(axis.title.x = element_blank(),
        legend.title = element_text(size = 12),
        legend.position = c(0.16,0.78),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), panel.background = element_blank(), 
        # plot.background = element_rect(color = "black", linewidth = 0.5),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=0.7),
        text = element_text(size = 12))

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



