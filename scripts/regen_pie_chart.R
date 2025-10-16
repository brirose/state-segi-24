library(tidyverse)
library(here)
library(ggplot2)

#####need to fix the high severity classification to match resistance

##bring in the overall resistance classes
rgn = read.csv("C:/Users/kshive/Documents/UCB/Projects/In Progress/State of the SEGI/2024/GIT-state-segi-24/outputs/regen_classification_full_1Aug2025.csv")

rgn_pivot = rgn %>%
  pivot_wider(names_from = dispersal, values_from = area_ha, values_fill = 0) %>%
  group_by(regen_status) %>%
  summarise(across(c(out_of_range,in_range),sum)) %>%
  mutate(totes = out_of_range+in_range, perc.out = out_of_range/totes, perc.in = in_range/totes) %>%
  mutate(regen_status = case_when(regen_status == "Unburned" ~ "Unburned/Undetected change",
                                  .default = regen_status))
rgn_pivot
sum(rgn_pivot$out_of_range)

rgn_imm = rgn %>%
  group_by(regen_status) %>%
  summarise(area_ha = sum(area_ha), perc = area_ha/10132.88) %>%
  mutate(regen_status = case_when(regen_status == "Unburned" ~ "Unburned/Undetected change",
                                  .default = regen_status))
rgn_imm




love.pies = ggplot(rgn_imm, aes(x="", y=area_ha, fill=factor(regen_status, 
                              levels = c("High risk of inadequate postfire regeneration",
                                         "Moderate risk of inadequate postfire regeneration","Unburned/Undetected change",
                                         "Potential for recent regeneration")))) +
  geom_bar(stat="identity", width=0.05) +
  coord_polar("y", start=0) +
  # scale_fill_manual(values=c("#999933","#006666","#1199BB","#2244CC","#523252")) +
  # scale_fill_manual(values=c("#999955","#996644","#5599BB","#338866")) +
  # scale_fill_manual(values=c("cornsilk3","sienna2","dodgerblue4","darkolivegreen4")) +
  scale_fill_manual(values=c("gold3","turquoise3","sienna3","ivory3")) +
  theme(panel.grid.major = element_blank(), 
        panel.background = element_blank(),
        axis.title = element_blank(), axis.text = element_blank()) +
  theme(legend.title = element_blank(), 
        legend.text = element_text(size = 8)) +
  theme(legend.key.size = unit(0.4, 'cm'), 
        legend.text = element_text(size=8)) 
love.pies

ggsave(here("outputs/figures_for_manuscript/regen_pie_chart_4Aug2025.png"), width = 4, height = 4, dpi = 1200)
##for legend text size
ggsave(here("outputs/figures_for_manuscript/regen_pie_chart_4Aug2025_legend.png"), width = 4, height = 4, dpi = 1200)

###########################
##try one with the long term overlapped
library(ggpattern)

regen_cross_names = rgn %>%
  mutate(cross.names = paste(regen_status,"--",dispersal))
unique(regen_cross_names$cross.names)


long.love.pies = ggplot(regen_cross_names, aes(x="", y=area_ha, fill=factor(cross.names, 
                              levels = c("High risk of inadequate postfire regeneration - out_of_range",
                                         "High risk of inadequate postfire regeneration - in_range"        ,
                                         "Moderate risk of inadequate postfire regeneration - out_of_range",
                                         "Moderate risk of inadequate postfire regeneration - in_range",
                                         "Potential for recent regeneration - out_of_range",
                                         "Potential for recent regeneration - in_range"                    ,
                                         "Unburned - in_range")))) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0) +
  # scale_fill_manual(values=c("#999933","#006666","#1199BB","#2244CC","#523252")) +
  scale_fill_manual(values=c("#999966","#999966","#226666","#226666","#5599BB","#5599BB","#523252"))  +
  
  ####poss add hatching??
  geom_col_pattern(aes(pattern_angle = impact),
                   pattern_color = NA,
                   pattern_fill = "black",
                   pattern = "stripe") +
  scale_x_reverse() + 
  scale_pattern_angle_manual(values = c(45, 135),
                             guide = guide_legend(title = "Impact", order = 2, override.aes = list(fill = "white", color = "black"))) +
  
  
  
  theme(panel.grid.major = element_blank(), 
        panel.background = element_blank(),
        axis.title = element_blank(), axis.text = element_blank()) +
  theme(legend.title = element_blank(), 
        legend.text = element_text(size = 8)) +
  theme(legend.key.size = unit(0.4, 'cm'), 
        legend.text = element_text(size=4)) 
#for adding percent labels, needs trouble shooting
# +
#   geom_text(aes(x = "", y = pos, label = paste0(perc_range,"%")))
long.love.pies

# ggsave(here("outputs/figures_for_manuscript/regen_pie_chart_30May2025.png"), width = 4, height = 4)

