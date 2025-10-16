library(tidyverse)
library(here)
library(ggplot2)

#####need to fix the high severity classification to match resistance

##bring in the overall resistance classes
resist_cls1 = read.csv("C:/Users/kshive/Documents/UCB/Projects/In Progress/State of the SEGI/2024/GIT-state-segi-24/outputs/resistance_thru_time_15yrs.csv")
resist_cls = resist_cls1 %>%
  group_by(resist_class) %>%
  summarise(area_ha_all = sum(area_ha_all))
resist_cls

fail = read.csv(here("outputs/regen_failure_by_grove_ownership_30May2025.csv"))
fail.summary = fail %>%
  mutate(regen_status = case_when(regen_status == "Immediate: Moderate risk" ~
                                         "Immediate: Low end of high severity - likely ok",
                                       .default = regen_status)) %>%
  group_by(regen_status, timeframe) %>%
  summarise(area_ha_all = sum(area_ha))

fail.imm = fail.summary %>%
  filter(timeframe == "immediate")
fail.imm

###get area that is thinned vs no resistance at all
# combo resistance data where low resistance is ever thinned and no resistance is kept
total_groveHa <- 10130.4
total_distHa = sum(resist_cls$area_ha_all)

all_regen_classes = resist_cls %>%
    filter(resist_class != "Loss of mature forest") %>%
    mutate(regen_status = case_when(
    resist_class %in% c("High resistance", "Moderate resistance") ~ "Some fire history (low and mod sev)",
    resist_class == "Low resistance" ~ "No fire history, thinning treatments only"
  )) %>%
  bind_rows(fail.imm) %>% 
  mutate(regen_status_lumped = case_when(
    regen_status == "Some fire history (low and mod sev)" ~ "Some wildfire/rx history",
    regen_status == "Immediate: Low end of high severity - likely ok" ~ "Some wildfire/rx history",
    .default = regen_status
  )) %>%
  group_by(regen_status_lumped) %>%
  summarise(area_ha = sum(area_ha_all)) %>%
  add_row(regen_status_lumped = "No fire/treatment history",
          area_ha = total_groveHa-total_distHa) %>%
  mutate(timeframe = "immediate") %>%
  mutate(perc_area = area_ha/total_groveHa)
all_regen_classes


regen_cls_names = all_regen_classes %>%
  mutate(better.names = case_when(
    regen_status_lumped == "Some wildfire/rx history" ~ "Burned areas with potential for regeneration",
    regen_status_lumped == "No fire/treatment history" ~ "Unburned/untreated - high potential for demographic bottleneck",
    # regen_status_lumped == "Immediate: Moderate risk" ~ "Immediately postfire - moderate risk of inadequate regeneration",
    regen_status_lumped == "Immediate: High risk" ~ "Immediately postfire - high risk of inadequate regeneration",
    regen_status_lumped == "No fire history, thinning treatments only" ~ "Unburned but thinned - moderate potential for demographic bottleneck"
))

love.pies = ggplot(regen_cls_names, aes(x="", y=area_ha, fill=factor(better.names, 
                              levels = c("Immediately postfire - high risk of inadequate regeneration",
                              # "Immediately postfire - moderate risk of inadequate regeneration",
                              "Unburned/untreated - high potential for demographic bottleneck",
                              "Unburned but thinned - moderate potential for demographic bottleneck",
                               "Burned areas with potential for regeneration")))) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0) +
  # scale_fill_manual(values=c("#999933","#006666","#1199BB","#2244CC","#523252")) +
  scale_fill_manual(values=c("#999966","#226666","#5599BB","#523252")) +
  theme(panel.grid.major = element_blank(), 
        panel.background = element_blank(),
        axis.title = element_blank(), axis.text = element_blank()) +
  theme(legend.title = element_blank(), 
        legend.text = element_text(size = 8)) +
  theme(legend.key.size = unit(0.4, 'cm'), 
        legend.text = element_text(size=8)) 
#for adding percent labels, needs trouble shooting
# +
#   geom_text(aes(x = "", y = pos, label = paste0(perc_range,"%")))
love.pies

# ggsave(here("outputs/figures_for_manuscript/regen_pie_chart_30May2025.png"), width = 4, height = 4)
##for legend text size
ggsave(here("outputs/figures_for_manuscript/regen_pie_chart_30May2025_legend.png"), width = 4, height = 4)

###########################
##try one with the long term overlapped
all_regen_classes_wLong = all_regen_classes %>%
  add_row(regen_status_lumped = "Moderate immediate risk of inadequate regeneration with long term limitation",
          area_ha = 373) %>%
  add_row(regen_status_lumped = "High immediate risk of inadequate regeneration with long term limitation",
          area_ha = 222) %>%
  add_row(regen_status_lumped = "Moderate immediate risk of inadequate regeneration with no limits on long term",
          area_ha = 502) %>%
  add_row(regen_status_lumped = "High immediate risk of inadequate regeneration with no limits on long term",
          area_ha = 134) %>%
  filter(!regen_status_lumped %in% c("Immediate: High risk","Immediate: Moderate risk")) %>%
  mutate(regen_status_wLong = regen_status_lumped,
         perc_area = area_ha/10129.04) %>%
  select(-regen_status_lumped,-timeframe)
all_regen_classes_wLong
222/356
373/875
regen_cls_names_wLong = all_regen_classes_wLong %>%
  mutate(better.names = case_when(
    regen_status_wLong == "Some wildfire/rx history" ~ "Burned areas with potential for regeneration",
    regen_status_wLong == "No fire/treatment history" ~ "Unburned/untreated - high potential for demographic bottleneck",
    regen_status_wLong == "No fire history, thinning treatments only" ~ "Unburned but thinned - moderate potential for demographic bottleneck",
    .default = regen_status_wLong
  ))


long.love.pies = ggplot(regen_cls_names_wLong, aes(x="", y=area_ha, fill=factor(better.names, 
                              levels = c("High immediate risk of inadequate regeneration with no limits on long term",
                                         "High immediate risk of inadequate regeneration with long term limitation",
                                         "Moderate immediate risk of inadequate regeneration with long term limitation",
                                          "Moderate immediate risk of inadequate regeneration with no limits on long term",
                                         "Unburned/untreated - high potential for demographic bottleneck",
                              "Unburned but thinned - moderate potential for demographic bottleneck",
                               "Burned areas with potential for regeneration")))) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0) +
  # scale_fill_manual(values=c("#999933","#006666","#1199BB","#2244CC","#523252")) +
  scale_fill_manual(values=c("#999988","#999966","#226677","#226666","#5599BB","#1166BB","#523252")) +
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




love.pies = ggplot(regen_cls_names, aes(x="", y=area_ha, fill=factor(better.names, 
                                                                     levels = c("Immediately postfire - high risk of inadequate regeneration",
                                                                                "Immediately postfire - moderate risk of inadequate regeneration",
                                                                                "Unburned/untreated - high potential for demographic bottleneck",
                                                                                "Unburned but thinned - moderate potential for demographic bottleneck",
                                                                                "Burned areas with potential for regeneration")))) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0) +
  # scale_fill_manual(values=c("#999933","#006666","#1199BB","#2244CC","#523252")) +
  scale_fill_manual(values=c("#999966","#226666","#5599BB","#1166BB","#523252")) +
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
love.pies

ggsave(here("outputs/figures_for_manuscript/regen_pie_chart_30May2025.png"), width = 4, height = 4)



