library(tidyverse)
library(here)
library(ggplot2)

#####need to fix the high severity classification to match resistance

##bring in the overall resistance classes
resist_cls = read.csv("C:/Users/kshive/Documents/UCB/Projects/In Progress/State of the SEGI/2024/GIT-state-segi-24/outputs/resistance_footprint_all.csv")
resist_cls
sum(resist_cls$area_ha_all)

imm = read.csv(here("outputs/immediate_failure_bygrove13feb25.csv"))
imm
long = read.csv(here("outputs/longterm_failure_bygrove13feb25.csv"))
long

#pull out the high/mod immediate risk
imm.dat = imm %>%
  filter(grove_name == "All") %>%
  rename(imm_risk_mod_area_ha = area_ha_moderate, 
         imm_risk_high_area_ha = area_ha_high) %>%
  mutate(area = "rangewide") %>%
  select(area, imm_risk_mod_area_ha, 
         imm_risk_high_area_ha)
imm.dat

#pull out total long term risk
long.dat = long %>%
  filter((grove_name == "Total")) %>%
  rename(long_term_risk = area_ha) %>%
  mutate(area = "rangewide") %>%
  select(area, long_term_risk)
long.dat

###get total area of high severity (forest loss) minus the area
###that is RdNBR >640, which is the total area of mod/high imm risk
#first pivot resist classes and join tables
regen_cls_all = resist_cls %>%
  select(resist_class, area_ha_all) %>%
  pivot_wider(names_from = resist_class, values_from = area_ha_all) %>%
  clean_names() %>%
  mutate(area = "rangewide") %>%
  left_join(long.dat) %>%
  left_join(imm.dat) %>%
  mutate(all_imm = (imm_risk_mod_area_ha+imm_risk_high_area_ha),
    high_sev_ok = loss_of_mature_forest - all_imm,
         some_burn_ok = high_resistance + moderate_resistance,
         no_burn_but_thin = low_resistance, no_burn_no_thin = no_resistance,
         total_hopefully = high_sev_ok + some_burn_ok + no_burn_no_thin + no_burn_but_thin + 
                                 imm_risk_mod_area_ha + imm_risk_high_area_ha) %>%
  select(area, total_hopefully, high_sev_ok,some_burn_ok,no_burn_no_thin,
           imm_risk_mod_area_ha,imm_risk_high_area_ha,
           long_term_risk,no_burn_but_thin) %>%
  relocate(area, .before = 1) %>%
  relocate(total_hopefully, .before = 2) %>%
  pivot_longer(2:9, names_to = "class", values_to = "ha")
regen_cls_all
sum(regen_cls_all$ha)

regen_cls_names = regen_cls_all %>%
  filter(!class %in% c("total_hopefully","long_term_risk")) %>%
  mutate(better.names = case_when(
    class == "high_sev_ok" ~ "Burned areas with potential for regeneration",
    class == "some_burn_ok" ~ "Burned areas with potential for regeneration",
    class == "no_burn_no_thin" ~ "Unburned/untreated - high potential for demographic bottleneck",
    class == "imm_risk_mod_area_ha" ~ "Postfire - moderate risk of inadequate regeneration",
    class == "imm_risk_high_area_ha" ~ "Postfire - high risk of inadequate regeneration",
    class == "no_burn_but_thin" ~ "Unburned but thinned - moderate potential for demographic bottleneck"
))

love.pies = ggplot(regen_cls_names, aes(x="", y=ha, fill=factor(better.names, 
                              levels = c("Postfire - high risk of inadequate regeneration",
                              "Postfire - moderate risk of inadequate regeneration",
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
        legend.text = element_text(size=6)) 
#for adding percent labels, needs trouble shooting
# +
#   geom_text(aes(x = "", y = pos, label = paste0(perc_range,"%")))
love.pies

ggsave(here("outputs/regen_figures/regen_pie_chart_21Feb2025.png"), width = 4, height = 2)


