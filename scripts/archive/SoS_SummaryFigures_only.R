fires_trts_plot = read.csv(here("outputs/fires_treatments_summarised_27March2025.csv"))
names(fires_trts_plot)

fire_clean_plot = fires_trts_plot %>%
  filter(dist_type != "rx_pile"&dist_type != "mech_under") %>%
  group_by(dist_year,dist_type) %>%
  summarise(area_ha = sum(Hectares))

pre.post.2015.area = fire_clean_plot %>%
  mutate(pre.post.2015 = case_when(dist_year>=2015 ~ "2015-2024",
                                   .default = "1984-2015")) %>%
  group_by(pre.post.2015) %>%
  summarise(Hectares = sum(ha_total), perc.range = Hectares/10130.45)
pre.post.2015.area

pre.post.2015.sev = fire_clean_plot %>%
  mutate(pre.post.2015 = case_when(dist_year>=2015 ~ "2015-2024",
                                   .default = "1984-2015")) %>%
  group_by(dist_type,pre.post.2015) %>%
  summarise(Hectares = sum(ha_total), perc.range = Hectares/10130.45)
pre.post.2015.sev

bene.fire = pre.post.2015.sev %>%
  select(-perc.range) %>%
  pivot_wider(names_from = dist_type, values_from = Hectares) %>%
  mutate(allFire = lowsev_fire + modsev_fire + highsev_fire + undetected_change, allFire.perc.range = allFire/10130.45, BeneficialFire = lowsev_fire + modsev_fire, perc.bene = BeneficialFire/allFire)
bene.fire

fire_clean_plot$dist_year = as.factor(fire_clean_plot$dist_year)
fire_clean_plot_84 = fire_clean_plot %>%
  complete(dist_year,dist_type) %>%
  mutate(across(3, replace_na, 0))
head(SNtrt.lo_comp)

ggplot(fire_clean_plot_84, aes(factor(dist_year), ha_total, 
                               fill = factor(dist_type, levels = c("highsev_fire","modsev_fire","lowsev_fire","undetected_change")))) +
  # fill = factor(dist_type, levels = c("undetected_change", "lowsev_fire","modesev_fire","highsev_fire")))) +
  
  geom_col()+
  theme_bw()+
  scale_fill_manual(
    name = "Fire severity",
    labels = c("High","Moderate","Low","Undetected change"),
    # values = c("#4499CC","#2288FF","#9944CC","#525252"))+
    values = c("#1199BB","#2244CC","#2288FF","#523252"))+
  labs(
    # title = "Annual Wildfire Footprint in Sequoia Groves",
    y = "Area burned (hectares)") +
  scale_y_continuous(labels = scales::comma, expand = c(0, 0), limits = c(0,4000)) +
  scale_x_discrete(expand = c(0.01, 0.01)) +
  # scale_x_continuous(expand = c(0.01, 0.01), breaks=0:13, labels=c(seq(1985,2024,by = 3))) +
  theme(
    axis.title.x = element_blank(),
    #legend.position = ,
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(), panel.background = element_blank(),
    # plot.background = element_rect(color = "black", linewidth = 0.5),
    axis.line = element_line(colour = "black"),
    axis.text.x = element_text(angle = 60, vjust = 1, hjust=1, size = 12),
    # axis.text = element_text(size = 14),
    # axis.title.y = element_text(size = 12)
    text = element_text(size = 14))
ggsave(here("outputs/figures_for_manuscript/annual_wildfire_allYrs_shown.png"), width = 10, height = 5)

