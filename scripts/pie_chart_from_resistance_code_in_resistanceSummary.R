###this starts after Bri creates resist_footprint_all in the resistance_assessment.Rmd file
##I didn't add it directly since I didn't know where it best fit. If we want it stand alone we can create a csv 
regen_class = resist_footprint_all %>%
  add_row(resist_class = "Loss of mature forest - regen ok", Acres = 712) %>%
  mutate(rgn_cls = case_when(resist_class == "High resistance" | 
                                 resist_class == "Moderate resistance" |
                               resist_class == "Loss of mature forest - regen ok" ~ 
                               "Burned areas with potential for regen", 
                               resist_class == "Low resistance" ~ "Unburned but thinned - demographic bottleneck",
                               resist_class == "Loss of mature forest" |
                                 resist_class == "Likely persistent type conversion" ~ "Loss of mature forest - total",
                               resist_class == "No resistance" ~ "Unburned/untreated - demographic bottleneck",
                             .default = "oops")) %>%
  group_by(rgn_cls) %>%
  summarise(Acreage = sum(Acres)) %>%
  add_row(rgn_cls = "Postfire - mixed potential for failure", Acreage = 890) %>%
  add_row(rgn_cls = "Postfire - at risk for failure", Acreage = 2150) %>%
  filter(rgn_cls != "Loss of mature forest - total") %>%
  mutate(perc_range = round(Acreage/25034.72,2)*100)
regen_class
sum(regen_class$Acreage)


#this code was supposed to make the labels better but not working
# regen_class <- regen_class %>% 
#         group_by(rgn_cls) %>% mutate(pos=cumsum(perc_range)-0.5*perc_range)

love.pies = ggplot(regen_class, aes(x="", y=Acreage, fill=factor(rgn_cls, 
                                  levels = c("Postfire - at risk for failure",
                                             "Postfire - mixed potential for failure",
                                             "Unburned/untreated - demographic bottleneck",
                                             "Unburned but thinned - demographic bottleneck",
                                             "Burned areas with potential for regen")))) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0) +
  scale_fill_manual(values=c("red","orange","darkgrey","olivedrab3","olivedrab")) +
  theme(panel.grid.major = element_blank(), 
        panel.background = element_blank(), 
        axis.title = element_blank(), axis.text = element_blank()) +
  theme(legend.title = element_blank(), 
          legend.text = element_text(size = 6)) +
  theme(legend.key.size = unit(0.4, 'cm'), 
            legend.text = element_text(size=6)) 
#for adding percent labels, needs trouble shooting
# +
#   geom_text(aes(x = "", y = pos, label = paste0(perc_range,"%")))
love.pies
ggsave(here("outputs/regen_figures/regen_pie_chart_21Feb2025.jpg"), width = 5, height = 3)

