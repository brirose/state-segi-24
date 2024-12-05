library(tidyverse)
library(readxl)
library(here)
#working directly in the shapefile was too slow, so exported to excel and re-saved as csv

ALL_SEVERITY_FIRES_union = 
  read_csv(here("data/COMBO_SEVERITY_TREATMENTS_union_26May2024.csv"))
head(ALL_SEVERITY_FIRES_union)
# one row for each polygon:burn, with columns for polyID, acres, year, sev, and 
# burn number (1st, 2nd, etc.). If you change your idea of what the goal 
# looks like then this is the easiest table to work with I think.
polygon_burns_tidy = 
  
  # start with the file
  ALL_SEVERITY_FIRES_union %>%
  
  # pivot longer, getting one row for each polygon:YearSev column
  pivot_longer(cols = c(-polyID, -Hectares),
               names_to = 'colname',
               values_to = 'dist_yr_type') %>%
  
  # ditch the empty ones
  filter(!is.na(dist_yr_type)) %>%
  
  # split up the burnyear string
  mutate(current_disturb_year = gsub(x = dist_yr_type, pattern = '-.*$', replacement = ''),
         current_disturb_type = gsub(x = dist_yr_type, pattern = '^.*-', replacement = '')) %>%
  dplyr::select(-colname, -dist_yr_type) %>%
  
  # order them by polygon and then burnyear within each polygon
  arrange(polyID, current_disturb_year)
  # 
  # # get the row number (sequential burn number) of each burn within each polygon group
  # group_by(polyID) %>%
  # mutate(dist_number = paste0('distnumber_', row_number())) %>%
  # mutate(dist_type_number = paste0('dist_typenumber_', row_number())) %>%
  # ungroup()
head(polygon_burns_tidy)

##add prior treatment for later screening
polys_dist_order_screen = polygon_burns_tidy %>%
  group_by(polyID) %>%
  mutate(next_disturb_year = lead(current_disturb_year, 1),
         next_disturb_type = lead(current_disturb_type, 1)) %>%
  ungroup() #%>%

  #set definition of classes, including the exclusion of burn then thinninn?
  mutate(burn_before_mech = 
           # will replace burnyear with current_disturb_year, and replace 
           # burnsev with current_disturb_type
         case_when(current_disturb_type %in% c("HIGH_SEV") &  next_disturb_type %in% c("COMM_HAZ","MECH_UNDR","RX_PILE") ~TRUE,
                   TRUE~FALSE))
head(polys_dist_order_screen)
# View(polys_dist_order_screen)

##create a wide dataframe for the classification
polys_wide = polys_dist_order_screen %>%
  mutate(TRUES=1) %>% 
  pivot_wider(names_from = current_disturb_type, values_from = TRUES, values_fill = 0)
head(polys_wide)  
names(polys_wide)  


# define resistance classes
polys_wide_sum = polys_wide %>%
  group_by(polyID) %>%
  summarise(across(c(6:11), sum))
# View(polygons_dist_classes)
head(polys_wide_sum)
#   
#   
polygons_dist_classes =  polys_wide_sum %>%
  group_by(polyID) %>%
  summarise(resist_class =
      case_when(
      #High resistance
        # mod or low or rx + any mech
        # mod or low or rx + mod or low or rx
        (COMM_HAZ + MECH_UNDER + RX_PILE + MOD_SEV + LOW_SEV) >= 2 & (RX_PILE + MOD_SEV + LOW_SEV)>=1 & HIGH_SEV == 0 ~ 'High resistance',
      # #Mod resistance
        # mod or low or rx
        (RX_PILE + MOD_SEV + LOW_SEV) == 1 & (COMM_HAZ+MECH_UNDER) == 0 & HIGH_SEV == 0 ~ 'Moderate resistance',
      # #Low resistance
       # any mech
       (RX_PILE + MOD_SEV + LOW_SEV) == 0 & (COMM_HAZ+MECH_UNDER) >= 1 & HIGH_SEV == 0 ~ 'Low resistance',
      #Loss of mature forest
       # any high
       HIGH_SEV == 1 ~ 'Loss of mature forest',
      #Likely persistant type conversion
       # High-High
       HIGH_SEV > 1 ~ 'Likely persistent type conversion'))
polygons_dist_classes

poly_data = merge(polygons_dist_classes,polys_dist_order_screen,by="polyID",all=FALSE)
head(poly_data)

#don't need burn before mech here - if HS before mech, it will just show loss of forest
#if mod or low sev - we will assume it was clean up
#confirm with Charlotte but it looks like summary tables were calculated annually as appropriate
true = poly_data %>%
  filter(burn_before_mech == FALSE)
head(true)

sum(true$Hectares)

# View(poly_data)
poly_data_ac = poly_data %>%
  group_by(polyID,resist_class) %>%
  summarise(Hectares = mean(Hectares))
head(poly_data_ac)

write.csv(polygons_dist_classes,"C:\\Users\\kshive\\Documents\\UCB\\Projects\\In Progress\\State of the Sierra\\R files\\ResistanceClasses_26May2024.csv")

library(dplyr)
range_overview1 = poly_data_ac %>%
  group_by(resist_class) %>%
  summarise(Hectares = sum(Hectares))
range_overview1
#total YPMC acres in 2000 = 5601782; hectares = 2266970
#calc no resistance acres
# 5601782-sum(range_overview1$Acres)
2266970-sum(range_overview1$Hectares)

range_overview2 = c(resist_class = "No resistance", Hectares = 1066435)
#, Acres = 2635207
range_overview = rbind(range_overview1,range_overview2)
# range_overview$Acres = as.numeric(range_overview$Acres)
# range_overview$Acres = as.numeric(round(range_overview$Acres,0))
range_overview$Hectares = as.numeric(range_overview$Hectares)
range_overview$Hectares = as.numeric(round(range_overview$Hectares,0))
range_overview$percentArea = range_overview$Hectares/2266970
range_overview


options(scipen=999)
library(ggplot2)
library(scales)
resistance = ggplot(range_overview, aes(factor(resist_class, 
                            levels = c("High resistance","Moderate resistance", "Low resistance",
                            "No resistance","Loss of mature forest",
                              "Likely persistent type conversion")), 
                            Hectares, fill = resist_class, width = 0.7)) +
  geom_bar(stat="identity") + theme_bw() + xlab("") + 
  scale_fill_manual(values = c("High resistance" = "#006600", "Moderate resistance" = "#00FF00",
                               "Low resistance" = "palegreen","No resistance" = "orange",
                               "Loss of mature forest" = "red","Likely persistent type conversion" = "black")) +
  scale_x_discrete(labels = c("High\nresistance","Moderate\nresistance","Low\nresistance",
                              "No\nresistance","Forest\nloss",
                              "Type\nconversion"), expand = c(0.1,0.1)) +
  theme(axis.text.x = element_text(size =8),
        axis.text.y = element_text(size=8),
        axis.title=element_text(size=8),
        strip.text.x = element_text(size = 8),
        legend.position = "none",
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), panel.background = element_blank(), 
        plot.background = element_rect(color = "black", linewidth = 0.5),
        axis.line = element_line(colour = "black")) + #ylim(0,2800000) +
  scale_y_continuous(labels = comma, n.breaks = 6, expand = c(0,0), limits = c(0, 2800000))
# +
#   scale_y_continuous()

resistance

ggsave("C:\\Users\\kshive\\Documents\\UCB\\Projects\\In Progress\\State of the Sierra\\R files\\figures\\ResistanceClasses_Figure.jpg",
       width = 4.2, height = 5)

# (30000*2)*22
# 229884 is in high resistance

#####################################################################
##RETREATMENT NEEDS

###get years since treatment to start talking treatment needs
#group by resistance class and get most recent treatment
yrs = poly_data %>%
  group_by(polyID,resist_class) %>%
  summarise(LastTreat = max(current_disturb_year), 
            yrsSince = (2025 - as.numeric(LastTreat)), Hectares = mean(Hectares))
yrs

#could say the high resistance already are low hanging fruit
#and a priority for retreatment
#do we make assumptions about how long they can go differently by resistance?

#If you do it really basic - when do things need retreatment to 
#capitalize on ANY of the benefits incurred (even if its low resistance)

yrs2 = yrs %>%
  mutate(RetreatmentNeeded = case_when(
            yrsSince == 22 ~ 2025,
            yrsSince == 21 ~ 2026,
            yrsSince == 20 ~ 2027,
            yrsSince == 19 ~ 2028,
            yrsSince == 18 ~ 2029,
            yrsSince == 17 ~ 2030,
            yrsSince == 16 ~ 2031,
            yrsSince == 15 ~ 2032,
            yrsSince == 14 ~ 2033,
            yrsSince == 13 ~ 2034,
            yrsSince == 12 ~ 2035,
            yrsSince == 11 ~ 2036,
            yrsSince == 10 ~ 2037,
            yrsSince == 9 ~ 2038,
            yrsSince == 8 ~ 2039,
            yrsSince == 7 ~ 2040,
            yrsSince == 6 ~ 2041,
            yrsSince == 5 ~ 2042,
            yrsSince == 4 ~ 2043,
            yrsSince == 3 ~ 2044,
            yrsSince == 2 ~ 2045,
            yrsSince == 1 ~ 2046
  )) %>%
  filter(!resist_class %in% c("Loss of mature forest",
                              "Likely persistent type conversion")) %>%
  group_by(RetreatmentNeeded) %>%
  summarise(Hectares = sum(Hectares)) %>%
  mutate(NewTreatmentOpps = 20126 - Hectares)
head(yrs2)

yrs2.1 = yrs2 %>%
  filter(NewTreatmentOpps>0)
  
sum(yrs2.1$Hectares)
sum(yrs2.1$NewTreatmentOpps)

trt.need = ggplot(yrs2, aes(RetreatmentNeeded, Hectares)) + 
  geom_col(stat="identity", width=0.5, position = "dodge") + 
  ylab("Hectares") +
  scale_fill_manual(values=c("#006600","#00FF00","palegreen")) +
  theme(panel.grid.major = element_blank(), panel.background = element_blank(), 
        axis.line = element_line(),
        axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) + 
  # scale_x_discrete(expand = c(0, 0)) + 
  scale_y_continuous(labels = scales::comma, expand = c(0, 0)) +
  ggtitle("Re-treatment needs")
  # theme(legend.position = c(0.1, 0.9),legend.key.size = unit(0.2, 'cm')) +
  # geom_hline(aes(yintercept=-Inf)) + 
  # coord_cartesian(clip="off")
trt.need

summary(yrs2$Acres)

yrs2$NewTreatmentOpps = 1000000 - yrs2$Acres


##years and resilience categories
yrs = poly_data %>%
  group_by(polyID,resist_class) %>%
  summarise(LastTreat = max(current_disturb_year), 
            yrsSince = (2023 - as.numeric(LastTreat)), 
            Hectares = mean(Hectares)) %>%
  mutate(RetreatmentNeeded = case_when(
    yrsSince == 22 ~ 2023,
    yrsSince == 21 ~ 2024,
    yrsSince == 20 ~ 2025,
    yrsSince == 19 ~ 2026,
    yrsSince == 18 ~ 2027,
    yrsSince == 17 ~ 2028,
    yrsSince == 16 ~ 2029,
    yrsSince == 15 ~ 2030,
    yrsSince == 14 ~ 2031,
    yrsSince == 13 ~ 2032,
    yrsSince == 12 ~ 2033,
    yrsSince == 11 ~ 2034,
    yrsSince == 10 ~ 2035,
    yrsSince == 9 ~ 2036,
    yrsSince == 8 ~ 2037,
    yrsSince == 7 ~ 2038,
    yrsSince == 6 ~ 2039,
    yrsSince == 5 ~ 2040,
    yrsSince == 4 ~ 2041,
    yrsSince == 3 ~ 2042,
    yrsSince == 2 ~ 2043,
    yrsSince == 1 ~ 2044
  )) 
head(yrs)

yrs0 = yrs %>%
  filter(!resist_class %in% c("Loss of mature forest",
                               "Likely persistent type conversion")) %>%
  group_by(RetreatmentNeeded) %>%
  summarise(Hectares = sum(Hectares)) %>%
  mutate(NewTreatmentOpps = 10800 - Hectares)
head(yrs0)

sum(yrs0$NewTreatmentOpps)


library(scales)

dd = ggplot(yrs0, aes(RetreatmentNeeded, Hectares, 
                      fill = factor(resist_class,
               levels = c("Low resistance","Moderate resistance",
                          "High resistance")))) +
  geom_bar(stat = "identity", position = "stack", width = 0.75) + 
  labs(fill="Resistance classes") +
  ylab("Hectares") +
  scale_fill_manual(values=c("turquoise1","darkturquoise","darkslategrey")) +
  theme(panel.grid.major = element_blank(), panel.background = element_blank(), 
        axis.line = element_line(),
        axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) + 
  scale_y_continuous(labels = scales::comma) +
#current mean annual treatment:
  geom_hline(yintercept=20126, linetype='dashed', col = 'black', linewidth = 0.75) +
#current mean annual treatment comm:
  geom_hline(yintercept=12739, linetype='dashed', col = 'red', linewidth = 0.75) +
#current mean annual treatment rx/mech:
  geom_hline(yintercept=3700, linetype='dotdash', col = 'orange', linewidth = 0.75)+ 
  scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0), labels = scales::comma)

dd

ggsave("C:\\Users\\kshive\\Documents\\UCB\\Projects\\In Progress\\State of the Sierra\\R files\\figures\\RetreatmentNeeds_by_ResistanceClasses.jpg",
       width = 6, height = 5)

# 72833 mean need
# 20537 actual current from 2022

