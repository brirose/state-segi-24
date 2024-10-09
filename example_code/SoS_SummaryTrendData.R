library(ggplot2)
library(dplyr)
library(tidyr)


########
#file of severity by ownership and year in Hectares
SNsev = read.csv("C:\\Users\\kshive\\Documents\\UCB\\Projects\\In Progress\\State of the Sierra\\R files\\CF\\FTE_Severity_allyears.csv")
SNsev$Ha = SNsev$Acres*0.4046856422

#get forested acres for start and end of the study period
SNsev.annual = SNsev %>%
  filter(Year==2001 |Year == 2022) %>%
  group_by(Year) %>%
  summarise(Ha_annual = sum(Ha), Ac_annual = sum(Acres))
SNsev.annual

#get percent high severity when unburned is included
457274+236293+319055+57381.

SNsev.perc = SNsev %>%
  filter(Year!=2000 & Severity.Category!="Unburned") %>%
  group_by(Severity.Category) %>%
  summarise(Ha_annual = sum(Ha)) %>%
  mutate(all_Ha = 1070003, percHa = Ha_annual/all_Ha)

head(SNsev.perc)


#prep for figures and analyses without unburned
SNsev = SNsev %>%
  filter(Year!=2000 & Severity.Category!="Undetected change" & Severity.Category!="Unburned")
head(SNsev)

#severity by year
SNsev.all = SNsev %>%
  group_by(Year,Severity.Category) %>%
  summarise(Hectares = sum(Ha)) %>%
  mutate(TrtSevClass = Severity.Category) %>%
  dplyr::select(-Severity.Category)
head(SNsev.all)

#get compelete cases
SNsev.all$Year1 = as.factor(SNsev.all$Year)
SNsev.all = SNsev.all[,c(4,3,2)]
SNsev.all$Year = SNsev.all$Year1
SNsev.all = SNsev.all[,c(4,2,3)]

SNsev.all_comp=NULL
SNsev.all_comp = SNsev.all %>%
  # select(TrtSevClass,Year1,Acreage) %>%
  complete(Year,TrtSevClass) %>%
  mutate(across(3, replace_na, 0))
head(SNsev.all_comp)

##get some stats:
#total area burned by sev thru study period
tapply(SNsev.all_comp$Hectares, SNsev.all_comp$TrtSevClass, sum)
sum(SNsev.all_comp$Hectares)

sev.stats = SNsev.all_comp %>%
  mutate(SevBeneficial = case_when(
    TrtSevClass == "Low severity"|TrtSevClass == "Moderate severity" ~ 
      "Beneficial_fire",
    TrtSevClass == "High severity" ~ "High_severity",
    .default = as.character(TrtSevClass))) %>%
  group_by(SevBeneficial, Year) %>%
  summarise(Hectares = sum(Hectares)) %>% 
  pivot_wider(names_from = SevBeneficial, values_from = Hectares) %>%
  mutate(TotalHaBurned = Beneficial_fire + High_severity,
         PercHigh = High_severity/TotalHaBurned,
         PercBene = Beneficial_fire/TotalHaBurned)
head(sev.stats)    
# View(sev.stats)
#total beneficial fire over whole period
#555348
#total rx
#80472.57
#total treats
#442786.6

sum(sev.stats$Beneficial_fire)

# sev.stats.percHi = as.data.frame(tapply(sev.stats$PercHigh, sev.stats$Year, sum))
# sev.stats.percBe = as.data.frame(tapply(sev.stats$PercBene, sev.stats$Year, sum))
# View(sev.stats.percBe)

#grouped chart
sev = ggplot(SNsev.all_comp, aes(Year, 
          Hectares,fill = factor(TrtSevClass,
              levels = c("Low severity",
                         "Moderate severity",
                         "High severity")))) + 
  geom_col(stat="identity", width=0.8, position = "dodge") + 
  ylab("Hectares") + xlab("Year") + 
  labs(fill="Severity classes") + 
  scale_fill_manual(values=c("#cccccc","#969696","#525252")) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
            axis.text.x = element_text(angle = 45, vjust = 0.9,
                                       hjust = 1)) + 
  scale_x_discrete(expand = c(0, 0)) + 
  scale_y_continuous(labels = scales::comma, expand = c(0, 0), limits = c(0,160000))
# +
#   geom_hline(yintercept = 6849, linetype = "longdash", color = "darkgrey", linewidth = 0.75) +
#   geom_hline(yintercept = 27891, linetype = "longdash", color = "limegreen", linewidth = 0.75) +
#   geom_hline(yintercept = 38348, linetype = "longdash", color = "orange", linewidth = 0.75) +
#   geom_hline(yintercept = 57618, linetype = "longdash", color = "red", linewidth = 0.75)
sev
# ggsave("C:\\Users\\kshive\\Documents\\UCB\\Projects\\In Progress\\State of the Sierra\\R files\\figures\\Severity_thru_time_0803table.jpg")


#stacked
sev = ggplot(SNsev.all_comp, aes(Year,Hectares,
                fill = factor(TrtSevClass,levels = c("High severity",
                        "Moderate severity","Low severity")))) + 
  geom_col(width=0.8, position = "stack")  +
  ylab("Hectares") + xlab("") + 
  labs(fill="Severity classes") + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 45, vjust = 0.9,
                                   hjust = 1)) + 
  scale_fill_manual(values=c("#525252","#cccccc","#969696")) +
  scale_x_discrete(expand = c(0, 0)) + 
  scale_y_continuous(labels = scales::comma, expand = c(0, 0), limits = c(0,350000))
sev
# ggsave("C:\\Users\\kshive\\Documents\\UCB\\Projects\\In Progress\\State of the Sierra\\R files\\figures\\Severity_thru_time_0803table_STACKED.jpg",
# width = 7, height = 4)

#If we lump L-M as beneficial fire
SNsev.all_comp$BeneficialSeverity = ifelse(SNsev.all_comp$TrtSevClass == "Moderate severity"|
                SNsev.all_comp$TrtSevClass == "Low severity","Beneficial fire",
                  SNsev.all_comp$TrtSevClass)

sn = SNsev.all_comp %>%
  group_by(BeneficialSeverity, Year) %>%
  summarise(Hectares = sum(Hectares))
sn

sev = ggplot(sn, aes(Year, Hectares,fill = factor(BeneficialSeverity,
                                levels = c("Beneficial fire","High severity")))) + 
  geom_col(stat="identity", width=0.8, position = "dodge") + 
  ylab("Hectares") + xlab("Year") + 
  labs(fill="Severity classes") + 
  scale_fill_manual(values=c( "#33CCCC","red")) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 45, vjust = 0.9,
                                   hjust = 1)) + 
  scale_x_discrete(expand = c(0, 0)) + 
  scale_y_continuous(labels = scales::comma, expand = c(0, 0)) 
sev

# sev = ggplot(sn2, aes(Year, Hectares,fill = factor(BeneficialSeverity,
#                       levels = c("High severity","Beneficial fire")))) + 
#   geom_bar(stat="identity", width=0.8, position = "stack") + 
#   ylab("Hectares") + xlab("Year") + 
#   labs(fill="Severity classes") + 
#   scale_fill_manual(values=c("red","#33CCCC")) + 
#   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), 
#         axis.line = element_line(colour = "black"),
#         axis.text.x = element_text(angle = 45, vjust = 0.9,
#                                    hjust = 1)) + 
#   scale_x_discrete(expand = c(0, 0)) + 
#   scale_y_continuous(labels = scales::comma, expand = c(0, 0)) 
# sev
# ggsave("C:\\Users\\kshive\\Documents\\UCB\\Projects\\In Progress\\State of the Sierra\\R files\\figures\\Severity_BeneficialFire_thru_time_0803table_STACKED.jpg", 
#        width = 7, height = 4)


#############################################################################################
#############################################################################################
##

#file of treatments
SNtrt = read.csv("C:\\Users\\kshive\\Documents\\UCB\\Projects\\In Progress\\State of the Sierra\\R files\\CF\\FTE_Treatment_allyears.csv")
SNtrt$Ha = SNtrt$Acres*0.4046856422
head(SNtrt)

SNtrt = SNtrt %>%
  filter(Year!=2000)

#Treatment by year
SNtrt.all = SNtrt %>%
  mutate(TrtSevClass = Treatment) %>%
  filter(TrtSevClass!="EXCLUDE"&TrtSevClass!="No Treatment") %>%
  dplyr::select(-Treatment) %>%
  group_by(Year,TrtSevClass) %>%
  summarise(Hectares = sum(Ha))
head(SNtrt.all)

#get compelete cases
SNtrt.all$Year1 = as.factor(SNtrt.all$Year)
SNtrt.all = SNtrt.all[,c(4,2,3)]
SNtrt.all$Year = SNtrt.all$Year1
SNtrt.all = SNtrt.all[,c(4,2,3)]

SNtrt.all_comp=NULL
SNtrt.all_comp = SNtrt.all %>%
    complete(Year,TrtSevClass) %>%
  mutate(across(3, replace_na, 0))
head(SNtrt.all_comp)

trty.trt = SNtrt.all_comp %>%
  group_by(Year,TrtSevClass) %>% summarise(Hectares = sum(Hectares))
head(trty.trt)
# View(trty.trt)
sum(trty.trt$Hectares)

trty.all = SNtrt.all_comp %>%
  group_by(Year) %>% 
  summarise(Hectares = sum(Hectares))
trty.all

tapply(SNtrt.all_comp$Hectares, SNtrt.all_comp$TrtSevClass, summary)
mean(trty.all$Hectares)


SNtrt.all_comp_temp = SNtrt.all_comp
SNtrt.all_comp_temp$TrtSevClassTemp = ifelse(SNtrt.all_comp_temp$TrtSevClass == 
                                          "Commercial thin/Large hazard tree removal",
                                        "comm",ifelse(SNtrt.all_comp_temp$TrtSevClass == "Fire-related treatment","fire","mech"))
ss = SNtrt.all_comp_temp %>%
  pivot_wider(names_from = TrtSevClassTemp, values_from = Hectares, values_fill = 0)
s = as.data.frame(ss)
names(s)
# s$comm = as.numeric(s[,3])
# s$mech = as.numeric(s[,5])
# s$rx = as.numeric(s[,4])

w = s %>%
  # select(-2,-3,-4,-5) %>%
  group_by(Year) %>%
  summarise(comm = sum(comm), fire = sum(fire), mech = sum(mech), 
            AllMech = comm+mech, total = AllMech+fire, percRx = fire/total)
w

annual.sev.trt = merge(sev.stats, w)
head(annual.sev.trt)
annual.sev.trt$Year = as.character(annual.sev.trt$Year)
annual.sev.trt$Year = as.numeric(annual.sev.trt$Year)
head(annual.sev.trt)
ast = annual.sev.trt %>%
  mutate(Period = case_when(
    Year<2012 ~ "Early",
    .default = "Late")) %>%
  group_by(Period) %>%
  summarise_at(c(2:11),sum) %>%
  mutate(PercBeneTotal = Beneficial_fire/total,
            PercBeneRx = Beneficial_fire/fire)
head(ast)
# View(ast)

mean(w$AllMech)
mean(w$rx)
median(w$AllMech)
median(w$rx)

trt = ggplot(SNtrt.all_comp_temp, aes(Year, Hectares,fill = factor(TrtSevClassTemp, 
                levels = c("Commercial thin",
                           "Mechanical fuels management",
                           "Fire-related treatment")))) + 
  geom_col(stat="identity", width=1, position = "dodge") + 
  ylab("Hectares") + xlab("Year") +
  labs(fill="Treatment classes") + 
  scale_fill_manual(values=c("#999933","brown","blue")) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), 
        axis.line = element_line(colour = "black")
        ,axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1)) + 
  scale_x_discrete(expand = c(0, 0)) + 
  scale_y_continuous(labels = scales::comma, expand = c(0, 0)) +
  geom_hline(yintercept = 12739, linetype = "longdash", color = "#666666", linewidth = 0.75) +
  geom_hline(yintercept = 3730, linetype = "longdash", color = "brown", linewidth = 0.75) +
  geom_hline(yintercept = 3658, linetype = "longdash", color = "blue", linewidth = 0.75)
trt

#stacked
trt = ggplot(SNtrt.all_comp_temp, aes(Year, Hectares,fill = factor(TrtSevClassTemp, 
               levels = c("Commercial thin","Mechanical fuels management",
                         "Fire-related treatment")))) + 
  geom_bar(stat="identity", width=0.8, position = "stack") + 
  ylab("Hectares") + xlab("Year") +
  labs(fill="Treatment classes") +
  scale_fill_manual(values=c("#999933","brown","blue")) + 
  # scale_fill_manual(values=c("#889977","brown","blue")) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black")
        ,axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) + 
  scale_x_discrete(expand = c(0, 0)) + 
  scale_y_continuous(labels = scales::comma, expand = c(0, 0)) +
  geom_hline(yintercept = 12739, linetype = "longdash", color = "black", linewidth = 0.75) +
  geom_hline(yintercept = 3730, linetype = "longdash", color = "brown", linewidth = 0.75) +
  geom_hline(yintercept = 3658, linetype = "dotdash", color = "black", linewidth = 0.75)
trt
# ggsave("C:\\Users\\kshive\\Documents\\UCB\\Projects\\In Progress\\State of the Sierra\\R files\\figures\\Treatment_thru_time_0803table_STACKED.jpg",
              # width = 7, height = 4)

SNsev.all_comp = SNsev.all_comp[,1:3]
SNsev.all_comp$DisturbanceType = "Severity"
SNtrt.all_comp$DisturbanceType = "Treatment"
SNtrt.sev1 = rbind(SNsev.all_comp,SNtrt.all_comp)
head(SNtrt.sev1)
write.csv(SNtrt.sev1,"C:\\Users\\kshive\\Documents\\UCB\\Projects\\In Progress\\State of the Sierra\\R files\\SoS_SeverityTreatment_annual.csv")

sev.trt.stats.all = SNtrt.sev1 %>%
  mutate(SevTrt_reclassBene = case_when(
    TrtSevClass == "Low severity"|TrtSevClass == "Moderate severity" ~ 
      "Beneficial_fire",
    TrtSevClass == "High severity" ~ "Nonbeneficial_fire",
    .default = "Active_treatment")) %>%
  group_by(SevTrt_reclassBene, Year) %>%
  summarise(Hectares = sum(Hectares)) %>% 
  pivot_wider(names_from = SevTrt_reclassBene, values_from = Hectares) %>%
  mutate(TotalHaBurned = Beneficial_fire + Nonbeneficial_fire,
         PercHigh = Nonbeneficial_fire/TotalHaBurned,
         TotalHaTreated = Active_treatment,
         PercTrt = TotalHaTreated/TotalHaBurned) %>%
  mutate(TrtExceed_AllFire = case_when(TotalHaTreated>TotalHaBurned ~ "Trt MORE than fire",
                                       .default ="Trt LESS than fire"),
        TrtExceed_GoodFire = case_when(TotalHaTreated>Beneficial_fire ~ "Trt Wins!",
                                           .default ="Good fire Wins!"
      ))
head(sev.trt.stats.all)    
View(sev.trt.stats.all)
table(sev.trt.stats.all$TrtExceed_AllFire)
table(sev.trt.stats.all$TrtExceed_GoodFire)
sum(sev.trt.stats.all$Active_treatment)


sev.trt.stats.percTrt = as.data.frame(tapply(sev.trt.stats.all$PercTrt, sev.trt.stats.all$Year, sum))
head(sev.trt.stats.percTrt)
View(sev.trt.stats.percTrt)

summary(sev.trt.stats.all$PercTrt)

#get mean and median annual area treated 
sev.trt.stats.TotalTrt = as.data.frame(tapply(sev.trt.stats.all$TotalHaTreated, 
                                              sev.trt.stats.all$Year, sum))
head(sev.trt.stats.TotalTrt)
median(sev.trt.stats.TotalTrt$`tapply(sev.trt.stats.all$TotalHaTreated, sev.trt.stats.all$Year, `)
mean(sev.trt.stats.TotalTrt$`tapply(sev.trt.stats.all$TotalHaTreated, sev.trt.stats.all$Year, `)
# View(sev.trt.stats.TotalTrt)

#get mean and median annual area burned
sev.trt.stats.TotalBurn = as.data.frame(tapply(sev.trt.stats.all$TotalHaBurned, 
                                              sev.trt.stats.all$Year, sum))
head(sev.trt.stats.TotalBurn)
median(sev.trt.stats.TotalBurn$`tapply(sev.trt.stats.all$TotalHaBurned, sev.trt.stats.all$Year, `)
mean(sev.trt.stats.TotalBurn$`tapply(sev.trt.stats.all$TotalHaBurned, sev.trt.stats.all$Year, `)


##Figure of severity and treatment through time
options(scipen = 999)
sev.trt = ggplot(SNtrt.sev1, aes(Year, Hectares,fill = factor(TrtSevClass, 
                levels = c("Commercial thin/Large hazard tree removal",
                "Mechanical fuels management","Fire-related treatment",
                "Low severity","Moderate severity",
                "High severity")))) + 
  geom_col(stat="identity", width=1, position = "dodge") + 
  ylab("Hectares") +
  labs(fill="Treatment and severity classes") + 
  scale_fill_manual(values=c("#999933","brown","blue",
                             "#525252","#969696","#cccccc")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), 
        axis.line = element_line(colour = "black")
        ,axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) + 
  scale_x_discrete(expand = c(0, 0)) + 
  scale_y_continuous(labels = scales::comma, expand = c(0, 0)) + 
  theme(legend.position = c(0.32, 0.75))
sev.trt

#stacked all togeher
sev.trt = ggplot(SNtrt.sev1, aes(Year, y = Hectares, fill = factor(TrtSevClass, 
                  levels = c("Low severity","Moderate severity",
                             "High severity","Commercial thin/Large hazard tree removal",
                             "Mechanical fuels management",
                             "Fire-related treatment")))) + 
  geom_bar(position = "stack", stat = "identity") + 
  ylab("Hectares") +
  labs(fill="") + 
  # scale_fill_manual(values=c("#fed98e","#fe9929","#cc4c02",
  #                            "#889977","brown","blue")) +
  # scale_fill_manual(values=c("#238443","#78c679","#c2e699",
  #                            "#889977","maroon","blue")) +
  scale_fill_manual(values=c("#cccccc","#969696","#525252",
                             "#666600","brown","blue")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), 
        axis.line = element_line(colour = "black")
        ,axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  scale_x_discrete(expand = c(0, 0)) + 
  scale_y_continuous(labels = scales::comma, expand = c(0, 0)) +
  theme(legend.position = c(0.25, 0.75))
sev.trt
# ggsave("C:\\Users\\kshive\\Documents\\UCB\\Projects\\In Progress\\State of the Sierra\\R files\\figures\\Treatment_AND_Severity_thru_time_0803table_STACKED.jpg",
#        width = 7, height = 4)


#stacked split treatment and severity
sev.trt = ggplot(SNtrt.sev1, aes(, x = DisturbanceType, y = Hectares, fill = factor(TrtSevClass, 
                                levels = c("Commercial thin/Large hazard tree removal",
                                       "Mechanical fuels management","Fire-related treatment",
                                       "Low severity","Moderate severity",
                                       "High severity")))) + 
  geom_bar(position = "stack", stat = "identity") + 
  ylab("Hectares") +
  labs(fill="Treatment and severity classes") + 
  scale_fill_manual(values=c("#999933","brown","blue",
                             "#525252","#969696","#cccccc")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), 
        axis.line = element_line(colour = "black")
        ,axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 1, hjust=1)) + 
  scale_x_discrete(expand = c(0, 0)) + 
  scale_y_continuous(labels = scales::comma, expand = c(0, 0)) + 
  # theme(legend.position = c(0.3, 0.75)) +
  facet_wrap("Year", scales = 'free')
sev.trt
# ggsave("C:\\Users\\kshive\\Documents\\UCB\\Projects\\In Progress\\State of the Sierra\\R files\\figures\\Treatment_and_Severity_byYear.jpg",
#        width = 4.2, height = 4)

#stacked split treatment and severity
sev.trt = ggplot(SNtrt.sev1, aes(x = Hectares, y = DisturbanceType, fill = factor(TrtSevClass, 
              levels = c("Commercial thin/Large hazard tree removal",
              "Mechanical fuels management","Fire-related treatment",
              "Low severity","Moderate severity",
              "High severity")))) + 
  geom_bar(position = "stack", stat = "identity") + 
  ylab("Hectares") +
  labs(fill="Treatment and Severity classes") + 
  scale_fill_manual(values=c("#999933","brown","blue",
                             "#525252","#969696","#cccccc")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), 
        axis.line = element_line(colour = "black")
        ,axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 1, hjust=1)) + 
  # scale_x_discrete(expand = c(0, 0)) + 
  # scale_y_continuous(labels = scales::comma, expand = c(0, 0)) + 
  # theme(legend.position = c(0.3, 0.75)) +
  facet_wrap("Year", ncol=3)
sev.trt
# ggsave("C:\\Users\\kshive\\Documents\\UCB\\Projects\\In Progress\\State of the Sierra\\R files\\figures\\Treatment_and_Severity_byYear.jpg",
#        width = 4.2, height = 4)

################################################################################################
##Add in landowner

#reclass landowners
SNtrt = SNtrt %>%
  mutate(Landowner = case_when(FTE == "No FTE LookUp" ~ "Private",
                               FTE == "Private Other" ~ "Private",
                               FTE == "Private/Other" ~ "Private",
                               FTE == "Private Timber" ~ "Private",
                               FTE == "Non Profit" ~ "Private",
                               FTE == "Recreation/Parks District" ~ "City/County Agency",
                               FTE == "City Agency" ~ "City/County Agency",
                               FTE == "County Agency" ~ "City/County Agency",
                               FTE == "Irrigation District" ~ "State Agency",
                               FTE == "Improvement District" ~ "City/County Agency",
                               .default = as.character(FTE)))
head(SNtrt)

##Treatment by agency and year
SNtrt.lo = SNtrt %>%
  mutate(TrtSevClass = Treatment) %>%
  filter(TrtSevClass!="EXCLUDE"&TrtSevClass!="No Treatment") %>%
  dplyr::select(-Treatment) %>%
  group_by(Year,Landowner,TrtSevClass) %>%
  summarise(Hectares = sum(Ha))
# %>%
#   pivot_wider(names_from = TrtSevClass, values_from = Acreage, values_fill = 0)
head(SNtrt.lo)

#get complete cases
SNtrt.lo$Year1 = as.factor(SNtrt.lo$Year)
SNtrt.lo$Landowner1 = as.factor(SNtrt.lo$Landowner)
head(SNtrt.lo)
SNtrt.lo = SNtrt.lo[,c(3:6)]
SNtrt.lo$Year = SNtrt.lo$Year1
SNtrt.lo$Landowner = SNtrt.lo$Landowner1
head(SNtrt.lo)
SNtrt.lo = SNtrt.lo[,c(5,6,1,2)]
head(SNtrt.lo)

SNtrt.lo_comp = SNtrt.lo %>%
  complete(Year,TrtSevClass,Landowner) %>%
  mutate(across(4, replace_na, 0))
head(SNtrt.lo_comp)

#get total treated Hectares for entire observation period 
acTRT = SNtrt.lo_comp %>%
  group_by(Landowner) %>%
  summarise(HaTreated = sum(Hectares))
acTRT

#get total fire-treated Hectares for entire observation period 
acTRT.fire = SNtrt.lo_comp %>%
  filter(TrtSevClass!="Commercial thin/Large hazard tree removal") %>%
  group_by(Landowner) %>%
  summarise(FireHaTreated = sum(Hectares))
acTRT.fire

#get total agency Hectares they manage
SNtrt.lo.all = SNtrt %>%
  mutate(TrtSevClass = Treatment) %>%
  dplyr::select(-Treatment) %>%
  filter(Year == 2001) %>%
  group_by(Landowner) %>%
  summarise(HaOwned = sum(Ha))
SNtrt.lo.all

#get percent of land treated relative to land area managed 
SNtrt.lo.all.perc = merge(acTRT,SNtrt.lo.all,by="Landowner")
SNtrt.lo.all.perc$percManagedOfOwnership = SNtrt.lo.all.perc$HaTreated/
  SNtrt.lo.all.perc$HaOwned
# View(SNtrt.lo.all.perc)
SNtrt.lo.all.perc

#get percent of land treated with a fire related treatment relative to land area managed 
SNtrt.lo.all.perc.fire = merge(acTRT.fire,SNtrt.lo.all,by="Landowner")
SNtrt.lo.all.perc.fire$percManagedOfOwnership = SNtrt.lo.all.perc.fire$FireHaTreated/
  SNtrt.lo.all.perc.fire$HaOwned
(SNtrt.lo.all.perc.fire)

#
SNtrt.lo_comp.all = SNtrt.lo_comp %>%
  group_by(Landowner,TrtSevClass) %>%
  summarise(TreatedHa = sum(Hectares))
SNtrt.lo_comp.all

SNtrt.lo_comp.all.perc = merge(SNtrt.lo_comp.all,SNtrt.lo.all,by="Landowner")
head(SNtrt.lo_comp.all.perc)

SNtrt.lo_comp.all.perc$PercentHa = SNtrt.lo_comp.all.perc$TreatedHa/SNtrt.lo_comp.all.perc$HaOwned

#query out landowners with a total of more than 2,000 Hectares
SNtrt.lo.redu = SNtrt.lo_comp %>%
  filter(Landowner %in% c("United States National Park Service",
                          "United States Forest Service","Private")) %>%
  mutate(LandownerInit = case_when(Landowner == "United States National Park Service" ~ "NPS",
                                   Landowner == "United States Forest Service" ~ "USFS",
                                   .default = Landowner))
head(SNtrt.lo.redu)

#trt by agencies
trt.lo = ggplot(SNtrt.lo.redu, aes(Year, Hectares,fill = factor(TrtSevClass, 
                        levels = c("Commercial thin/Large hazard tree removal",
                        "Mechanical fuels management","Fire-related treatment")))) + 
  geom_col(stat="identity", width=0.5, position = "dodge") + 
  facet_grid("LandownerInit", scales = "free_y") +
  ylab("Hectares") +
  labs(fill="Treatment classes") + 
  scale_fill_manual(values=c("#999933","brown","blue")) +
  theme(panel.grid.major = element_blank(), panel.background = element_blank(), 
        axis.line = element_line(),
        axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) + 
  scale_x_discrete(expand = c(0, 0)) + 
  scale_y_continuous(labels = scales::comma, expand = c(0, 0)) + 
  theme(legend.position = c(0.7, 0.93),legend.key.size = unit(0.1, 'cm')) +
  geom_hline(aes(yintercept=-Inf)) + 
  coord_cartesian(clip="off")
trt.lo

#trt by agencies - stacked
SNtrt.lo.redu$LandownerInit_f = factor(SNtrt.lo.redu$LandownerInit, levels = c("USFS","Private","NPS"))
trt.lo = ggplot(SNtrt.lo.redu, aes(Year, Hectares,fill = factor(TrtSevClass, 
                             levels = c("Commercial thin/Large hazard tree removal",
                                    "Mechanical fuels management","Fire-related treatment")))) + 
  geom_col(stat="identity", width=0.5, position = "stack") + 
  facet_grid("LandownerInit_f", scales = "free_y") +
  ylab("Hectares") +
  labs(fill="Treatment classes") + 
  scale_fill_manual(values=c("#999933","brown","blue")) +
  theme(panel.grid.major = element_blank(), panel.background = element_blank(), 
        axis.line = element_line(),
        axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) + 
  scale_x_discrete(expand = c(0, 0)) + 
  scale_y_continuous(labels = scales::comma, expand = c(0, 0)) + 
  theme(legend.position = c(0.75, 0.26),legend.key.size = unit(0.3, 'cm'),
        legend.title = element_blank(), legend.text = element_text(size=6)) +
  geom_hline(aes(yintercept=-Inf)) + 
  coord_cartesian(clip="off")
trt.lo
ggsave("C:\\Users\\kshive\\Documents\\UCB\\Projects\\In Progress\\State of the Sierra\\R files\\figures\\Treatment_by_Agency.jpg",
       width = 5, height = 4.5)

#trt by agencies, fire-related treatments and mech fuels only
SNtrt.lo.redu.fuels = SNtrt.lo.redu %>%
  filter(TrtSevClass == "Fire-related treatment"|TrtSevClass == "Mechanical fuels management")
trt.lo = ggplot(SNtrt.lo.redu.fuels, aes(Year, Hectares, fill = factor(TrtSevClass,
                                          levels = c("Fire-related treatment","Mechanical fuels management")))) + 
  geom_col(stat="identity", width=0.75, position = "dodge") + 
  # facet_grid("LandownerInit", scales = "free_y") +
  facet_grid("LandownerInit") +
  ylab("Hectares") +
  scale_fill_manual(values=c("brown","blue")) +
  theme(panel.grid.major = element_blank(), panel.background = element_blank(), 
        axis.line = element_line(),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 5),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size = 5),
        axis.text.y = element_text(size = 5)) + 
  scale_x_discrete(expand = c(0, 0)) + 
  scale_y_continuous(labels = scales::comma, expand = c(0, 0)) + 
  theme(legend.position = c(0.715, 0.95),legend.key.size = unit(0.2, 'cm'), legend.text=element_text(size=4),
        legend.title=element_text(size=4)) +
  labs(fill="Treatment classes") +
  geom_hline(aes(yintercept=-Inf)) + 
  coord_cartesian(clip="off") + 
  theme(strip.text.y = element_text(size = 6))
trt.lo
ggsave("C:\\Users\\kshive\\Documents\\UCB\\Projects\\In Progress\\State of the Sierra\\R files\\figures\\Mech&FireTreatments_by_Agency.jpg",
       width = 2.5, height = 4)

#trt by agencies, commercial only
SNtrt.lo.redu.comm = SNtrt.lo.redu %>%
  filter(TrtSevClass == "Commercial thin/Large hazard tree removal"&
           LandownerInit!="NPS")
trt.lo = ggplot(SNtrt.lo.redu.comm, aes(Year, Hectares, fill = TrtSevClass)) + 
  geom_col(stat="identity", width=0.3, position = "dodge") + 
  # facet_grid("LandownerInit", scales = "free_y") +
  facet_grid("LandownerInit") +
  ylab("Hectares") +
  scale_fill_manual(values=c("#889977")) +
  theme(panel.grid.major = element_blank(), panel.background = element_blank(), 
        axis.line = element_line(),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 5),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size = 5),
        axis.text.y = element_text(size = 5)) + 
  theme(legend.position = c(0.38, 0.95),legend.key.size = unit(0.2, 'cm'), legend.text=element_text(size=4),
        legend.title=element_text(size=4)) +
  labs(fill="Treatment classes") +
  geom_hline(aes(yintercept=-Inf)) + 
  coord_cartesian(clip="off") + 
  theme(strip.text.y = element_text(size = 6)) + ylim(0,30000) +
  scale_x_discrete(expand = c(0, 0)) + 
  scale_y_continuous(labels = scales::comma, expand = c(0, 0)) 
  trt.lo
ggsave("C:\\Users\\kshive\\Documents\\UCB\\Projects\\In Progress\\State of the Sierra\\R files\\figures\\CommTreatments_by_Agency.jpg",
       width = 2.5, height = 4)


# severity by agency and year
SNsev.lo = SNsev %>%
  mutate(Landowner = case_when(FTE == "No FTE LookUp" ~ "Private",
                               FTE == "Private Other" ~ "Private",
                               FTE == "Private/Other" ~ "Private",
                               FTE == "Private Timber" ~ "Private",
                               FTE == "Non Profit" ~ "Private",
                               FTE == "Recreation/Parks District" ~ "City/County Agency",
                               FTE == "City Agency" ~ "City/County Agency",
                               FTE == "County Agency" ~ "City/County Agency",
                               FTE == "Irrigation District" ~ "State Agency",
                               FTE == "Improvement District" ~ "City/County Agency",
                               .default = as.character(FTE))) %>%
  mutate(TrtSevClass = Severity.Category) %>%
  dplyr::select(-Severity.Category) %>%
  filter(TrtSevClass!="Unburned") %>%
  group_by(Year,Landowner,TrtSevClass) %>%
  summarise(Hectares = sum(Ha)) 
head(SNsev.lo)

#get complete cases
SNsev.lo$Year1 = as.factor(SNsev.lo$Year)
SNsev.lo$Landowner1 = as.factor(SNsev.lo$Landowner)
head(SNsev.lo)
SNsev.lo = SNsev.lo[,c(3:6)]
SNsev.lo$Year = SNsev.lo$Year1
SNsev.lo$Landowner = SNsev.lo$Landowner1
head(SNsev.lo)
SNsev.lo = SNsev.lo[,c(5,6,1,2)]
head(SNsev.lo)

SNsev.lo_comp = SNsev.lo %>%
  complete(Year,TrtSevClass,Landowner) %>%
  mutate(across(4, replace_na, 0))
head(SNsev.lo_comp)

#get total Hectares for entire study period by agency
acSEV = SNsev.lo_comp %>%
  group_by(Landowner) %>%
  summarise(Hectares = sum(Hectares))
acSEV

#query out landowners with a total of more than 2,000 Hectares
SNsev.lo.redu = SNsev.lo_comp %>%
  filter(Landowner %in% c("United States National Park Service","United States Forest Service",
                          "Private")) %>%
  mutate(LandownerInit = case_when(Landowner == "United States National Park Service" ~ "NPS",
                                   Landowner == "United States Forest Service" ~ "USFS", .default = Landowner))
head(SNsev.lo.redu)

#get percent area burned by high severity by owner
SNsev.lo.redu.percHi = SNsev.lo.redu %>%
  mutate(Tsc = gsub(" ","",TrtSevClass)) %>%
  select(-TrtSevClass,-Landowner) %>%
  pivot_wider(names_from = Tsc, values_from = Hectares,values_fill = 0) %>%
  mutate(AreaBurned = Lowseverity+Moderateseverity+Highseverity) %>%
  mutate(percHi = Highseverity/AreaBurned)
SNsev.lo.redu.percHi

SNsev.lo.redu.percHi.all = SNsev.lo.redu.percHi %>%
  group_by(LandownerInit) %>%
  summarise(High_HaBurned = sum(Highseverity), 
         HaBurned = sum(AreaBurned),
         percHi.all = High_HaBurned/HaBurned)  
SNsev.lo.redu.percHi.all

sev.lo = ggplot(SNsev.lo.redu, aes(Year, Hectares,fill = factor(TrtSevClass, 
                     levels = c("Low severity",
                                "Moderate severity",
                                "High severity")))) + 
  geom_col(stat="identity", width=0.5, position = "stack") + 
  facet_grid("LandownerInit", scales = "free_y" ) +
  ylab("Hectares") +
  labs(fill="") +
  scale_fill_manual(values=c("#525252","#969696","#cccccc")) +
  theme(panel.grid.major = element_blank(), panel.background = element_blank(), 
        axis.line = element_line(),
        axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) + 
  scale_x_discrete(expand = c(0, 0)) + 
  scale_y_continuous(labels = scales::comma, expand = c(0, 0)) +
  theme(legend.position = c(0.87, 0.99),legend.key.size = unit(0.2, 'cm'), legend.text = element_text(size = 6),
        legend.title = element_text()) +
  geom_hline(aes(yintercept=-Inf)) + 
  coord_cartesian(clip="off")
sev.lo
ggsave("C:\\Users\\kshive\\Documents\\UCB\\Projects\\In Progress\\State of the Sierra\\R files\\figures\\Severity_by_Agency.jpg",
       width = 5, height = 4.5)


###combine by agency and disturbance
SNtrt.sev.lo = rbind(SNsev.lo,SNtrt.lo)
head(SNtrt.sev.lo)

SNtrt.sev.lo.redu = SNtrt.sev.lo %>%
  filter(Landowner=="Private"|
           Landowner=="United States National Park Service"|
           Landowner=="United States Forest Service")
head(SNtrt.sev.lo.redu)

sev.trt = ggplot(SNtrt.sev.lo.redu, aes(Year, Hectares,fill = factor(TrtSevClass, 
               levels = c("Commercial thin/Large hazard tree removal",
                          "Mechanical fuels management","Fire-related treatment",
                          "Low severity","Moderate severity",
                           "High severity")))) + 
  geom_col(stat="identity", width=1, position = "dodge") + 
  facet_grid("Landowner") +
  ylab("Hectares") +
  labs(fill="Treatment and severity classes") + 
  scale_fill_manual(values=c("#999933","brown","blue",
                             "#525252","#969696","#cccccc")) +
  scale_y_continuous(labels = scales::comma) +
  theme_bw()
sev.trt
# ggsave("C:\\Users\\kshive\\Documents\\UCB\\Projects\\In Progress\\State of the Sierra\\R files\\figures\\Severity_Treatment_by_Agency.jpg",
#        width = 4.2, height = 4)

#stacked

sev.trt = ggplot(SNtrt.sev.lo.redu, aes(Year, Hectares,fill = factor(TrtSevClass, 
                                levels = c("High severity","Moderate severity",
                                           "Low severity",
                                           "Fire-related treatment","Mechanical fuels management",
                                           "Commercial thin/Large hazard tree removal")))) + 
  geom_col(stat="identity", width=0.7) + 
  facet_grid("Landowner") +
  ylab("Hectares") +
  labs(fill="Treatment and severity classes") + 
  scale_fill_manual(values=c("#cccccc","#969696","#525252",
                             "blue","brown","#999933")) +
  theme(panel.grid.major = element_blank(), panel.background = element_blank(), 
        axis.line = element_line(),
        axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) + 
  scale_x_discrete(expand = c(0, 0)) + 
  scale_y_continuous(labels = scales::comma, expand = c(0, 0))
sev.trt
# ggsave("C:\\Users\\kshive\\Documents\\UCB\\Projects\\In Progress\\State of the Sierra\\R files\\figures\\Treatment_by_Agency.jpg",
#        width = 4.2, height = 4)

