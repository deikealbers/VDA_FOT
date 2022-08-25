#### preparations ####
rm(list = ls())
library(tidyverse)
library(skimr)
library(ggplot2)
library(dplyr)
library(rstatix)
library(skimr)

# library(ggpubr)
# library(reshape2)
# library(moments)
# library(ggpmisc)
setwd("~/R/VDA_FOT")

#### write function for summary ####
# adjusted from skim() function
mean_FOT = function(x) mean(x, na.rm = TRUE)
FOT_skim <- skim_with(numeric = sfl(mean = mean_FOT), append = FALSE)

#### notes on colors ####
## Sekundaerfarben
#005293 --- dunkelblau
#64A0C8 --- mittelblau
#98C6EA --- hellblau
#999999 --- grau

## Akzentfarben
#E37222 --- orange
#A2AD00 --- gruen
#DAD7CB --- taupe

#### write function for summary stats ####
fun_mean <- function(x){ return(data.frame(y=mean(x),label=round(mean(x,na.rm=T), 2)))}
fun_median <- function(x){ return(data.frame(y=median(x),label=round(median(x,na.rm=T), 2)))}

#### import data ####
# Read in files
vorbefragung <- read.csv("data/preprocessed/vorbefragung_scores.csv", encoding = "UTF-8")
nachbefragung <- read.csv("data/preprocessed/nachbefragung_scores.csv", encoding = "UTF-8")

#### adjust data set for visualization ####
nachbefragung <- nachbefragung %>%
  rename(group = X.U.FEFF.group) %>%
  mutate(group = factor(group, levels = c("A", "B"), ordered = TRUE)) %>%
  mutate(interval = factor(interval, levels = c("A_on_fc", "A_on_fam", "A_off", "B_off"), ordered = TRUE)) %>%
  mutate(SubjUeberwachguete.1. = factor(SubjUeberwachguete.1., ordered = TRUE)) %>%
  mutate(SubjEinflussSetting = factor(SubjEinflussSetting, ordered = TRUE)) %>%
  mutate(Ranking = factor(Ranking, ordered = TRUE))

vorbefragung <- vorbefragung %>%
  rename(group = X.U.FEFF.group) %>%
  mutate(Fahrtfrequenz = factor(Fahrtfrequenz, levels = c("0", "1", "2", "3", "4"), ordered = TRUE)) %>%
  mutate(Fahrtstrecke = factor(Fahrtstrecke, levels = c("0", "1", "2", "3", "4", "5"), ordered = TRUE)) %>%
  mutate(FrequenzAutobahn = factor(FrequenzAutobahn, levels = c("0", "1", "2", "3", "4"), ordered = TRUE)) %>%
  mutate(StreckeAutobahn = factor(StreckeAutobahn, levels = c("0", "1", "2", "3", "4", "5"), ordered = TRUE)) %>%
  mutate(across(starts_with("KenntnisAS."), ~factor(., levels = c("0", "1", "2", "3"), ordered = TRUE)))


################## post questionnaire ###############################################################

#### plot TiA overall + subscales ####
TiA_overall <- nachbefragung %>%
  select(interval, VPNr, TiA_overall) %>%
  rename(score = TiA_overall) %>%
  add_column(scale = "TiA_overall", .after = "VPNr")
TiA_RC <- nachbefragung %>%
  select(interval, VPNr, TiA_RC) %>%
  rename(score = TiA_RC) %>%
  add_column(scale = "TiA_RC", .after = "VPNr")
TiA_UP <- nachbefragung %>%
  select(interval, VPNr, TiA_UP) %>%
  rename(score = TiA_UP) %>%
  add_column(scale = "TiA_UP", .after = "VPNr")
TiA_F <- nachbefragung %>%
  select(interval, VPNr, TiA_F) %>%
  rename(score = TiA_F) %>%
  add_column(scale = "TiA_F", .after = "VPNr")
TiA_IoD <- nachbefragung %>%
  select(interval, VPNr, TiA_IoD) %>%
  rename(score = TiA_IoD) %>%
  add_column(scale = "TiA_IoD", .after = "VPNr")
TiA_PtT <- nachbefragung %>%
  select(interval, VPNr, TiA_PtT) %>%
  rename(score = TiA_PtT) %>%
  add_column(scale = "TiA_PtT", .after = "VPNr")
TiA_TiA <- nachbefragung %>%
  select(interval, VPNr, TiA_TiA) %>%
  rename(score = TiA_TiA) %>%
  add_column(scale = "TiA_TiA", .after = "VPNr")

TiA <- bind_rows(TiA_overall, TiA_RC, TiA_UP, TiA_F, TiA_IoD, TiA_PtT, TiA_TiA) %>%
  mutate(scale = factor(scale, levels = c("TiA_overall", "TiA_RC", "TiA_UP", "TiA_F", "TiA_IoD", "TiA_PtT", "TiA_TiA"), ordered = TRUE))

labels_TiA = c("Overall", "Reliability/Competence", "Understanding/Predictability", 
               "Familiarity", "Intention of Developers", "Propensity to Trust", "Trust in Automation")

plot_TiA_all_scales <- ggplot(TiA, aes(x=scale, y=score, fill=scale)) + 
  geom_violin() +
  ylim(1, 5) +
  stat_summary(fun = median, geom="point",colour="black", size=1) +
  stat_summary(fun.data = fun_median, geom="text", vjust=-.55, size=3.3) +
  scale_x_discrete(labels= labels_TiA) +
  facet_wrap(~interval) +
  scale_fill_manual(values = c("#999999", "#DAD7CB", "#DAD7CB", "#DAD7CB", "#DAD7CB", "#DAD7CB", "#DAD7CB")) +
  labs(y="Score", x="",
       title = "Trust in Automation - overall score & subscales") +
  theme_bw() +
  theme(text=element_text(family = "sans", color="black", size=12),
        panel.grid.minor.y = element_blank(), 
        legend.position = "none", 
        axis.text.x=element_text(color = "black", size=10, angle=30, vjust=.8, hjust=0.8))
plot_TiA_all_scales

ggsave(filename = "data/results/figures/TiA.png", plot_TiA_all_scales, 
       dpi = 300, width = 8, height = 5, units = "in", device='png')

rm(list = setdiff(ls(), c("vorbefragung", "nachbefragung", "fun_mean", "fun_median", "mean_FOT", "FOT_skim", 
                          "plot_TiA_all_scales", "TiA")))

#### plot CTAM subscales ####
CTAM_PE <- nachbefragung %>%
  select(interval, VPNr, CTAM_PE) %>%
  rename(score = CTAM_PE) %>%
  add_column(scale = "CTAM_PE", .after = "VPNr")
CTAM_EE <- nachbefragung %>%
  select(interval, VPNr, CTAM_EE) %>%
  rename(score = CTAM_EE) %>%
  add_column(scale = "CTAM_EE", .after = "VPNr")
CTAM_ATT <- nachbefragung %>%
  select(interval, VPNr, CTAM_ATT) %>%
  rename(score = CTAM_ATT) %>%
  add_column(scale = "CTAM_ATT", .after = "VPNr")
CTAM_FC <- nachbefragung %>%
  select(interval, VPNr, CTAM_FC) %>%
  rename(score = CTAM_FC) %>%
  add_column(scale = "CTAM_FC", .after = "VPNr")
CTAM_ITU <- nachbefragung %>%
  select(interval, VPNr, CTAM_ITU) %>%
  rename(score = CTAM_ITU) %>%
  add_column(scale = "CTAM_ITU", .after = "VPNr")
CTAM_PS <- nachbefragung %>%
  select(interval, VPNr, CTAM_PS) %>%
  rename(score = CTAM_PS) %>%
  add_column(scale = "CTAM_PS", .after = "VPNr")

CTAM <- bind_rows(CTAM_PE, CTAM_EE, CTAM_ATT, CTAM_FC, CTAM_ITU, CTAM_PS) %>%
  mutate(scale = factor(scale, levels = c("CTAM_PE", "CTAM_EE", "CTAM_ATT", "CTAM_FC", "CTAM_ITU", "CTAM_PS"), ordered = TRUE))

labels_CTAM = c("Performance \n Expectancy", "Effort Expectancy", "Attitude towards \n using Technology", 
                "Facilitating Conditions", "Behavioral Intention  \n to use the System", "Perceived Safety")

plot_CTAM_all_scales <- ggplot(CTAM, aes(x=scale, y=score, fill=scale)) + 
  geom_violin() +
  ylim(1, 7) +
  stat_summary(fun = median, geom="point",colour="black", size=1) +
  stat_summary(fun.data = fun_median, geom="text", vjust=1.40, size=3.3) +
  scale_x_discrete(labels= labels_CTAM) +
  facet_wrap(~interval) +
  scale_fill_manual(values = c("#DAD7CB", "#DAD7CB", "#DAD7CB", "#DAD7CB", "#DAD7CB", "#DAD7CB")) +
  labs(y="Score", x="",
       title = "Car Technology Acceptance Model - subscales") +
  theme_bw() +
  theme(text=element_text(family = "sans", color="black", size=12),
        panel.grid.minor.y = element_blank(), 
        legend.position = "none", 
        axis.text.x=element_text(color = "black", size=10, angle=30, vjust=.9, hjust=0.8))
plot_CTAM_all_scales

ggsave(filename = "data/results/figures/CTAM.png", plot_CTAM_all_scales, 
       dpi = 300, units = "in", width = 8, height = 5, device='png')

rm(list = setdiff(ls(), c("vorbefragung", "nachbefragung", "fun_mean", "fun_median", "mean_FOT", "FOT_skim", 
                          "plot_TiA_all_scales", "TiA",
                          "plot_CTAM_all_scales", "CTAM")))

#### plot NDRTs ####
NDRT_1 <- nachbefragung %>%
  select(interval, VPNr, NDRTs.NDRT1.) %>%
  rename(score = NDRTs.NDRT1.) %>%
  add_column(scale = "NDRT_1", .after = "VPNr")
NDRT_2 <- nachbefragung %>%
  select(interval, VPNr, NDRTs.NDRT2.) %>%
  rename(score = NDRTs.NDRT2.) %>%
  add_column(scale = "NDRT_2", .after = "VPNr")
NDRT_3 <- nachbefragung %>%
  select(interval, VPNr, NDRTs.NDRT3.) %>%
  rename(score = NDRTs.NDRT3.) %>%
  add_column(scale = "NDRT_3", .after = "VPNr")
NDRT_4 <- nachbefragung %>%
  select(interval, VPNr, NDRTs.NDRT4.) %>%
  rename(score = NDRTs.NDRT4.) %>%
  add_column(scale = "NDRT_4", .after = "VPNr")
NDRT_5 <- nachbefragung %>%
  select(interval, VPNr, NDRTs.NDRT5.) %>%
  rename(score = NDRTs.NDRT5.) %>%
  add_column(scale = "NDRT_5", .after = "VPNr")
NDRT_6 <- nachbefragung %>%
  select(interval, VPNr, NDRTs.NDRT6.) %>%
  rename(score = NDRTs.NDRT6.) %>%
  add_column(scale = "NDRT_6", .after = "VPNr")
NDRT_7 <- nachbefragung %>%
  select(interval, VPNr, NDRTs.NDRT7.) %>%
  rename(score = NDRTs.NDRT7.) %>%
  add_column(scale = "NDRT_7", .after = "VPNr")
NDRT_8 <- nachbefragung %>%
  select(interval, VPNr, NDRTs.NDRT8.) %>%
  rename(score = NDRTs.NDRT8.) %>%
  add_column(scale = "NDRT_8", .after = "VPNr")

NDRT <- bind_rows(NDRT_1, NDRT_2, NDRT_3, NDRT_4, NDRT_5, NDRT_6, NDRT_7, NDRT_8) %>%
  mutate(scale = factor(scale, levels = c("NDRT_1", "NDRT_2", "NDRT_3", "NDRT_4", "NDRT_5", "NDRT_6", "NDRT_7", "NDRT_8"), ordered = TRUE))

labels_NDRT = c("Mobile device \n in hand - handling", "Mobile device \n in hand - talking", 
                "Mobile device \n fixated - speaking", "Vehicle related inputs",
                "Eating/drinking/smoking", "Grooming", 
                "Interaction with passengers", "Searching/grabbing/rummaging")

plot_NDRT_all_scales <- ggplot(NDRT, aes(x=scale, y=score, fill=scale)) + 
  geom_violin() +
  ylim(0, 5) +
  stat_summary(fun = median, geom="point",colour="black", size=1) +
  stat_summary(fun.data = fun_median, geom="text", vjust=-.55, size=3.3) +
  scale_x_discrete(labels= labels_NDRT) +
  facet_wrap(~interval) +
  scale_fill_manual(values = c("#DAD7CB", "#DAD7CB", "#DAD7CB", "#DAD7CB", "#DAD7CB", "#DAD7CB", "#DAD7CB", "#DAD7CB")) +
  labs(y="Score", x="",
       title = "Non-driving related tasks") +
  theme_bw() +
  theme(text=element_text(family = "sans", color="black", size=12),
        panel.grid.minor.y = element_blank(), 
        legend.position = "none", 
        axis.text.x=element_text(color = "black", size=10, angle=45, vjust=.85, hjust=0.8))
plot_NDRT_all_scales

ggsave(filename = "data/results/figures/NDRT.png", plot_NDRT_all_scales, 
       dpi = 300, units = "in", width = 8, height = 5, device='png')

rm(list = setdiff(ls(), c("vorbefragung", "nachbefragung", "fun_mean", "fun_median", "mean_FOT", "FOT_skim", 
                          "plot_TiA_all_scales", "TiA",
                          "plot_CTAM_all_scales", "CTAM",
                          "plot_NDRT_all_scales", "NDRT")))

#### plot L2components ####
L2u_gen <- nachbefragung %>%
  filter(interval != "A_on_fc") %>%
  select(interval, VPNr, L2PrivNutzung) %>%
  rename(score = L2PrivNutzung) %>%
  add_column(scale = "L2u_gen", .after = "VPNr")
L2u_long <- nachbefragung %>%
  filter(interval != "A_on_fc") %>%
  select(interval, VPNr, L2Komponenten.Laengs.) %>%
  rename(score = L2Komponenten.Laengs.) %>%
  add_column(scale = "L2u_long", .after = "VPNr")
L2u_lat <- nachbefragung %>%
  filter(interval != "A_on_fc") %>%
  select(interval, VPNr, L2Komponenten.Quer.) %>%
  rename(score = L2Komponenten.Quer.) %>%
  add_column(scale = "L2u_lat", .after = "VPNr")
L2u_hoff <- nachbefragung %>%
  filter(interval != "A_on_fc") %>%
  select(interval, VPNr, L2Komponenten.Hoff.) %>%
  rename(score = L2Komponenten.Hoff.) %>%
  add_column(scale = "L2u_hoff", .after = "VPNr")

L2compo <- bind_rows(L2u_gen, L2u_long, L2u_lat, L2u_hoff) %>%
  mutate(scale = factor(scale, levels = c("L2u_gen", "L2u_long", "L2u_lat", "L2u_hoff"), 
                        ordered = TRUE))

labels_L2compo = c("overall", "longitudinal", 
                   "lateral", "h-off")

plot_L2components <- ggplot(L2compo, aes(x=scale, y=score, fill=scale)) + 
  geom_violin() +
  ylim(0, 4) +
  stat_summary(fun = median, geom="point",colour="black", size=1) +
  stat_summary(fun.data = fun_median, geom="text", vjust=1.2, size=3.3) +
  scale_x_discrete(labels= labels_L2compo) +
  facet_wrap(~interval) +
  scale_fill_manual(values = c("#999999", "#DAD7CB", "#DAD7CB", "#DAD7CB")) +
  labs(y="Score", x="",
       title = "L2 intention to use - overall & specific components") +
  theme_bw() +
  theme(text=element_text(family = "sans", color="black", size=12),
        panel.grid.minor.y = element_blank(), 
        legend.position = "none", 
        axis.text.x=element_text(color = "black", size=10, angle=30, vjust=.8, hjust=0.8))
plot_L2components

ggsave(filename = "data/results/figures/L2components.png", plot_L2components, 
       dpi = 300, units = "in", width = 8, height = 3, device='png')

rm(list = setdiff(ls(), c("vorbefragung", "nachbefragung", "fun_mean", "fun_median", "mean_FOT", "FOT_skim", 
                          "plot_TiA_all_scales", "TiA",
                          "plot_CTAM_all_scales", "CTAM",
                          "plot_NDRT_all_scales", "NDRT",
                          "plot_L2components", "L2compo")))

#### plot system understanding ####
plot_SystemUnderstanding <- ggplot(nachbefragung, aes(x = interval, y=100 * System_sum)) +
  geom_violin(fill = "#DAD7CB") +
  stat_summary(fun = mean, geom="point",colour="black", size=1) +
  stat_summary(fun.data = fun_mean, geom="text", vjust=-.55, size=3.3) +
  labs(y="Correct answers [%]", x="",
       title = "System understanding") +
  theme_bw() +
  theme(text=element_text(family = "sans", color="black", size=12),
        panel.grid.minor.y = element_blank(), 
        legend.position = "none", 
        axis.text.x=element_text(color = "black", size=10, angle=0, vjust=.8, hjust=0.8))
plot_SystemUnderstanding

ggsave(filename = "data/results/figures/SystemUnderstanding.png", plot_SystemUnderstanding, 
       dpi = 300, units = "in", width = 4, height = 3, device='png')

SystemUnderstanding_singleQ_means <- nachbefragung %>%
  select(interval, starts_with("System0"), starts_with("System1")) %>%
  group_by(interval) %>%
  FOT_skim() %>%
  select(interval, skim_variable, numeric.mean) %>%
  rename(question = skim_variable) %>%
  rename(mean = numeric.mean)

plot_SystemUnderstanding_singleQ <- ggplot(SystemUnderstanding_singleQ_means, aes(x = question, y=mean*100)) +
  geom_line(aes(group = interval)) +
  geom_point() +
  stat_summary(fun = mean, geom="point",colour="black", size=1) +
  stat_summary(fun.data = fun_mean, geom="text", vjust=1.8, size=3.3) +
  labs(y="Correct answers [%]", x="",
       title = "System understanding - single questions") +
  facet_grid(interval ~.) +
  ylim(0, 100) +
  theme_bw() +
  theme(text=element_text(family = "sans", color="black", size=12),
        panel.grid.minor.y = element_blank(), 
        legend.position = "none", 
        axis.text.x=element_text(color = "black", size=10, angle=30, vjust=.8, hjust=0.8))
plot_SystemUnderstanding_singleQ

ggsave(filename = "data/results/figures/SystemUnderstanding_singleQ.png", plot_SystemUnderstanding_singleQ, 
       dpi = 300, units = "in", width = 8, height = 5, device='png')

rm(list = setdiff(ls(), c("vorbefragung", "nachbefragung", "fun_mean", "fun_median", "mean_FOT", "FOT_skim", 
                          "plot_TiA_all_scales", "TiA",
                          "plot_CTAM_all_scales", "CTAM",
                          "plot_NDRT_all_scales", "NDRT",
                          "plot_L2components", "L2compo", 
                          "plot_SystemUnderstanding", "plot_SystemUnderstanding_singleQ")))


#### plot monitoring (subj. ueberwachungsguete) ####
labels_monitoring <- c("0 - inattentive", "1", "2", "3", "4", "5", "6 - always attentive")

plot_monitoring <- ggplot(nachbefragung, aes(x = SubjUeberwachguete.1.)) +
  geom_bar(fill = "#DAD7CB", color = "black") +
  facet_grid(~interval) +
  xlim(-0.5,6.5) +
  scale_x_discrete(limit = c("0", "1", "2", "3", "4", "5", "6"), labels = labels_monitoring) + # NA removed
  labs(y="n", x="",
       title = "Subjective rating of monitoring performance") +
  geom_text(stat='count', aes(label=..count..), position=position_dodge2(width=0.9, preserve=c("single")), vjust=-0.3) +
  theme_bw() +
  theme(text=element_text(family = "sans", color="black", size=12),
        panel.grid.minor.y = element_blank(), 
        legend.position = "none", 
        axis.text.x=element_text(color = "black", size=10, angle=40, vjust=1, hjust=0.9))
plot_monitoring

ggsave(filename = "data/results/figures/SubjMonitoringPerformance.png", plot_monitoring, 
       dpi = 300, units = "in", width = 8, height = 5, device='png')

rm(list = setdiff(ls(), c("vorbefragung", "nachbefragung", "fun_mean", "fun_median", "mean_FOT", "FOT_skim", 
                         "plot_TiA_all_scales", "TiA",
                         "plot_CTAM_all_scales", "CTAM",
                         "plot_NDRT_all_scales", "NDRT",
                         "plot_L2components", "L2compo", 
                         "plot_monitoring")))
#### plot influence setting ####
labels_setting <- c("affected", "not affected")

plot_setting <- ggplot(nachbefragung, aes(x = SubjEinflussSetting)) +
  geom_bar(fill = "#DAD7CB", color = "black", width = 0.8) +
  facet_grid(~interval) +
  xlim(-0.5,1.5) +
  ylim(0, 27) +
  scale_x_discrete(limit = c("0", "1"), labels = labels_setting) + # NA removed
  labs(y="n", x="",
       title = "Subjective assessment of the influence of the experimental setting on behavior") +
  geom_text(stat='count', aes(label=..count..), position=position_dodge2(width=0.9, preserve=c("single")), vjust=-0.3) +
  theme_bw() +
  theme(text=element_text(family = "sans", color="black", size=12),
        panel.grid.minor.y = element_blank(), 
        legend.position = "none", 
        axis.text.x=element_text(color = "black", size=10, angle=40, vjust=1, hjust=0.9))
plot_setting

ggsave(filename = "data/results/figures/SubjInfluenceSetting.png", plot_setting, 
       dpi = 300, units = "in", width = 8, height = 5, device='png')

rm(list = setdiff(ls(), c("vorbefragung", "nachbefragung", "fun_mean", "fun_median", "mean_FOT", "FOT_skim", 
                         "plot_TiA_all_scales", "TiA",
                         "plot_CTAM_all_scales", "CTAM",
                         "plot_NDRT_all_scales", "NDRT",
                         "plot_L2components", "L2compo", 
                         "plot_monitoring", 
                         "plot_setting")))
#### plot ranking ####
labels_ranking <- c("H-on", "H-off")

nachbefragung_Aoff <- nachbefragung %>% # if 
  filter(interval == "A_off")

plot_ranking <- ggplot(nachbefragung_Aoff, aes(x = Ranking)) +
  geom_bar(fill = "#DAD7CB", color = "black") +
  xlim(-0.5,1.5) +
  scale_x_discrete(limit = c("1", "2"), labels = labels_ranking) + # NA removed
  labs(y="n", x="",
       title = "A_off: Preferred L2 system") +
  geom_text(stat='count', aes(label=..count..), position=position_dodge2(width=0.9, preserve=c("single")), vjust=-0.3) +
  theme_bw() +
  theme(text=element_text(family = "sans", color="black", size=12),
        panel.grid.minor.y = element_blank(), 
        legend.position = "none", 
        axis.text.x=element_text(color = "black", size=10, angle=40, vjust=1, hjust=0.9))
plot_ranking

ggsave(filename = "data/results/figures/Ranking.png", plot_ranking, 
       dpi = 300, units = "in", width = 4, height = 5, device='png')

rm(list = setdiff(ls(), c("vorbefragung", "nachbefragung", "fun_mean", "fun_median", "mean_FOT", "FOT_skim", 
                         "plot_TiA_all_scales", "TiA",
                         "plot_CTAM_all_scales", "CTAM",
                         "plot_NDRT_all_scales", "NDRT",
                         "plot_L2components", "L2compo", 
                         "plot_monitoring", 
                         "plot_setting", 
                         "plot_ranking")))


################## sample characteristics ###########################################################

#### plot age ####
plot_age <- ggplot(vorbefragung, aes(x = group, y=Alter)) +
  geom_violin(fill = "#DAD7CB") +
  stat_summary(fun = mean, geom="point",colour="black", size=1) +
  stat_summary(fun.data = fun_mean, geom="text", vjust=-.55, size=3.3) +
  labs(y="Age in years", x="",
       title = "Age") +
  theme_bw() +
  ylim(18, 80) +
  theme(text=element_text(family = "sans", color="black", size=12),
        panel.grid.minor.y = element_blank(), 
        legend.position = "none", 
        axis.text.x=element_text(color = "black", size=10, angle=0, vjust=.8, hjust=0.8))
plot_age

ggsave(filename = "data/results/figures/Age.png", plot_age, 
       dpi = 300, units = "in", width = 4, height = 3, device='png')

rm(list = setdiff(ls(), c("vorbefragung", "nachbefragung", "fun_mean", "fun_median", "mean_FOT", "FOT_skim", 
                          "plot_TiA_all_scales", "TiA",
                          "plot_CTAM_all_scales", "CTAM",
                          "plot_NDRT_all_scales", "NDRT",
                          "plot_L2components", "L2compo", 
                          "plot_monitoring", 
                          "plot_setting", 
                          "plot_ranking",
                          "plot_age")))

#### plot driver's license ####
plot_license <- ggplot(vorbefragung, aes(x = group, y=Fuehrerschein)) +
  geom_violin(fill = "#DAD7CB") +
  stat_summary(fun = mean, geom="point",colour="black", size=1) +
  stat_summary(fun.data = fun_mean, geom="text", vjust=-.55, size=3.3) +
  labs(y="Year", x="",
       title = "Year - obtainment of driver's license") +
  theme_bw() +
  ylim(1960, 2020) +
  theme(text=element_text(family = "sans", color="black", size=12),
        panel.grid.minor.y = element_blank(), 
        legend.position = "none", 
        axis.text.x=element_text(color = "black", size=10, angle=0, vjust=.8, hjust=0.8))
plot_license

ggsave(filename = "data/results/figures/License.png", plot_license, 
       dpi = 300, units = "in", width = 4, height = 3, device='png')

rm(list = setdiff(ls(), c("vorbefragung", "nachbefragung", "fun_mean", "fun_median", "mean_FOT", "FOT_skim", 
                          "plot_TiA_all_scales", "TiA",
                          "plot_CTAM_all_scales", "CTAM",
                          "plot_NDRT_all_scales", "NDRT",
                          "plot_L2components", "L2compo", 
                          "plot_monitoring", 
                          "plot_setting", 
                          "plot_ranking",
                          "plot_age", 
                          "plot_license")))
#### plot DSQ #### 
DSQ_Speed <- vorbefragung %>%
  select(group, VPNr, DSQ_Speed) %>%
  rename(score = DSQ_Speed) %>%
  add_column(scale = "DSQ_Speed", .after = "VPNr")
DSQ_Calmness <- vorbefragung %>%
  select(group, VPNr, DSQ_Calmness) %>%
  rename(score = DSQ_Calmness) %>%
  add_column(scale = "DSQ_Calmness", .after = "VPNr")
DSQ_SocialResistance <- vorbefragung %>%
  select(group, VPNr, DSQ_SocialResistance) %>%
  rename(score = DSQ_SocialResistance) %>%
  add_column(scale = "DSQ_SocialResistance", .after = "VPNr")
DSQ_Focus <- vorbefragung %>%
  select(group, VPNr, DSQ_Focus) %>%
  rename(score = DSQ_Focus) %>%
  add_column(scale = "DSQ_Focus", .after = "VPNr")
DSQ_Planning <- vorbefragung %>%
  select(group, VPNr, DSQ_Planning) %>%
  rename(score = DSQ_Planning) %>%
  add_column(scale = "DSQ_Planning", .after = "VPNr")
DSQ_Deviance <- vorbefragung %>%
  select(group, VPNr, DSQ_Deviance) %>%
  rename(score = DSQ_Deviance) %>%
  add_column(scale = "DSQ_Deviance", .after = "VPNr")

DSQ <- bind_rows(DSQ_Speed, DSQ_Calmness, DSQ_SocialResistance, DSQ_Focus, DSQ_Planning, DSQ_Deviance) %>%
  mutate(scale = factor(scale, levels = c("DSQ_Speed", "DSQ_Calmness", "DSQ_SocialResistance", "DSQ_Focus", "DSQ_Planning", "DSQ_Deviance"), ordered = TRUE))

labels_DSQ = c("Speed", "Calmness", "Social Resistance", 
               "Focus", "Planning", "Deviance")

plot_DSQ <- ggplot(DSQ, aes(x=scale, y=score, fill=scale)) + 
  geom_violin() +
  ylim(2, 18) +
  stat_summary(fun = mean, geom="point",colour="black", size=1) +
  stat_summary(fun.data = fun_mean, geom="text", vjust=1.40, size=3.3) +
  scale_x_discrete(labels= labels_DSQ) +
  facet_wrap(~group) +
  scale_fill_manual(values = c("#DAD7CB", "#DAD7CB", "#DAD7CB", "#DAD7CB", "#DAD7CB", "#DAD7CB")) +
  labs(y="Score", x="",
       title = "Driving Style Questionnaire - subscales") +
  theme_bw() +
  theme(text=element_text(family = "sans", color="black", size=12),
        panel.grid.minor.y = element_blank(), 
        legend.position = "none", 
        axis.text.x=element_text(color = "black", size=10, angle=30, vjust=.9, hjust=0.8)) +
  geom_segment(aes(x = 0.6, y = 18, xend = 1.4, yend = 18)) + # speed
  geom_segment(aes(x = 0.6, y = 3, xend = 1.4, yend = 3)) +
  geom_segment(aes(x = 1.6, y = 18, xend = 2.4, yend = 18)) + # calmness
  geom_segment(aes(x = 1.6, y = 3, xend = 2.4, yend = 3)) +
  geom_segment(aes(x = 2.6, y = 12, xend = 3.4, yend = 12)) + # social resistance
  geom_segment(aes(x = 2.6, y = 2, xend = 3.4, yend = 2)) +
  geom_segment(aes(x = 3.6, y = 18, xend = 4.4, yend = 18)) + # focus
  geom_segment(aes(x = 3.6, y = 3, xend = 4.4, yend = 3)) +
  geom_segment(aes(x = 4.6, y = 12, xend = 5.4, yend = 12)) + # planning
  geom_segment(aes(x = 4.6, y = 2, xend = 5.4, yend = 2)) +
  geom_segment(aes(x = 5.6, y = 12, xend = 6.4, yend = 12)) + # deviance
  geom_segment(aes(x = 5.6, y = 2, xend = 6.4, yend = 2))
plot_DSQ

ggsave(filename = "data/results/figures/DSQ.png", plot_DSQ, 
       dpi = 300, units = "in", width = 8, height = 4, device='png')

rm(list = setdiff(ls(), c("vorbefragung", "nachbefragung", "fun_mean", "fun_median", "mean_FOT", "FOT_skim", 
                          "plot_TiA_all_scales", "TiA",
                          "plot_CTAM_all_scales", "CTAM",
                          "plot_NDRT_all_scales", "NDRT",
                          "plot_L2components", "L2compo", 
                          "plot_monitoring", 
                          "plot_setting", 
                          "plot_ranking",
                          "plot_age", 
                          "plot_license", 
                          "plot_DSQ", "DSQ")))

#### plot ATI-S ####
plot_ATIS <- ggplot(vorbefragung, aes(x = group, y=ATIS)) +
  geom_violin(fill = "#DAD7CB") +
  stat_summary(fun = mean, geom="point",colour="black", size=1) +
  stat_summary(fun.data = fun_mean, geom="text", vjust=-.55, size=3.3) +
  labs(y="Score", x="",
       title = "Affinity for Technology Interaction Scale") +
  theme_bw() +
  ylim(1, 6) +
  theme(text=element_text(family = "sans", color="black", size=12),
        panel.grid.minor.y = element_blank(), 
        legend.position = "none", 
        axis.text.x=element_text(color = "black", size=10, angle=0, vjust=.8, hjust=0.8))
plot_ATIS

ggsave(filename = "data/results/figures/ATI-S.png", plot_ATIS, 
       dpi = 300, units = "in", width = 4, height = 3, device='png')

rm(list = setdiff(ls(), c("vorbefragung", "nachbefragung", "fun_mean", "fun_median", "mean_FOT", "FOT_skim", 
                          "plot_TiA_all_scales", "TiA",
                          "plot_CTAM_all_scales", "CTAM",
                          "plot_NDRT_all_scales", "NDRT",
                          "plot_L2components", "L2compo", 
                          "plot_monitoring", 
                          "plot_setting", 
                          "plot_ranking",
                          "plot_age", 
                          "plot_license", 
                          "plot_DSQ", "DSQ",
                          "plot_ATIS")))

#### plot driving frequency ####
labels_frequency <- c("(seldom or) never", "< 1x/month", "> 1x/month", 
                      "> 1x/week", "daily")

frequency_general <- vorbefragung %>%
  select(group, VPNr, Fahrtfrequenz) %>%
  rename(score = Fahrtfrequenz) %>%
  add_column(scale = "general", .after = "VPNr")

frequency_highway <- vorbefragung %>%
  select(group, VPNr, FrequenzAutobahn) %>%
  rename(score = FrequenzAutobahn) %>%
  add_column(scale = "highway", .after = "VPNr")

frequency <- bind_rows(frequency_general, frequency_highway) %>%
  mutate(scale = factor(scale, levels = c("general", "highway"), ordered = TRUE))

plot_frequency <- ggplot(frequency, aes(x = score)) +
  geom_bar(fill = "#DAD7CB", color = "black", width = 0.6) +
  facet_grid(scale~group) +
  ylim(0,25) +
  scale_x_discrete(limit = c("0", "1", "2", "3", "4"), labels = labels_frequency) + # NA removed
  labs(y="n", x="",
       title = "Frequency - general & on highway") +
  geom_text(stat='count', aes(label=..count..), position=position_dodge2(width=0.9, preserve=c("single")), vjust=-0.3) +
  theme_bw() +
  theme(text=element_text(family = "sans", color="black", size=12),
        panel.grid.minor.y = element_blank(), 
        legend.position = "none", 
        axis.text.x=element_text(color = "black", size=10, angle=35, vjust=1, hjust=0.9))
plot_frequency

ggsave(filename = "data/results/figures/Frequency.png", plot_frequency, 
       dpi = 300, units = "in", width = 6, height = 6, device='png')

rm(list = setdiff(ls(), c("vorbefragung", "nachbefragung", "fun_mean", "fun_median", "mean_FOT", "FOT_skim", 
                          "plot_TiA_all_scales", "TiA",
                          "plot_CTAM_all_scales", "CTAM",
                          "plot_NDRT_all_scales", "NDRT",
                          "plot_L2components", "L2compo", 
                          "plot_monitoring", 
                          "plot_setting", 
                          "plot_ranking",
                          "plot_age", 
                          "plot_license", 
                          "plot_DSQ", "DSQ",
                          "plot_ATIS", 
                          "plot_frequency", "frequency")))

#### plot mileage ####
labels_mileage <- c("0 km", "1 km - 5.000 km", "5.001 km - 20.000 km", "20.001 km - 50.000 km", 
                    "50.001 km - 100.000 km", "> 100.000 km")

mileage_general <- vorbefragung %>%
  select(group, VPNr, Fahrtstrecke) %>%
  rename(score = Fahrtstrecke) %>%
  add_column(scale = "general", .after = "VPNr")

mileage_highway <- vorbefragung %>%
  select(group, VPNr, StreckeAutobahn) %>%
  rename(score = StreckeAutobahn) %>%
  add_column(scale = "highway", .after = "VPNr")

mileage <- bind_rows(mileage_general, mileage_highway) %>%
  mutate(scale = factor(scale, levels = c("general", "highway"), ordered = TRUE))

plot_mileage <- ggplot(mileage, aes(x = score)) +
  geom_bar(fill = "#DAD7CB", color = "black", width = 0.6) +
  facet_grid(scale~group) +
  ylim(0,25) +
  scale_x_discrete(limit = c("0", "1", "2", "3", "4", "5"), labels = labels_mileage) + # NA removed
  labs(y="n", x="",
       title = "Mileage - general & on highway") +
  geom_text(stat='count', aes(label=..count..), position=position_dodge2(width=0.9, preserve=c("single")), vjust=-0.3) +
  theme_bw() +
  theme(text=element_text(family = "sans", color="black", size=12),
        panel.grid.minor.y = element_blank(), 
        legend.position = "none", 
        axis.text.x=element_text(color = "black", size=10, angle=30, vjust=1, hjust=0.9))
plot_mileage

ggsave(filename = "data/results/figures/Mileage.png", plot_mileage, 
       dpi = 300, units = "in", width = 6, height = 6, device='png')

rm(list = setdiff(ls(), c("vorbefragung", "nachbefragung", "fun_mean", "fun_median", "mean_FOT", "FOT_skim", 
                          "plot_TiA_all_scales", "TiA",
                          "plot_CTAM_all_scales", "CTAM",
                          "plot_NDRT_all_scales", "NDRT",
                          "plot_L2components", "L2compo", 
                          "plot_monitoring", 
                          "plot_setting", 
                          "plot_ranking",
                          "plot_age", 
                          "plot_license", 
                          "plot_DSQ", "DSQ",
                          "plot_ATIS", 
                          "plot_frequency", "frequency", 
                          "plot_mileage", "mileage")))

#### plot familiarity with ADAS ####
labels_ADAS <- c("unknown", "known, but never used", "seldomly used", "used regularly")

ADAS_CC <- vorbefragung %>%
  select(group, VPNr, KenntnisAS.CC.) %>%
  rename(score = KenntnisAS.CC.) %>%
  add_column(scale = "CC", .after = "VPNr")

ADAS_ACC <- vorbefragung %>%
  select(group, VPNr, KenntnisAS.ACC.) %>%
  rename(score = KenntnisAS.ACC.) %>%
  add_column(scale = "ACC", .after = "VPNr")

ADAS_LKA <- vorbefragung %>%
  select(group, VPNr, KenntnisAS.Spurhalte.) %>%
  rename(score = KenntnisAS.Spurhalte.) %>%
  add_column(scale = "LKA", .after = "VPNr")

ADAS_TJA <- vorbefragung %>%
  select(group, VPNr, KenntnisAS.Stauassistent.) %>%
  rename(score = KenntnisAS.Stauassistent.) %>%
  add_column(scale = "TJA", .after = "VPNr")

ADAS_PA <- vorbefragung %>%
  select(group, VPNr, KenntnisAS.ParkAssist.) %>%
  rename(score = KenntnisAS.ParkAssist.) %>%
  add_column(scale = "PA", .after = "VPNr")

ADAS_L2 <- vorbefragung %>%
  select(group, VPNr, KenntnisAS.Teilautomation.) %>%
  rename(score = KenntnisAS.Teilautomation.) %>%
  add_column(scale = "L2", .after = "VPNr")

ADAS <- bind_rows(ADAS_CC, ADAS_ACC, ADAS_LKA, ADAS_TJA, ADAS_PA, ADAS_L2) %>%
  mutate(scale = factor(scale, levels = c("CC", "ACC", "LKA", "TJA", "PA", "L2"), ordered = TRUE))

plot_ADAS <- ggplot(ADAS, aes(x = score)) +
  geom_bar(fill = "#DAD7CB", color = "black", width = 0.6) +
  facet_grid(scale~group) +
  ylim(0,25) +
  scale_x_discrete(limit = c("0", "1", "2", "3"), labels = labels_ADAS) + # NA removed | limit = c("0", "1", "2", "3", "4", "5", "6"), 
  labs(y="n", x="",
       title = "Familiarity with driver assistance systems") +
  geom_text(stat='count', aes(label=..count..), position=position_dodge2(width=0.9, preserve=c("single")), vjust=-0.3) +
  theme_bw() +
  theme(text=element_text(family = "sans", color="black", size=12),
        panel.grid.minor.y = element_blank(), 
        legend.position = "none", 
        axis.text.x=element_text(color = "black", size=10, angle=20, vjust=1, hjust=0.9))
plot_ADAS

ggsave(filename = "data/results/figures/ADAS.png", plot_ADAS, 
       dpi = 300, units = "in", width = 5, height = 10, device='png')

rm(list = setdiff(ls(), c("vorbefragung", "nachbefragung", "fun_mean", "fun_median", "mean_FOT", "FOT_skim", 
                          "plot_TiA_all_scales", "TiA",
                          "plot_CTAM_all_scales", "CTAM",
                          "plot_NDRT_all_scales", "NDRT",
                          "plot_L2components", "L2compo", 
                          "plot_monitoring", 
                          "plot_setting", 
                          "plot_ranking",
                          "plot_age", 
                          "plot_license", 
                          "plot_DSQ", "DSQ",
                          "plot_ATIS", 
                          "plot_frequency", "frequency", 
                          "plot_mileage", "mileage", 
                          "plot_ADAS", "ADAS")))


          
################## not applied ######################################################################

#### not applied: plot TiA_overall ####
# plot_TiA_overall <- ggplot(nachbefragung, aes(x=interval, y=TiA_overall, fill=interval)) + 
#   geom_boxplot() +
#   theme_bw() +
#   ylim(1, 5) +
#   theme(panel.grid.minor.y = element_blank(), legend.position = "bottom") +
#   scale_fill_manual("interval", values = c("A_on_fc" = "#64A0C8", "A_on_fam" = "#98C6EA", "A_off" = "#999999", "B_off" = "#DAD7CB")) +
#   labs(y="Score", x="",
#        title = "TiA_overall") +
#   stat_summary(fun = mean, geom="point",colour="black", size=2) +
#   stat_summary(fun.data = fun_mean, geom="text", vjust=-.7)
# plot_TiA_overall

#### not applied: plot TiA_overall with summary table (works only if facet) ####
# plot_TiA_overall_summ <- plot_TiA_overall
# summ <- nachbefragung %>% 
#   group_by(interval) %>% 
#   summarize(min = min(TiA_overall), max = max(TiA_overall), 
#             mean = mean(TiA_overall), q1= quantile(TiA_overall, probs = 0.25), 
#             median = median(TiA_overall), q3= quantile(TiA_overall, probs = 0.75),
#             sd = sd(TiA_overall)) %>%
#   mutate_if(is.numeric, round, digits=2) %>%
#   mutate(lab = paste("min = ", min, "\nmax = ", max, "\nmean = ", mean, 
#                      "\nmedian = ", median, "\nsd = ", sd),
#          position=c(1.5, 0.8, 0.25, -2)) %>% select(interval, lab, position)
# 
# plot_TiA_overall_summ <- plot_TiA_overall_summ + geom_text(data = summ, aes(label = lab), x = Inf, y = Inf, hjust = 1, vjust = 1.2, size = 3)
# plot_TiA_overall_summ

#### not applied: plot TiA_overall with summary table in different facets ####
# plot_TiA_overall_summ_facet <- ggplot(nachbefragung, aes(x=interval, y=TiA_overall, fill=interval)) + 
#   geom_violin() + # geom_boxplot()
#   theme_bw() +
#   ylim(1, 5) +
#   theme(panel.grid.minor.y = element_blank(), legend.position = "bottom") +
#   facet_wrap(~interval) +
#   scale_fill_manual("interval", values = c("A_on_fc" = "#64A0C8", "A_on_fam" = "#98C6EA", "A_off" = "#999999", "B_off" = "#DAD7CB")) +
#   labs(y="Score", x="",
#        title = "TiA_overall") +
#   stat_summary(fun = mean, geom="point",colour="black", size=2) +
#   stat_summary(fun.data = fun_mean, geom="text", vjust=-.7)
# plot_TiA_overall_summ_facet
# 
# plot_TiA_overall_summ_facet <- plot_TiA_overall_summ_facet + geom_text(data = summ, aes(label = lab), x = Inf, y = Inf, hjust = 1, vjust = 1.2, size = 3)
# plot_TiA_overall_summ_facet

#### not applied: alternative plotting approach ####
# ggsummarystats(
#   nachbefragung, x = "interval", y = "CTAM_PS", 
#   ggfunc = ggviolin, add = c("jitter", "median"),
#   
#   color = "interval", palette = "npg"
# )
