#### preparations ####
rm(list = ls())
library(tidyverse)
library(skimr)
library(ggplot2)
library(dplyr)
library(rstatix)
library(skimr)
library(grid)

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
#                 L2H-on RGB	  Hex	      L2H-off	RGB	  Hex	      L0/L1/neutral	RGB	Hex
#   Primärfarbe		255, 192, 0	  #FFC000		91, 155, 213	#5B9BD5		231, 230, 230	    #E7E6E6
# 1. Abstufung		255, 217, 102	#FFD966		155, 194, 230	#9BC2E6		208, 206, 206	    #D0CECE
# 2. Abstufung		255, 230, 153	#FFE699		189, 215, 238	#BDD7EE		174, 170, 170	    #AEAAAA
# 3. Abstufung		255, 242, 204	#FFF2CC		221, 235, 247	#DDEBF7		117, 113, 113	    #757171

# FOT colors:
# A L2H-off 			 "#9BC2E6"
# B L2H-off			  "#5B9BD5"
# A L2H-on (fc)		"#FFC000"
# A L2H-on (fam)	"#FFD966"
# 
# Neutral/A:		"#E7E6E6"
# overall/B:		"#AEAAAA"


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
  mutate(interval = ifelse(interval == "A_on_fc", "A L2H-on (fc)", 
                        ifelse(interval == "A_on_fam", "A L2H-on (fam)",
                               ifelse(interval == "A_off", "A L2H-off",
                                      ifelse(interval == "B_off", "B L2H-off", "OTHER"))))) %>%
  mutate(interval = factor(interval, levels = c("A L2H-on (fc)", "A L2H-on (fam)", "A L2H-off", "B L2H-off"), ordered = TRUE)) %>%
  # mutate(SubjUeberwachguete.1. = factor(SubjUeberwachguete.1., ordered = TRUE)) %>%
  mutate(SubjEinflussSetting = factor(SubjEinflussSetting, ordered = TRUE)) %>%
  mutate(Ranking = factor(Ranking, ordered = TRUE))

vorbefragung <- vorbefragung %>%
  rename(group = X.U.FEFF.group) # %>%
  # mutate(Fahrtfrequenz = factor(Fahrtfrequenz, levels = c("0", "1", "2", "3", "4"), ordered = TRUE)) %>%
  # mutate(Fahrtstrecke = factor(Fahrtstrecke, levels = c("0", "1", "2", "3", "4", "5"), ordered = TRUE)) %>%
  # mutate(FrequenzAutobahn = factor(FrequenzAutobahn, levels = c("0", "1", "2", "3", "4"), ordered = TRUE)) %>%
  # mutate(StreckeAutobahn = factor(StreckeAutobahn, levels = c("0", "1", "2", "3", "4", "5"), ordered = TRUE)) %>%
  # mutate(across(starts_with("KenntnisAS."), ~factor(., levels = c("0", "1", "2", "3"), ordered = TRUE)))


################## post questionnaire ###############################################################

#### plot TiA overall + subscales ####
## subset TiA overall + subscales ##
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
## build subset ##
TiA <- bind_rows(TiA_overall, TiA_RC, TiA_UP, TiA_F, TiA_IoD, TiA_PtT, TiA_TiA) %>%
  mutate(scale = factor(scale, levels = c("TiA_overall", "TiA_RC", "TiA_UP", "TiA_F", "TiA_IoD", "TiA_PtT", "TiA_TiA"), ordered = TRUE))
## labels ##
labels_TiA = c("Overall", "Reliability/Competence", "Understanding/Predictability", 
               "Familiarity", "Intention of Developers", "Propensity to Trust", "Trust in Automation")

## basic plot ##
p <- ggplot(TiA, aes(x=scale, y=score, fill=scale)) + 
  stat_boxplot(geom ='errorbar', width = 0.3, lwd=0.2) +
  geom_boxplot(outlier.shape = 21, lwd=0.2, outlier.size = 0.7) +
  ylim(1, 5) +
  stat_summary(fun = mean, geom = "point" , colour="black", size=1, shape = 4) +
  scale_x_discrete(labels= labels_TiA) +
  facet_wrap(~interval) +
  scale_fill_manual(values = c("#AEAAAA", "#E7E6E6", "#E7E6E6", "#E7E6E6", "#E7E6E6", "#E7E6E6", "#E7E6E6")) +
  labs(y="Score", x="") +
  theme_bw() +
  theme(text=element_text(family = "sans", color="black", size=11, face = "bold"),
        panel.grid.minor.y = element_blank(), 
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(size = 0.2),
        legend.position = "none", 
        plot.background = element_rect(fill = "transparent",
                                       colour = NA_character_),
        axis.text.x=element_text(color = "black", size=9, angle=30, vjust=.88, hjust=0.8, face = "plain"),
        axis.text.y=element_text(color = "black", size=9, face = "plain"))
## basic plot ##
p <- ggplot(TiA, aes(x=scale, y=score, fill=scale)) + 
  stat_boxplot(geom ='errorbar', width = 0.3, lwd=0.2) +
  geom_boxplot(outlier.shape = 21, lwd=0.2, outlier.size = 0.7) +
  ylim(1, 5) +
  stat_summary(fun = mean, geom = "point" , colour="black", size=1, shape = 4) +
  scale_x_discrete(labels= labels_TiA) +
  facet_wrap(~interval) +
  scale_fill_manual(values = c("#AEAAAA", "#E7E6E6", "#E7E6E6", "#E7E6E6", "#E7E6E6", "#E7E6E6", "#E7E6E6")) +
  labs(y="Score", x="") +
  theme_bw() +
  theme(text=element_text(family = "sans", color="black", size=11, face = "bold"),
        panel.grid.minor.y = element_blank(), 
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(size = 0.2),
        legend.position = "none", 
        plot.background = element_rect(fill = "transparent",
                                       colour = NA_character_),
        axis.text.x=element_text(color = "black", size=9, angle=30, vjust=.88, hjust=0.8, face = "plain"),
        axis.text.y=element_text(color = "black", size=9, face = "plain"))
## code of Valentin_Stefan to edit colors of facet boxes: 
# https://stackoverflow.com/questions/53455092/r-ggplot2-change-colour-of-font-and-background-in-facet-strip
# Generate the ggplot2 plot grob
g <- grid.force(ggplotGrob(p))
# Get the names of grobs and their gPaths into a data.frame structure
grobs_df <- do.call(cbind.data.frame, grid.ls(g, print = FALSE))
# Build optimal gPaths that will be later used to identify grobs and edit them
grobs_df$gPath_full <- paste(grobs_df$gPath, grobs_df$name, sep = "::")
grobs_df$gPath_full <- gsub(pattern = "layout::", 
                            replacement = "", 
                            x = grobs_df$gPath_full, 
                            fixed = TRUE)
# Get the gPaths of the strip background grobs
strip_bg_gpath <- grobs_df$gPath_full[grepl(pattern = ".*strip\\.background.*", 
                                            x = grobs_df$gPath_full)]
strip_bg_gpath[1]
strip_txt_gpath <- grobs_df$gPath_full[grepl(pattern = "strip.*titleGrob.*text.*", 
                                             x = grobs_df$gPath_full)]
strip_txt_gpath[1] # example of a gPath for strip title
## [1] "strip-t-1.7-5-7-5::strip.1-1-1-1::GRID.titleGrob.5368::GRID.text.5364"
# Generate some color
n_cols <- length(strip_bg_gpath)
fills <- c("#9BC2E6","#5B9BD5","#FFC000","#FFD966")
# Edit the grobs
for (i in 1:length(strip_bg_gpath)){
  g <- editGrob(grob = g, gPath = strip_bg_gpath[i], gp = gpar(fill = fills[i]))
}
# Draw the edited plot
grid.newpage(); grid.draw(g)

plot_TiA_all_scales <- g
ggsave(filename = "data/results/figures/new/TiA_new.png", plot_TiA_all_scales , 
       dpi = 500, width = 8, height = 5, units = "in", device='png', bg = "transparent")

## remove not needed data ##
rm(list = setdiff(ls(), c("vorbefragung", "nachbefragung", "fun_mean", "fun_median", "mean_FOT", "FOT_skim", 
                          "plot_TiA_all_scales", "TiA")))

#### plot CTAM subscales ####
## subset subscales ##
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
## build subset ##
CTAM <- bind_rows(CTAM_PE, CTAM_EE, CTAM_ATT, CTAM_FC, CTAM_ITU, CTAM_PS) %>%
  mutate(scale = factor(scale, levels = c("CTAM_PE", "CTAM_EE", "CTAM_ATT", "CTAM_FC", "CTAM_ITU", "CTAM_PS"), ordered = TRUE))
## lables ##
labels_CTAM = c("Performance\nExpectancy", "Effort Expectancy", "Attitude towards\nusing Technology", 
                "Facilitating Conditions", "Behavioral Intention \nto use the System", "Perceived Safety")

## basic plot ##
p <- ggplot(CTAM, aes(x=scale, y=score, fill=scale)) + 
  stat_boxplot(geom ='errorbar', width = 0.3, lwd=0.2) +
  geom_boxplot(outlier.shape = 21, lwd=0.2, outlier.size = 0.7) +
  stat_summary(fun = mean, geom = "point" , colour="black", size=1, shape = 4) +
  scale_x_discrete(labels = labels_CTAM) +
  scale_y_continuous(limits = c(1,7), breaks = seq(1,7,1)) +
  facet_wrap(~interval) +
  scale_fill_manual(values = c("#E7E6E6", "#E7E6E6", "#E7E6E6", "#E7E6E6", "#E7E6E6", "#E7E6E6")) +
  labs(y="Score", x="") +
  theme_bw() +
  theme(text=element_text(family = "sans", color="black", size=11, face = "bold"),
        panel.grid.minor.y = element_blank(), 
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(size = 0.2),
        legend.position = "none", 
        plot.background = element_rect(fill = "transparent",
                                       colour = NA_character_),
        axis.text.x=element_text(color = "black", size=9, angle=30, vjust=.88, hjust=0.8, face = "plain"),
        axis.text.y=element_text(color = "black", size=9, face = "plain"))
p
## code of Valentin_Stefan to edit colors of facet boxes: 
# https://stackoverflow.com/questions/53455092/r-ggplot2-change-colour-of-font-and-background-in-facet-strip
# Generate the ggplot2 plot grob
g <- grid.force(ggplotGrob(p))
# Get the names of grobs and their gPaths into a data.frame structure
grobs_df <- do.call(cbind.data.frame, grid.ls(g, print = FALSE))
# Build optimal gPaths that will be later used to identify grobs and edit them
grobs_df$gPath_full <- paste(grobs_df$gPath, grobs_df$name, sep = "::")
grobs_df$gPath_full <- gsub(pattern = "layout::", 
                            replacement = "", 
                            x = grobs_df$gPath_full, 
                            fixed = TRUE)
# Get the gPaths of the strip background grobs
strip_bg_gpath <- grobs_df$gPath_full[grepl(pattern = ".*strip\\.background.*", 
                                            x = grobs_df$gPath_full)]
strip_bg_gpath[1]
strip_txt_gpath <- grobs_df$gPath_full[grepl(pattern = "strip.*titleGrob.*text.*", 
                                             x = grobs_df$gPath_full)]
strip_txt_gpath[1] # example of a gPath for strip title
## [1] "strip-t-1.7-5-7-5::strip.1-1-1-1::GRID.titleGrob.5368::GRID.text.5364"
# Generate some color
n_cols <- length(strip_bg_gpath)
fills <- c("#9BC2E6","#5B9BD5","#FFC000","#FFD966")
# Edit the grobs
for (i in 1:length(strip_bg_gpath)){
  g <- editGrob(grob = g, gPath = strip_bg_gpath[i], gp = gpar(fill = fills[i]))
}
# Draw the edited plot
grid.newpage(); grid.draw(g)

plot_CTAM_all_scales <- g
ggsave(filename = "data/results/figures/new/CTAM_new.png", g, 
       dpi = 500, width = 8, height = 5, units = "in", device='png', bg = "transparent")

## remove not needed data ##
rm(list = setdiff(ls(), c("vorbefragung", "nachbefragung", "fun_mean", "fun_median", "mean_FOT", "FOT_skim", 
                          "plot_TiA_all_scales", "TiA",
                          "plot_CTAM_all_scales", "CTAM")))

#### plot NDRTs ####
## subsets ##
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
##  build subset ##
NDRT <- bind_rows(NDRT_1, NDRT_2, NDRT_3, NDRT_4, NDRT_5, NDRT_6, NDRT_7, NDRT_8) %>%
  mutate(scale = factor(scale, levels = c("NDRT_1", "NDRT_2", "NDRT_3", "NDRT_4", "NDRT_5", "NDRT_6", "NDRT_7", "NDRT_8"), ordered = TRUE))
## lables
labels_NDRT = c("Mobile device\nin hand - handling", "Mobile device\nin hand - talking", 
                "Mobile device\nfixated - talking", "Vehicle related inputs",
                "Eating/drinking/smoking", "Grooming", 
                "Interaction with passengers", "Searching/grabbing/\nrummaging")

## basic plot ##
p <- ggplot(NDRT, aes(x=scale, y=score, fill=scale)) + 
  geom_rect(aes(xmin = 0.55, xmax = 1.45, ymin = 0, ymax = 5), 
            fill = scales::alpha("#FFFF00", 0.005)) +
  geom_rect(aes(xmin = 3.55, xmax = 4.45, ymin = 0, ymax = 5), 
            fill = scales::alpha("#FFFF00", 0.005)) +
  geom_rect(aes(xmin = 5.55, xmax = 6.45, ymin = 0, ymax = 5), 
            fill = scales::alpha("#FFFF00", 0.005)) +
  geom_rect(aes(xmin = 7.55, xmax = 8.45, ymin = 0, ymax = 5), 
            fill = scales::alpha("#FFFF00", 0.005)) +
  geom_rect(aes(xmin = 1.55, xmax = 2.45, ymin = 0, ymax = 5), 
            fill = scales::alpha("#92D050", 0.009)) +
  geom_rect(aes(xmin = 4.55, xmax = 5.45, ymin = 0, ymax = 5), 
            fill = scales::alpha("#92D050", 0.009)) +
  geom_rect(aes(xmin = 2.55, xmax = 3.45, ymin = 0, ymax = 5), 
            fill = scales::alpha("#E4321D", 0.005)) +
  geom_rect(aes(xmin = 6.55, xmax = 7.45, ymin = 0, ymax = 5), 
            fill = scales::alpha("#E4321D", 0.005)) +
  stat_boxplot(geom ='errorbar', width = 0.3, lwd=0.2) +
  geom_boxplot(outlier.shape = 21, lwd=0.2, outlier.size = 0.7) +
  stat_summary(fun = mean, geom = "point" , colour="black", size=1, shape = 4) +
  scale_x_discrete(labels = labels_NDRT) +
  scale_y_continuous(limits = c(0,5), breaks = seq(0,5,1), 
                     labels = c("never", "very rarely", "rarely", "occasionally", "often", "very often")) +
  facet_wrap(~interval) +
  scale_fill_manual(values = c("#E7E6E6", "#E7E6E6", "#E7E6E6", "#E7E6E6", "#E7E6E6", "#E7E6E6", "#E7E6E6", "#E7E6E6")) +
  labs(y="Score", x="") +
  theme_bw() +
  theme(text=element_text(family = "sans", color="black", size=11, face = "bold"),
        panel.grid.minor.y = element_blank(), 
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(size = 0.2),
        legend.position = "none", 
        plot.background = element_rect(fill = "transparent",
                                       colour = NA_character_),
        axis.text.x=element_text(color = "black", size=9, angle=40, vjust=.88, hjust=0.8, face = "plain"),
        axis.text.y=element_text(color = "black", size=9, face = "plain"))
p
## code of Valentin_Stefan to edit colors of facet boxes: 
# https://stackoverflow.com/questions/53455092/r-ggplot2-change-colour-of-font-and-background-in-facet-strip
# Generate the ggplot2 plot grob
g <- grid.force(ggplotGrob(p))
# Get the names of grobs and their gPaths into a data.frame structure
grobs_df <- do.call(cbind.data.frame, grid.ls(g, print = FALSE))
# Build optimal gPaths that will be later used to identify grobs and edit them
grobs_df$gPath_full <- paste(grobs_df$gPath, grobs_df$name, sep = "::")
grobs_df$gPath_full <- gsub(pattern = "layout::", 
                            replacement = "", 
                            x = grobs_df$gPath_full, 
                            fixed = TRUE)
# Get the gPaths of the strip background grobs
strip_bg_gpath <- grobs_df$gPath_full[grepl(pattern = ".*strip\\.background.*", 
                                            x = grobs_df$gPath_full)]
strip_bg_gpath[1]
strip_txt_gpath <- grobs_df$gPath_full[grepl(pattern = "strip.*titleGrob.*text.*", 
                                             x = grobs_df$gPath_full)]
strip_txt_gpath[1] # example of a gPath for strip title
## [1] "strip-t-1.7-5-7-5::strip.1-1-1-1::GRID.titleGrob.5368::GRID.text.5364"
# Generate some color
n_cols <- length(strip_bg_gpath)
fills <- c("#9BC2E6","#5B9BD5","#FFC000","#FFD966")
# Edit the grobs
for (i in 1:length(strip_bg_gpath)){
  g <- editGrob(grob = g, gPath = strip_bg_gpath[i], gp = gpar(fill = fills[i]))
}
# Draw the edited plot
grid.newpage(); grid.draw(g)

plot_NDRT_all_scales <- g
ggsave(filename = "data/results/figures/new/NDRT_new.png", g, 
       dpi = 500, width = 9, height = 5.5, units = "in", device='png', bg = "transparent")

## remove not needed data ##
rm(list = setdiff(ls(), c("vorbefragung", "nachbefragung", "fun_mean", "fun_median", "mean_FOT", "FOT_skim", 
                          "plot_TiA_all_scales", "TiA",
                          "plot_CTAM_all_scales", "CTAM",
                          "plot_NDRT_all_scales", "NDRT")))

#### plot L2components ####
## subsets ##
L2u_gen <- nachbefragung %>%
  filter(interval != "A L2H-on (fc)") %>%
  select(interval, VPNr, L2PrivNutzung) %>%
  rename(score = L2PrivNutzung) %>%
  add_column(scale = "L2u_gen", .after = "VPNr")
L2u_long <- nachbefragung %>%
  filter(interval != "A L2H-on (fc)") %>%
  select(interval, VPNr, L2Komponenten.Laengs.) %>%
  rename(score = L2Komponenten.Laengs.) %>%
  add_column(scale = "L2u_long", .after = "VPNr")
L2u_lat <- nachbefragung %>%
  filter(interval != "A L2H-on (fc)") %>%
  select(interval, VPNr, L2Komponenten.Quer.) %>%
  rename(score = L2Komponenten.Quer.) %>%
  add_column(scale = "L2u_lat", .after = "VPNr")
L2u_hoff <- nachbefragung %>%
  filter(interval != "A L2H-on (fc)") %>%
  select(interval, VPNr, L2Komponenten.Hoff.) %>%
  rename(score = L2Komponenten.Hoff.) %>%
  add_column(scale = "L2u_hoff", .after = "VPNr")
## build subsets ##
L2compo <- bind_rows(L2u_gen, L2u_long, L2u_lat, L2u_hoff) %>%
  mutate(scale = factor(scale, levels = c("L2u_gen", "L2u_long", "L2u_lat", "L2u_hoff"), 
                        ordered = TRUE))
## lables ##
labels_L2compo = c("overall", "longitudinal", 
                   "lateral", "h-off")

## basic plot ##
p <- ggplot(L2compo, aes(x=scale, y=score, fill=scale)) + 
  stat_boxplot(geom ='errorbar', width = 0.3, lwd=0.2) +
  geom_boxplot(outlier.shape = 21, lwd=0.2, outlier.size = 0.7) +
  stat_summary(fun = mean, geom = "point" , colour="black", size=1, shape = 4) +
  scale_x_discrete(labels = labels_L2compo) +
  scale_y_continuous(limits = c(0,4), breaks = seq(0,4,1), 
                     labels = c("no", "rather no", "uncertain", "rather yes", "yes")) +
  facet_wrap(~interval) +
  scale_fill_manual(values = c("#AEAAAA", "#E7E6E6", "#E7E6E6", "#E7E6E6")) +
  labs(y="Score", x="") +
  theme_bw() +
  theme(text=element_text(family = "sans", color="black", size=11, face = "bold"),
        panel.grid.minor.y = element_blank(), 
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(size = 0.2),
        legend.position = "none", 
        plot.background = element_rect(fill = "transparent",
                                       colour = NA_character_),
        axis.text.x=element_text(color = "black", size=9, angle=40, vjust=.88, hjust=0.8, face = "plain"),
        axis.text.y=element_text(color = "black", size=9, face = "plain"))
p
## code of Valentin_Stefan to edit colors of facet boxes: 
# https://stackoverflow.com/questions/53455092/r-ggplot2-change-colour-of-font-and-background-in-facet-strip
# Generate the ggplot2 plot grob
g <- grid.force(ggplotGrob(p))
# Get the names of grobs and their gPaths into a data.frame structure
grobs_df <- do.call(cbind.data.frame, grid.ls(g, print = FALSE))
# Build optimal gPaths that will be later used to identify grobs and edit them
grobs_df$gPath_full <- paste(grobs_df$gPath, grobs_df$name, sep = "::")
grobs_df$gPath_full <- gsub(pattern = "layout::", 
                            replacement = "", 
                            x = grobs_df$gPath_full, 
                            fixed = TRUE)
# Get the gPaths of the strip background grobs
strip_bg_gpath <- grobs_df$gPath_full[grepl(pattern = ".*strip\\.background.*", 
                                            x = grobs_df$gPath_full)]
strip_bg_gpath[1]
strip_txt_gpath <- grobs_df$gPath_full[grepl(pattern = "strip.*titleGrob.*text.*", 
                                             x = grobs_df$gPath_full)]
strip_txt_gpath[1] # example of a gPath for strip title
## [1] "strip-t-1.7-5-7-5::strip.1-1-1-1::GRID.titleGrob.5368::GRID.text.5364"
# Generate some color
n_cols <- length(strip_bg_gpath)
fills <- c("#FFD966", "#9BC2E6","#5B9BD5")
# Edit the grobs
for (i in 1:length(strip_bg_gpath)){
  g <- editGrob(grob = g, gPath = strip_bg_gpath[i], gp = gpar(fill = fills[i]))
}
# Draw the edited plot
grid.newpage(); grid.draw(g)

plot_L2components_all_scales <- g
ggsave(filename = "data/results/figures/new/L2components_new.png", g, 
       dpi = 500, width = 8, height = 3, units = "in", device='png', bg = "transparent")
## remove not needed data ##
rm(list = setdiff(ls(), c("vorbefragung", "nachbefragung", "fun_mean", "fun_median", "mean_FOT", "FOT_skim", 
                          "plot_TiA_all_scales", "TiA",
                          "plot_CTAM_all_scales", "CTAM",
                          "plot_NDRT_all_scales", "NDRT",
                          "plot_L2components", "L2compo")))

#### plot system understanding ####
SU_scales_System <- nachbefragung %>%
  select(interval, VPNr, SU_System_sum) %>%
  rename(score = SU_System_sum) %>%
  add_column(scale = "System", .after = "VPNr")

SU_scales_Role <- nachbefragung %>%
  select(interval, VPNr, SU_Role_sum) %>%
  rename(score = SU_Role_sum) %>%
  add_column(scale = "Role", .after = "VPNr")

SU_scales <- bind_rows(SU_scales_System, SU_scales_Role) %>%
  mutate(scale = factor(scale, levels = c("System", "Role"),
                           ordered = TRUE))

p <- ggplot(SU_scales, aes(x=scale, y=score*100, fill=scale)) + 
  stat_boxplot(geom ='errorbar', width = 0.3, lwd=0.2) +
  geom_boxplot(outlier.shape = 21, lwd=0.2, outlier.size = 0.7) +
  stat_summary(fun = mean, geom = "point" , colour="black", size=1, shape = 4) +
  scale_y_continuous(limits = c(0,100), breaks = seq(0,100,50)) +
  facet_grid(~interval) +
  scale_fill_manual(values = c("#AEAAAA", "#D0CECE", "#D0CECE", "#D0CECE")) +
  labs(y="Correct answers [%]", x="") +
  theme_bw() +
  theme(text=element_text(family = "sans", color="black", size=11, face = "bold"),
        panel.grid.minor.y = element_blank(), 
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(size = 0.2),
        legend.position = "none", 
        plot.background = element_rect(fill = "transparent",
                                       colour = NA_character_),
        axis.text.x=element_text(color = "black", size=9, angle=0, vjust=.88, hjust=0.5, face = "plain"),
        axis.text.y=element_text(color = "black", size=9, face = "plain"))
p

## code of Valentin_Stefan to edit colors of facet boxes: 
# https://stackoverflow.com/questions/53455092/r-ggplot2-change-colour-of-font-and-background-in-facet-strip
# Generate the ggplot2 plot grob
g <- grid.force(ggplotGrob(p))
# Get the names of grobs and their gPaths into a data.frame structure
grobs_df <- do.call(cbind.data.frame, grid.ls(g, print = FALSE))
# Build optimal gPaths that will be later used to identify grobs and edit them
grobs_df$gPath_full <- paste(grobs_df$gPath, grobs_df$name, sep = "::")
grobs_df$gPath_full <- gsub(pattern = "layout::", 
                            replacement = "", 
                            x = grobs_df$gPath_full, 
                            fixed = TRUE)
# Get the gPaths of the strip background grobs
strip_bg_gpath <- grobs_df$gPath_full[grepl(pattern = ".*strip\\.background.*", 
                                            x = grobs_df$gPath_full)]
strip_bg_gpath[1]
strip_txt_gpath <- grobs_df$gPath_full[grepl(pattern = "strip.*titleGrob.*text.*", 
                                             x = grobs_df$gPath_full)]
strip_txt_gpath[1] # example of a gPath for strip title
## [1] "strip-t-1.7-5-7-5::strip.1-1-1-1::GRID.titleGrob.5368::GRID.text.5364"
# Generate some color
n_cols <- length(strip_bg_gpath)
fills <- c("#FFC000","#FFD966", "#9BC2E6","#5B9BD5")
# Edit the grobs
for (i in 1:length(strip_bg_gpath)){
  g <- editGrob(grob = g, gPath = strip_bg_gpath[i], gp = gpar(fill = fills[i]))
}
# Draw the edited plot
grid.newpage(); grid.draw(g)

plot_SystemUnderstanding <- g

ggsave(filename = "data/results/figures/new/SystemUnderstanding_new.png", plot_SystemUnderstanding, 
       dpi = 500, width = 8, height = 3, units = "in", device='png', bg = "transparent")

#### plot System Understanding - single questions ####
## build subset ##
SystemUnderstanding_singleQ_means <- nachbefragung %>%
  select(interval, starts_with("System0"), starts_with("System1"),
         starts_with("Role0"), starts_with("Role1")) %>%
  group_by(interval) %>%
  FOT_skim() %>%
  select(interval, skim_variable, numeric.mean) %>%
  rename(question = skim_variable) %>%
  rename(mean = numeric.mean) %>%
  mutate(question = factor(question, levels = c("System01", "System02", "System03", "System04",
                                                "System05", "System06", "System07", "System08",
                                                "System09", "System16", "System17",
                                                "Role01", "Role02", "Role03", "Role04",
                                                "Role08", "Role09", "Role10", "Role11"),
                           ordered = TRUE))

## basic plot ##
p <- ggplot(SystemUnderstanding_singleQ_means, aes(x = question, y=mean*100)) +
  geom_rect(aes(xmin = 0.5, xmax = 6.5, ymin = -Inf, ymax = Inf), 
            fill = scales::alpha("#D0CECE", 0.05)) +
  geom_rect(aes(xmin = 8.5, xmax = 12.5, ymin = -Inf, ymax = Inf), 
            fill = scales::alpha("#D0CECE", 0.05)) +
  geom_line(aes(group = interval)) +
  geom_point() +
  stat_summary(fun = mean, geom="point",colour="black", size=1) +
  stat_summary(fun.data = fun_mean, geom="text", vjust=1.8, size=3.1) +
  labs(y="Correct answers [%]") +
  facet_grid(interval ~.) +
  scale_y_continuous(limits = c(0,100), breaks = seq(0,100,50)) +
  scale_x_discrete() +
  theme_bw() +
  theme(text=element_text(family = "sans", color="black", size=11, face="bold"),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_line(size = 0.2),
        panel.grid.major.x = element_blank(),
        legend.position = "none", 
        legend.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent",
                                       colour = NA_character_),
        axis.title.x=element_blank(),
        axis.text.x=element_text(color = "black", size=9, angle=30, vjust=.9, hjust=0.8, face="plain"),
        axis.text.y=element_text(color = "black", size=9, face = "plain"))
p

## code of Valentin_Stefan to edit colors of facet boxes: 
# https://stackoverflow.com/questions/53455092/r-ggplot2-change-colour-of-font-and-background-in-facet-strip
# Generate the ggplot2 plot grob
g <- grid.force(ggplotGrob(p))
# Get the names of grobs and their gPaths into a data.frame structure
grobs_df <- do.call(cbind.data.frame, grid.ls(g, print = FALSE))
# Build optimal gPaths that will be later used to identify grobs and edit them
grobs_df$gPath_full <- paste(grobs_df$gPath, grobs_df$name, sep = "::")
grobs_df$gPath_full <- gsub(pattern = "layout::", 
                            replacement = "", 
                            x = grobs_df$gPath_full, 
                            fixed = TRUE)
# Get the gPaths of the strip background grobs
strip_bg_gpath <- grobs_df$gPath_full[grepl(pattern = ".*strip\\.background.*", 
                                            x = grobs_df$gPath_full)]
strip_bg_gpath[1]
strip_txt_gpath <- grobs_df$gPath_full[grepl(pattern = "strip.*titleGrob.*text.*", 
                                             x = grobs_df$gPath_full)]
strip_txt_gpath[1] # example of a gPath for strip title
## [1] "strip-t-1.7-5-7-5::strip.1-1-1-1::GRID.titleGrob.5368::GRID.text.5364"
# Generate some color
n_cols <- length(strip_bg_gpath)
fills <- c("#FFC000","#FFD966", "#9BC2E6","#5B9BD5")
# Edit the grobs
for (i in 1:length(strip_bg_gpath)){
  g <- editGrob(grob = g, gPath = strip_bg_gpath[i], gp = gpar(fill = fills[i]))
}
# Draw the edited plot
grid.newpage(); grid.draw(g)


plot_SystemUnderstanding_singleQ <- g
ggsave(filename = "data/results/figures/new/SystemUnderstanding_singleQ.png", plot_SystemUnderstanding_singleQ, 
       dpi = 500, units = "in", width = 8, height = 5, device='png', bg = "transparent")

rm(list = setdiff(ls(), c("vorbefragung", "nachbefragung", "fun_mean", "fun_median", "mean_FOT", "FOT_skim", 
                          "plot_TiA_all_scales", "TiA",
                          "plot_CTAM_all_scales", "CTAM",
                          "plot_NDRT_all_scales", "NDRT",
                          "plot_L2components", "L2compo", 
                          "plot_SystemUnderstanding", "plot_SystemUnderstanding_singleQ")))


#### plot monitoring (subj. ueberwachungsguete) ####
labels_monitoring <- c("0 -\ninattentive", "1", "2", "3", "4", "5", "6 -\nalways\nattentive")

plot_monitoring <- ggplot(nachbefragung, aes(x = interval, y=SubjUeberwachguete.1., fill = interval)) +
  stat_boxplot(geom ='errorbar', width = 0.3, lwd=0.2) +
  geom_boxplot(outlier.shape = 21, lwd=0.2, outlier.size = 0.7) +
  stat_summary(fun = mean, geom = "point" , colour="black", size=1, shape = 4) +
  scale_y_continuous(limits = c(0,6), breaks = seq(0,6,1), labels = labels_monitoring) +
  scale_fill_manual(values = c("#FFC000","#FFD966", "#9BC2E6","#5B9BD5")) +
  labs(y="Response", x="") +
  theme_bw() +
  theme(text=element_text(family = "sans", color="black", size=11, face = "bold"),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_line(size = 0.2),
        panel.grid.major.x = element_blank(),
        legend.position = "bottom", 
        legend.title = element_blank(),
        legend.text = element_text(family = "sans", color="black", size=11, face = "plain"),
        legend.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent",
                                       colour = NA_character_),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        panel.border=element_blank(),
        axis.line = element_line(colour = "black"),
        axis.text.y=element_text(color = "black", size=9, face = "plain"))
plot_monitoring

ggsave(filename = "data/results/figures/new/SubjMonitoringPerformance_new.png", plot_monitoring, 
       dpi = 500, width = 5, height = 3, units = "in", device='png', bg = "transparent")

rm(list = setdiff(ls(), c("vorbefragung", "nachbefragung", "fun_mean", "fun_median", "mean_FOT", "FOT_skim", 
                         "plot_TiA_all_scales", "TiA",
                         "plot_CTAM_all_scales", "CTAM",
                         "plot_NDRT_all_scales", "NDRT",
                         "plot_L2components", "L2compo", 
                         "plot_monitoring")))
#### plot influence setting ####
labels_setting <- c("affected", "not affected")

p <- ggplot(nachbefragung, aes(x = SubjEinflussSetting)) +
  geom_bar(fill = "#E7E6E6", color = "black", width = 0.8, size = 0.2) +
  facet_grid(~interval) +
  ylim(0, 30) +
  scale_x_discrete(limit = c("0", "1"), labels = labels_setting) + # NA removed
  labs(y="n", x="") +
  geom_text(size=3.2, stat='count', aes(label=..count..), position=position_dodge2(width=0.9, preserve=c("single")), vjust=-0.3) +
  theme_bw() +
  theme(text=element_text(family = "sans", color="black", size=11, face="bold"),
        panel.grid.minor.y = element_blank(), 
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(size = 0.2),
        legend.position = "none", 
        legend.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent",
                                       colour = NA_character_),
        axis.text.x=element_text(color = "black", face="plain", size=9, angle=0, vjust=1),
        axis.text.y=element_text(color = "black", face="plain", size=9))
p
## code of Valentin_Stefan to edit colors of facet boxes: 
# https://stackoverflow.com/questions/53455092/r-ggplot2-change-colour-of-font-and-background-in-facet-strip
# Generate the ggplot2 plot grob
g <- grid.force(ggplotGrob(p))
# Get the names of grobs and their gPaths into a data.frame structure
grobs_df <- do.call(cbind.data.frame, grid.ls(g, print = FALSE))
# Build optimal gPaths that will be later used to identify grobs and edit them
grobs_df$gPath_full <- paste(grobs_df$gPath, grobs_df$name, sep = "::")
grobs_df$gPath_full <- gsub(pattern = "layout::", 
                            replacement = "", 
                            x = grobs_df$gPath_full, 
                            fixed = TRUE)
# Get the gPaths of the strip background grobs
strip_bg_gpath <- grobs_df$gPath_full[grepl(pattern = ".*strip\\.background.*", 
                                            x = grobs_df$gPath_full)]
strip_bg_gpath[1]
strip_txt_gpath <- grobs_df$gPath_full[grepl(pattern = "strip.*titleGrob.*text.*", 
                                             x = grobs_df$gPath_full)]
strip_txt_gpath[1] # example of a gPath for strip title
## [1] "strip-t-1.7-5-7-5::strip.1-1-1-1::GRID.titleGrob.5368::GRID.text.5364"
# Generate some color
n_cols <- length(strip_bg_gpath)
fills <- c("#FFC000","#FFD966", "#9BC2E6","#5B9BD5")
# Edit the grobs
for (i in 1:length(strip_bg_gpath)){
  g <- editGrob(grob = g, gPath = strip_bg_gpath[i], gp = gpar(fill = fills[i]))
}
# Draw the edited plot
grid.newpage(); grid.draw(g)

plot_setting <- g
ggsave(filename = "data/results/figures/new/SubjInfluenceSetting_new.png", g, 
       dpi = 500, units = "in", width = 8, height = 3, device='png', bg = "transparent")

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
  filter(interval == "A L2H-off")

plot_ranking <- ggplot(nachbefragung_Aoff, aes(x = Ranking)) +
  geom_bar(fill = "#E7E6E6", color = "black", width = 0.8, size = 0.2) +
  scale_x_discrete(limit = c("1", "2"), labels = labels_ranking) + # NA removed
  ylim(0, 30) +
  labs(y="n", x="") +
  geom_text(size=3.2,stat='count', 
            aes(label=..count..), 
            position=position_dodge2(width=0.9, preserve=c("single")), vjust=-0.3) +
  theme_bw() +
  theme(text=element_text(family = "sans", color="black", size=11, face="bold"),
        panel.grid.minor.y = element_blank(), 
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(size = 0.2),
        legend.position = "none", 
        legend.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent",
                                       colour = NA_character_),
        panel.border=element_blank(),
        axis.line = element_line(colour = "black"),
        axis.text.x=element_text(color = "black", face="plain", size=9, angle=0, vjust=1),
        axis.text.y=element_text(color = "black", face="plain", size=9))
plot_ranking

ggsave(filename = "data/results/figures/new/Ranking_new.png", plot_ranking,  
       dpi = 500, units = "in", width = 4, height = 5, device='png', bg = "transparent")

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
plot_age <- ggplot(vorbefragung, aes(x = group, y=Alter, fill = group)) +
  stat_boxplot(geom ='errorbar', width = 0.3, lwd=0.2) +
  geom_boxplot(outlier.shape = 21, lwd=0.2, outlier.size = 0.7) +
  stat_summary(fun = mean, geom = "point" , colour="black", size=1, shape = 4) +
  scale_y_continuous(limits = c(20,80), breaks = seq(20,80,10)) +
  scale_fill_manual(values = c("#E7E6E6","#AEAAAA")) +
  labs(y="Age in years") +
  theme_bw() +
  theme(text=element_text(family = "sans", color="black", size=11, face = "bold"),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_line(size = 0.2),
        panel.grid.major.x = element_blank(),
        legend.position = "bottom", 
        legend.title = element_blank(),
        legend.text = element_text(family = "sans", color="black", size=11, face = "plain"),
        legend.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent",
                                       colour = NA_character_),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        panel.border=element_blank(),
        axis.line = element_line(colour = "black"),
        axis.text.y=element_text(color = "black", size=9, face = "plain"))
plot_age

ggsave(filename = "data/results/figures/new/Age_new.png", plot_age, 
       dpi = 500, width = 3, height = 3, units = "in", device='png', bg = "transparent")

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
plot_license <- ggplot(vorbefragung, aes(x = group, y=Fuehrerschein, fill = group)) +
  stat_boxplot(geom ='errorbar', width = 0.3, lwd=0.2) +
  geom_boxplot(outlier.shape = 21, lwd=0.2, outlier.size = 0.7) +
  stat_summary(fun = mean, geom = "point" , colour="black", size=1, shape = 4) +
  scale_y_continuous(limits = c(1960,2020), breaks = seq(1960,2020,10)) +
  scale_fill_manual(values = c("#E7E6E6","#AEAAAA")) +
  labs(y="Year - obtainment of driver's license") +
  theme_bw() +
  theme(text=element_text(family = "sans", color="black", size=11, face = "bold"),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_line(size = 0.2),
        panel.grid.major.x = element_blank(),
        legend.position = "bottom", 
        legend.title = element_blank(),
        legend.text = element_text(family = "sans", color="black", size=11, face = "plain"),
        legend.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent",
                                       colour = NA_character_),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        panel.border=element_blank(),
        axis.line = element_line(colour = "black"),
        axis.text.y=element_text(color = "black", size=9, face = "plain"))
plot_license

ggsave(filename = "data/results/figures/new/License_new.png", plot_license, 
       dpi = 500, width = 3, height = 4, units = "in", device='png', bg = "transparent")

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
## subsets ##
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
## build subset ##
DSQ <- bind_rows(DSQ_Speed, DSQ_Calmness, DSQ_SocialResistance, DSQ_Focus, DSQ_Planning, DSQ_Deviance) %>%
  mutate(scale = factor(scale, levels = c("DSQ_Speed", "DSQ_Calmness", "DSQ_SocialResistance", "DSQ_Focus", "DSQ_Planning", "DSQ_Deviance"), ordered = TRUE))
## lables ##
labels_DSQ = c("Speed", "Calmness", "Social Resistance", 
               "Focus", "Planning", "Deviance")

## basic plot ##
p <- ggplot(DSQ, aes(x=scale, y=score, fill=scale)) + 
  stat_boxplot(geom ='errorbar', width = 0.3, lwd=0.2) +
  geom_boxplot(outlier.shape = 21, lwd=0.2, outlier.size = 0.7) +
  stat_summary(fun = mean, geom = "point" , colour="black", size=1, shape = 4) +
  scale_x_discrete(labels = labels_DSQ) +
  scale_y_continuous(limits = c(2,18), breaks = seq(2,18,2)) +
  facet_wrap(~group) +
  scale_fill_manual(values = c("#E7E6E6", "#E7E6E6", "#E7E6E6", "#E7E6E6", "#E7E6E6", "#E7E6E6")) +
  labs(y="Score", x="") +
  theme_bw() +
  theme(text=element_text(family = "sans", color="black", size=11, face = "bold"),
        panel.grid.minor.y = element_blank(), 
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(size = 0.2),
        legend.position = "none", 
        plot.background = element_rect(fill = "transparent",
                                       colour = NA_character_),
        axis.text.x=element_text(color = "black", size=9, angle=20, vjust=.88, hjust=0.8, face = "plain"),
        axis.text.y=element_text(color = "black", size=9, face = "plain")) +
  geom_segment(aes(x = 0.6, y = 18, xend = 1.4, yend = 18), linetype = 2) + # speed
  geom_segment(aes(x = 0.6, y = 3, xend = 1.4, yend = 3), linetype = 2) +
  geom_segment(aes(x = 1.6, y = 18, xend = 2.4, yend = 18), linetype = 2) + # calmness
  geom_segment(aes(x = 1.6, y = 3, xend = 2.4, yend = 3), linetype = 2) +
  geom_segment(aes(x = 2.6, y = 12, xend = 3.4, yend = 12), linetype = 2) + # social resistance
  geom_segment(aes(x = 2.6, y = 2, xend = 3.4, yend = 2), linetype = 2) +
  geom_segment(aes(x = 3.6, y = 18, xend = 4.4, yend = 18), linetype = 2) + # focus
  geom_segment(aes(x = 3.6, y = 3, xend = 4.4, yend = 3), linetype = 2) +
  geom_segment(aes(x = 4.6, y = 12, xend = 5.4, yend = 12), linetype = 2) + # planning
  geom_segment(aes(x = 4.6, y = 2, xend = 5.4, yend = 2), linetype = 2) +
  geom_segment(aes(x = 5.6, y = 12, xend = 6.4, yend = 12), linetype = 2) + # deviance
  geom_segment(aes(x = 5.6, y = 2, xend = 6.4, yend = 2), linetype = 2)
p

## code of Valentin_Stefan to edit colors of facet boxes: 
# https://stackoverflow.com/questions/53455092/r-ggplot2-change-colour-of-font-and-background-in-facet-strip
# Generate the ggplot2 plot grob
g <- grid.force(ggplotGrob(p))
# Get the names of grobs and their gPaths into a data.frame structure
grobs_df <- do.call(cbind.data.frame, grid.ls(g, print = FALSE))
# Build optimal gPaths that will be later used to identify grobs and edit them
grobs_df$gPath_full <- paste(grobs_df$gPath, grobs_df$name, sep = "::")
grobs_df$gPath_full <- gsub(pattern = "layout::", 
                            replacement = "", 
                            x = grobs_df$gPath_full, 
                            fixed = TRUE)
# Get the gPaths of the strip background grobs
strip_bg_gpath <- grobs_df$gPath_full[grepl(pattern = ".*strip\\.background.*", 
                                            x = grobs_df$gPath_full)]
strip_bg_gpath[1]
strip_txt_gpath <- grobs_df$gPath_full[grepl(pattern = "strip.*titleGrob.*text.*", 
                                             x = grobs_df$gPath_full)]
strip_txt_gpath[1] # example of a gPath for strip title
## [1] "strip-t-1.7-5-7-5::strip.1-1-1-1::GRID.titleGrob.5368::GRID.text.5364"
# Generate some color
n_cols <- length(strip_bg_gpath)
fills <- c("#E7E6E6","#AEAAAA")
# Edit the grobs
for (i in 1:length(strip_bg_gpath)){
  g <- editGrob(grob = g, gPath = strip_bg_gpath[i], gp = gpar(fill = fills[i]))
}
# Draw the edited plot
grid.newpage(); grid.draw(g)

plot_DSQ <- g
ggsave(filename = "data/results/figures/new/DSQ_new.png", g, 
       dpi = 500, width = 8, height = 3, units = "in", device='png', bg = "transparent")

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
plot_ATIS <- ggplot(vorbefragung, aes(x = group, y=ATIS, fill = group)) +
  stat_boxplot(geom ='errorbar', width = 0.3, lwd=0.2) +
  geom_boxplot(outlier.shape = 21, lwd=0.2, outlier.size = 0.7) +
  stat_summary(fun = mean, geom = "point" , colour="black", size=1, shape = 4) +
  scale_y_continuous(limits = c(1,6), breaks = seq(1,6,1)) +
  scale_fill_manual(values = c("#E7E6E6","#AEAAAA")) +
  labs(y="Score") +
  theme_bw() +
  theme(text=element_text(family = "sans", color="black", size=11, face = "bold"),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_line(size = 0.2),
        panel.grid.major.x = element_blank(),
        legend.position = "bottom", 
        legend.title = element_blank(),
        legend.text = element_text(family = "sans", color="black", size=11, face = "plain"),
        legend.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent",
                                       colour = NA_character_),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        panel.border=element_blank(),
        axis.line = element_line(colour = "black"),
        axis.text.y=element_text(color = "black", size=9, face = "plain"))
plot_ATIS

ggsave(filename = "data/results/figures/new/ATI-S_new.png", plot_ATIS, 
       dpi = 500, width = 3, height = 3, units = "in", device='png', bg = "transparent")

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
labels_frequency <- c("(seldom or)\nnever", "< 1x/month", "> 1x/month", 
                      "> 1x/week", "daily")

frequency_general <- vorbefragung %>%
  select(group, VPNr, Fahrtfrequenz) %>%
  rename(score = Fahrtfrequenz) %>%
  add_column(scale = "general", .after = "VPNr")

frequency_highway <- vorbefragung %>%
  select(group, VPNr, FrequenzAutobahn) %>%
  rename(score = FrequenzAutobahn) %>%
  add_column(scale = "highway", .after = "VPNr")

frequency <- bind_rows(frequency_general, frequency_highway)#  %>%
  # mutate(scale = factor(scale, levels = c("general", "highway"), ordered = TRUE))

  ## basic plot ##
  p <- ggplot(frequency, aes(x=scale, y=score, fill=scale)) + 
    stat_boxplot(geom ='errorbar', width = 0.3, lwd=0.2) +
    geom_boxplot(outlier.shape = 21, lwd=0.2, outlier.size = 0.7) +
    stat_summary(fun = mean, geom = "point" , colour="black", size=1, shape = 4) +
    scale_y_continuous(limits = c(0,4), breaks = seq(0,4,1), labels = labels_frequency) +
    facet_wrap(~group) +
    scale_fill_manual(values = c("#E7E6E6", "#E7E6E6", "#E7E6E6", "#E7E6E6", "#E7E6E6", "#E7E6E6")) +
    labs(y="Score", x="") +
    theme_bw() +
    theme(text=element_text(family = "sans", color="black", size=11, face = "bold"),
          panel.grid.minor.y = element_blank(), 
          panel.grid.major.x = element_blank(),
          panel.grid.major.y = element_line(size = 0.2),
          legend.position = "none", 
          plot.background = element_rect(fill = "transparent",
                                         colour = NA_character_),
          axis.text.x=element_text(color = "black", size=9, angle=00, vjust=.88, face = "plain"),
          axis.text.y=element_text(color = "black", size=9, face = "plain"))
  p
  ## code of Valentin_Stefan to edit colors of facet boxes: 
  # https://stackoverflow.com/questions/53455092/r-ggplot2-change-colour-of-font-and-background-in-facet-strip
  # Generate the ggplot2 plot grob
  g <- grid.force(ggplotGrob(p))
  # Get the names of grobs and their gPaths into a data.frame structure
  grobs_df <- do.call(cbind.data.frame, grid.ls(g, print = FALSE))
  # Build optimal gPaths that will be later used to identify grobs and edit them
  grobs_df$gPath_full <- paste(grobs_df$gPath, grobs_df$name, sep = "::")
  grobs_df$gPath_full <- gsub(pattern = "layout::", 
                              replacement = "", 
                              x = grobs_df$gPath_full, 
                              fixed = TRUE)
  # Get the gPaths of the strip background grobs
  strip_bg_gpath <- grobs_df$gPath_full[grepl(pattern = ".*strip\\.background.*", 
                                              x = grobs_df$gPath_full)]
  strip_bg_gpath[1]
  strip_txt_gpath <- grobs_df$gPath_full[grepl(pattern = "strip.*titleGrob.*text.*", 
                                               x = grobs_df$gPath_full)]
  strip_txt_gpath[1] # example of a gPath for strip title
  ## [1] "strip-t-1.7-5-7-5::strip.1-1-1-1::GRID.titleGrob.5368::GRID.text.5364"
  # Generate some color
  n_cols <- length(strip_bg_gpath)
  fills <- c("#E7E6E6","#AEAAAA")
  # Edit the grobs
  for (i in 1:length(strip_bg_gpath)){
    g <- editGrob(grob = g, gPath = strip_bg_gpath[i], gp = gpar(fill = fills[i]))
  }
  # Draw the edited plot
  grid.newpage(); grid.draw(g)
  
  plot_frequency <- g
  ggsave(filename = "data/results/figures/new/Frequency_new.png", g, 
         dpi = 500, width = 8, height = 3, units = "in", device='png', bg = "transparent")
  

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
labels_mileage <- c("0 km", "1 km -\n5.000 km", "5.001 km -\n20.000 km", "20.001 km -\n50.000 km", 
                    "50.001 km -\n100.000 km", "> 100.000 km")

mileage_general <- vorbefragung %>%
  select(group, VPNr, Fahrtstrecke) %>%
  rename(score = Fahrtstrecke) %>%
  add_column(scale = "general", .after = "VPNr")

mileage_highway <- vorbefragung %>%
  select(group, VPNr, StreckeAutobahn) %>%
  rename(score = StreckeAutobahn) %>%
  add_column(scale = "highway", .after = "VPNr")

mileage <- bind_rows(mileage_general, mileage_highway) # %>%
  # mutate(scale = factor(scale, levels = c("general", "highway"), ordered = TRUE))

## basic plot ##
p <- ggplot(mileage, aes(x=scale, y=score, fill=scale)) + 
  stat_boxplot(geom ='errorbar', width = 0.3, lwd=0.2) +
  geom_boxplot(outlier.shape = 21, lwd=0.2, outlier.size = 0.7) +
  stat_summary(fun = mean, geom = "point" , colour="black", size=1, shape = 4) +
  scale_y_continuous(limits = c(0,5), breaks = seq(0,5,1), labels = labels_mileage) +
  facet_wrap(~group) +
  scale_fill_manual(values = c("#E7E6E6", "#E7E6E6", "#E7E6E6", "#E7E6E6", "#E7E6E6", "#E7E6E6")) +
  labs(y="Score", x="") +
  theme_bw() +
  theme(text=element_text(family = "sans", color="black", size=11, face = "bold"),
        panel.grid.minor.y = element_blank(), 
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(size = 0.2),
        legend.position = "none", 
        plot.background = element_rect(fill = "transparent",
                                       colour = NA_character_),
        axis.text.x=element_text(color = "black", size=9, angle=00, vjust=.88, face = "plain"),
        axis.text.y=element_text(color = "black", size=9, face = "plain"))
p
## code of Valentin_Stefan to edit colors of facet boxes: 
# https://stackoverflow.com/questions/53455092/r-ggplot2-change-colour-of-font-and-background-in-facet-strip
# Generate the ggplot2 plot grob
g <- grid.force(ggplotGrob(p))
# Get the names of grobs and their gPaths into a data.frame structure
grobs_df <- do.call(cbind.data.frame, grid.ls(g, print = FALSE))
# Build optimal gPaths that will be later used to identify grobs and edit them
grobs_df$gPath_full <- paste(grobs_df$gPath, grobs_df$name, sep = "::")
grobs_df$gPath_full <- gsub(pattern = "layout::", 
                            replacement = "", 
                            x = grobs_df$gPath_full, 
                            fixed = TRUE)
# Get the gPaths of the strip background grobs
strip_bg_gpath <- grobs_df$gPath_full[grepl(pattern = ".*strip\\.background.*", 
                                            x = grobs_df$gPath_full)]
strip_bg_gpath[1]
strip_txt_gpath <- grobs_df$gPath_full[grepl(pattern = "strip.*titleGrob.*text.*", 
                                             x = grobs_df$gPath_full)]
strip_txt_gpath[1] # example of a gPath for strip title
## [1] "strip-t-1.7-5-7-5::strip.1-1-1-1::GRID.titleGrob.5368::GRID.text.5364"
# Generate some color
n_cols <- length(strip_bg_gpath)
fills <- c("#E7E6E6","#AEAAAA")
# Edit the grobs
for (i in 1:length(strip_bg_gpath)){
  g <- editGrob(grob = g, gPath = strip_bg_gpath[i], gp = gpar(fill = fills[i]))
}
# Draw the edited plot
grid.newpage(); grid.draw(g)

plot_mileage <- g
ggsave(filename = "data/results/figures/new/Mileage_new.png", g, 
       dpi = 500, width = 8, height = 3, units = "in", device='png', bg = "transparent")

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
labels_ADAS <- c("unknown", "known, but\nnever used", "seldomly\nused", "used\nregularly")

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

ADAS <- bind_rows(ADAS_CC, ADAS_ACC, ADAS_LKA, ADAS_TJA, ADAS_PA, ADAS_L2)  %>%
   mutate(scale = factor(scale, levels = c("CC", "ACC", "LKA", "TJA", "PA", "L2"), ordered = TRUE))

## basic plot ##
p <- ggplot(ADAS, aes(x=scale, y=score, fill=scale)) + 
  stat_boxplot(geom ='errorbar', width = 0.3, lwd=0.2) +
  geom_boxplot(outlier.shape = 21, lwd=0.2, outlier.size = 0.7) +
  stat_summary(fun = mean, geom = "point" , colour="black", size=1, shape = 4) +
  scale_y_continuous(limits = c(0,3), breaks = seq(0,3,1), labels = labels_ADAS) +
  facet_wrap(~group) +
  scale_fill_manual(values = c("#E7E6E6", "#E7E6E6", "#E7E6E6", "#E7E6E6", "#E7E6E6", "#E7E6E6")) +
  labs(y="Score", x="") +
  theme_bw() +
  theme(text=element_text(family = "sans", color="black", size=11, face = "bold"),
        panel.grid.minor.y = element_blank(), 
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(size = 0.2),
        legend.position = "none", 
        plot.background = element_rect(fill = "transparent",
                                       colour = NA_character_),
        axis.text.x=element_text(color = "black", size=9, angle=00, vjust=.88, face = "plain"),
        axis.text.y=element_text(color = "black", size=9, face = "plain"))
p
## code of Valentin_Stefan to edit colors of facet boxes: 
# https://stackoverflow.com/questions/53455092/r-ggplot2-change-colour-of-font-and-background-in-facet-strip
# Generate the ggplot2 plot grob
g <- grid.force(ggplotGrob(p))
# Get the names of grobs and their gPaths into a data.frame structure
grobs_df <- do.call(cbind.data.frame, grid.ls(g, print = FALSE))
# Build optimal gPaths that will be later used to identify grobs and edit them
grobs_df$gPath_full <- paste(grobs_df$gPath, grobs_df$name, sep = "::")
grobs_df$gPath_full <- gsub(pattern = "layout::", 
                            replacement = "", 
                            x = grobs_df$gPath_full, 
                            fixed = TRUE)
# Get the gPaths of the strip background grobs
strip_bg_gpath <- grobs_df$gPath_full[grepl(pattern = ".*strip\\.background.*", 
                                            x = grobs_df$gPath_full)]
strip_bg_gpath[1]
strip_txt_gpath <- grobs_df$gPath_full[grepl(pattern = "strip.*titleGrob.*text.*", 
                                             x = grobs_df$gPath_full)]
strip_txt_gpath[1] # example of a gPath for strip title
## [1] "strip-t-1.7-5-7-5::strip.1-1-1-1::GRID.titleGrob.5368::GRID.text.5364"
# Generate some color
n_cols <- length(strip_bg_gpath)
fills <- c("#E7E6E6","#AEAAAA")
# Edit the grobs
for (i in 1:length(strip_bg_gpath)){
  g <- editGrob(grob = g, gPath = strip_bg_gpath[i], gp = gpar(fill = fills[i]))
}
# Draw the edited plot
grid.newpage(); grid.draw(g)

plot_ADAS <- g
ggsave(filename = "data/results/figures/new/ADAS_new.png", g, 
       dpi = 500, width = 8, height = 3, units = "in", device='png', bg = "transparent")

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
#   scale_fill_manual("interval", values = c("A L2H-on (fc)" = "#64A0C8", "A L2H-on (fam)" = "#98C6EA", "A L2H-off" = "#999999", "B L2H-off" = "#DAD7CB")) +
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
#   scale_fill_manual("interval", values = c("A L2H-on (fc)" = "#64A0C8", "A L2H-on (fam)" = "#98C6EA", "A L2H-off" = "#999999", "B L2H-off" = "#DAD7CB")) +
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


#### not applied: beginning of visualization of correct and uncertain answers in one plot (SU) ####
# SU_new <- SystemUnderstanding_singleQ_means %>%
#   mutate(mean_uncertain = 1 -mean )
# q <- ggplot(SU_new, aes(x = question)) +
#   geom_line(aes(group = interval, y = mean*100)) +
#   geom_line(aes(group = interval, y = mean_uncertain*100)) +
#   geom_point(aes(group = interval, y = mean_uncertain*100)) +
#   geom_point(aes(group = interval, y = mean*100)) +
#   # stat_summary(fun = mean, geom="point",colour="black", size=1) +
#   # stat_summary(fun.data = fun_mean, geom="text", vjust=1.8, size=3.1) +
#   labs(y="Correct answers [%]") +
#   facet_grid(interval ~.) +
#   ylim(0, 100) +
#   theme_bw() +
#   theme(text=element_text(family = "sans", color="black", size=11, face="bold"),
#         panel.grid.minor.y = element_blank(),
#         panel.grid.major.x = element_blank(),
#         legend.position = "none", 
#         axis.title.x=element_blank(),
#         axis.ticks.x=element_blank(),
#         axis.text.x=element_text(color = "black", size=9, angle=30, vjust=.8, hjust=0.8, face="plain"),
#         axis.text.y=element_text(color = "black", size=9, face = "plain"))
# q