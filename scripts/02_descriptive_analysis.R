#### preparations ####
rm(list = ls())
library(tidyverse)
library(skimr)
library(data.table)
setwd("~/R/VDA_FOT")

#### write function for summary ####
# adjusted from skim() function
mean_FOT = function(x) mean(x, na.rm = TRUE)
sd_FOT = function(x) sd(x, na.rm = TRUE)
median_FOT = function(x) median(x, na.rm = TRUE)
min_FOT = function(x) min(x, na.rm = TRUE)
max_FOT = function(x) max(x, na.rm = TRUE)

FOT_skim <- skim_with(numeric = sfl(n = length, mean = mean_FOT, sd = sd_FOT, median = median_FOT, min =  min_FOT, max = max_FOT), append = FALSE)

#### import data ####
# Read in files
vorbefragung <- read.csv("data/preprocessed/vorbefragung_scores.csv", encoding = "UTF-8")
nachbefragung <- read.csv("data/preprocessed/nachbefragung_scores.csv", encoding = "UTF-8")

#### notes on variables of vorbefragung ####
# ## meta variables, n = 2 ##
#     group
#     VPNr
# ## not needed variables, n = 150 ##
#     FiltHersteller... (n = 126)
#     raw data,  Fahrstil.DSQ... (n = 15)
#     raw data, Technikaffinitaet.ATIS... (n = 9)
# ## nominal data, n = 5 ##
#     Geschlecht
#     Haendigkeit
#     Sehschwaeche
#     Farbfehlsichtigkeit
#     Hoerschwaeche
# ## ordinal data, n = 10 ##
#     Fahrtfrequenz
#     Fahrtstrecke
#     FrequenzAutobahn
#     StreckeAutobahn
#     KenntnisAS.CC.
#     KenntnisAS.ACC.
#     KenntnisAS.Spurhalte.
#     KenntnisAS.Stauassistent.
#     KenntnisAS.ParkAssist.
#     KenntnisAS.Teilautomation.
# ## interval data, n = 9 ##
#     Alter
#     Fuehrerschein
#     DSQ_Speed
#     DSQ_Calmness
#     DSQ_SocialRestistance
#     DSQ_Focus
#     DSQ_Planning
#     DSQ_Deviance
#     ATIS

#### subsets vorbefragung ####
vorbefragung <- vorbefragung %>% 
  rename(group = X.U.FEFF.group)

subset_vor_nominal <- vorbefragung %>%
  select(group, Geschlecht, Haendigkeit, Sehschwaeche, Farbfehlsichtigkeit, Hoerschwaeche) %>%
  mutate(Geschlecht = as.factor(Geschlecht)) %>%
  mutate(Haendigkeit = as.factor(Haendigkeit)) %>%
  mutate(Sehschwaeche = as.factor(Sehschwaeche)) %>%
  mutate(Farbfehlsichtigkeit = as.factor(Farbfehlsichtigkeit)) %>%
  mutate(Hoerschwaeche = as.factor(Hoerschwaeche))

subset_vor_ordinal_nofactors <- vorbefragung %>%
  select(group, Fahrtfrequenz, Fahrtstrecke, FrequenzAutobahn, StreckeAutobahn, starts_with("KenntnisAS."))

subset_vor_ordinal_factors <- vorbefragung %>%
  select(group, Fahrtfrequenz, Fahrtstrecke, FrequenzAutobahn, StreckeAutobahn, starts_with("KenntnisAS.")) %>%
  mutate(Fahrtfrequenz = as.factor(Fahrtfrequenz)) %>%
  mutate(Fahrtstrecke = as.factor(Fahrtstrecke)) %>%
  mutate(FrequenzAutobahn = as.factor(FrequenzAutobahn)) %>%
  mutate(StreckeAutobahn = as.factor(StreckeAutobahn)) %>%
  mutate(Fahrtfrequenz = as.factor(Fahrtfrequenz)) %>%
  mutate(KenntnisAS.CC. = as.factor(KenntnisAS.CC.)) %>%
  mutate(KenntnisAS.ACC. = as.factor(KenntnisAS.ACC.)) %>%
  mutate(KenntnisAS.Spurhalte. = as.factor(KenntnisAS.Spurhalte.)) %>%
  mutate(KenntnisAS.Stauassistent. = as.factor(KenntnisAS.Stauassistent.)) %>%
  mutate(KenntnisAS.ParkAssist. = as.factor(KenntnisAS.ParkAssist.)) %>%
  mutate(KenntnisAS.Teilautomation. = as.factor(KenntnisAS.Teilautomation.))

subset_vor_interval <- vorbefragung %>%
  select(group, Alter, Fuehrerschein, starts_with("DSQ"), ATIS)

#### descriptive summaries vorbefragung ####
summary_vor_nominal <- subset_vor_nominal %>%
  dplyr::group_by(group) %>%
  FOT_skim(.) %>%
  select (-c(skim_type, complete_rate, factor.ordered, factor.n_unique))

summary_vor_ordinal_factors <- subset_vor_ordinal_factors %>%
  dplyr::group_by(group) %>%
  FOT_skim(.) %>% 
  select (-c(skim_type, complete_rate, factor.ordered, factor.n_unique))

summary_vor_ordinal_nofactors <- subset_vor_ordinal_nofactors %>%
  dplyr::group_by(group) %>%
  FOT_skim(.) %>%
  select (-c(skim_type, complete_rate))

summary_vor_interval <- subset_vor_interval %>%
  dplyr::group_by(group) %>%
  FOT_skim(.) %>%
  select (-c(skim_type, complete_rate))

summary_vor_ordinal <- left_join(summary_vor_ordinal_nofactors, summary_vor_ordinal_factors)

summary_vorbefragung <- bind_rows(summary_vor_nominal, summary_vor_ordinal, summary_vor_interval)

summary_headers <- c("variable", "group", "n_missing", "frequencies", "n", "mean", "sd", "median", "min", "max")
names(summary_vorbefragung) <- summary_headers

summ_vorbefragung <- summary_vorbefragung %>%
  mutate(n = n - n_missing) %>%
  select(-c(n_missing)) %>%
  mutate(mean = round(mean, 2)) %>%
  mutate(sd = round(sd, 2)) %>%
  mutate(median = round(median, 2)) %>%
  mutate(min = round(min, 0)) %>%
  mutate(max = round(max, 0)) %>%
  relocate(frequencies, .after = "max")

#### remove not needed data ####
rm(list=setdiff(ls(), c("vorbefragung", "nachbefragung", "summ_vorbefragung", 
                        "mean_FOT", "sd_FOT", "median_FOT", "min_FOT", "max_FOT", "FOT_skim")))

#### notes on variables of nachbefragung ####
# ## meta variables, n = 4  ##
#     group
#     interval
#     VPNr
#     DatumFahrt
# ## not needed variables, n = 115 ##
#     Vertrauen.TiA... (n = 19)
#     Akzeptanz... (n = 23)
#     System... (n = 48)
#     MatrixNDRTsFreitext... (n = 6)
#     SubjEinflussSettKomm:Studie (n = 19)
# ## nominal data, n = 1 ##
#     AutoFahrt
# ## ordinal data, n = 15 ##
#     NDRTs.NDRT... (n = 8)
#     SubjUeberwachguete.1.
#     SubjEinflussSetting
#     L2PrivNutzung
#     L2Komponenten... (n = 3)
#     Ranking
# ## interval data, n =  17 ##
#     DauerFahrt
#     L2AnteilFahrt
#     KmVorFahrt
#     TiA... (n = 7)
#     CTAM... (n = 6)
#     SU_System_sum
#     SU_Role_sum

#### subsets nachbefragung ####
nachbefragung <- nachbefragung %>%
  mutate(AutoFahrt = ifelse(AutoFahrt == "On_BMW", "OnBMW", ifelse(AutoFahrt == "On_VW", "OnVW",
                     ifelse(AutoFahrt == "On_Ford", "OnFord", ifelse(AutoFahrt == "On_Porsche", "OnPorsche",
                     ifelse(AutoFahrt == "Off_BMW", "OfBMW", ifelse(AutoFahrt == "Off_Ford", "OfFord",                                                
                                                                     "missing")))))))

subset_nach_nominal <- nachbefragung %>%
  select(interval, AutoFahrt) %>%
  mutate(AutoFahrt = as.factor(AutoFahrt))

vars_ordinal <- c("NDRTs.NDRT1.", "NDRTs.NDRT2.", "NDRTs.NDRT3.", "NDRTs.NDRT4.",
                  "NDRTs.NDRT5.", "NDRTs.NDRT6.","NDRTs.NDRT7.", "NDRTs.NDRT8.",
                  "SubjUeberwachguete.1.", "SubjEinflussSetting", "L2PrivNutzung",
                  "L2Komponenten.Laengs.", "L2Komponenten.Quer.", "L2Komponenten.Hoff.", "Ranking")

subset_nach_ordinal_factors <- nachbefragung %>%
  select(interval, vars_ordinal) %>%
  mutate_at(c(vars_ordinal), factor)

subset_nach_ordinal_nofactors <- nachbefragung %>%
  select(interval, vars_ordinal)

subset_nach_interval <- nachbefragung %>%
  select(interval, DauerFahrt, L2AnteilFahrt, KmVorFahrt, 
         starts_with("TiA"), starts_with("CTAM"), SU_System_sum, SU_Role_sum)

#### descriptive summaries nachbefragung ####
summary_nach_nominal <- subset_nach_nominal %>%
  dplyr::group_by(interval) %>%
  FOT_skim(.) %>%
  select (-c(skim_type, complete_rate, factor.ordered, factor.n_unique))

summary_nach_ordinal_factors <- subset_nach_ordinal_factors %>%
  dplyr::group_by(interval) %>%
  FOT_skim(.) %>% 
  select (-c(skim_type, complete_rate, factor.ordered, factor.n_unique))

summary_nach_ordinal_nofactors <- subset_nach_ordinal_nofactors %>%
  dplyr::group_by(interval) %>%
  FOT_skim(.) %>%
  select (-c(skim_type, complete_rate))

summary_nach_interval <- subset_nach_interval %>%
  dplyr::group_by(interval) %>%
  FOT_skim(.) %>%
  select (-c(skim_type, complete_rate))

summary_nach_ordinal <- left_join(summary_nach_ordinal_nofactors, summary_nach_ordinal_factors)

summary_nachbefragung <- bind_rows(summary_nach_nominal, summary_nach_ordinal, summary_nach_interval)

summary_headers <- c("variable", "group", "n_missing", "frequencies", "n", "mean", "sd", "median", "min", "max")
names(summary_nachbefragung) <- summary_headers

summ_nachbefragung <- summary_nachbefragung %>%
  mutate(n = n - n_missing) %>%
  select(-c(n_missing)) %>%
  mutate(mean = round(mean, 2)) %>%
  mutate(sd = round(sd, 2)) %>%
  mutate(median = round(median, 2)) %>%
  mutate(min = round(min, 0)) %>%
  mutate(max = round(max, 0)) %>%
  relocate(frequencies, .after = "max")

#### remove not needed data ####
rm(list=setdiff(ls(), c("vorbefragung", "nachbefragung", "summ_vorbefragung", "summ_nachbefragung",
                        "mean_FOT", "sd_FOT", "median_FOT", "min_FOT", "max_FOT", "FOT_skim")))

#### table familiarity with ADAS - manufacturers ####
# function for frequency tables
notemptycount_FOT <- function(x) {  (sum(x!="", na.rm = TRUE))}

list_man <- c("VW", "Opel", "Mercedes", "Ford", "BMW", 
              "Audi", "Skoda", "Renault", "Toyota", "Seat", 
              "Hyundai", "Fiat", "Nissan", "Peugeot", "Mazda",
              "Kia", "Dacia", "Citroen", "Volvo", "Mitsubishi", 
              "other")
## group A
sel_ADAS_A <- vorbefragung %>%
  select(group, VPNr, starts_with("FiltHersteller")) %>%
  filter(group == "A")
ADAS_a <- as.data.frame(sapply(sel_ADAS_A, notemptycount_FOT))
ADAS_a_t <- transpose(ADAS_a)
colnames(ADAS_a_t) <- rownames(ADAS_a)

A_CC <- ADAS_a_t %>%
  select(starts_with("FiltHerstellerCC.")) %>%
  setNames(., list_man) %>%
  add_column(group = "A", ADAS = "CC", .before = "VW")
A_ACC <- ADAS_a_t %>%
  select(starts_with("FiltHerstellerACC.")) %>%
  setNames(., list_man) %>%
  add_column(group = "A", ADAS = "ACC", .before = "VW")
A_LKA <- ADAS_a_t %>%
  select(starts_with("FiltHerstellerSpurha.")) %>%
  setNames(., list_man) %>%
  add_column(group = "A", ADAS = "LKA", .before = "VW")
A_TJA <- ADAS_a_t %>%
  select(starts_with("FiltHerstellerStauas.")) %>%
  setNames(., list_man) %>%
  add_column(group = "A", ADAS = "TJA", .before = "VW")
A_PA <- ADAS_a_t %>%
  select(starts_with("FiltHerstellerParkAs.")) %>%
  setNames(., list_man) %>%
  add_column(group = "A", ADAS = "PA", .before = "VW")
A_L2 <- ADAS_a_t %>%
  select(starts_with("FiltHerstellerTeilau.")) %>%
  setNames(., list_man) %>%
  add_column(group = "A", ADAS = "L2", .before = "VW")

A_man_ADAS <- bind_rows(A_CC, A_ACC, A_LKA, A_TJA, A_PA, A_L2)

## group B
sel_ADAS_B <- vorbefragung %>%
  select(group, VPNr, starts_with("FiltHersteller")) %>%
  filter(group == "B")
ADAS_b <- as.data.frame(sapply(sel_ADAS_B, notemptycount_FOT))
ADAS_b_t <- transpose(ADAS_b)
colnames(ADAS_b_t) <- rownames(ADAS_b)

B_CC <- ADAS_b_t %>%
  select(starts_with("FiltHerstellerCC.")) %>%
  setNames(., list_man) %>%
  add_column(group = "B", ADAS = "CC", .before = "VW")
B_ACC <- ADAS_b_t %>%
  select(starts_with("FiltHerstellerACC.")) %>%
  setNames(., list_man) %>%
  add_column(group = "B", ADAS = "ACC", .before = "VW")
B_LKA <- ADAS_b_t %>%
  select(starts_with("FiltHerstellerSpurha.")) %>%
  setNames(., list_man) %>%
  add_column(group = "B", ADAS = "LKA", .before = "VW")
B_TJA <- ADAS_b_t %>%
  select(starts_with("FiltHerstellerStauas.")) %>%
  setNames(., list_man) %>%
  add_column(group = "B", ADAS = "TJA", .before = "VW")
B_PA <- ADAS_b_t %>%
  select(starts_with("FiltHerstellerParkAs.")) %>%
  setNames(., list_man) %>%
  add_column(group = "B", ADAS = "PA", .before = "VW")
B_L2 <- ADAS_b_t %>%
  select(starts_with("FiltHerstellerTeilau.")) %>%
  setNames(., list_man) %>%
  add_column(group = "B", ADAS = "L2", .before = "VW")

B_man_ADAS <- bind_rows(B_CC, B_ACC, B_LKA, B_TJA, B_PA, B_L2)

## combine A & B
man_ADAS <- bind_rows(A_man_ADAS, B_man_ADAS)

## replace column other
man_ADAS_other <- data.frame (group = c("A", "A", "A", "A", "A", "A", "B", "B", "B", "B", "B", "B"),
                              ADAS = c("CC", "ACC", "LKA", "TJA", "PA", "L2"),
                              Saab  =     c(1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
                              Landrover = c(1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
                              Lexus =     c(1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
                              Jaguar =    c(0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0),
                              Mini =      c(0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0),     
                              Suzuki =    c(0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0),
                              Tesla =     c(0, 0, 0, 0, 0, 0, 2, 1, 0, 1, 0, 0),
                              MAN =       c(0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1))

man_ADAS_complete <- left_join(man_ADAS, man_ADAS_other, by = c("group", "ADAS")) %>%
  mutate(Volvo = ifelse(group == "A" & ADAS == "CC", Volvo+1, Volvo)) %>%
  select(-c("other")) %>%
  replace(is.na(.), 0)

#### remove not needed data ####
rm(list=setdiff(ls(), c("vorbefragung", "nachbefragung", "summ_vorbefragung", "summ_nachbefragung",
                        "mean_FOT", "sd_FOT", "median_FOT", "min_FOT", "max_FOT", "FOT_skim", 
                        "notemptycount_FOT","man_ADAS_complete")))

#### frequency tables of nominal and ordinal data: vor- and nachbefragung ####
# functions for frequency tables
count0_FOT <- function(x) {  (sum(x==0, na.rm = TRUE))}
count1_FOT <- function(x) {  (sum(x==1, na.rm = TRUE))}
count2_FOT <- function(x) {  (sum(x==2, na.rm = TRUE))}
count3_FOT <- function(x) {  (sum(x==3, na.rm = TRUE))}
count4_FOT <- function(x) {  (sum(x==4, na.rm = TRUE))}
count5_FOT <- function(x) {  (sum(x==5, na.rm = TRUE))}
count6_FOT <- function(x) {  (sum(x==6, na.rm = TRUE))}

### vorbefragung ###
## preparation
vor_variables = c("Geschlecht", "Haendigkeit", "Sehschwaeche", "Farbfehlsichtigkeit", "Hoerschwaeche", 
                  "Fahrtfrequenz", "Fahrtstrecke", "FrequenzAutobahn", "StreckeAutobahn", 
                  "KenntnisAS.CC.", "KenntnisAS.ACC.", "KenntnisAS.Spurhalte.",
                  "KenntnisAS.Stauassistent.", "KenntnisAS.ParkAssist.", "KenntnisAS.Teilautomation.")

## A
vor_A <- vorbefragung %>%
  filter(group == "A") %>%
  select(all_of(vor_variables))

vor_A0 <- as.data.frame(sapply(vor_A, count0_FOT))
vor_A1 <- as.data.frame(sapply(vor_A, count1_FOT))
vor_A2 <- as.data.frame(sapply(vor_A, count2_FOT))
vor_A3 <- as.data.frame(sapply(vor_A, count3_FOT))
vor_A4 <- as.data.frame(sapply(vor_A, count4_FOT))
vor_A5 <- as.data.frame(sapply(vor_A, count5_FOT))
vor_An <- as.data.frame(sapply(vor_A, notemptycount_FOT))

vor_freq_A <- data.frame(vor_An, vor_A0, vor_A1, vor_A2, vor_A3, vor_A4, vor_A5) %>%
  setNames(., c("n", "0", "1", "2", "3", "4", "5")) %>%
  add_column(group = "A", variable = vor_variables, .before = "n")

## B
vor_B <- vorbefragung %>%
  filter(group == "B") %>%
  select(all_of(vor_variables))

vor_B0 <- as.data.frame(sapply(vor_B, count0_FOT))
vor_B1 <- as.data.frame(sapply(vor_B, count1_FOT))
vor_B2 <- as.data.frame(sapply(vor_B, count2_FOT))
vor_B3 <- as.data.frame(sapply(vor_B, count3_FOT))
vor_B4 <- as.data.frame(sapply(vor_B, count4_FOT))
vor_B5 <- as.data.frame(sapply(vor_B, count5_FOT))
vor_Bn <- as.data.frame(sapply(vor_B, notemptycount_FOT))

vor_freq_B <- data.frame(vor_Bn, vor_B0, vor_B1, vor_B2, vor_B3, vor_B4, vor_B5) %>%
  setNames(., c("n", "0", "1", "2", "3", "4", "5")) %>%
  add_column(group = "B", variable = vor_variables, .before = "n")

## combine A & B
vor_freq <- bind_rows(vor_freq_A, vor_freq_B)
rownames(vor_freq) <- c()

### nachbefragung ###
## preparation
nach_variables = c("NDRTs.NDRT1.", "NDRTs.NDRT2.", "NDRTs.NDRT3.", "NDRTs.NDRT4.", 
                   "NDRTs.NDRT5.", "NDRTs.NDRT6.", "NDRTs.NDRT7.", "NDRTs.NDRT8.",
                   "SubjUeberwachguete.1.", "SubjEinflussSetting",
                   "L2PrivNutzung", "L2Komponenten.Laengs.",
                   "L2Komponenten.Quer.", "L2Komponenten.Hoff.",
                   "Ranking", 
                   "Role_u_01", "Role_u_02", "Role_u_03", "Role_u_04",
                   "Role_u_08", "Role_u_09", "Role_u_10", "Role_u_11",
                   "System_u_01", "System_u_02", "System_u_03", "System_u_04",
                   "System_u_05", "System_u_06", "System_u_16", "System_u_17")

## A_on_fc
nach_Aonfc <- nachbefragung %>%
  filter(interval == "A_on_fc") %>%
  select(all_of(nach_variables))

nach_Aonfc0 <- as.data.frame(sapply(nach_Aonfc, count0_FOT))
nach_Aonfc1 <- as.data.frame(sapply(nach_Aonfc, count1_FOT))
nach_Aonfc2 <- as.data.frame(sapply(nach_Aonfc, count2_FOT))
nach_Aonfc3 <- as.data.frame(sapply(nach_Aonfc, count3_FOT))
nach_Aonfc4 <- as.data.frame(sapply(nach_Aonfc, count4_FOT))
nach_Aonfc5 <- as.data.frame(sapply(nach_Aonfc, count5_FOT))
nach_Aonfc6 <- as.data.frame(sapply(nach_Aonfc, count6_FOT))
nach_Aonfcn <- as.data.frame(sapply(nach_Aonfc, notemptycount_FOT))

nach_freq_Aonfc <- data.frame(nach_Aonfcn, nach_Aonfc0, nach_Aonfc1, 
                              nach_Aonfc2, nach_Aonfc3, nach_Aonfc4, nach_Aonfc5, 
                              nach_Aonfc6) %>%
  setNames(., c("n", "0", "1", "2", "3", "4", "5", "6")) %>%
  add_column(group = "A_on_fc", variable = nach_variables, .before = "n")

## A_on_fam
nach_Aonfam <- nachbefragung %>%
  filter(interval == "A_on_fam") %>%
  select(all_of(nach_variables))

nach_Aonfam0 <- as.data.frame(sapply(nach_Aonfam, count0_FOT))
nach_Aonfam1 <- as.data.frame(sapply(nach_Aonfam, count1_FOT))
nach_Aonfam2 <- as.data.frame(sapply(nach_Aonfam, count2_FOT))
nach_Aonfam3 <- as.data.frame(sapply(nach_Aonfam, count3_FOT))
nach_Aonfam4 <- as.data.frame(sapply(nach_Aonfam, count4_FOT))
nach_Aonfam5 <- as.data.frame(sapply(nach_Aonfam, count5_FOT))
nach_Aonfam6 <- as.data.frame(sapply(nach_Aonfam, count6_FOT))
nach_Aonfamn <- as.data.frame(sapply(nach_Aonfam, notemptycount_FOT))

nach_freq_Aonfam <- data.frame(nach_Aonfamn, nach_Aonfam0, nach_Aonfam1, 
                               nach_Aonfam2, nach_Aonfam3, nach_Aonfam4, nach_Aonfam5, 
                               nach_Aonfam6) %>%
  setNames(., c("n", "0", "1", "2", "3", "4", "5", "6")) %>%
  add_column(group = "A_on_fam", variable = nach_variables, .before = "n")

## A_off
nach_Aoff <- nachbefragung %>%
  filter(interval == "A_off") %>%
  select(all_of(nach_variables))

nach_Aoff0 <- as.data.frame(sapply(nach_Aoff, count0_FOT))
nach_Aoff1 <- as.data.frame(sapply(nach_Aoff, count1_FOT))
nach_Aoff2 <- as.data.frame(sapply(nach_Aoff, count2_FOT))
nach_Aoff3 <- as.data.frame(sapply(nach_Aoff, count3_FOT))
nach_Aoff4 <- as.data.frame(sapply(nach_Aoff, count4_FOT))
nach_Aoff5 <- as.data.frame(sapply(nach_Aoff, count5_FOT))
nach_Aoff6 <- as.data.frame(sapply(nach_Aoff, count6_FOT))
nach_Aoffn <- as.data.frame(sapply(nach_Aoff, notemptycount_FOT))

nach_freq_Aoff <- data.frame(nach_Aoffn, nach_Aoff0, nach_Aoff1, 
                             nach_Aoff2, nach_Aoff3, nach_Aoff4, nach_Aoff5, 
                             nach_Aoff6) %>%
  setNames(., c("n", "0", "1", "2", "3", "4", "5", "6")) %>%
  add_column(group = "A_off", variable = nach_variables, .before = "n")

## B_off
nach_Boff <- nachbefragung %>%
  filter(interval == "B_off") %>%
  select(all_of(nach_variables))

nach_Boff0 <- as.data.frame(sapply(nach_Boff, count0_FOT))
nach_Boff1 <- as.data.frame(sapply(nach_Boff, count1_FOT))
nach_Boff2 <- as.data.frame(sapply(nach_Boff, count2_FOT))
nach_Boff3 <- as.data.frame(sapply(nach_Boff, count3_FOT))
nach_Boff4 <- as.data.frame(sapply(nach_Boff, count4_FOT))
nach_Boff5 <- as.data.frame(sapply(nach_Boff, count5_FOT))
nach_Boff6 <- as.data.frame(sapply(nach_Boff, count6_FOT))
nach_Boffn <- as.data.frame(sapply(nach_Boff, notemptycount_FOT))

nach_freq_Boff <- data.frame(nach_Boffn, nach_Boff0, nach_Boff1, 
                             nach_Boff2, nach_Boff3, nach_Boff4, nach_Boff5, 
                             nach_Boff6) %>%
  setNames(., c("n", "0", "1", "2", "3", "4", "5", "6")) %>%
  add_column(group = "B_off", variable = nach_variables, .before = "n")

## combine all 4 intervals
nach_freq <- bind_rows(nach_freq_Aonfc, nach_freq_Aonfam, nach_freq_Aoff, nach_freq_Boff)
rownames(nach_freq) <- c()


#### remove not needed data ####
rm(list=setdiff(ls(), c("vorbefragung", "nachbefragung", "summ_vorbefragung", "summ_nachbefragung",
                        "mean_FOT", "sd_FOT", "median_FOT", "min_FOT", "max_FOT", "FOT_skim", 
                        "man_ADAS_complete", 
                        "vor_freq", "nach_freq")))
#### save results ####
write_excel_csv(summ_vorbefragung, "data/results/summary_vorbefragung.csv")
write_excel_csv(summ_nachbefragung, "data/results/summary_nachbefragung.csv")
write_excel_csv(man_ADAS_complete, "data/results/vorbefragung_manufacturerADAS.csv")
write_excel_csv(vor_freq, "data/results/frequencies_vorbefragung.csv")
write_excel_csv(nach_freq, "data/results/frequencies_nachbefragung.csv")