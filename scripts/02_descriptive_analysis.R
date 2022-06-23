#### preparations ####
rm(list = ls())
library(tidyverse)
library(skimr)
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
# ## not needed variables, n = 99 ##
#     Vertrauen.TiA... (n = 19)
#     Akzeptanz... (n = 23)
#     System... (n = 32)
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
#     System_sum

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
  select(interval, DauerFahrt, L2AnteilFahrt, KmVorFahrt, starts_with("TiA"), starts_with("CTAM"), System_sum)

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

#### save results ####
write_excel_csv(summ_vorbefragung, "data/results/summary_vorbefragung.csv")
write_excel_csv(summ_nachbefragung, "data/results/summary_nachbefragung.csv")

#### further tryouts for frequency tables ####
## check: 
# help(top_counts)
# https://tidyr.tidyverse.org/reference/separate.html
# https://rdrr.io/cran/psych/man/multi.hist.html
# http://dwoll.de/rexrepos/posts/frequencies.html


# names <- c("Geschlecht", "Haendigkeit", "Sehschwaeche")
# subset_vor_nominal %>%
#   group_by(group) %>%
#   count(Geschlecht, Sehschwaeche)
# 
# subset_vor_nominal %>%
#   count(Geschlecht, Sehschwaeche)
# 
# 
# trennvariablen <- c(Geschlecht, Haendigkeit, Sehschwaeche)
# 
# for (i in trennvariablen) {
#   subset_vor_nominal %>%
#     count(group, i)
# }
# 
# subset_vor_nominal %>%
#   count(group, Geschlecht)
# 
# 
# test <- summary_vor_nominal
# 
# separate(test, factor.top_counts, into = c("f0", "f1", "f2", "f3", "f4", "f5"))
# 
# 
# FOT_skim <- skim_with(numeric = sfl(sorted_count = sorted_count, n = length, mean = mean_FOT, sd = sd_FOT, median = median_FOT, min =  min_FOT, max = max_FOT), append = FALSE)
# 
# summary_vor_nominal <- subset_vor_nominal %>%
#   dplyr::group_by(group) %>%
#   FOT_skim(.)