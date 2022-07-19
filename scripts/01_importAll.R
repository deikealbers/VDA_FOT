#### note on data####
# missing data:
    # interview: VP77: data missing, only protocol available
    # h-on, A_on_fam: OTAL9/VP27: data missing, only meta data (and partly trust) available
# exclusions:
    # VP01 --> pilot test
    # VP67 --> exclusion because seat was adjusted in a way that DMS didn't work and 
    # numerous warnings occurred before setting could be adjusted
# other notes:
    # VP9 & VP11 have the same VPCode (twins)
    # VPCodes were adjusted where participants acidentally used different codes
    # VP20 & VP31 were assigned to B_off, however they belong to A_off
    # datasets ..._rec : contain recoded raw data: inverse items are converted!

#### preparations ####
rm(list = ls())
library(tidyverse)
setwd("~/R/VDA_FOT")

#### import data ####
# Read in files
raw_vorbefragung <- read.csv("data/raw/01_FOT_Rohdaten_LS_Vorbefragung_Codes.csv", encoding = "UTF-8")
raw_h_on <- read.csv("data/raw/02_FOT_Rohdaten_LS_H-on-Nachbefragung_Codes.csv", encoding = "UTF-8")
raw_h_off <- read.csv("data/raw/03_FOT_Rohdaten_LS_H-off-Nachbefragung_Codes.csv", encoding = "UTF-8")
raw_interview <- read.csv("data/raw/04_FOT_Rohdaten_LS_Interview_Codes.csv", encoding = "UTF-8")
VPCodes <- read.csv("data/other/VPCodes.csv", encoding = "UTF-8") # needed to add VP numbers to vorbefragung and h-on

#### VPCodes ####
VPCodes <- VPCodes %>%
  mutate(VPCode_h_on1 = ifelse(VPCode_h_on1 == "SERH9" & X.U.FEFF.VP.Nr == 11, "SERH9adjust", VPCode_h_on1)) # this code is used twice (VP9 and VP11)

#### vorbefragung ####
# delete and mutate
vorbefragung <- raw_vorbefragung %>%
  select(-c(X.U.FEFF.id, submitdate, lastpage, startlanguage, seed, startdate, datestamp, 
            Name.Vorname. : Kommentare, Instruktion)) %>%
  unite("VPCode_vorbefragung", Probandencode.Vorname.: Probandencode.Tag., sep = "") %>%
  mutate(VPCode_vorbefragung = ifelse(VPCode_vorbefragung == "Ra Mo51", "RaMo51", VPCode_vorbefragung)) # delete space otherwise join function doesn't work
# join with VP numbers and add information
processed_vorbefragung <- left_join(VPCodes, vorbefragung, by = "VPCode_vorbefragung") %>%
  rename(VPNr = X.U.FEFF.VP.Nr) %>% 
  select(-c(VPCode_vorbefragung, VPCode_h_on1, VPCode_h_on2)) %>%
  add_column(group = ifelse(.$VPNr <50, "A", "B"), .before = "VPNr") %>% # add group names
  subset(., VPNr != 1 & VPNr != 67) # exclude participants VPNr 1 (pilot test) & VPNr 67 (dysfunctional DMS because of seat settings)

#### h-on Nachbefragung ####
# delete and mutate
h_on <- raw_h_on %>%
  select(-c(X.U.FEFF.id, submitdate, lastpage, startlanguage, seed, datestamp, Einleitung, 
            DefGesamtsystem1, DefGesamtsystem2, DefGesamtsystem3, DefGesamtsystem4, DefGesamtsystem5,
            interviewtime : SonstKommentareTime)) %>%
  unite("VPCode_h_on1", Probandencode.Vorname.: Probandencode.Tag., sep = "") %>%
  mutate(VPCode_h_on1 = ifelse(VPCode_h_on1 == "TEMI8", "teMi8", VPCode_h_on1)) %>% # differing capitalization of codes causes problems when joining
  mutate(VPCode_h_on1 = ifelse(VPCode_h_on1 == "LiRh4", "LiRH4", VPCode_h_on1)) %>% # ""
  mutate(VPCode_h_on1 = ifelse(VPCode_h_on1 == "DeLa1", "deLa1", VPCode_h_on1)) %>% # ""
  mutate(VPCode_h_on1 = ifelse(VPCode_h_on1 == "OTAL9", "OtAl9", VPCode_h_on1)) %>% # ""
  mutate(VPCode_h_on1 = ifelse(VPCode_h_on1 == "THBO15", "THBO11", VPCode_h_on1)) %>% # different codes used by participants, see VPCodes.csv
  mutate(VPCode_h_on1 = ifelse(VPCode_h_on1 == "JUMÜ6", "TAMÜ6", VPCode_h_on1)) %>% # ""
  mutate(VPCode_h_on1 = ifelse(VPCode_h_on1 == "IAMÜ1", "IAMU1", VPCode_h_on1)) %>% # ""
  mutate(VPCode_h_on1 = ifelse(VPCode_h_on1 == "EnMü4", "NeMü4", VPCode_h_on1)) %>% # ""
  mutate(VPCode_h_on1 = ifelse(VPCode_h_on1 == "SERH9" & startdate == "2022-03-28 19:45:40", "SERH9adjust", VPCode_h_on1)) %>%
  mutate(VPCode_h_on1 = ifelse(VPCode_h_on1 == "SERH9" & startdate == "2022-04-01 09:57:54", "SERH9adjust", VPCode_h_on1)) %>% # ""
  mutate(AutoFahrt = ifelse(AutoFahrt == 0, "On_BMW", ifelse(AutoFahrt == 1, "On_VW", 
                                                             ifelse(AutoFahrt == 2, "On_Ford", ifelse(AutoFahrt == 3, "On_Porsche", "missing")))))

# join with VP numbers and add information
processed_h_on <- right_join(VPCodes, h_on, by = "VPCode_h_on1") %>%
  rename(VPNr = X.U.FEFF.VP.Nr) %>%
  select(-c(VPCode_vorbefragung, VPCode_h_on1, VPCode_h_on2)) %>%
  add_column(group = ifelse(.$VPNr <50, "A", "B"), .before = "VPNr") %>%  # add group names
  add_column(interval = ifelse(.$Fahrtblock == 1, "A_on_fc", "A_on_fam"), .after = "group") %>%  # add interval names
  relocate(startdate, .before = "group") %>%
  select(-c(Fahrtblock))
  
#### h-off Nachbefragung ####
# delete, modify and add information
processed_h_off <- raw_h_off %>%
  select(-c(X.U.FEFF.id, submitdate, lastpage, startlanguage, seed, datestamp, Einleitung,
            Probandencode.Vorname., Probandencode.Strasse., Probandencode.Tag.,
            DefGesamtsystem1, DefGesamtsystem2, DefGesamtsystem3, DefGesamtsystem4, DefGesamtsystem5,
            interviewtime : SonstKommentareTime)) %>%
  rename(VPNr = VP) %>%
  add_column(group = ifelse(.$VPNr <50, "A", "B"), .before = "VPNr") %>%  # add group names
  add_column(interval = ifelse(.$group == "A", "A_off", "B_off"), .before = "VPNr") %>%  # add interval names
  add_column(KmVorFahrt = 0, .after = "L2AnteilFahrt") %>% # add dummy column for merge with h-on
  mutate(AutoFahrt = ifelse(AutoFahrt == 0, "Off_BMW", ifelse(AutoFahrt == 1, "Off_Ford", "missing")))

#### interview ####
# delete, modify and add information
processed_interview <- raw_interview %>%
  select(-c(X.U.FEFF.id, submitdate, lastpage, startlanguage, seed, datestamp,
            interviewtime:StudieTime)) %>%
  rename(VPNr = VP) %>%
  add_column(group = ifelse(.$VPNr <50, "A", "B"), .before = "VPNr") %>%  # add group names
  mutate(Fahrt = ifelse(Fahrt == "B" & VPNr == 20, "A2", Fahrt)) %>% # incorrect assignment of experimenter
  mutate(Fahrt = ifelse(Fahrt == "B" & VPNr == 31, "A2", Fahrt)) %>% # ""
  add_column(interval = ifelse(.$Fahrt == "A1", "A_on_fam", 
                               ifelse(.$Fahrt == "A2", "A_off", "B_off")), .before = "VPNr")  %>% # add intervall names
  select(-c(Fahrt))%>%
  mutate(L2Komponenten.Hoff. = ifelse(.$interval == "A_on_fam", NA, L2Komponenten.Hoff.)) # question not applicable for participants of interval A_on_fam

#### remove not needed data ####
rm(list=setdiff(ls(), c("processed_vorbefragung", "processed_h_on", "processed_h_off", 
   "processed_interview", "processed_quant_interview", "processed_qual_interview")))

#### merge h-on & h-off + join with quant_interview ####
h_on_h_off <- bind_rows(processed_h_on, processed_h_off)
nachbefragung_complete <- full_join(h_on_h_off, processed_interview, by = c("group", "interval", "VPNr")) %>%
  select(-c(startdate.x, startdate.y)) %>%
  subset(., VPNr != 1 & VPNr != 67) # exclude participants VPNr 1 (pilot test) & VPNr 67 (dysfunctional DMS because of seat settings)


#### remove not needed data ####
rm(list=setdiff(ls(), c("processed_vorbefragung", "nachbefragung_complete")))

#### score calculation vorbefragung ####
# DSQ:
  # values: raw scores are 0-5, but they should be 1-6
  # Scoring:
    #   Speed=Q1+Q2+Q3.
    #   Calmness=14-Q4+Q5-Q6
    #   Social resistance=7-Q7+Q8
    #   Focus=Q9+Q10+Q11
    #   Planning=7-Q12+Q13
    #   Deviance=Q14+Q15
 # ATI-S_
   # valus: raw scores are 0-5, but they should be 1-6
    # Scoring: 
      # SUM[ATI-S1:ATI-S9]/9

processed_vorbefragung_rec <- processed_vorbefragung %>%
  mutate(across(starts_with("Fahrstil.DSQ"), ~case_when(
    . == 5 ~6, 
    . == 4 ~5, 
    . == 3 ~4,
    . == 2 ~3, 
    . == 1 ~2, 
    . == 0 ~1,
    TRUE ~ as.numeric(.)))) %>%
  mutate(Technikaffinitaet.ATIS1. = Technikaffinitaet.ATIS1.  + 1) %>%
  mutate(Technikaffinitaet.ATIS2. = Technikaffinitaet.ATIS2.  + 1) %>%
  mutate(Technikaffinitaet.ATIS3. = 6 - Technikaffinitaet.ATIS3.) %>% # inverse item
  mutate(Technikaffinitaet.ATIS4. = Technikaffinitaet.ATIS4.  + 1) %>%
  mutate(Technikaffinitaet.ATIS5. = Technikaffinitaet.ATIS5.  + 1) %>%
  mutate(Technikaffinitaet.ATIS6. = 6 - Technikaffinitaet.ATIS6.) %>% # inverse item
  mutate(Technikaffinitaet.ATIS7. = Technikaffinitaet.ATIS7.  + 1) %>%
  mutate(Technikaffinitaet.ATIS8. = 6 - Technikaffinitaet.ATIS8.) %>% # inverse item
  mutate(Technikaffinitaet.ATIS9. = Technikaffinitaet.ATIS9.  + 1) 

vorbefragung_scores <- processed_vorbefragung_rec %>%
  add_column(DSQ_Speed = .$Fahrstil.DSQ1. + .$Fahrstil.DSQ2. + .$Fahrstil.DSQ3., .before = "FiltHerstellerCC.VW.") %>% # 
  add_column(DSQ_Calmness = 14 - .$Fahrstil.DSQ4. + .$Fahrstil.DSQ5. - .$Fahrstil.DSQ6., .before = "FiltHerstellerCC.VW.") %>%
  add_column(DSQ_SocialResistance = 7- .$Fahrstil.DSQ7. + .$Fahrstil.DSQ8., .before = "FiltHerstellerCC.VW.") %>%
  add_column(DSQ_Focus = .$Fahrstil.DSQ9. + .$Fahrstil.DSQ10. + .$Fahrstil.DSQ11., .before = "FiltHerstellerCC.VW.") %>%
  add_column(DSQ_Planning = 7- .$Fahrstil.DSQ12. + .$Fahrstil.DSQ13., .before = "FiltHerstellerCC.VW.") %>%
  add_column(DSQ_Deviance = .$Fahrstil.DSQ14. + .$Fahrstil.DSQ15., .before = "FiltHerstellerCC.VW.") %>%
  rowwise() %>% 
  mutate(ATIS = mean(c_across(Technikaffinitaet.ATIS1.:Technikaffinitaet.ATIS9.), na.rm = TRUE), .before = "FiltHerstellerCC.VW.")

#### SU statements ####
SU01_on <- 1 # wrong for off_interval
SU01_off <- 0
SU02 <- 1
SU03 <- 1
SU04 <- 0
SU05 <- 0
SU06 <- 1
SU07 <- 1
SU08 <- 0
SU09 <- 0
SU10 <- 0
SU11 <- 0
SU12 <- 1
SU13 <- 1
SU14 <- 1
SU15 <- 0
SU16 <- 1
 
#### score calculation nachbefragung ####
 # Trust TiA:
    # values: 
      # raw scores are 0-5, but they should be 1-6
      # raw scores of items 5, 7, 10, 15, 16 need to be inverted
    # scoring:
      # TiA_overall = average all items
      # TiA_Reliability_Competence = sum 1, 6, 10inverse, 13, 15inverse, 19
      # TiA_Understanding-Predictability = sum 2, 7inverse, 11, 16inverse
      # TiA_Familiarity = sum 3, 17
      # TiA_IntentionOfDevelopers = sum 4, 8
      # TiA_PropensityToTrust = sum 5inverse, 12, 18
      # TiA_TrustInAutomation = 9, 14
 # Acceptance CTAM:
    # values: raw scores of PS1, PS2, PS3 need to be inverted
    # scoring: 
      # Acceptance_PerformanceExpectancy_Score = sum 1, 3, 4
      # Acceptance_EffortExpectancy_Score = sum 1-4
      # Acceptance_AttitudeTowardsUsingTechnology_Score = sum 1-4
      # Acceptance_FacilitatingConditions_Score	= sum 1-3
      # Acceptance_BehavioralIntentionToUseTheSystem_Score = sum 1-3	
      # Acceptance_PerceivedSafety_Score = sum r1, r2, r3, 4-6
 # Systemverstaendnis: 
    # values: subjective answers [0 = "nicht zutreffend", 1 = "zutreffend", 2 = "unsicher"]need to be transferred into "is correct"/"not correct" 
              # --- for this the values in section SU statements are used
              # 2 ("unsicher") are treated as NAs to simplify calculation of correct answers (percentage); there are no missing values except for A_on_fc---27
    # scoring:
      # SystemUnderstanding_score = sum correct answers ("unsicher" treated as wrong)

nachbefragung_rec <- nachbefragung_complete %>%
    mutate(across(starts_with("Vertrauen.TiA"), ~case_when(
      . == 5 ~1000, # is NA
      . == 4 ~5, 
      . == 3 ~4,
      . == 2 ~3, 
      . == 1 ~2, 
      . == 0 ~1,
      TRUE ~ as.numeric(.)))) %>%
  mutate(across(starts_with("Vertrauen.TiA"), ~ifelse(. == 1000, NA, .))) %>%
  mutate(Vertrauen.TiA5. = 6 - Vertrauen.TiA5.) %>% # inverse item
  mutate(Vertrauen.TiA7. = 6 - Vertrauen.TiA7.) %>% # inverse item
  mutate(Vertrauen.TiA10. = 6 - Vertrauen.TiA10.) %>% # inverse item
  mutate(Vertrauen.TiA15. = 6 - Vertrauen.TiA15.) %>% # inverse item
  mutate(Vertrauen.TiA16. = 6 - Vertrauen.TiA16.) %>% # inverse item
  mutate(Akzeptanz.PS1. = 8 - Akzeptanz.PS1.) %>% # inverse item
  mutate(Akzeptanz.PS2. = 8 - Akzeptanz.PS2.) %>% # inverse item
  mutate(Akzeptanz.PS3. = 8 - Akzeptanz.PS3.) %>% # inverse item
  add_column(System01 = ifelse(.$Systemverstaendnis.1. == 2, NA, # statement is correct for On, wrong for off
                               ifelse((.$Systemverstaendnis.1. == SU01_on & .$interval == "A_on_fc") | (.$Systemverstaendnis.1. == SU01_on & .$interval == "A_on_fam"), 1, 
                                      ifelse((.$Systemverstaendnis.1. == SU01_off  & .$interval == "A_off") | (.$Systemverstaendnis.1. == SU01_off & .$interval == "B_off"), 1, 
                                      0))), .before = "Systemverstaendnis.1.") %>%
  add_column(System02 = ifelse(.$Systemverstaendnis.2. == SU02, 1, ifelse(.$Systemverstaendnis.1. == 2, NA, 0)), .before = "Systemverstaendnis.1.") %>%
  add_column(System03 = ifelse(.$Systemverstaendnis.3. == SU03, 1, ifelse(.$Systemverstaendnis.1. == 2, NA, 0)), .before = "Systemverstaendnis.1.") %>%
  add_column(System04 = ifelse(.$Systemverstaendnis.4. == SU04, 1, ifelse(.$Systemverstaendnis.1. == 2, NA, 0)), .before = "Systemverstaendnis.1.") %>%
  add_column(System05 = ifelse(.$Systemverstaendnis.5. == SU05, 1, ifelse(.$Systemverstaendnis.1. == 2, NA, 0)), .before = "Systemverstaendnis.1.") %>%
  add_column(System06 = ifelse(.$Systemverstaendnis.6. == SU06, 1, ifelse(.$Systemverstaendnis.1. == 2, NA, 0)), .before = "Systemverstaendnis.1.") %>%
  add_column(System07 = ifelse(.$Systemverstaendnis.7. == SU07, 1, ifelse(.$Systemverstaendnis.1. == 2, NA, 0)), .before = "Systemverstaendnis.1.") %>%
  add_column(System08 = ifelse(.$Systemverstaendnis.8. == SU08, 1, ifelse(.$Systemverstaendnis.1. == 2, NA, 0)), .before = "Systemverstaendnis.1.") %>%
  add_column(System09 = ifelse(.$Systemverstaendnis.9. == SU09, 1, ifelse(.$Systemverstaendnis.1. == 2, NA, 0)), .before = "Systemverstaendnis.1.") %>%
  add_column(System10 = ifelse(.$Systemverstaendnis.10. == SU10, 1, ifelse(.$Systemverstaendnis.1. == 2, NA, 0)), .before = "Systemverstaendnis.1.") %>%
  add_column(System11 = ifelse(.$Systemverstaendnis.11. == SU11, 1, ifelse(.$Systemverstaendnis.1. == 2, NA, 0)), .before = "Systemverstaendnis.1.") %>%
  add_column(System12 = ifelse(.$Systemverstaendnis.12. == SU12, 1, ifelse(.$Systemverstaendnis.1. == 2, NA, 0)), .before = "Systemverstaendnis.1.") %>%
  add_column(System13 = ifelse(.$Systemverstaendnis.13. == SU13, 1, ifelse(.$Systemverstaendnis.1. == 2, NA, 0)), .before = "Systemverstaendnis.1.") %>%
  add_column(System14 = ifelse(.$Systemverstaendnis.14. == SU14, 1, ifelse(.$Systemverstaendnis.1. == 2, NA, 0)), .before = "Systemverstaendnis.1.") %>%
  add_column(System15 = ifelse(.$Systemverstaendnis.15. == SU15, 1, ifelse(.$Systemverstaendnis.1. == 2, NA, 0)), .before = "Systemverstaendnis.1.") %>%
  add_column(System16 = ifelse(.$Systemverstaendnis.16. == SU16, 1, ifelse(.$Systemverstaendnis.1. == 2, NA, 0)), .before = "Systemverstaendnis.1.") %>%
  add_column(System_u_01 = ifelse(.$Systemverstaendnis.1. == 2, 2, # statement is correct for On, wrong for off
                                  ifelse((.$Systemverstaendnis.1. == SU01_on & .$interval == "A_on_fc") | (.$Systemverstaendnis.1. == SU01_on & .$interval == "A_on_fam"), 1, 
                                         ifelse((.$Systemverstaendnis.1. == SU01_off  & .$interval == "A_off") | (.$Systemverstaendnis.1. == SU01_off & .$interval == "B_off"), 1, 
                                                0))), .before = "Systemverstaendnis.1.") %>%
  add_column(System_u_02 = ifelse(.$Systemverstaendnis.2. == SU02, 1, ifelse(.$Systemverstaendnis.1. == 2, 2, 0)), .before = "Systemverstaendnis.1.") %>%
  add_column(System_u_03 = ifelse(.$Systemverstaendnis.3. == SU03, 1, ifelse(.$Systemverstaendnis.1. == 2, 2, 0)), .before = "Systemverstaendnis.1.") %>%
  add_column(System_u_04 = ifelse(.$Systemverstaendnis.4. == SU04, 1, ifelse(.$Systemverstaendnis.1. == 2, 2, 0)), .before = "Systemverstaendnis.1.") %>%
  add_column(System_u_05 = ifelse(.$Systemverstaendnis.5. == SU05, 1, ifelse(.$Systemverstaendnis.1. == 2, 2, 0)), .before = "Systemverstaendnis.1.") %>%
  add_column(System_u_06 = ifelse(.$Systemverstaendnis.6. == SU06, 1, ifelse(.$Systemverstaendnis.1. == 2, 2, 0)), .before = "Systemverstaendnis.1.") %>%
  add_column(System_u_07 = ifelse(.$Systemverstaendnis.7. == SU07, 1, ifelse(.$Systemverstaendnis.1. == 2, 2, 0)), .before = "Systemverstaendnis.1.") %>%
  add_column(System_u_08 = ifelse(.$Systemverstaendnis.8. == SU08, 1, ifelse(.$Systemverstaendnis.1. == 2, 2, 0)), .before = "Systemverstaendnis.1.") %>%
  add_column(System_u_09 = ifelse(.$Systemverstaendnis.9. == SU09, 1, ifelse(.$Systemverstaendnis.1. == 2, 2, 0)), .before = "Systemverstaendnis.1.") %>%
  add_column(System_u_10 = ifelse(.$Systemverstaendnis.10. == SU10, 1, ifelse(.$Systemverstaendnis.1. == 2, 2, 0)), .before = "Systemverstaendnis.1.") %>%
  add_column(System_u_11 = ifelse(.$Systemverstaendnis.11. == SU11, 1, ifelse(.$Systemverstaendnis.1. == 2, 2, 0)), .before = "Systemverstaendnis.1.") %>%
  add_column(System_u_12 = ifelse(.$Systemverstaendnis.12. == SU12, 1, ifelse(.$Systemverstaendnis.1. == 2, 2, 0)), .before = "Systemverstaendnis.1.") %>%
  add_column(System_u_13 = ifelse(.$Systemverstaendnis.13. == SU13, 1, ifelse(.$Systemverstaendnis.1. == 2, 2, 0)), .before = "Systemverstaendnis.1.") %>%
  add_column(System_u_14 = ifelse(.$Systemverstaendnis.14. == SU14, 1, ifelse(.$Systemverstaendnis.1. == 2, 2, 0)), .before = "Systemverstaendnis.1.") %>%
  add_column(System_u_15 = ifelse(.$Systemverstaendnis.15. == SU15, 1, ifelse(.$Systemverstaendnis.1. == 2, 2, 0)), .before = "Systemverstaendnis.1.") %>%
  add_column(System_u_16 = ifelse(.$Systemverstaendnis.16. == SU16, 1, ifelse(.$Systemverstaendnis.1. == 2, 2, 0)), .before = "Systemverstaendnis.1.")
  

nachbefragung_scores <- nachbefragung_rec %>%
  rowwise() %>% 
  mutate(TiA_overall = mean(c_across(Vertrauen.TiA1.:Vertrauen.TiA19.), na.rm = TRUE), .before = "Vertrauen.TiA1.") %>%
  mutate(TiA_RC = mean(c(Vertrauen.TiA1., Vertrauen.TiA6., Vertrauen.TiA10., 
                         Vertrauen.TiA13., Vertrauen.TiA15., Vertrauen.TiA19.),  # item 10 & 15 are already inverted
                         na.rm = TRUE), .before = "Vertrauen.TiA1.") %>%
  mutate(TiA_UP = mean(c(Vertrauen.TiA2., Vertrauen.TiA7., Vertrauen.TiA11., Vertrauen.TiA16.),  # item 7 & 16 are already inverted
                         na.rm = TRUE), .before = "Vertrauen.TiA1.") %>%
  mutate(TiA_F = mean(c(Vertrauen.TiA3., Vertrauen.TiA17.), 
                        na.rm = TRUE), .before = "Vertrauen.TiA1.") %>%
  mutate(TiA_IoD = mean(c(Vertrauen.TiA4., Vertrauen.TiA8.), 
                        na.rm = TRUE), .before = "Vertrauen.TiA1.") %>%
  mutate(TiA_PtT = mean(c(Vertrauen.TiA5., Vertrauen.TiA12., Vertrauen.TiA18.), # item 5 is already inverted
                        na.rm = TRUE), .before = "Vertrauen.TiA1.") %>%
  mutate(TiA_TiA = mean(c(Vertrauen.TiA9., Vertrauen.TiA14.), 
                        na.rm = TRUE), .before = "Vertrauen.TiA1.") %>%
  mutate(CTAM_PE = mean(c_across(Akzeptanz.PE1.:Akzeptanz.PE4.), na.rm = TRUE), .before = "Vertrauen.TiA1.") %>% # 1, 3, 4
  mutate(CTAM_EE = mean(c_across(Akzeptanz.EE1.:Akzeptanz.EE4.), na.rm = TRUE), .before = "Vertrauen.TiA1.") %>% # 1-4
  mutate(CTAM_ATT = mean(c_across(Akzeptanz.ATT1.:Akzeptanz.ATT4.), na.rm = TRUE), .before = "Vertrauen.TiA1.") %>% # 1-4
  mutate(CTAM_FC = mean(c_across(Akzeptanz.FC1.:Akzeptanz.FC3.), na.rm = TRUE), .before = "Vertrauen.TiA1.") %>% # 1-3
  mutate(CTAM_ITU = mean(c_across(Akzeptanz.ITU1.:Akzeptanz.ITU3.), na.rm = TRUE), .before = "Vertrauen.TiA1.") %>% # 1-3
  mutate(CTAM_PS = mean(c_across(Akzeptanz.PS1.:Akzeptanz.PS6.), na.rm = TRUE), .before = "Vertrauen.TiA1.") %>% # r1, r2, r3, 4-6
  mutate(System_sum = mean(c_across(System01:System16), na.rm = TRUE), .before = "Vertrauen.TiA1.") %>%
  relocate(any_of(c("NDRTs.NDRT1.", "NDRTs.NDRT2.", "NDRTs.NDRT3.", "NDRTs.NDRT4.", 
                    "NDRTs.NDRT5.", "NDRTs.NDRT6.", "NDRTs.NDRT7.", "NDRTs.NDRT8.", 
                    "SubjUeberwachguete.1.", "SubjEinflussSetting", 
                    "L2PrivNutzung", "L2Komponenten.Laengs.", "L2Komponenten.Quer.", "L2Komponenten.Hoff.", "Ranking"
                    )), .before = "Vertrauen.TiA1.")

#### save ####
## caution: items are already inverted (CTAM & TiA); all raw scores are recoded in order to match the scale descriptions
rm(list=setdiff(ls(), c("vorbefragung_scores", "nachbefragung_scores")))

write_excel_csv(vorbefragung_scores, "data/preprocessed/vorbefragung_scores.csv")
write_excel_csv(nachbefragung_scores, "data/preprocessed/nachbefragung_scores.csv")

## in case separate datasets for qual- and quant-analysises are wanted
# nachbefragung_qual <- nachbefragung_scores %>%
#   select(c(group, interval, VPNr, AutoFahrt, 
#            MatrixNDRTsFreitext.0_SQ001.:Studie))
# nachbefragung_quant <- nachbefragung_scores %>%
#   select(c(group:Ranking))
# write_excel_csv(nachbefragung_quant, "data/preprocessed/nachbefragung_quant.csv")
# write_excel_csv(nachbefragung_qual, "data/preprocessed/nachbefragung_qual.csv")
