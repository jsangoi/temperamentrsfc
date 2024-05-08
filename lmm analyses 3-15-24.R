#load libraries
library(haven)
library(foreign)
library(dplyr)
library(lmerTest)
library(sjPlot)

#open thesisdata
thesisdata <- read.csv("/Users/Jaden/Desktop/thesis/data & analyses/v2/thesisdata.csv")
View(thesisdata)
#n = 9759

#assigning variables as factors -- RUN EVERY TIME FIRST
thesisdata$TWOCLASS_CFA_ADHD <- as.factor(thesisdata$TWOCLASS_CFA_ADHD)
thesisdata$groupsTD3 <- as.factor(thesisdata$groupsTD3)
thesisdata$SEX_NUMERIC <- as.factor(thesisdata$SEX_NUMERIC)
thesisdata$meds_yes_no <- as.factor(thesisdata$meds_yes_no)
thesisdata$race_ethnicity <- as.factor(thesisdata$race_ethnicity)

table(thesisdata$TWOCLASS_CFA_ADHD, thesisdata$eventname.y)
#                               2_year_follow_up_y_arm_1
#0TD                            6967
#1surgent                        103
#2irritable                      160

#########################################
##LMM analyses (TD as comparison group)##
#########################################

#TWOCLASS_CFA_ADHD*SEX_NUMERIC for interaction between group and sex
#ageyears continuous variable
#meds_yes_no binary variable for stimulant medication, yes = 1
#race_ethnicity sets race as factor 1-5 options

#auditory
auLA <- lmer(rsfmri_cor_ngd_au_scs_aglh ~ TWOCLASS_CFA_ADHD*SEX_NUMERIC 
                  + ageyears + meds_yes_no + race_ethnicity
                  + (1 | site_id_l/rel_family_id), data = thesisdata)
summary(auLA)

#calculating means & SD for each group
auLA_desc <- thesisdata %>%
  group_by(TWOCLASS_CFA_ADHD) %>%
  summarise(mean_value = mean(rsfmri_cor_ngd_au_scs_aglh, na.rm = TRUE),
            sd_value = sd(rsfmri_cor_ngd_au_scs_aglh, na.rm = TRUE))
auLA_desc

auRA <- lmer(rsfmri_cor_ngd_au_scs_agrh ~ TWOCLASS_CFA_ADHD*SEX_NUMERIC 
                  + ageyears + meds_yes_no + race_ethnicity 
                  + (1 | site_id_l/rel_family_id), data = thesisdata)
summary(auRA)

auRA_desc <- thesisdata %>%
  group_by(TWOCLASS_CFA_ADHD) %>%
  summarise(mean_value = mean(rsfmri_cor_ngd_au_scs_agrh, na.rm = TRUE),
            sd_value = sd(rsfmri_cor_ngd_au_scs_agrh, na.rm = TRUE))
auRA_desc

#cingulo-opercular
cercLA <- lmer(rsfmri_cor_ngd_cerc_scs_aglh ~ TWOCLASS_CFA_ADHD*SEX_NUMERIC 
                  + ageyears + meds_yes_no + race_ethnicity
                  + (1 | site_id_l/rel_family_id), data = thesisdata)
summary(cercLA)

cercLA_desc <- thesisdata %>%
  group_by(TWOCLASS_CFA_ADHD) %>%
  summarise(mean_value = mean(rsfmri_cor_ngd_cerc_scs_aglh, na.rm = TRUE),
            sd_value = sd(rsfmri_cor_ngd_cerc_scs_aglh, na.rm = TRUE))
cercLA_desc

cercRA <- lmer(rsfmri_cor_ngd_cerc_scs_agrh ~ TWOCLASS_CFA_ADHD*SEX_NUMERIC 
                  + ageyears + meds_yes_no + race_ethnicity
                  + (1 | site_id_l/rel_family_id), data = thesisdata)
summary(cercRA)

cercRA_desc <- thesisdata %>%
  group_by(TWOCLASS_CFA_ADHD) %>%
  summarise(mean_value = mean(rsfmri_cor_ngd_cerc_scs_agrh, na.rm = TRUE),
            sd_value = sd(rsfmri_cor_ngd_cerc_scs_agrh, na.rm = TRUE))
cercRA_desc

#cingulo-parietal
copaLA <- lmer(rsfmri_cor_ngd_copa_scs_aglh ~ TWOCLASS_CFA_ADHD*SEX_NUMERIC
                 + ageyears + meds_yes_no + race_ethnicity
                 + (1 | site_id_l/rel_family_id), data = thesisdata)
summary(copaLA)

copaLA_desc <- thesisdata %>%
  group_by(TWOCLASS_CFA_ADHD) %>%
  summarise(mean_value = mean(rsfmri_cor_ngd_copa_scs_aglh, na.rm = TRUE),
            sd_value = sd(rsfmri_cor_ngd_copa_scs_aglh, na.rm = TRUE))
copaLA_desc

copaRA <- lmer(rsfmri_cor_ngd_copa_scs_agrh ~ TWOCLASS_CFA_ADHD*SEX_NUMERIC 
                 + ageyears + meds_yes_no + race_ethnicity
                 + (1 | site_id_l/rel_family_id), data = thesisdata)
summary(copaRA)

copaRA_desc <- thesisdata %>%
  group_by(TWOCLASS_CFA_ADHD) %>%
  summarise(mean_value = mean(rsfmri_cor_ngd_copa_scs_agrh, na.rm = TRUE),
            sd_value = sd(rsfmri_cor_ngd_copa_scs_agrh, na.rm = TRUE))
copaRA_desc

#default mode
dfLA <- lmer(rsfmri_cor_ngd_df_scs_aglh ~ TWOCLASS_CFA_ADHD*SEX_NUMERIC 
                 + ageyears + meds_yes_no + race_ethnicity
                 + (1 | site_id_l/rel_family_id), data = thesisdata)
summary(dfLA)

dfLA_desc <- thesisdata %>%
  group_by(TWOCLASS_CFA_ADHD) %>%
  summarise(mean_value = mean(rsfmri_cor_ngd_df_scs_aglh, na.rm = TRUE),
            sd_value = sd(rsfmri_cor_ngd_df_scs_aglh, na.rm = TRUE))
dfLA_desc

dfRA <- lmer(rsfmri_cor_ngd_df_scs_agrh ~ TWOCLASS_CFA_ADHD*SEX_NUMERIC 
                 + ageyears + meds_yes_no + race_ethnicity
                 + (1 | site_id_l/rel_family_id), data = thesisdata)
summary(dfRA)

dfRA_desc <- thesisdata %>%
  group_by(TWOCLASS_CFA_ADHD) %>%
  summarise(mean_value = mean(rsfmri_cor_ngd_df_scs_agrh, na.rm = TRUE),
            sd_value = sd(rsfmri_cor_ngd_df_scs_agrh, na.rm = TRUE))
dfRA_desc

#dorsal attention
dsaLA <- lmer(rsfmri_cor_ngd_dsa_scs_aglh ~ TWOCLASS_CFA_ADHD*SEX_NUMERIC 
                   + ageyears + meds_yes_no + race_ethnicity
                   + (1 | site_id_l/rel_family_id), data = thesisdata)
summary(dsaLA)

dsaLA_desc <- thesisdata %>%
  group_by(TWOCLASS_CFA_ADHD) %>%
  summarise(mean_value = mean(rsfmri_cor_ngd_dsa_scs_aglh, na.rm = TRUE),
            sd_value = sd(rsfmri_cor_ngd_dsa_scs_aglh, na.rm = TRUE))
dsaLA_desc

dsaRA <- lmer(rsfmri_cor_ngd_dsa_scs_agrh ~ TWOCLASS_CFA_ADHD*SEX_NUMERIC 
                   + ageyears + meds_yes_no + race_ethnicity
                   + (1 | site_id_l/rel_family_id), data = thesisdata)
summary(dsaRA)

dsaRA_desc <- thesisdata %>%
  group_by(TWOCLASS_CFA_ADHD) %>%
  summarise(mean_value = mean(rsfmri_cor_ngd_dsa_scs_agrh, na.rm = TRUE),
            sd_value = sd(rsfmri_cor_ngd_dsa_scs_agrh, na.rm = TRUE))
dsaRA_desc

#frontoparietal
fopaLA <- lmer(rsfmri_cor_ngd_fopa_scs_aglh ~ TWOCLASS_CFA_ADHD*SEX_NUMERIC 
                  + ageyears + meds_yes_no + race_ethnicity
                  + (1 | site_id_l/rel_family_id), data = thesisdata)
summary(fopaLA)

fopaLA_desc <- thesisdata %>%
  group_by(TWOCLASS_CFA_ADHD) %>%
  summarise(mean_value = mean(rsfmri_cor_ngd_fopa_scs_aglh, na.rm = TRUE),
            sd_value = sd(rsfmri_cor_ngd_fopa_scs_aglh, na.rm = TRUE))
fopaLA_desc

fopaRA <- lmer(rsfmri_cor_ngd_fopa_scs_agrh ~ TWOCLASS_CFA_ADHD*SEX_NUMERIC 
                  + ageyears + meds_yes_no + race_ethnicity
                  + (1 | site_id_l/rel_family_id), data = thesisdata)
summary(fopaRA)

fopaRA_desc <- thesisdata %>%
  group_by(TWOCLASS_CFA_ADHD) %>%
  summarise(mean_value = mean(rsfmri_cor_ngd_fopa_scs_agrh, na.rm = TRUE),
            sd_value = sd(rsfmri_cor_ngd_fopa_scs_agrh, na.rm = TRUE))
fopaRA_desc

#none network
noneLA <- lmer(rsfmri_cor_ngd_none_scs_aglh ~ TWOCLASS_CFA_ADHD*SEX_NUMERIC 
              + ageyears + meds_yes_no + race_ethnicity
              + (1 | site_id_l/rel_family_id), data = thesisdata)
summary(noneLA)

noneLA_desc <- thesisdata %>%
  group_by(TWOCLASS_CFA_ADHD) %>%
  summarise(mean_value = mean(rsfmri_cor_ngd_none_scs_aglh, na.rm = TRUE),
            sd_value = sd(rsfmri_cor_ngd_none_scs_aglh, na.rm = TRUE))
noneLA_desc

noneRA <- lmer(rsfmri_cor_ngd_none_scs_agrh ~ TWOCLASS_CFA_ADHD*SEX_NUMERIC 
              + ageyears + meds_yes_no + race_ethnicity
              + (1 | site_id_l/rel_family_id), data = thesisdata)
summary(noneRA)

noneRA_desc <- thesisdata %>%
  group_by(TWOCLASS_CFA_ADHD) %>%
  summarise(mean_value = mean(rsfmri_cor_ngd_none_scs_agrh, na.rm = TRUE),
            sd_value = sd(rsfmri_cor_ngd_none_scs_agrh, na.rm = TRUE))
noneRA_desc

#retrosplenial temporal
rstLA <- lmer(rsfmri_cor_ngd_rst_scs_aglh ~ TWOCLASS_CFA_ADHD*SEX_NUMERIC 
             + ageyears + meds_yes_no + race_ethnicity
             + (1 | site_id_l/rel_family_id), data = thesisdata)
summary(rstLA)

rstLA_desc <- thesisdata %>%
  group_by(TWOCLASS_CFA_ADHD) %>%
  summarise(mean_value = mean(rsfmri_cor_ngd_rst_scs_aglh, na.rm = TRUE),
            sd_value = sd(rsfmri_cor_ngd_rst_scs_aglh, na.rm = TRUE))
rstLA_desc

rstRA <- lmer(rsfmri_cor_ngd_rst_scs_agrh ~ TWOCLASS_CFA_ADHD*SEX_NUMERIC 
             + ageyears + meds_yes_no + race_ethnicity
             + (1 | site_id_l/rel_family_id), data = thesisdata)
summary(rstRA)

rstRA_desc <- thesisdata %>%
  group_by(TWOCLASS_CFA_ADHD) %>%
  summarise(mean_value = mean(rsfmri_cor_ngd_rst_scs_agrh, na.rm = TRUE),
            sd_value = sd(rsfmri_cor_ngd_rst_scs_agrh, na.rm = TRUE))
rstRA_desc

#salience
saLA <- lmer(rsfmri_cor_ngd_sa_scs_aglh ~ TWOCLASS_CFA_ADHD*SEX_NUMERIC 
                  + ageyears + meds_yes_no + race_ethnicity 
                  + (1 | site_id_l/rel_family_id), data = thesisdata)
summary(saLA)

saLA_desc <- thesisdata %>%
  group_by(TWOCLASS_CFA_ADHD) %>%
  summarise(mean_value = mean(rsfmri_cor_ngd_sa_scs_aglh, na.rm = TRUE),
            sd_value = sd(rsfmri_cor_ngd_sa_scs_aglh, na.rm = TRUE))
saLA_desc

saRA <- lmer(rsfmri_cor_ngd_sa_scs_agrh ~ TWOCLASS_CFA_ADHD*SEX_NUMERIC 
                  + ageyears + meds_yes_no + race_ethnicity 
                  + (1 | site_id_l/rel_family_id), data = thesisdata)
summary(saRA)

saRA_desc <- thesisdata %>%
  group_by(TWOCLASS_CFA_ADHD) %>%
  summarise(mean_value = mean(rsfmri_cor_ngd_sa_scs_agrh, na.rm = TRUE),
            sd_value = sd(rsfmri_cor_ngd_sa_scs_agrh, na.rm = TRUE))
saRA_desc

#sensorimotor hand
smhLA <- lmer(rsfmri_cor_ngd_smh_scs_aglh ~ TWOCLASS_CFA_ADHD*SEX_NUMERIC 
             + ageyears + meds_yes_no + race_ethnicity
             + (1 | site_id_l/rel_family_id), data = thesisdata)
summary(smhLA)

smhLA_desc <- thesisdata %>%
  group_by(TWOCLASS_CFA_ADHD) %>%
  summarise(mean_value = mean(rsfmri_cor_ngd_smh_scs_aglh, na.rm = TRUE),
            sd_value = sd(rsfmri_cor_ngd_smh_scs_aglh, na.rm = TRUE))
smhLA_desc

smhRA <- lmer(rsfmri_cor_ngd_smh_scs_agrh ~ TWOCLASS_CFA_ADHD*SEX_NUMERIC 
             + ageyears + meds_yes_no + race_ethnicity 
             + (1 | site_id_l/rel_family_id), data = thesisdata)
summary(smhRA)

smhRA_desc <- thesisdata %>%
  group_by(TWOCLASS_CFA_ADHD) %>%
  summarise(mean_value = mean(rsfmri_cor_ngd_smh_scs_agrh, na.rm = TRUE),
            sd_value = sd(rsfmri_cor_ngd_smh_scs_agrh, na.rm = TRUE))
smhRA_desc

#sensorimotor mouth
smmLA <- lmer(rsfmri_cor_ngd_smm_scs_aglh ~ TWOCLASS_CFA_ADHD*SEX_NUMERIC 
             + ageyears + meds_yes_no + race_ethnicity 
             + (1 | site_id_l/rel_family_id), data = thesisdata)
summary(smmLA)

smmLA_desc <- thesisdata %>%
  group_by(TWOCLASS_CFA_ADHD) %>%
  summarise(mean_value = mean(rsfmri_cor_ngd_smm_scs_aglh, na.rm = TRUE),
            sd_value = sd(rsfmri_cor_ngd_smm_scs_aglh, na.rm = TRUE))
smmLA_desc

smmRA <- lmer(rsfmri_cor_ngd_smm_scs_agrh ~ TWOCLASS_CFA_ADHD*SEX_NUMERIC 
             + ageyears + meds_yes_no + race_ethnicity 
             + (1 | site_id_l/rel_family_id), data = thesisdata)
summary(smmRA)

smmRA_desc <- thesisdata %>%
  group_by(TWOCLASS_CFA_ADHD) %>%
  summarise(mean_value = mean(rsfmri_cor_ngd_smm_scs_agrh, na.rm = TRUE),
            sd_value = sd(rsfmri_cor_ngd_smm_scs_agrh, na.rm = TRUE))
smmRA_desc

#ventral attention
vtaLA <- lmer(rsfmri_cor_ngd_vta_scs_aglh ~ TWOCLASS_CFA_ADHD*SEX_NUMERIC 
                 + ageyears + meds_yes_no + race_ethnicity  
                 + (1 | site_id_l/rel_family_id), data = thesisdata)
summary(vtaLA)

vtaLA_desc <- thesisdata %>%
  group_by(TWOCLASS_CFA_ADHD) %>%
  summarise(mean_value = mean(rsfmri_cor_ngd_vta_scs_aglh, na.rm = TRUE),
            sd_value = sd(rsfmri_cor_ngd_vta_scs_aglh, na.rm = TRUE))
vtaLA_desc

vtaRA <- lmer(rsfmri_cor_ngd_vta_scs_agrh ~ TWOCLASS_CFA_ADHD*SEX_NUMERIC 
                 + ageyears + meds_yes_no + race_ethnicity   
                 + (1 | site_id_l/rel_family_id), data = thesisdata)
summary(vtaRA)

vtaRA_desc <- thesisdata %>%
  group_by(TWOCLASS_CFA_ADHD) %>%
  summarise(mean_value = mean(rsfmri_cor_ngd_vta_scs_agrh, na.rm = TRUE),
            sd_value = sd(rsfmri_cor_ngd_vta_scs_agrh, na.rm = TRUE))
vtaRA_desc

#visual
vsLA <- lmer(rsfmri_cor_ngd_vs_scs_aglh ~ TWOCLASS_CFA_ADHD*SEX_NUMERIC 
                + ageyears + meds_yes_no + race_ethnicity  
                + (1 | site_id_l/rel_family_id), data = thesisdata)
summary(vsLA)

vsLA_desc <- thesisdata %>%
  group_by(TWOCLASS_CFA_ADHD) %>%
  summarise(mean_value = mean(rsfmri_cor_ngd_vs_scs_aglh, na.rm = TRUE),
            sd_value = sd(rsfmri_cor_ngd_vs_scs_aglh, na.rm = TRUE))
vsLA_desc

vsRA <- lmer(rsfmri_cor_ngd_vs_scs_agrh ~ TWOCLASS_CFA_ADHD*SEX_NUMERIC 
                + ageyears + meds_yes_no + race_ethnicity  
                + (1 | site_id_l/rel_family_id), data = thesisdata)
summary(vsRA)

vsRA_desc <- thesisdata %>%
  group_by(TWOCLASS_CFA_ADHD) %>%
  summarise(mean_value = mean(rsfmri_cor_ngd_vs_scs_agrh, na.rm = TRUE),
            sd_value = sd(rsfmri_cor_ngd_vs_scs_agrh, na.rm = TRUE))
vsRA_desc

#################################################################################

#############################################################
##LMM analyses part 2 (Surgent as comparison group, TD = 3)##
#############################################################

#auditory
auLA1 <- lmer(rsfmri_cor_ngd_au_scs_aglh ~ groupsTD3*SEX_NUMERIC 
                   + ageyears + meds_yes_no + race_ethnicity 
                   + (1 | site_id_l/rel_family_id), data = thesisdata)
summary(auLA1)

auRA1 <- lmer(rsfmri_cor_ngd_au_scs_agrh ~ groupsTD3*SEX_NUMERIC 
                   + ageyears + meds_yes_no + race_ethnicity 
                   + (1 | site_id_l/rel_family_id), data = thesisdata)
summary(auRA1)

#cingulo-opercular
cercLA1 <- lmer(rsfmri_cor_ngd_cerc_scs_aglh ~ groupsTD3*SEX_NUMERIC
                   + ageyears + meds_yes_no + race_ethnicity 
                   + (1 | site_id_l/rel_family_id), data = thesisdata)
summary(cercLA1)

cercRA1 <- lmer(rsfmri_cor_ngd_cerc_scs_agrh ~ groupsTD3*SEX_NUMERIC 
                   + ageyears + meds_yes_no + race_ethnicity  
                   + (1 | site_id_l/rel_family_id), data = thesisdata)
summary(cercRA1)

#cingulo-parietal
copaLA1 <- lmer(rsfmri_cor_ngd_copa_scs_aglh ~ groupsTD3*SEX_NUMERIC 
                  + ageyears + meds_yes_no + race_ethnicity  
                  + (1 | site_id_l/rel_family_id), data = thesisdata)
summary(copaLA1)

copaRA1 <- lmer(rsfmri_cor_ngd_copa_scs_agrh ~ groupsTD3*SEX_NUMERIC 
                  + ageyears + meds_yes_no + race_ethnicity  
                  + (1 | site_id_l/rel_family_id), data = thesisdata)
summary(copaRA1)

#default mode
dfLA1 <- lmer(rsfmri_cor_ngd_df_scs_aglh ~ groupsTD3*SEX_NUMERIC 
                  + ageyears + meds_yes_no + race_ethnicity 
                  + (1 | site_id_l/rel_family_id), data = thesisdata)
summary(dfLA1)

dfRA1 <- lmer(rsfmri_cor_ngd_df_scs_agrh ~ groupsTD3*SEX_NUMERIC 
                  + ageyears + meds_yes_no + race_ethnicity  
                  + (1 | site_id_l/rel_family_id), data = thesisdata)
summary(dfRA1)

#dorsal attention
dsaLA1 <- lmer(rsfmri_cor_ngd_dsa_scs_aglh ~ groupsTD3*SEX_NUMERIC 
                    + ageyears + meds_yes_no + race_ethnicity 
                    + (1 | site_id_l/rel_family_id), data = thesisdata)
summary(dsaLA1)

dsaRA1 <- lmer(rsfmri_cor_ngd_dsa_scs_agrh ~ groupsTD3*SEX_NUMERIC 
                    + ageyears + meds_yes_no + race_ethnicity  
                    + (1 | site_id_l/rel_family_id), data = thesisdata)
summary(dsaRA1)

#fronto-parietal
fopaLA1 <- lmer(rsfmri_cor_ngd_fopa_scs_aglh ~ groupsTD3*SEX_NUMERIC 
                   + ageyears + meds_yes_no + race_ethnicity  
                   + (1 | site_id_l/rel_family_id), data = thesisdata)
summary(fopaLA1)

fopaRA1 <- lmer(rsfmri_cor_ngd_fopa_scs_agrh ~ groupsTD3*SEX_NUMERIC 
                   + ageyears + meds_yes_no + race_ethnicity   
                   + (1 | site_id_l/rel_family_id), data = thesisdata)
summary(fopaRA1)

#none networks
noneLA1 <- lmer(rsfmri_cor_ngd_none_scs_aglh ~ groupsTD3*SEX_NUMERIC 
               + ageyears + meds_yes_no + race_ethnicity 
               + (1 | site_id_l/rel_family_id), data = thesisdata)
summary(noneLA1)

noneRA1 <- lmer(rsfmri_cor_ngd_none_scs_agrh ~ groupsTD3*SEX_NUMERIC 
               + ageyears + meds_yes_no + race_ethnicity  
               + (1 | site_id_l/rel_family_id), data = thesisdata)
summary(noneRA1)

#retrosplenial temporal
rstLA1 <- lmer(rsfmri_cor_ngd_rst_scs_aglh ~ groupsTD3*SEX_NUMERIC 
              + ageyears + meds_yes_no + race_ethnicity  
              + (1 | site_id_l/rel_family_id), data = thesisdata)
summary(rstLA1)

rstRA1 <- lmer(rsfmri_cor_ngd_rst_scs_agrh ~ groupsTD3*SEX_NUMERIC 
              + ageyears + meds_yes_no + race_ethnicity  
              + (1 | site_id_l/rel_family_id), data = thesisdata)
summary(rstRA1)

#salience
saLA1 <- lmer(rsfmri_cor_ngd_sa_scs_aglh ~ groupsTD3*SEX_NUMERIC 
                   + ageyears + meds_yes_no + race_ethnicity 
                   + (1 | site_id_l/rel_family_id), data = thesisdata)
summary(saLA1)

saRA1 <- lmer(rsfmri_cor_ngd_sa_scs_agrh ~ groupsTD3*SEX_NUMERIC 
                   + ageyears + meds_yes_no + race_ethnicity 
                   + (1 | site_id_l/rel_family_id), data = thesisdata)
summary(saRA1)

#sensorimotor hand
smhLA1 <- lmer(rsfmri_cor_ngd_smh_scs_aglh ~ groupsTD3*SEX_NUMERIC 
              + ageyears + meds_yes_no + race_ethnicity
              + (1 | site_id_l/rel_family_id), data = thesisdata)
summary(smhLA1)

smhRA1 <- lmer(rsfmri_cor_ngd_smh_scs_agrh ~ groupsTD3*SEX_NUMERIC 
              + ageyears + meds_yes_no + race_ethnicity
              + (1 | site_id_l/rel_family_id), data = thesisdata)
summary(smhRA1)

#sensorimotor mouth
smmLA1 <- lmer(rsfmri_cor_ngd_smm_scs_aglh ~ groupsTD3*SEX_NUMERIC 
              + ageyears + meds_yes_no + race_ethnicity 
              + (1 | site_id_l/rel_family_id), data = thesisdata)
summary(smmLA1)

smmRA1 <- lmer(rsfmri_cor_ngd_smm_scs_agrh ~ groupsTD3*SEX_NUMERIC 
              + ageyears + meds_yes_no + race_ethnicity 
              + (1 | site_id_l/rel_family_id), data = thesisdata)
summary(smmRA1)

#ventral attention
vtaLA1 <- lmer(rsfmri_cor_ngd_vta_scs_aglh ~ groupsTD3*SEX_NUMERIC 
                  + ageyears + meds_yes_no + race_ethnicity 
                  + (1 | site_id_l/rel_family_id), data = thesisdata)
summary(vtaLA1)

vtaRA1 <- lmer(rsfmri_cor_ngd_vta_scs_agrh ~ groupsTD3*SEX_NUMERIC 
                  + ageyears + meds_yes_no + race_ethnicity 
                  + (1 | site_id_l/rel_family_id), data = thesisdata)
summary(vtaRA1)

#visual
vsLA1 <- lmer(rsfmri_cor_ngd_vs_scs_aglh ~ groupsTD3*SEX_NUMERIC 
                 + ageyears + meds_yes_no + race_ethnicity  
                 + (1 | site_id_l/rel_family_id), data = thesisdata)
summary(vsLA1)

vsRA1 <- lmer(rsfmri_cor_ngd_vs_scs_agrh ~ groupsTD3*SEX_NUMERIC 
                 + ageyears + meds_yes_no + race_ethnicity  
                 + (1 | site_id_l/rel_family_id), data = thesisdata)
summary(vsRA1)
