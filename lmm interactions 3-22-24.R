library(reghelper)
library(emmeans)
library(ggplot2)
library(r2mlm)

thesisdata <- read.csv("/Users/Jaden/Desktop/thesis/data & analyses/v2/thesisdata.csv")
View(thesisdata)

#auditory
summary(auRA)

#examining on interactions
emauRA <- emmeans(auRA, ~ TWOCLASS_CFA_ADHD*SEX_NUMERIC, pbkrtest.limit = 7177)
contrast(emauRA, "revpairwise",by="SEX_NUMERIC",adjust="none")
eff_size(emauRA, sigma = sigma(auRA), edf = df.residual(auRA), method = "revpairwise")

#interaction means & sd
auRA_int <- thesisdata %>%
  group_by(TWOCLASS_CFA_ADHD, SEX_NUMERIC) %>%
  summarise(mean_value = mean(rsfmri_cor_ngd_au_scs_agrh, na.rm = TRUE),
            sd_value = sd(rsfmri_cor_ngd_au_scs_agrh, na.rm = TRUE))
auRA_int

#cingulo-parietal
summary(copaLA)

#examining interactions
emcopaLA <- emmeans(copaLA, ~ TWOCLASS_CFA_ADHD*SEX_NUMERIC, pbkrtest.limit = 7177)
contrast(emcopaLA, "revpairwise",by="SEX_NUMERIC",adjust="none")
eff_size(emcopaLA, sigma = sigma(copaLA), edf = df.residual(copaLA), method = "revpairwise")

#interaction means & sd
copaLA_int <- thesisdata %>%
  group_by(TWOCLASS_CFA_ADHD, SEX_NUMERIC) %>%
  summarise(mean_value = mean(rsfmri_cor_ngd_copa_scs_aglh, na.rm = TRUE),
            sd_value = sd(rsfmri_cor_ngd_copa_scs_aglh, na.rm = TRUE))
copaLA_int

#dorsal attention
summary(dsaLA1)

#examining interaction
emdsaLA1 <- emmeans(dsaLA1, ~ groupsTD3*SEX_NUMERIC, pbkrtest.limit = 7177)
contrast(emdsaLA1, "revpairwise",by="SEX_NUMERIC",adjust="none")
eff_size(emdsaLA1, sigma = sigma(dsaLA1), edf = df.residual(dsaLA1), method = "revpairwise")

#interaction means & sd
dsaLA_int <- thesisdata %>%
  group_by(TWOCLASS_CFA_ADHD, SEX_NUMERIC) %>%
  summarise(mean_value = mean(rsfmri_cor_ngd_dsa_scs_aglh, na.rm = TRUE),
            sd_value = sd(rsfmri_cor_ngd_dsa_scs_aglh, na.rm = TRUE))
dsaLA_int

#fronto-parietal
summary(fopaLA1)

#examining interactions
emfopaLA1 <- emmeans(fopaLA1, ~ groupsTD3*SEX_NUMERIC, pbkrtest.limit = 7177)
contrast(emfopaLA1, "revpairwise",by="SEX_NUMERIC",adjust="none")

#interaction means & sd
fopaLA1_int <- thesisdata %>%
  group_by(TWOCLASS_CFA_ADHD, SEX_NUMERIC) %>%
  summarise(mean_value = mean(rsfmri_cor_ngd_fopa_scs_aglh, na.rm = TRUE),
            sd_value = sd(rsfmri_cor_ngd_fopa_scs_aglh, na.rm = TRUE))
fopaLA1_int

#none
summary(noneLA)

#examining interactions
emnoneLA <- emmeans(noneLA, ~ TWOCLASS_CFA_ADHD*SEX_NUMERIC, pbkrtest.limit = 7177)
contrast(emnoneLA, "revpairwise",by="SEX_NUMERIC",adjust="none")
eff_size(emnoneLA, sigma = sigma(noneLA), edf = df.residual(noneLA), method = "revpairwise")

#interaction means & sd
noneLA_int <- thesisdata %>%
  group_by(TWOCLASS_CFA_ADHD, SEX_NUMERIC) %>%
  summarise(mean_value = mean(rsfmri_cor_ngd_none_scs_aglh, na.rm = TRUE),
            sd_value = sd(rsfmri_cor_ngd_none_scs_aglh, na.rm = TRUE))
noneLA_int

summary(noneLA1)
#see above for interaction comparison

#sensorimotor hand
summary(smhLA)

#examining interactions
emsmhLA <- emmeans(smhLA, ~ TWOCLASS_CFA_ADHD*SEX_NUMERIC, pbkrtest.limit = 7177)
contrast(emsmhLA, "revpairwise",by="SEX_NUMERIC",adjust="none")
eff_size(emsmhLA, sigma = sigma(smhLA), edf = df.residual(smhLA), method = "revpairwise")

#interaction means & sd
smhLA_int <- thesisdata %>%
  group_by(TWOCLASS_CFA_ADHD, SEX_NUMERIC) %>%
  summarise(mean_value = mean(rsfmri_cor_ngd_smh_scs_aglh, na.rm = TRUE),
            sd_value = sd(rsfmri_cor_ngd_smh_scs_aglh, na.rm = TRUE))
smhLA_int

#sensorimotor
summary(smhRA)

#examining interactions
emsmhRA <- emmeans(smhRA, ~ TWOCLASS_CFA_ADHD*SEX_NUMERIC, pbkrtest.limit = 7177)
contrast(emsmhRA, "revpairwise",by="SEX_NUMERIC",adjust="none")
eff_size(emsmhRA, sigma = sigma(smhRA), edf = df.residual(smhRA), method = "revpairwise")

#interaction means & sd
smhRA_int <- thesisdata %>%
  group_by(TWOCLASS_CFA_ADHD, SEX_NUMERIC) %>%
  summarise(mean_value = mean(rsfmri_cor_ngd_smh_scs_agrh, na.rm = TRUE),
            sd_value = sd(rsfmri_cor_ngd_smh_scs_agrh, na.rm = TRUE))
smhRA_int

#sensorimotor mouth to LA
summary(smmLA)

#examining on interactions
emsmmLA <- emmeans(smmLA, ~ TWOCLASS_CFA_ADHD*SEX_NUMERIC, pbkrtest.limit = 7177)
contrast(emsmmLA, "revpairwise",by="SEX_NUMERIC",adjust="none")
eff_size(emsmmLA, sigma = sigma(smmLA), edf = df.residual(smmLA), method = "revpairwise")

#interaction means & sd
smmLA_int <- thesisdata %>%
  group_by(TWOCLASS_CFA_ADHD, SEX_NUMERIC) %>%
  summarise(mean_value = mean(rsfmri_cor_ngd_smm_scs_aglh, na.rm = TRUE),
            sd_value = sd(rsfmri_cor_ngd_smm_scs_aglh, na.rm = TRUE))
smmLA_int

#ventral attention
summary(vtaLA)

#examining interactions
emvtaLA <- emmeans(vtaLA, ~ TWOCLASS_CFA_ADHD*SEX_NUMERIC, pbkrtest.limit = 7177)
contrast(emvtaLA, "revpairwise",by="SEX_NUMERIC",adjust="none")

#interaction means & sd
vtaLA_int <- thesisdata %>%
  group_by(TWOCLASS_CFA_ADHD, SEX_NUMERIC) %>%
  summarise(mean_value = mean(rsfmri_cor_ngd_vta_scs_aglh, na.rm = TRUE),
            sd_value = sd(rsfmri_cor_ngd_vta_scs_aglh, na.rm = TRUE))
vtaLA_int

summary(vtaRA)

#examining interactions
emvtaRA <- emmeans(vtaRA, ~ TWOCLASS_CFA_ADHD*SEX_NUMERIC, pbkrtest.limit = 7177)
contrast(emvtaRA, "revpairwise",by="SEX_NUMERIC",adjust="none")
eff_size(emvtaRA, sigma = sigma(vtaRA), edf = df.residual(vtaRA), method = "revpairwise")

#interaction means & sd
vtaRA_int <- thesisdata %>%
  group_by(TWOCLASS_CFA_ADHD, SEX_NUMERIC) %>%
  summarise(mean_value = mean(rsfmri_cor_ngd_vta_scs_agrh, na.rm = TRUE),
            sd_value = sd(rsfmri_cor_ngd_vta_scs_agrh, na.rm = TRUE))
vtaRA_int

summary(vtaRA1)
#see above for interaction comparisons

#visual
summary(vsRA)

#examining interactions
emvsRA <- emmeans(vsRA, ~ TWOCLASS_CFA_ADHD*SEX_NUMERIC, pbkrtest.limit = 7177)
contrast(emvsRA, "revpairwise",by="SEX_NUMERIC",adjust="none")
eff_size(emvsRA, sigma = sigma(vsRA), edf = df.residual(vsRA), method = "revpairwise")

#interaction means & sd
vsRA_int <- thesisdata %>%
  group_by(TWOCLASS_CFA_ADHD, SEX_NUMERIC) %>%
  summarise(mean_value = mean(rsfmri_cor_ngd_vs_scs_agrh, na.rm = TRUE),
            sd_value = sd(rsfmri_cor_ngd_vs_scs_agrh, na.rm = TRUE))
vsRA_int


#all profile*sex interaction means
audLAint <- thesisdata %>%
  group_by(TWOCLASS_CFA_ADHD, SEX_NUMERIC) %>%
  summarise(mean_value = mean(rsfmri_cor_ngd_au_scs_aglh, na.rm = TRUE))
audLAint

audRAint <- thesisdata %>%
  group_by(TWOCLASS_CFA_ADHD, SEX_NUMERIC) %>%
  summarise(mean_value = mean(rsfmri_cor_ngd_au_scs_agrh, na.rm = TRUE))
audRAint

conLAint <- thesisdata %>%
  group_by(TWOCLASS_CFA_ADHD, SEX_NUMERIC) %>%
  summarise(mean_value = mean(rsfmri_cor_ngd_cerc_scs_aglh, na.rm = TRUE))
conLAint

conRAint <- thesisdata %>%
  group_by(TWOCLASS_CFA_ADHD, SEX_NUMERIC) %>%
  summarise(mean_value = mean(rsfmri_cor_ngd_cerc_scs_agrh, na.rm = TRUE))
conRAint

cpnLAint <- thesisdata %>%
  group_by(TWOCLASS_CFA_ADHD, SEX_NUMERIC) %>%
  summarise(mean_value = mean(rsfmri_cor_ngd_copa_scs_aglh, na.rm = TRUE))
cpnLAint

cpnRAint <- thesisdata %>%
  group_by(TWOCLASS_CFA_ADHD, SEX_NUMERIC) %>%
  summarise(mean_value = mean(rsfmri_cor_ngd_copa_scs_agrh, na.rm = TRUE))
cpnRAint

dmnLAint <- thesisdata %>%
  group_by(TWOCLASS_CFA_ADHD, SEX_NUMERIC) %>%
  summarise(mean_value = mean(rsfmri_cor_ngd_df_scs_aglh, na.rm = TRUE))
dmnLAint

dmnRAint <- thesisdata %>%
  group_by(TWOCLASS_CFA_ADHD, SEX_NUMERIC) %>%
  summarise(mean_value = mean(rsfmri_cor_ngd_df_scs_agrh, na.rm = TRUE))
dmnRAint

danLAint <- thesisdata %>%
  group_by(TWOCLASS_CFA_ADHD, SEX_NUMERIC) %>%
  summarise(mean_value = mean(rsfmri_cor_ngd_dsa_scs_aglh, na.rm = TRUE))
danLAint

danRAint <- thesisdata %>%
  group_by(TWOCLASS_CFA_ADHD, SEX_NUMERIC) %>%
  summarise(mean_value = mean(rsfmri_cor_ngd_dsa_scs_agrh, na.rm = TRUE))
danRAint

fpnLAint <- thesisdata %>%
  group_by(TWOCLASS_CFA_ADHD, SEX_NUMERIC) %>%
  summarise(mean_value = mean(rsfmri_cor_ngd_fopa_scs_aglh, na.rm = TRUE))
fpnLAint

fpnRAint <- thesisdata %>%
  group_by(TWOCLASS_CFA_ADHD, SEX_NUMERIC) %>%
  summarise(mean_value = mean(rsfmri_cor_ngd_fopa_scs_agrh, na.rm = TRUE))
fpnRAint

noneLAint <- thesisdata %>%
  group_by(TWOCLASS_CFA_ADHD, SEX_NUMERIC) %>%
  summarise(mean_value = mean(rsfmri_cor_ngd_none_scs_aglh, na.rm = TRUE))
noneLAint

noneRAint <- thesisdata %>%
  group_by(TWOCLASS_CFA_ADHD, SEX_NUMERIC) %>%
  summarise(mean_value = mean(rsfmri_cor_ngd_none_scs_agrh, na.rm = TRUE))
noneRAint

rstLAint <- thesisdata %>%
  group_by(TWOCLASS_CFA_ADHD, SEX_NUMERIC) %>%
  summarise(mean_value = mean(rsfmri_cor_ngd_rst_scs_aglh, na.rm = TRUE))
rstLAint

rstRAint <- thesisdata %>%
  group_by(TWOCLASS_CFA_ADHD, SEX_NUMERIC) %>%
  summarise(mean_value = mean(rsfmri_cor_ngd_rst_scs_agrh, na.rm = TRUE))
rstRAint

salLAint <- thesisdata %>%
  group_by(TWOCLASS_CFA_ADHD, SEX_NUMERIC) %>%
  summarise(mean_value = mean(rsfmri_cor_ngd_sa_scs_aglh, na.rm = TRUE))
salLAint

salRAint <- thesisdata %>%
  group_by(TWOCLASS_CFA_ADHD, SEX_NUMERIC) %>%
  summarise(mean_value = mean(rsfmri_cor_ngd_sa_scs_agrh, na.rm = TRUE))
salRAint

smhLAint <- thesisdata %>%
  group_by(TWOCLASS_CFA_ADHD, SEX_NUMERIC) %>%
  summarise(mean_value = mean(rsfmri_cor_ngd_smh_scs_aglh, na.rm = TRUE))
smhLAint

smhRAint <- thesisdata %>%
  group_by(TWOCLASS_CFA_ADHD, SEX_NUMERIC) %>%
  summarise(mean_value = mean(rsfmri_cor_ngd_smh_scs_agrh, na.rm = TRUE))
smhRAint

smmLAint <- thesisdata %>%
  group_by(TWOCLASS_CFA_ADHD, SEX_NUMERIC) %>%
  summarise(mean_value = mean(rsfmri_cor_ngd_smm_scs_aglh, na.rm = TRUE))
smmLAint

smmRAint <- thesisdata %>%
  group_by(TWOCLASS_CFA_ADHD, SEX_NUMERIC) %>%
  summarise(mean_value = mean(rsfmri_cor_ngd_smm_scs_agrh, na.rm = TRUE))
smmRAint

vanLAint <- thesisdata %>%
  group_by(TWOCLASS_CFA_ADHD, SEX_NUMERIC) %>%
  summarise(mean_value = mean(rsfmri_cor_ngd_vta_scs_aglh, na.rm = TRUE))
vanLAint

vanRAint <- thesisdata %>%
  group_by(TWOCLASS_CFA_ADHD, SEX_NUMERIC) %>%
  summarise(mean_value = mean(rsfmri_cor_ngd_vta_scs_agrh, na.rm = TRUE))
vanRAint

visLAint <- thesisdata %>%
  group_by(TWOCLASS_CFA_ADHD, SEX_NUMERIC) %>%
  summarise(mean_value = mean(rsfmri_cor_ngd_vs_scs_aglh, na.rm = TRUE))
visLAint

visRAint <- thesisdata %>%
  group_by(TWOCLASS_CFA_ADHD, SEX_NUMERIC) %>%
  summarise(mean_value = mean(rsfmri_cor_ngd_vs_scs_agrh, na.rm = TRUE))
visRAint
