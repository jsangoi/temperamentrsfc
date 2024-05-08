#auditory
pauLA <- lmer(rsfmri_cor_ngd_au_scs_aglh ~ TWOCLASS_CFA_ADHD + SEX_NUMERIC 
             + meds_yes_no + cbcl_scr_dsm5_depress_t + cbcl_scr_dsm5_anxdisord_t
             + (1 | site_id_l/rel_family_id), data = thesisdata)
summary(pauLA)

pauRA <- lmer(rsfmri_cor_ngd_au_scs_agrh ~ TWOCLASS_CFA_ADHD + SEX_NUMERIC
             + meds_yes_no + cbcl_scr_dsm5_depress_t + cbcl_scr_dsm5_anxdisord_t
             + (1 | site_id_l/rel_family_id), data = thesisdata)
summary(pauRA)

#cingulo-opercular
pcercLA <- lmer(rsfmri_cor_ngd_cerc_scs_aglh ~ TWOCLASS_CFA_ADHD + SEX_NUMERIC
               + meds_yes_no + cbcl_scr_dsm5_depress_t + cbcl_scr_dsm5_anxdisord_t
               + (1 | site_id_l/rel_family_id), data = thesisdata)
summary(pcercLA)

pcercRA <- lmer(rsfmri_cor_ngd_cerc_scs_agrh ~ TWOCLASS_CFA_ADHD + SEX_NUMERIC
               + meds_yes_no + cbcl_scr_dsm5_depress_t + cbcl_scr_dsm5_anxdisord_t
               + (1 | site_id_l/rel_family_id), data = thesisdata)
summary(pcercRA)

#cingulo-parietal
pcopaLA <- lmer(rsfmri_cor_ngd_copa_scs_aglh ~ TWOCLASS_CFA_ADHD + SEX_NUMERIC
               + meds_yes_no + cbcl_scr_dsm5_depress_t + cbcl_scr_dsm5_anxdisord_t
               + (1 | site_id_l/rel_family_id), data = thesisdata)
summary(pcopaLA)

pcopaRA <- lmer(rsfmri_cor_ngd_copa_scs_agrh ~ TWOCLASS_CFA_ADHD + SEX_NUMERIC
               + meds_yes_no + cbcl_scr_dsm5_depress_t + cbcl_scr_dsm5_anxdisord_t
               + (1 | site_id_l/rel_family_id), data = thesisdata)
summary(pcopaRA)

#default mode
pdfLA <- lmer(rsfmri_cor_ngd_df_scs_aglh ~ TWOCLASS_CFA_ADHD + SEX_NUMERIC
             + meds_yes_no + cbcl_scr_dsm5_depress_t + cbcl_scr_dsm5_anxdisord_t
             + (1 | site_id_l/rel_family_id), data = thesisdata)
summary(pdfLA)

pdfRA <- lmer(rsfmri_cor_ngd_df_scs_agrh ~ TWOCLASS_CFA_ADHD + SEX_NUMERIC
             + meds_yes_no + cbcl_scr_dsm5_depress_t + cbcl_scr_dsm5_anxdisord_t
             + (1 | site_id_l/rel_family_id), data = thesisdata)
summary(pdfRA)

#dorsal attention
pdsaLA <- lmer(rsfmri_cor_ngd_dsa_scs_aglh ~ TWOCLASS_CFA_ADHD + SEX_NUMERIC
              + meds_yes_no + cbcl_scr_dsm5_depress_t + cbcl_scr_dsm5_anxdisord_t
              + (1 | site_id_l/rel_family_id), data = thesisdata)
summary(pdsaLA)

pdsaRA <- lmer(rsfmri_cor_ngd_dsa_scs_agrh ~ TWOCLASS_CFA_ADHD + SEX_NUMERIC
              + meds_yes_no + cbcl_scr_dsm5_depress_t + cbcl_scr_dsm5_anxdisord_t
              + (1 | site_id_l/rel_family_id), data = thesisdata)
summary(pdsaRA)

#frontoparietal
pfopaLA <- lmer(rsfmri_cor_ngd_fopa_scs_aglh ~ TWOCLASS_CFA_ADHD + SEX_NUMERIC
               + meds_yes_no + cbcl_scr_dsm5_depress_t + cbcl_scr_dsm5_anxdisord_t
               + (1 | site_id_l/rel_family_id), data = thesisdata)
summary(pfopaLA)

pfopaRA <- lmer(rsfmri_cor_ngd_fopa_scs_agrh ~ TWOCLASS_CFA_ADHD + SEX_NUMERIC
               + meds_yes_no + cbcl_scr_dsm5_depress_t + cbcl_scr_dsm5_anxdisord_t
               + (1 | site_id_l/rel_family_id), data = thesisdata)
summary(pfopaRA)

#none network
pnoneLA <- lmer(rsfmri_cor_ngd_none_scs_aglh ~ TWOCLASS_CFA_ADHD + SEX_NUMERIC
               + meds_yes_no + cbcl_scr_dsm5_depress_t + cbcl_scr_dsm5_anxdisord_t
               + (1 | site_id_l/rel_family_id), data = thesisdata)
summary(pnoneLA)

pnoneRA <- lmer(rsfmri_cor_ngd_none_scs_agrh ~ TWOCLASS_CFA_ADHD + SEX_NUMERIC
               + meds_yes_no + cbcl_scr_dsm5_depress_t + cbcl_scr_dsm5_anxdisord_t
               + (1 | site_id_l/rel_family_id), data = thesisdata)
summary(pnoneRA)

#retrosplenial temporal
prstLA <- lmer(rsfmri_cor_ngd_rst_scs_aglh ~ TWOCLASS_CFA_ADHD + SEX_NUMERIC
              + meds_yes_no + cbcl_scr_dsm5_depress_t + cbcl_scr_dsm5_anxdisord_t
              + (1 | site_id_l/rel_family_id), data = thesisdata)
summary(prstLA)

prstRA <- lmer(rsfmri_cor_ngd_rst_scs_agrh ~ TWOCLASS_CFA_ADHD + SEX_NUMERIC
              + meds_yes_no + cbcl_scr_dsm5_depress_t + cbcl_scr_dsm5_anxdisord_t
              + (1 | site_id_l/rel_family_id), data = thesisdata)
summary(prstRA)

#salience
psaLA <- lmer(rsfmri_cor_ngd_sa_scs_aglh ~ TWOCLASS_CFA_ADHD + SEX_NUMERIC
             + meds_yes_no + cbcl_scr_dsm5_depress_t + cbcl_scr_dsm5_anxdisord_t
             + (1 | site_id_l/rel_family_id), data = thesisdata)
summary(psaLA)

psaRA <- lmer(rsfmri_cor_ngd_sa_scs_agrh ~ TWOCLASS_CFA_ADHD + SEX_NUMERIC
             + meds_yes_no + cbcl_scr_dsm5_depress_t + cbcl_scr_dsm5_anxdisord_t
             + (1 | site_id_l/rel_family_id), data = thesisdata)
summary(psaRA)

#sensorimotor hand
psmhLA <- lmer(rsfmri_cor_ngd_smh_scs_aglh ~ TWOCLASS_CFA_ADHD + SEX_NUMERIC
              + meds_yes_no + cbcl_scr_dsm5_depress_t + cbcl_scr_dsm5_anxdisord_t
              + (1 | site_id_l/rel_family_id), data = thesisdata)
summary(psmhLA)

psmhRA <- lmer(rsfmri_cor_ngd_smh_scs_agrh ~ TWOCLASS_CFA_ADHD + SEX_NUMERIC
              + meds_yes_no + cbcl_scr_dsm5_depress_t + cbcl_scr_dsm5_anxdisord_t
              + (1 | site_id_l/rel_family_id), data = thesisdata)
summary(psmhRA)

#sensorimotor mouth
psmmLA <- lmer(rsfmri_cor_ngd_smm_scs_aglh ~ TWOCLASS_CFA_ADHD + SEX_NUMERIC
              + meds_yes_no + cbcl_scr_dsm5_depress_t + cbcl_scr_dsm5_anxdisord_t
              + (1 | site_id_l/rel_family_id), data = thesisdata)
summary(psmmLA)

psmmRA <- lmer(rsfmri_cor_ngd_smm_scs_agrh ~ TWOCLASS_CFA_ADHD + SEX_NUMERIC
              + meds_yes_no + cbcl_scr_dsm5_depress_t + cbcl_scr_dsm5_anxdisord_t
              + (1 | site_id_l/rel_family_id), data = thesisdata)
summary(psmmRA)

#ventral attention
pvtaLA <- lmer(rsfmri_cor_ngd_vta_scs_aglh ~ TWOCLASS_CFA_ADHD + SEX_NUMERIC
              + meds_yes_no + cbcl_scr_dsm5_depress_t + cbcl_scr_dsm5_anxdisord_t
              + (1 | site_id_l/rel_family_id), data = thesisdata)
summary(pvtaLA)

pvtaRA <- lmer(rsfmri_cor_ngd_vta_scs_agrh ~ TWOCLASS_CFA_ADHD + SEX_NUMERIC
              + meds_yes_no + cbcl_scr_dsm5_depress_t + cbcl_scr_dsm5_anxdisord_t
              + (1 | site_id_l/rel_family_id), data = thesisdata)
summary(pvtaRA)

#visual
pvsLA <- lmer(rsfmri_cor_ngd_vs_scs_aglh ~ TWOCLASS_CFA_ADHD + SEX_NUMERIC
             + meds_yes_no + cbcl_scr_dsm5_depress_t + cbcl_scr_dsm5_anxdisord_t
             + (1 | site_id_l/rel_family_id), data = thesisdata)
summary(pvsLA)

pvsRA <- lmer(rsfmri_cor_ngd_vs_scs_agrh ~ TWOCLASS_CFA_ADHD + SEX_NUMERIC
             + meds_yes_no + cbcl_scr_dsm5_depress_t + cbcl_scr_dsm5_anxdisord_t
             + (1 | site_id_l/rel_family_id), data = thesisdata)
summary(pvsRA)


newp_surg <- c(.024, .334, .103, .075, .210, .142, .064, .152, .691, .002, .058, .358, .693, .482, .282, .001, .098, .150, .616, .695, .490, .839, .392, .104, .518, .260)
FDRestimation::p.fdr(p = newp_surg, threshold = .05, adjust.method = "BH")

newp_irr <- c(.517, .031, .058, .836, .052, .405, .680, .094, .455, .184, .237, .889, .010, .023, .238, .370, .799, .037, .162, .912, .375, .314, .355, .217, .139, .322)
FDRestimation::p.fdr(p = newp_irr, threshold = .05, adjust.method = "BH")

newp_int_surg <- c(0.4003,0.92324,0.13444,0.824,0.050778,0.3412,0.891473,0.9627,0.0709,0.37261,0.098367,
                   0.431,0.0161,0.485609,0.630310,0.077439,0.882,0.6837,0.7954,0.5418,0.0260,0.5681,
                   0.92079,0.04808,0.16309,0.04942)
FDRestimation::p.fdr(p = newp_int_surg, threshold = .05, adjust.method = "BH")

newp_int_irr <- c(0.7485,0.02179,0.69901,0.781,0.842691,0.3407,0.953912,0.8442,0.3975,0.90254,0.227473,
                  0.699,0.7773,0.744803,0.527165,0.860754,0.688,0.3858,0.0137,0.0141,0.5717,0.2310,
                  0.03928,0.35800,0.44570,0.52315)
FDRestimation::p.fdr(p = newp_int_irr, threshold = .05, adjust.method = "BH")

#irritable vs. surgent
pauLA1 <- lmer(rsfmri_cor_ngd_au_scs_aglh ~ groupsTD3 + SEX_NUMERIC 
              + meds_yes_no + cbcl_scr_dsm5_depress_t + cbcl_scr_dsm5_anxdisord_t
              + (1 | site_id_l/rel_family_id), data = thesisdata)
summary(pauLA1)

pauRA1 <- lmer(rsfmri_cor_ngd_au_scs_agrh ~ groupsTD3 + SEX_NUMERIC 
              + meds_yes_no + cbcl_scr_dsm5_depress_t + cbcl_scr_dsm5_anxdisord_t
              + (1 | site_id_l/rel_family_id), data = thesisdata)
summary(pauRA1)

#cingulo-opercular
pcercLA1 <- lmer(rsfmri_cor_ngd_cerc_scs_aglh ~ groupsTD3 + SEX_NUMERIC 
                + meds_yes_no + cbcl_scr_dsm5_depress_t + cbcl_scr_dsm5_anxdisord_t
                + (1 | site_id_l/rel_family_id), data = thesisdata)
summary(pcercLA1)

pcercRA1 <- lmer(rsfmri_cor_ngd_cerc_scs_agrh ~ groupsTD3 + SEX_NUMERIC 
                + meds_yes_no + cbcl_scr_dsm5_depress_t + cbcl_scr_dsm5_anxdisord_t
                + (1 | site_id_l/rel_family_id), data = thesisdata)
summary(pcercRA1)

#cingulo-parietal
pcopaLA1 <- lmer(rsfmri_cor_ngd_copa_scs_aglh ~ groupsTD3 + SEX_NUMERIC 
                + meds_yes_no + cbcl_scr_dsm5_depress_t + cbcl_scr_dsm5_anxdisord_t
                + (1 | site_id_l/rel_family_id), data = thesisdata)
summary(pcopaLA1)

pcopaRA1 <- lmer(rsfmri_cor_ngd_copa_scs_agrh ~ groupsTD3 + SEX_NUMERIC 
                + meds_yes_no + cbcl_scr_dsm5_depress_t + cbcl_scr_dsm5_anxdisord_t
                + (1 | site_id_l/rel_family_id), data = thesisdata)
summary(pcopaRA1)

#default mode
pdfLA1 <- lmer(rsfmri_cor_ngd_df_scs_aglh ~ groupsTD3 + SEX_NUMERIC 
              + meds_yes_no + cbcl_scr_dsm5_depress_t + cbcl_scr_dsm5_anxdisord_t
              + (1 | site_id_l/rel_family_id), data = thesisdata)
summary(pdfLA1)

pdfRA1 <- lmer(rsfmri_cor_ngd_df_scs_agrh ~ groupsTD3 + SEX_NUMERIC 
              + meds_yes_no + cbcl_scr_dsm5_depress_t + cbcl_scr_dsm5_anxdisord_t
              + (1 | site_id_l/rel_family_id), data = thesisdata)
summary(pdfRA1)

#dorsal attention
pdsaLA1 <- lmer(rsfmri_cor_ngd_dsa_scs_aglh ~ groupsTD3 + SEX_NUMERIC 
               + meds_yes_no + cbcl_scr_dsm5_depress_t + cbcl_scr_dsm5_anxdisord_t
               + (1 | site_id_l/rel_family_id), data = thesisdata)
summary(pdsaLA1)

pdsaRA1 <- lmer(rsfmri_cor_ngd_dsa_scs_agrh ~ groupsTD3 + SEX_NUMERIC 
               + meds_yes_no + cbcl_scr_dsm5_depress_t + cbcl_scr_dsm5_anxdisord_t
               + (1 | site_id_l/rel_family_id), data = thesisdata)
summary(pdsaRA1)

#fronto-parietal
pfopaLA1 <- lmer(rsfmri_cor_ngd_fopa_scs_aglh ~ groupsTD3 + SEX_NUMERIC 
                + meds_yes_no + cbcl_scr_dsm5_depress_t + cbcl_scr_dsm5_anxdisord_t
                + (1 | site_id_l/rel_family_id), data = thesisdata)
summary(pfopaLA1)

pfopaRA1 <- lmer(rsfmri_cor_ngd_fopa_scs_agrh ~ groupsTD3 + SEX_NUMERIC 
                + meds_yes_no + cbcl_scr_dsm5_depress_t + cbcl_scr_dsm5_anxdisord_t
                + (1 | site_id_l/rel_family_id), data = thesisdata)
summary(pfopaRA1)

#none networks
pnoneLA1 <- lmer(rsfmri_cor_ngd_none_scs_aglh ~ groupsTD3 + SEX_NUMERIC 
                + meds_yes_no + cbcl_scr_dsm5_depress_t + cbcl_scr_dsm5_anxdisord_t
                + (1 | site_id_l/rel_family_id), data = thesisdata)
summary(pnoneLA1)

pnoneRA1 <- lmer(rsfmri_cor_ngd_none_scs_agrh ~ groupsTD3 + SEX_NUMERIC 
                + meds_yes_no + cbcl_scr_dsm5_depress_t + cbcl_scr_dsm5_anxdisord_t
                + (1 | site_id_l/rel_family_id), data = thesisdata)
summary(pnoneRA1)

#retrosplenial temporal
prstLA1 <- lmer(rsfmri_cor_ngd_rst_scs_aglh ~ groupsTD3 + SEX_NUMERIC 
               + meds_yes_no + cbcl_scr_dsm5_depress_t + cbcl_scr_dsm5_anxdisord_t
               + (1 | site_id_l/rel_family_id), data = thesisdata)
summary(prstLA1)

prstRA1 <- lmer(rsfmri_cor_ngd_rst_scs_agrh ~ groupsTD3 + SEX_NUMERIC 
               + meds_yes_no + cbcl_scr_dsm5_depress_t + cbcl_scr_dsm5_anxdisord_t
               + (1 | site_id_l/rel_family_id), data = thesisdata)
summary(prstRA1)

#salience
psaLA1 <- lmer(rsfmri_cor_ngd_sa_scs_aglh ~ groupsTD3 + SEX_NUMERIC 
              + meds_yes_no + cbcl_scr_dsm5_depress_t + cbcl_scr_dsm5_anxdisord_t
              + (1 | site_id_l/rel_family_id), data = thesisdata)
summary(psaLA1)

psaRA1 <- lmer(rsfmri_cor_ngd_sa_scs_agrh ~ groupsTD3 + SEX_NUMERIC 
              + meds_yes_no + cbcl_scr_dsm5_depress_t + cbcl_scr_dsm5_anxdisord_t
              + (1 | site_id_l/rel_family_id), data = thesisdata)
summary(psaRA1)

#sensorimotor hand
psmhLA1 <- lmer(rsfmri_cor_ngd_smh_scs_aglh ~ groupsTD3 + SEX_NUMERIC 
               + meds_yes_no + cbcl_scr_dsm5_depress_t + cbcl_scr_dsm5_anxdisord_t
               + (1 | site_id_l/rel_family_id), data = thesisdata)
summary(psmhLA1)

psmhRA1 <- lmer(rsfmri_cor_ngd_smh_scs_agrh ~ groupsTD3 + SEX_NUMERIC 
               + meds_yes_no + cbcl_scr_dsm5_depress_t + cbcl_scr_dsm5_anxdisord_t
               + (1 | site_id_l/rel_family_id), data = thesisdata)
summary(psmhRA1)

#sensorimotor mouth
psmmLA1 <- lmer(rsfmri_cor_ngd_smm_scs_aglh ~ groupsTD3 + SEX_NUMERIC 
               + meds_yes_no + cbcl_scr_dsm5_depress_t + cbcl_scr_dsm5_anxdisord_t
               + (1 | site_id_l/rel_family_id), data = thesisdata)
summary(psmmLA1)

psmmRA1 <- lmer(rsfmri_cor_ngd_smm_scs_agrh ~ groupsTD3 + SEX_NUMERIC 
               + meds_yes_no + cbcl_scr_dsm5_depress_t + cbcl_scr_dsm5_anxdisord_t
               + (1 | site_id_l/rel_family_id), data = thesisdata)
summary(psmmRA1)

#ventral attention
pvtaLA1 <- lmer(rsfmri_cor_ngd_vta_scs_aglh ~ groupsTD3 + SEX_NUMERIC 
               + meds_yes_no + cbcl_scr_dsm5_depress_t + cbcl_scr_dsm5_anxdisord_t
               + (1 | site_id_l/rel_family_id), data = thesisdata)
summary(pvtaLA1)

pvtaRA1 <- lmer(rsfmri_cor_ngd_vta_scs_agrh ~ groupsTD3 + SEX_NUMERIC 
               + meds_yes_no + cbcl_scr_dsm5_depress_t + cbcl_scr_dsm5_anxdisord_t
               + (1 | site_id_l/rel_family_id), data = thesisdata)
summary(pvtaRA1)

#visual
pvsLA1 <- lmer(rsfmri_cor_ngd_vs_scs_aglh ~ groupsTD3 + SEX_NUMERIC 
              + meds_yes_no + cbcl_scr_dsm5_depress_t + cbcl_scr_dsm5_anxdisord_t
              + (1 | site_id_l/rel_family_id), data = thesisdata)
summary(pvsLA1)

pvsRA1 <- lmer(rsfmri_cor_ngd_vs_scs_agrh ~ groupsTD3 + SEX_NUMERIC 
              + meds_yes_no + cbcl_scr_dsm5_depress_t + cbcl_scr_dsm5_anxdisord_t
              + (1 | site_id_l/rel_family_id), data = thesisdata)
summary(pvsRA1)

newp_ivs <- c(.171, .510, .011, .202, .769, .531, .230, .024, .417, .118, .468, .523, .044, .345, .943, .044, .138, .809, .597, .670, .255, .407, .196, .035, .135, .810)
FDRestimation::p.fdr(p = newp_ivs, threshold = .05, adjust.method = "BH")

newp_int_ivs <- c(0.382,0.150,0.3222,0.729,0.08766,0.8346,0.9388,0.9374,0.0486,0.4246,0.03922,0.68075,
                  0.03338, 0.707948,0.443,0.123275,0.71970,0.8546,0.094,0.0506,0.1395,0.8052,0.25328,
                  0.03120, 0.49508,0.2223)
FDRestimation::p.fdr(p = newp_int_ivs, threshold = .05, adjust.method = "BH")
