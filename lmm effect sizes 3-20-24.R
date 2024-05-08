library(performance)
library(sjPlot)

#using copaLA as an example#

#intraclass correlation (for variance of random effects)
icc(copaLA)

#for table with random effects, marginal R^2 & conditional R^2, AND unstandardized CIs
tab_model(copaLA, digits = 5, digits.re = 5)

#function for creating standarized coefficients & CI's
stdCoef.merMod1 <- function(object) {
  sdy <- sd(getME(object,"y"))
  sdx <- apply(getME(object,"X"), 2, sd)
  sc <- fixef(object)*sdx/sdy
  se.fixef <- coef(summary(object))[,"Std. Error"]
  se <- se.fixef*sdx/sdy
  lb <- sc-1.96*se
  ub <- sc+1.96*se
  return(round(data.frame(stdcoef=sc, stdse=se, lb=lb, ub=ub),4))
}

#returning stdcoef, stdse, CIs
stdCoef.merMod1(auLA)
stdCoef.merMod1(auRA)
stdCoef.merMod1(cercLA)
stdCoef.merMod1(cercRA)
stdCoef.merMod1(copaLA)
stdCoef.merMod1(copaRA)
stdCoef.merMod1(dfLA)
stdCoef.merMod1(dfRA)
stdCoef.merMod1(dsaLA)
stdCoef.merMod1(dsaRA)
stdCoef.merMod1(fopaLA)
stdCoef.merMod1(fopaRA)
stdCoef.merMod1(noneLA)
stdCoef.merMod1(noneRA)
stdCoef.merMod1(rstLA)
stdCoef.merMod1(rstRA)
stdCoef.merMod1(saLA)
stdCoef.merMod1(saRA)
stdCoef.merMod1(smhLA)
stdCoef.merMod1(smhRA)
stdCoef.merMod1(smmLA)
stdCoef.merMod1(smmRA)
stdCoef.merMod1(vtaLA)
stdCoef.merMod1(vtaRA)
stdCoef.merMod1(vsLA)
stdCoef.merMod1(vsRA)

stdCoef.merMod1(auLA1)
stdCoef.merMod1(auRA1)
stdCoef.merMod1(cercLA1)
stdCoef.merMod1(cercRA1)
stdCoef.merMod1(copaLA1)
stdCoef.merMod1(copaRA1)
stdCoef.merMod1(dfLA1)
stdCoef.merMod1(dfRA1)
stdCoef.merMod1(dsaLA1)
stdCoef.merMod1(dsaRA1)
stdCoef.merMod1(fopaLA1)
stdCoef.merMod1(fopaRA1)
stdCoef.merMod1(noneLA1)
stdCoef.merMod1(noneRA1)
stdCoef.merMod1(rstLA1)
stdCoef.merMod1(rstRA1)
stdCoef.merMod1(saLA1)
stdCoef.merMod1(saRA1)
stdCoef.merMod1(smhLA1)
stdCoef.merMod1(smhRA1)
stdCoef.merMod1(smmLA1)
stdCoef.merMod1(smmRA1)
stdCoef.merMod1(vtaLA1)
stdCoef.merMod1(vtaRA1)
stdCoef.merMod1(vsLA1)
stdCoef.merMod1(vsRA1)
