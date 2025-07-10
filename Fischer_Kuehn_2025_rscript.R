# ANALYSIS OF DATA FOR MANUSCRIPT: Fischer & Kühn 2025 #
# Scent of Nature- Investigating Cognitive and Affective Effects of Tree Essential Oils

## last update: june 2025 (d.fischer@uke.de; Djo Fischer)

# library
library(ggplot2)
library(sjPlot)
library(tidyr)
library(psych)
library(ggpubr)
library(see)
library(forcats)
library(lme4)
library(stats)
library(sjlabelled)
library(sjmisc)
library(performance)
library(emmeans)
library(ggeffects)
library(dplyr)
library(purrr)
library(Hmisc)
library(parameters)

rm(list= ls())

# read in data
# setwd("X:/scent-of-nature/Paper/R_Skripts/") 
load('fischer-kuehn_2025_data.RData')
str(all)

all$ST_Hyposmie[all$ST >= 6] <- 0
all$ST_Hyposmie[all$ST < 6] <- 1
all$Exposition[all$Exposition == "0"] <- "Placebo"
all$Exposition[all$Exposition == "1"] <- "Geruch"
all$Baum[all$Baum == "0"] <- "Hinoki"
all$Baum[all$Baum == "1"] <- "DF"
all$Exposition <- as.factor(all$Exposition)
all$Baum <- as.factor(all$Baum)
all$Bedingung <- as.factor(all$Bedingung)
all$TZP <- as.factor(all$TZP)
all$Bedingung <- factor(all$Bedingung, levels = c("ex-e", "ex-x", "xe-x", "xe-e"))
str(all)

#2. Splitting the data set into study one and two
H1 <- subset(all[all$studie  == "h1",]) #study one
H2 <- subset(all[all$studie  == "h2",]) #study two

#
#3. Detection of Outlier 
# Identify the outliers with median absolute deviation (MAD) - method (Package: Routliers)
# with a MAD-threshold of 2.5 units; 1=Outlier, 0=no Outlier
library(Routliers)

# for the All Dataset
all$ZVT_isoutlier <- ifelse(all$ZVT %in% outliers_mad(all$ZVT, b = 1.4826,threshold = 2.5,na.rm = TRUE)$outliers, 1, 0)
all$DSB_bTE_ML_isoutlier <- ifelse(all$bTE_ML %in% outliers_mad(all$bTE_ML, b = 1.4826,threshold = 2.5,na.rm = TRUE)$outliers, 1, 0)
all$DSB_bTE_TT_isoutlier <- ifelse(all$bTE_TT %in% outliers_mad(all$bTE_TT, b = 1.4826,threshold = 2.5,na.rm = TRUE)$outliers, 1, 0)
all$ssrt_isoutlier <- ifelse(all$ssrt %in% outliers_mad(all$ssrt, b = 1.4826,threshold = 2.5,na.rm = TRUE)$outliers, 1, 0)

all$CST_RTSC_isoutlier <- ifelse(all$CST_RTSC %in% outliers_mad(all$CST_RTSC, b = 1.4826,threshold = 2.5,na.rm = TRUE)$outliers, 1, 0)
all$CST_ACCSC_isoutlier <- ifelse(all$CST_ACCSC %in% outliers_mad(all$CST_ACCSC, b = 1.4826,threshold = 2.5,na.rm = TRUE)$outliers, 1, 0)

all$RVIP_SumFA_isoutlier <- ifelse(all$RVIP_SumFA %in% outliers_mad(all$RVIP_SumFA, b = 1.4826,threshold = 2.5,na.rm = TRUE)$outliers, 1, 0)
all$RVIP_pc_isoutlier <- ifelse(all$RVIP_pc %in% outliers_mad(all$RVIP_pc, b = 1.4826,threshold = 2.5,na.rm = TRUE)$outliers, 1, 0)
all$RVIP_RThit_isoutlier <- ifelse(all$RVIP_RThit %in% outliers_mad(all$RVIP_RThit, b = 1.4826,threshold = 2.5,na.rm = TRUE)$outliers, 1, 0)

all$SANT_allACC_isoutlier <- ifelse(all$SANT_allACC %in% outliers_mad(all$SANT_allACC, b = 1.4826,threshold = 2.5,na.rm = TRUE)$outliers, 1, 0)
all$SANT_meanrt_isoutlier <- ifelse(all$SANT_meanrt %in% outliers_mad(all$SANT_meanrt, b = 1.4826,threshold = 2.5,na.rm = TRUE)$outliers, 1, 0)
all$SANT_ori_isoutlier <- ifelse(all$SANT_ori %in% outliers_mad(all$SANT_ori, b = 1.4826,threshold = 2.5,na.rm = TRUE)$outliers, 1, 0)
all$SANT_con_isoutlier <- ifelse(all$SANT_con %in% outliers_mad(all$SANT_con, b = 1.4826,threshold = 2.5,na.rm = TRUE)$outliers, 1, 0)

all$NBACK_max_isoutlier <- ifelse(all$NBACK_max %in% outliers_mad(all$NBACK_max , b = 1.4826,threshold = 2.5,na.rm = TRUE)$outliers, 1, 0)
all$NBACK_mean_isoutlier <- ifelse(all$NBACK_mean %in% outliers_mad(all$NBACK_mean , b = 1.4826,threshold = 2.5,na.rm = TRUE)$outliers, 1, 0)
all$NBACK_hit_isoutlier <- ifelse(all$TotalHits %in% outliers_mad(all$TotalHits , b = 1.4826,threshold = 2.5,na.rm = TRUE)$outliers, 1, 0)
all$NBACK_fa_isoutlier <- ifelse(all$TotalFA %in% outliers_mad(all$TotalFA , b = 1.4826,threshold = 2.5,na.rm = TRUE)$outliers, 1, 0)
all$NBACK_dv_isoutlier <- ifelse(all$DV %in% outliers_mad(all$DV , b = 1.4826,threshold = 2.5,na.rm = TRUE)$outliers, 1, 0)

all$f1_isoutlier <- ifelse(all$f1_Niedergeschlagenheit %in% outliers_mad(all$f1_Niedergeschlagenheit, b = 1.4826,threshold = 2.5,na.rm = TRUE)$outliers, 1, 0)
all$f2_isoutlier <- ifelse(all$f2_Tatendrang %in% outliers_mad(all$f2_Tatendrang, b = 1.4826,threshold = 2.5,na.rm = TRUE)$outliers, 1, 0)
all$f3_isoutlier <- ifelse(all$f3_Muedigkeit %in% outliers_mad(all$f3_Muedigkeit, b = 1.4826,threshold = 2.5,na.rm = TRUE)$outliers, 1, 0)
all$f4_isoutlier <- ifelse(all$f4_Missmut %in% outliers_mad(all$f4_Missmut, b = 1.4826,threshold = 2.5,na.rm = TRUE)$outliers, 1, 0)
all$PSS_isoutlier <- ifelse(all$PSS_ALL %in% outliers_mad(all$PSS_ALL, b = 1.4826,threshold = 2.5,na.rm = TRUE)$outliers, 1, 0)

# for the study one (H1)
H1$ZVT_isoutlier <- ifelse(H1$ZVT %in% outliers_mad(H1$ZVT, b = 1.4826,threshold = 2.5,na.rm = TRUE)$outliers, 1, 0)
H1$DSB_bTE_ML_isoutlier <- ifelse(H1$bTE_ML %in% outliers_mad(H1$bTE_ML, b = 1.4826,threshold = 2.5,na.rm = TRUE)$outliers, 1, 0)
H1$DSB_bTE_TT_isoutlier <- ifelse(H1$bTE_TT %in% outliers_mad(H1$bTE_TT, b = 1.4826,threshold = 2.5,na.rm = TRUE)$outliers, 1, 0)
H1$ssrt_isoutlier <- ifelse(H1$ssrt %in% outliers_mad(H1$ssrt, b = 1.4826,threshold = 2.5,na.rm = TRUE)$outliers, 1, 0)

H1$CST_RTSC_isoutlier <- ifelse(H1$CST_RTSC %in% outliers_mad(H1$CST_RTSC, b = 1.4826,threshold = 2.5,na.rm = TRUE)$outliers, 1, 0)
H1$CST_ACCSC_isoutlier <- ifelse(H1$CST_ACCSC %in% outliers_mad(H1$CST_ACCSC, b = 1.4826,threshold = 2.5,na.rm = TRUE)$outliers, 1, 0)

H1$RVIP_SumFA_isoutlier <- ifelse(H1$RVIP_SumFA %in% outliers_mad(H1$RVIP_SumFA, b = 1.4826,threshold = 2.5,na.rm = TRUE)$outliers, 1, 0)
H1$RVIP_pc_isoutlier <- ifelse(H1$RVIP_pc %in% outliers_mad(H1$RVIP_pc, b = 1.4826,threshold = 2.5,na.rm = TRUE)$outliers, 1, 0)
H1$RVIP_RThit_isoutlier <- ifelse(H1$RVIP_RThit %in% outliers_mad(H1$RVIP_RThit, b = 1.4826,threshold = 2.5,na.rm = TRUE)$outliers, 1, 0)

H1$SANT_allACC_isoutlier <- ifelse(H1$SANT_allACC %in% outliers_mad(H1$SANT_allACC, b = 1.4826,threshold = 2.5,na.rm = TRUE)$outliers, 1, 0)
H1$SANT_meanrt_isoutlier <- ifelse(H1$SANT_meanrt %in% outliers_mad(H1$SANT_meanrt, b = 1.4826,threshold = 2.5,na.rm = TRUE)$outliers, 1, 0)
H1$SANT_ori_isoutlier <- ifelse(H1$SANT_ori %in% outliers_mad(H1$SANT_ori, b = 1.4826,threshold = 2.5,na.rm = TRUE)$outliers, 1, 0)
H1$SANT_con_isoutlier <- ifelse(H1$SANT_con %in% outliers_mad(H1$SANT_con, b = 1.4826,threshold = 2.5,na.rm = TRUE)$outliers, 1, 0)

H1$NBACK_max_isoutlier <- ifelse(H1$NBACK_max %in% outliers_mad(H1$NBACK_max , b = 1.4826,threshold = 2.5,na.rm = TRUE)$outliers, 1, 0)
H1$NBACK_mean_isoutlier <- ifelse(H1$NBACK_mean %in% outliers_mad(H1$NBACK_mean , b = 1.4826,threshold = 2.5,na.rm = TRUE)$outliers, 1, 0)
H1$NBACK_hit_isoutlier <- ifelse(H1$TotalHits %in% outliers_mad(H1$TotalHits , b = 1.4826,threshold = 2.5,na.rm = TRUE)$outliers, 1, 0)
H1$NBACK_fa_isoutlier <- ifelse(H1$TotalFA %in% outliers_mad(H1$TotalFA , b = 1.4826,threshold = 2.5,na.rm = TRUE)$outliers, 1, 0)
H1$NBACK_dv_isoutlier <- ifelse(H1$DV %in% outliers_mad(H1$DV , b = 1.4826,threshold = 2.5,na.rm = TRUE)$outliers, 1, 0)

H1$f1_isoutlier <- ifelse(H1$f1_Niedergeschlagenheit %in% outliers_mad(H1$f1_Niedergeschlagenheit, b = 1.4826,threshold = 2.5,na.rm = TRUE)$outliers, 1, 0)
H1$f2_isoutlier <- ifelse(H1$f2_Tatendrang %in% outliers_mad(H1$f2_Tatendrang, b = 1.4826,threshold = 2.5,na.rm = TRUE)$outliers, 1, 0)
H1$f3_isoutlier <- ifelse(H1$f3_Muedigkeit %in% outliers_mad(H1$f3_Muedigkeit, b = 1.4826,threshold = 2.5,na.rm = TRUE)$outliers, 1, 0)
H1$f4_isoutlier <- ifelse(H1$f4_Missmut %in% outliers_mad(H1$f4_Missmut, b = 1.4826,threshold = 2.5,na.rm = TRUE)$outliers, 1, 0)
H1$PSS_isoutlier <- ifelse(H1$PSS_ALL %in% outliers_mad(H1$PSS_ALL, b = 1.4826,threshold = 2.5,na.rm = TRUE)$outliers, 1, 0)


# for the second study (H2)
H2$ZVT_isoutlier <- ifelse(H2$ZVT %in% outliers_mad(H2$ZVT, b = 1.4826,threshold = 2.5,na.rm = TRUE)$outliers, 1, 0)
H2$DSB_bTE_ML_isoutlier <- ifelse(H2$bTE_ML %in% outliers_mad(H2$bTE_ML, b = 1.4826,threshold = 2.5,na.rm = TRUE)$outliers, 1, 0)
H2$DSB_bTE_TT_isoutlier <- ifelse(H2$bTE_TT %in% outliers_mad(H2$bTE_TT, b = 1.4826,threshold = 2.5,na.rm = TRUE)$outliers, 1, 0)
H2$ssrt_isoutlier <- ifelse(H2$ssrt %in% outliers_mad(H2$ssrt, b = 1.4826,threshold = 2.5,na.rm = TRUE)$outliers, 1, 0)

H2$CST_RTSC_isoutlier <- ifelse(H2$CST_RTSC %in% outliers_mad(H2$CST_RTSC, b = 1.4826,threshold = 2.5,na.rm = TRUE)$outliers, 1, 0)
H2$CST_ACCSC_isoutlier <- ifelse(H2$CST_ACCSC %in% outliers_mad(H2$CST_ACCSC, b = 1.4826,threshold = 2.5,na.rm = TRUE)$outliers, 1, 0)

H2$RVIP_SumFA_isoutlier <- ifelse(H2$RVIP_SumFA %in% outliers_mad(H2$RVIP_SumFA, b = 1.4826,threshold = 2.5,na.rm = TRUE)$outliers, 1, 0)
H2$RVIP_pc_isoutlier <- ifelse(H2$RVIP_pc %in% outliers_mad(H2$RVIP_pc, b = 1.4826,threshold = 2.5,na.rm = TRUE)$outliers, 1, 0)
H2$RVIP_RThit_isoutlier <- ifelse(H2$RVIP_RThit %in% outliers_mad(H2$RVIP_RThit, b = 1.4826,threshold = 2.5,na.rm = TRUE)$outliers, 1, 0)

H2$SANT_allACC_isoutlier <- ifelse(H2$SANT_allACC %in% outliers_mad(H2$SANT_allACC, b = 1.4826,threshold = 2.5,na.rm = TRUE)$outliers, 1, 0)
H2$SANT_meanrt_isoutlier <- ifelse(H2$SANT_meanrt %in% outliers_mad(H2$SANT_meanrt, b = 1.4826,threshold = 2.5,na.rm = TRUE)$outliers, 1, 0)
H2$SANT_ori_isoutlier <- ifelse(H2$SANT_ori %in% outliers_mad(H2$SANT_ori, b = 1.4826,threshold = 2.5,na.rm = TRUE)$outliers, 1, 0)
H2$SANT_con_isoutlier <- ifelse(H2$SANT_con %in% outliers_mad(H2$SANT_con, b = 1.4826,threshold = 2.5,na.rm = TRUE)$outliers, 1, 0)

H2$NBACK_max_isoutlier <- ifelse(H2$NBACK_max %in% outliers_mad(H2$NBACK_max , b = 1.4826,threshold = 2.5,na.rm = TRUE)$outliers, 1, 0)
H2$NBACK_mean_isoutlier <- ifelse(H2$NBACK_mean %in% outliers_mad(H2$NBACK_mean , b = 1.4826,threshold = 2.5,na.rm = TRUE)$outliers, 1, 0)
H2$NBACK_hit_isoutlier <- ifelse(H2$TotalHits %in% outliers_mad(H2$TotalHits , b = 1.4826,threshold = 2.5,na.rm = TRUE)$outliers, 1, 0)
H2$NBACK_fa_isoutlier <- ifelse(H2$TotalFA %in% outliers_mad(H2$TotalFA , b = 1.4826,threshold = 2.5,na.rm = TRUE)$outliers, 1, 0)
H2$NBACK_dv_isoutlier <- ifelse(H2$DV %in% outliers_mad(H2$DV , b = 1.4826,threshold = 2.5,na.rm = TRUE)$outliers, 1, 0)

H2$f1_isoutlier <- ifelse(H2$f1_Niedergeschlagenheit %in% outliers_mad(H2$f1_Niedergeschlagenheit, b = 1.4826,threshold = 2.5,na.rm = TRUE)$outliers, 1, 0)
H2$f2_isoutlier <- ifelse(H2$f2_Tatendrang %in% outliers_mad(H2$f2_Tatendrang, b = 1.4826,threshold = 2.5,na.rm = TRUE)$outliers, 1, 0)
H2$f3_isoutlier <- ifelse(H2$f3_Muedigkeit %in% outliers_mad(H2$f3_Muedigkeit, b = 1.4826,threshold = 2.5,na.rm = TRUE)$outliers, 1, 0)
H2$f4_isoutlier <- ifelse(H2$f4_Missmut %in% outliers_mad(H2$f4_Missmut, b = 1.4826,threshold = 2.5,na.rm = TRUE)$outliers, 1, 0)
H2$PSS_isoutlier <- ifelse(H2$PSS_ALL %in% outliers_mad(H2$PSS_ALL, b = 1.4826,threshold = 2.5,na.rm = TRUE)$outliers, 1, 0)



# Requrements
# 4. test for normal distribution of the data 

shapiro.test(H1[H1$ZVT_isoutlier == 0 , ]$ZVT)
ggplot(H1[H1$ZVT_isoutlier == 0 , ], aes(x=ZVT)) + geom_histogram()

shapiro.test(H2[H2$ZVT_isoutlier == 0 & H2$Condition == 1, ]$ZVT)
ggplot(H2[H2$ZVT_isoutlier == 0 & H2$Condition == 1, ], aes(x=ZVT)) + geom_histogram()

shapiro.test(all[all$ZVT_isoutlier == 0 , ]$ZVT)
ggplot(all[all$ZVT_isoutlier == 0 , ], aes(x=ZVT)) + geom_histogram()

# graphical representation 
all$X <- all$RVIP_SumFA

ggplot(data = all[all$Baum == "DF" & all$RVIP_SumFA_isoutlier == 0, ], aes(x = Exposition, y = X, fill=Exposition)) +
  geom_violinhalf(flip = c(1), color = "transparent") +
  stat_summary(fun.data = "mean_cl_boot", geom = "pointrange", 
               colour = "black") +
  geom_line(data = all[all$Baum == "DF"& all$RVIP_SumFA_isoutlier == 0, ], aes(x = Exposition, y = X, group=ID), na.rm = TRUE, colour="grey") +
  labs(y="RVIP_SumFA", x= "Exposition" )+ 
  theme_bw(base_size = 13) +
  theme_modern() +
  scale_fill_brewer(palette="Pastel2") 



# Results
#LMER MODEls 
#DVs: ZVT, bTE_ML, bTE_TT, ssrt, CST_RTSC, $CST_ACCSC,
#RVIP_SumFA, RVIP_pc, RVIP_RThit, SANT_allACC, SANT_meanrt, SANT_ori, SANT_con,
#NBACK_max, NBACK_mean, TotalHits, TotalFA, DV,
#f1_Niedergeschlagenheit, f2_Tatendrang, f3_Muedigkeit, f4_Missmut, PSS_ALL

all$X <- all$CST_RTSC

lmer <- lmer(X ~ Exposition+TZP+(1|ID),
                  data=all[all$CST_RTSC_isoutlier == 0  & all$ST_Hyposmie == 0 &
                             all$Baum == "Hinoki" , ] ,na.action = na.omit)
tab_model(lmer, show.se = TRUE)
summary(lmer)
anova(lmer)
qqnorm(residuals(lmer)); qqline(residuals(lmer))
check_model(lmer)


#bayesian approach
library(rstanarm)
library(bayestestR)
library(insight)

Bay <- stan_glmer(DV ~ Exposition + TZP + (1 | ID), 
                data=all[all$NBACK_dv_isoutlier == 0  & all$ST_Hyposmie == 0 & 
                all$Baum == "DF" , ] ,
                chains = 10, iter = 5000, warmup = 1000, 
                prior = normal(0, 2),
                na.action = na.omit)

BF_intervall <- bayesfactor_parameters(Bay, null = rope_range(Bay))
plot(BF_intervall)
BF_intervall



###################
# Supplementary Material
# Table S4
# the Welch two sample t-test of the first study analyzing the difference between the odorant groups (Douglas fir vs. Hinoki) 
# in the descriptive and cognitive performance of the first test time point.


t.test(DV ~ Baum, var.equal = FALSE, alternative = c("two.sided"), 
       data = H1[H1$NBACK_dv_isoutlier == 0 & H1$TZP == 1, ])

sd(H1[H1$NBACK_dv_isoutlier == 0 & H1$TZP == 1 & H1$Baum == "DF", ]$DV)




# supplement Table S5 
# Analyses of the combined data without any exclusion of datapoints

lmer <- lmer(ssrt ~ Exposition+TZP+(1|ID),
             data=all[all$Baum == "DF" , ] ,na.action = na.omit)
tab_model(lmer, show.se = TRUE)

	
#all pvalues of the table S5 analysis (study without data exclusion)
pvalues_without_exclusion_DF <- c(0.04,	0.37,	0.30,	0.42,	0.26,	0.04,	0.87,	0.25,	0.64,
                               0.38,	0.71,	0.93, 0.11,	0.17,	0.21,	0.11,	0.48,	0.51)

pvalues_without_exclusion_H <- c(0.05, 0.89,	0.57,	0.50,	0.51,	0.35,	0.24,	0.51,	0.87,
                                 0.51,	0.09,	0.40,	0.92,	0.88,	0.77,	0.84,	0.83,	0.84)

fdrs<-p.adjust(pvalues_without_exclusion_H, method="BH")
print(fdrs)





# supplement Table S6
# Analyses of the first study for the Douglas fir odorant group

lmer <- lmer(ssrt ~ Exposition + TZP + (1|ID),
             data=H1[H1$ssrt_isoutlier == 0  & H1$Baum == "DF" , ] ,na.action = na.omit)
tab_model(lmer, show.se = TRUE)
summary(lmer)


#all pvalues of the table S5 analysis (study one Douglas Fir only)
pvaluesH1 <- c(0.75, 0.43, 0.25, 0.17, 0.25, 0.11, 0.61, 0.02,
               0.38, 0.58, 0.93, 0.27, 0.27, 0.22, 0.89, 0.56, 0.64, 0.24)

fdrs<-p.adjust(pvaluesH1, method="BH")
print(fdrs)




# supplement Table S7
# Analyses of the second study (Douglas fir odorant group)

lmer <- lmer(ZVT ~ Exposition+TZP+(1|ID),
             data=H2[H2$ZVT_isoutlier == 0   & H2$ST_Hyposmie == 0, ] ,na.action = na.omit)
tab_model(lmer, show.se = TRUE)
summary(lmer)

#all pvalues of the table S6 analysis (study two Douglas Fir only)
pvaluesH2 <- c(0.002,	0.33,	0.32,	0.72,	0.49,	0.69,	0.63,	0.71,	0.82,	
               0.29,	0.98,	0.33,	0.54,	0.24,	0.21,	0.71,	0.26,	0.44)

fdrs<-p.adjust(pvaluesH2, method="BH")
print(fdrs)







# supplement Table S8
# Analyses based on combined data of both studies for the Douglas fir 
# odorant group, including only participants who identified the odor 
# correctly as tree odor or could assign the odor to the correct category.

lmer <- lmer(DV ~ Exposition + TZP + (1|ID),
             data=all[all$NBACK_dv_isoutlier == 0 & all$ST_Hyposmie == 0 
                      & all$Geruch_korrekt_besch_o_cat == 1 & all$Baum == "DF" ,] ,na.action = na.omit)
tab_model(lmer, show.se = TRUE)
summary(lmer)


#all pvalues of the table S6 analysis (study two Douglas Fir only)
pvaluesALLkorrekt <- c(0.91, 0.41, 0.99, 0.11, 0.35, 0.73, 0.22, 0.31, 0.41,
                       0.38, 0.57, 0.88, 0.90, 0.51, 0.23, 0.89, 0.06, 0.03)

fdrs<-p.adjust(pvaluesALLkorrekt, method="BH")
print(fdrs)
