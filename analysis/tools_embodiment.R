#' ---
#' title: Tools Embodiment Analysis
#' author: Kevin A Smith
#' date: Apr 8, 2020
#' output:
#'    html_document:
#'      toc: true
#'      toc_depth: 1
#'      toc_float: false
#'      theme: default
#'      highlight: tango
#' ---
#' 
#+ General settings, echo = FALSE, results = 'hide', fig.width = 4, fig.height = 4 ------------------------------------------------------------------------------

knitr::opts_chunk$set(warning=F, message=F, cache = F, echo=F)
options(digits = 3)
kable = knitr::kable
export = F
FIGURE_DIR = "figures/"

#+ Initialization --------------

#' # Setup

#' ## Load in the required libraries and data

library(tidyverse)
library(lme4)
library(lmerTest)
library(car)
library(DHARMa)
library(ggrepel)

dat_full = read.csv('data/full_handedness.csv')
dat_laterality = read.csv('data/laterality.csv')
dat_device = read.csv('data/devices.csv')
dat_residual = read.csv('data/intact_residual.csv')
dat_residual = dat_residual %>% 
  mutate(Intact = recode_factor(Intact, `feet` = "0H", `residual` = "0H", `other`="0H",`intact`="1H"))
dat_laterality = dat_laterality %>%
  merge(dat_device)
dat_prosthesis = read.csv('data/prosthesis.csv') %>% select(WorkerID, Prosthesis_Use) %>% 
  rbind(read.csv('data/prosthesis_kids.csv'))

dat_bysubj = read.csv('data/subject_handedness.csv')
# Add z-scaled variables for easier GLM fitting
dat_bytrial = read.csv('data/trial_handedness.csv') %>% 
  mutate(ZAge = scale(Age), ZMRT = scale(MedianMotorRT))
dat_bytrial_succ = read.csv('data/trial_succ_handedness.csv') %>% 
  mutate(ZAge = scale(Age), ZMRT = scale(MedianMotorRT))
dat_bytrial_succ = dat_bytrial_succ %>% 
  merge(dat_laterality)
dat_bytrial = dat_bytrial %>% 
  merge(dat_laterality)
dat_bytrial = dat_bytrial %>% 
  merge(dat_prosthesis,all.x=T)
dat_bytrial_succ = dat_bytrial_succ %>% 
  merge(dat_prosthesis,all.x=T)

dat_bytrial_succ_resid = dat_bytrial_succ %>% 
  merge(dat_residual)
dat_bytrial_resid = dat_bytrial %>% 
  merge(dat_residual)


#' Coefficients for unscaling age
USCALE_AGE_MEAN = mean(dat_bytrial$Age)
USCALE_AGE_SD = sd(dat_bytrial$Age)
USCALE_AGE_SUCC_MEAN = mean(dat_bytrial_succ$Age)
USCALE_AGE_SUCC_SD = sd(dat_bytrial_succ$Age)

#' Aggregate data for plotting
se = function(x, na.rm=T) {
  if (na.rm) {
    l = length(x[!(is.na(x))])
  } else {
    l = length(x)
  }
  return(sd(x, na.rm=T)/sqrt(l))
}

se_binom = function(x, na.rm=T) {
  p = mean(x, na.rm=T)
  if (na.rm) {
    l = length(x[!(is.na(x))])
  } else {
    l = length(x)
  }
  return(sqrt(p*(1-p)/l))
}


dat_plotagg_base = dat_bytrial %>% 
  rename(S = SuccessByOnline) %>% 
  group_by(WorkerID, AgeCategory, Hands) %>% 
  summarize(
    Accuracy = mean(S), Placements = mean(NPlacements), Placements_oS = mean(NPlacements[S]),
    FP = mean(FirstPlace), FP_oS = mean(FirstPlace[S]), LP = mean(LastPlace), LP_oS = mean(LastPlace[S]),
    TimeBetween = mean(AvgTimeDiff, na.rm=T), TimeBetween_oS = mean(AvgTimeDiff[S], na.rm=T),
    XOff = mean(AvgAbsXOffset), XOff_oS = mean(AvgAbsXOffset[S]), YOff = mean(AvgAbsYOffset), YOff_oS = mean(AvgAbsYOffset[S])
    ) %>% 
  ungroup

dat_plotagg = dat_plotagg_base %>% 
  group_by(AgeCategory, Hands) %>% 
  summarize(
    Avg_Accuracy = mean(Accuracy), SE_Accuracy = se_binom(Accuracy),
    Avg_Placements = mean(Placements), SE_Placements = se(Placements), Avg_Placements_oS = mean(Placements_oS), SE_Placements_oS = se(Placements_oS),
    Avg_FirstPlace = mean(FP), SE_FirstPlace = se(FP), Avg_FirstPlace_oS = mean(FP_oS), SE_FirstPlace_oS = se(FP_oS),
    Avg_LastPlace = mean(LP), SE_LastPlace = se(LP), Avg_LastPlace_oS = mean(LP_oS), SE_LastPlace_oS = se(LP_oS),
    Avg_TimeBetween = mean(TimeBetween), SE_TimeBetween = se(TimeBetween), Avg_TimeBetween_oS = mean(TimeBetween_oS), SE_TimeBetween_oS = se(TimeBetween_oS),
    Avg_XOff = mean(XOff), SE_XOff = se(XOff), Avg_XOff_oS = mean(XOff_oS), SE_XOff_oS = se(XOff_oS),
    Avg_YOff = mean(YOff), SE_YOff = se(YOff), Avg_YOff_oS = mean(YOff_oS), SE_YOff_oS = se(YOff_oS)
  ) %>% 
  ungroup %>% 
  mutate(AgeCategory = fct_relevel(AgeCategory, 'kids'))

dat_plotagg_byagecat = dat_plotagg_base %>% 
  group_by(AgeCategory) %>% 
  summarize(
    Avg_Accuracy = mean(Accuracy), SE_Accuracy = se_binom(Accuracy),
    Avg_Placements = mean(Placements), SE_Placements = se(Placements), Avg_Placements_oS = mean(Placements_oS), SE_Placements_oS = se(Placements_oS),
    Avg_FirstPlace = mean(FP), SE_FirstPlace = se(FP), Avg_FirstPlace_oS = mean(FP_oS), SE_FirstPlace_oS = se(FP_oS),
    Avg_LastPlace = mean(LP), SE_LastPlace = se(LP), Avg_LastPlace_oS = mean(LP_oS), SE_LastPlace_oS = se(LP_oS),
    Avg_TimeBetween = mean(TimeBetween), SE_TimeBetween = se(TimeBetween), Avg_TimeBetween_oS = mean(TimeBetween_oS), SE_TimeBetween_oS = se(TimeBetween_oS),
    Avg_XOff = mean(XOff), SE_XOff = se(XOff), Avg_XOff_oS = mean(XOff_oS), SE_XOff_oS = se(XOff_oS),
    Avg_YOff = mean(YOff), SE_YOff = se(YOff), Avg_YOff_oS = mean(YOff_oS), SE_YOff_oS = se(YOff_oS)
  ) %>% 
  ungroup %>% 
  merge(dat_bytrial_succ %>% 
          group_by(AgeCategory, WorkerID) %>% 
          summarize(AvgTrn = mean(Transition)) %>% 
          ungroup %>% 
          group_by(AgeCategory) %>% 
          summarize(Avg_Transition = mean(AvgTrn), SE_Transition = se(AvgTrn)) %>% 
          ungroup)


#' ## Contrasts and helper functions

# Set up contrasts for categorical sum contrasts going 1H->2h, kids->adults
contrasts(dat_bytrial_succ$Hands) = c(-.5, .5)
contrasts(dat_bytrial_succ$AgeCategory) = c(.5, -.5)
contrasts(dat_bytrial$Hands) = c(-.5, .5)
contrasts(dat_bytrial$AgeCategory) = c(.5, -.5)
contrasts(dat_bysubj$Hands) = c(-.5, .5)
contrasts(dat_bysubj$AgeCategory) = c(.5, -.5)

# Function to transform labels for plotting
transform_category_labels = function(dat, use_long=F) {
  if (use_long) {
    htr = c('With Limb Differences', 'Without Limb Differences')
  } else {
    htr = c('LD', 'NLD')
  }
  dat %>% 
    mutate(Hands = recode_factor(Hands, `1h` = htr[1], `2h` = htr[2]),
           AgeCategory = recode_factor(AgeCategory, `adult` = "Adult", `kids` = "Child")) %>% 
    mutate(AgeCategory = fct_relevel(AgeCategory, 'Child')) %>% 
    return
}
transform_exp_labels = function(dat) {
  dat %>%
    mutate(Exp = Hands+AgeCategory) %>%
    return
}
# Function to pull out confidence intervals on parameter estimates from lmers
get_parameter_ci = function(model, paramname) {
  cfs = summary(model)$coefficients
  est = cfs[paramname, 'Estimate']
  err = cfs[paramname, 'Std. Error']
  return(c(est, est-1.96*err, est+1.96*err))
}

#' ## Tools game model setup
#' 
#' For each analysis test type, include:
#' * _null: Just random effects
#' * _nullcov: Covariates on top of random effects (Age X AgeCategory + MotorReactionTime)
#' * _full: Including hands and interaction between age X hands
#' * _nointeract: No interaction between age X hands
#' * _noagecont: Like nointeract, but does not include a continuous age variable
#' 
#' Also, split by kids/adults for follow-up separate tests -- note this uses raw age rather than z-scored for 
#' parameter estimation purposes
#' 
#' For some dependent variables, we also test the effect of hand laterality or device input. For each, there are 
#' three types:
#' * _{laterality,device}_base: like the above models but cutting data with ambiguous records (e.g., ambidextrous or `other` devices)
#' * _{laterality,device}_main: includes the IV in question as a main effect
#' * _{laterality,device}_interact: includes the IV interacting with handedness
#' 
#' Thus all models are named `mod_{DV}_{TestType}_{Modifier}`
#' 
#' ### Accuracy

mod_accuracy_null =
  glmer(data = dat_bytrial, family=binomial,
        SuccessByOnline ~ 1 + (1|WorkerID) + (1|TrialName))

mod_accuracy_nullcov = 
  glmer(data = dat_bytrial, family=binomial,
        SuccessByOnline ~ ZAge*AgeCategory + ZMRT + (1|WorkerID) + (1|TrialName))

mod_accuracy_full =
  glmer(data = dat_bytrial, family=binomial,
        SuccessByOnline ~ Hands*AgeCategory + ZAge*AgeCategory + ZMRT + (1|WorkerID) + (1|TrialName),
        control=glmerControl(optimizer='bobyqa', 
                             optCtrl=list(maxfun=2e6)))

mod_accuracy_nomotor =
  glmer(data = dat_bytrial, family=binomial,
        SuccessByOnline ~ Hands*AgeCategory + ZAge*AgeCategory + (1|WorkerID) + (1|TrialName),
        control=glmerControl(optimizer='bobyqa', 
                             optCtrl=list(maxfun=2e6)))

mod_accuracy_nocov =
  glmer(data = dat_bytrial, family=binomial,
        SuccessByOnline ~ Hands*AgeCategory + (1|WorkerID) + (1|TrialName),
        control=glmerControl(optimizer='bobyqa', 
                             optCtrl=list(maxfun=2e6)))

mod_accuracy_nointeract = 
  glmer(data = dat_bytrial, family=binomial,
        SuccessByOnline ~ Hands + ZAge*AgeCategory + ZMRT + (1|WorkerID) + (1|TrialName),
        control=glmerControl(optimizer='bobyqa', 
                             optCtrl=list(maxfun=2e6)))

mod_accuracy_noagecont = 
  glmer(data = dat_bytrial, family=binomial,
        SuccessByOnline ~ Hands + AgeCategory + ZMRT + (1|WorkerID) + (1|TrialName),
        control=glmerControl(optimizer='bobyqa', 
                             optCtrl=list(maxfun=2e6)))

mod_accuracy_nointeract_adult =
  glmer(data = dat_bytrial %>% filter(AgeCategory=='adult'), family=binomial,
        SuccessByOnline ~ Hands + Age + ZMRT + (1|WorkerID) + (1|TrialName),
        control=glmerControl(optimizer='bobyqa', 
                             optCtrl=list(maxfun=2e6)))

mod_accuracy_nocov_adult =
  glmer(data = dat_bytrial %>% filter(AgeCategory=='adult'), family=binomial,
        SuccessByOnline ~ Hands + (1|WorkerID) + (1|TrialName),
        control=glmerControl(optimizer='bobyqa', 
                             optCtrl=list(maxfun=2e6)))

mod_accuracy_nointeract_kid =
  glmer(data = dat_bytrial %>% filter(AgeCategory=='kids'), family=binomial,
        SuccessByOnline ~ Hands + Age + ZMRT + (1|WorkerID) + (1|TrialName),
        control=glmerControl(optimizer='bobyqa', 
                             optCtrl=list(maxfun=2e6)))

mod_accuracy_nocov_kid =
  glmer(data = dat_bytrial %>% filter(AgeCategory=='kids'), family=binomial,
        SuccessByOnline ~ Hands + (1|WorkerID) + (1|TrialName),
        control=glmerControl(optimizer='bobyqa', 
                             optCtrl=list(maxfun=2e6)))

mod_accuracy_nointeract_laterality_base = 
  glmer(data = dat_bytrial %>% filter(Laterality != "Ambidextrous"), family=binomial,
        SuccessByOnline ~ Hands + ZAge*AgeCategory + ZMRT + (1|WorkerID) + (1|TrialName),
        control=glmerControl(optimizer='bobyqa', 
                             optCtrl=list(maxfun=2e6)))

mod_accuracy_nointeract_laterality_main = 
  glmer(data = dat_bytrial %>% filter(Laterality != "Ambidextrous"), family=binomial,
        SuccessByOnline ~ Laterality + Hands + ZAge*AgeCategory + ZMRT + (1|WorkerID) + (1|TrialName),
        control=glmerControl(optimizer='bobyqa', 
                             optCtrl=list(maxfun=2e6)))

mod_accuracy_nointeract_laterality_interact = 
  glmer(data = dat_bytrial %>% filter(Laterality != "Ambidextrous"), family=binomial,
        SuccessByOnline ~ Laterality * Hands + ZAge*AgeCategory + ZMRT + (1|WorkerID) + (1|TrialName),
        control=glmerControl(optimizer='bobyqa', 
                             optCtrl=list(maxfun=2e6)))

mod_accuracy_nointeract_device_base = 
  glmer(data = dat_bytrial %>% filter(Device != "other"), family=binomial,
        SuccessByOnline ~ Hands + ZAge*AgeCategory + ZMRT + (1|WorkerID) + (1|TrialName),
        control=glmerControl(optimizer='bobyqa', 
                             optCtrl=list(maxfun=2e6)))

mod_accuracy_nointeract_device_main = 
  glmer(data = dat_bytrial %>% filter(Device != "other"), family=binomial,
        SuccessByOnline ~ Device + Hands + ZAge*AgeCategory + ZMRT + (1|WorkerID) + (1|TrialName),
        control=glmerControl(optimizer='bobyqa', 
                             optCtrl=list(maxfun=2e6)))

mod_accuracy_nointeract_device_interact = 
  glmer(data = dat_bytrial %>% filter(Device != "other"), family=binomial,
        SuccessByOnline ~ Device * Hands + ZAge*AgeCategory + ZMRT + (1|WorkerID) + (1|TrialName),
        control=glmerControl(optimizer='bobyqa', 
                             optCtrl=list(maxfun=2e6)))

mod_accuracy_nointeract_gender_base = 
  glmer(data = dat_bytrial %>% filter(!is.na(Gender)), family=binomial,
        SuccessByOnline ~ Device + ZAge*AgeCategory + ZMRT + (1|WorkerID) + (1|TrialName),
        control=glmerControl(optimizer='bobyqa', 
                             optCtrl=list(maxfun=2e6)))

mod_accuracy_nointeract_gender_main = 
  glmer(data = dat_bytrial %>% filter(!is.na(Gender)), family=binomial,
        SuccessByOnline ~ Gender + Hands + ZAge*AgeCategory + ZMRT + (1|WorkerID) + (1|TrialName),
        control=glmerControl(optimizer='bobyqa', 
                             optCtrl=list(maxfun=2e6)))

mod_accuracy_nointeract_gender_interact = 
  glmer(data = dat_bytrial %>% filter(!is.na(Gender)), family=binomial,
        SuccessByOnline ~ Gender * Hands + ZAge*AgeCategory + ZMRT + (1|WorkerID) + (1|TrialName),
        control=glmerControl(optimizer='bobyqa', 
                             optCtrl=list(maxfun=2e6)))


mod_accuracy_prosthesis_base = 
  glmer(data = dat_bytrial %>% filter(!is.na(Prosthesis_Use)), family=binomial,
        SuccessByOnline ~ Prosthesis_Use*AgeCategory + ZAge*AgeCategory + ZMRT + (1|WorkerID) + (1|TrialName),
        control=glmerControl(optimizer='bobyqa', 
                             optCtrl=list(maxfun=2e6)))

mod_accuracy_prosthesis_none = 
  glmer(data = dat_bytrial %>% filter(!is.na(Prosthesis_Use)), family=binomial,
        SuccessByOnline ~ ZAge*AgeCategory + ZMRT + (1|WorkerID) + (1|TrialName),
        control=glmerControl(optimizer='bobyqa', 
                             optCtrl=list(maxfun=2e6)))

mod_accuracy_prosthesis_nointeract = 
  glmer(data = dat_bytrial %>% filter(!is.na(Prosthesis_Use)), family=binomial,
        SuccessByOnline ~ Prosthesis_Use + ZAge*AgeCategory + ZMRT + (1|WorkerID) + (1|TrialName),
        control=glmerControl(optimizer='bobyqa', 
                             optCtrl=list(maxfun=2e6)))



#' ### Placements to success

mod_place_null = 
  glmer(data = dat_bytrial_succ, family=poisson,
        I(NPlacements-1) ~ 1 + (1|WorkerID) + (1|TrialName))

mod_place_nullcov = 
  glmer(data = dat_bytrial_succ, family=poisson,
        I(NPlacements-1) ~ ZAge*AgeCategory + ZMRT + (1|WorkerID) + (1|TrialName))

mod_place_full =
  glmer(data = dat_bytrial_succ, family=poisson,
        I(NPlacements-1) ~ Hands*AgeCategory + ZAge*AgeCategory + ZMRT + (1|WorkerID) + (1|TrialName),
        control=glmerControl(optimizer='bobyqa', 
                             optCtrl=list(maxfun=2e6)))

mod_place_nomotor =
  glmer(data = dat_bytrial_succ, family=poisson,
        I(NPlacements-1) ~ Hands*AgeCategory + ZAge*AgeCategory + (1|WorkerID) + (1|TrialName),
        control=glmerControl(optimizer='bobyqa', 
                             optCtrl=list(maxfun=2e6)))

mod_place_nocov =
  glmer(data = dat_bytrial_succ, family=poisson,
        I(NPlacements-1) ~ Hands*AgeCategory + (1|WorkerID) + (1|TrialName),
        control=glmerControl(optimizer='bobyqa', 
                             optCtrl=list(maxfun=2e6)))

mod_place_nointeract = 
  glmer(data = dat_bytrial_succ, family=poisson,
        I(NPlacements-1) ~ Hands + ZAge*AgeCategory + ZMRT + (1|WorkerID) + (1|TrialName),
        control=glmerControl(optimizer='bobyqa', 
                             optCtrl=list(maxfun=2e6)))

mod_place_noagecont = 
  glmer(data = dat_bytrial_succ, family=poisson,
        I(NPlacements-1) ~ Hands + AgeCategory + ZMRT + (1|WorkerID) + (1|TrialName),
        control=glmerControl(optimizer='bobyqa', 
                             optCtrl=list(maxfun=2e6)))

mod_place_nointeract_adult = 
  glmer(data = dat_bytrial_succ %>% filter(AgeCategory=='adult'), family=poisson,
        I(NPlacements-1) ~ Hands + Age + ZMRT + (1|WorkerID) + (1|TrialName),
        control=glmerControl(optimizer='bobyqa', 
                             optCtrl=list(maxfun=2e6)))

mod_place_nocov_adult = 
  glmer(data = dat_bytrial_succ %>% filter(AgeCategory=='adult'), family=poisson,
        I(NPlacements-1) ~ Hands + (1|WorkerID) + (1|TrialName),
        control=glmerControl(optimizer='bobyqa', 
                             optCtrl=list(maxfun=2e6)))

mod_place_nointeract_kid = 
  glmer(data = dat_bytrial_succ %>% filter(AgeCategory=='kids'), family=poisson,
        I(NPlacements-1) ~ Hands + Age + ZMRT + (1|WorkerID) + (1|TrialName),
        control=glmerControl(optimizer='bobyqa', 
                             optCtrl=list(maxfun=2e6)))

mod_place_nocov_kid = 
  glmer(data = dat_bytrial_succ %>% filter(AgeCategory=='kids'), family=poisson,
        I(NPlacements-1) ~ Hands + (1|WorkerID) + (1|TrialName),
        control=glmerControl(optimizer='bobyqa', 
                             optCtrl=list(maxfun=2e6)))

mod_place_nointeract_laterality_base = 
  glmer(data = dat_bytrial_succ %>% filter(Laterality != "Ambidextrous"), family=poisson,
        I(NPlacements-1) ~ Hands + ZAge*AgeCategory + ZMRT + (1|WorkerID) + (1|TrialName))

mod_place_nointeract_laterality_main = 
  glmer(data = dat_bytrial_succ %>% filter(Laterality != "Ambidextrous"), family=poisson,
        I(NPlacements-1) ~ Laterality + Hands + ZAge*AgeCategory + ZMRT + (1|WorkerID) + (1|TrialName),
        control=glmerControl(optimizer='bobyqa', 
                             optCtrl=list(maxfun=2e6)))

mod_place_nointeract_laterality_interact = 
  glmer(data = dat_bytrial_succ %>% filter(Laterality != "Ambidextrous"), family=poisson,
        I(NPlacements-1) ~ Hands*Laterality + ZAge*AgeCategory + ZMRT + (1|WorkerID) + (1|TrialName),
        control=glmerControl(optimizer='bobyqa', 
                             optCtrl=list(maxfun=2e6)))

mod_place_nointeract_device_base = 
  glmer(data = dat_bytrial_succ %>% filter(Device != "other"), family=poisson,
        I(NPlacements-1) ~ Hands + ZAge*AgeCategory + ZMRT + (1|WorkerID) + (1|TrialName))

mod_place_nointeract_device_main = 
  glmer(data = dat_bytrial_succ %>% filter(Device != "other"), family=poisson,
        I(NPlacements-1) ~ Device + Hands + ZAge*AgeCategory + ZMRT + (1|WorkerID) + (1|TrialName),
        control=glmerControl(optimizer='bobyqa', 
                             optCtrl=list(maxfun=2e6)))

mod_place_nointeract_device_interact = 
  glmer(data = dat_bytrial_succ %>% filter(Device != "other"), family=poisson,
        I(NPlacements-1) ~ Device*Hands + ZAge*AgeCategory + ZMRT + (1|WorkerID) + (1|TrialName),
        control=glmerControl(optimizer='bobyqa', 
                             optCtrl=list(maxfun=2e6)))

mod_place_nointeract_gender_base = 
  glmer(data = dat_bytrial_succ %>% filter(!is.na(Gender)), family=poisson,
        I(NPlacements-1) ~ Hands + ZAge*AgeCategory + ZMRT + (1|WorkerID) + (1|TrialName))

mod_place_nointeract_gender_main = 
  glmer(data = dat_bytrial_succ %>% filter(!is.na(Gender)), family=poisson,
        I(NPlacements-1) ~ Gender + Hands + ZAge*AgeCategory + ZMRT + (1|WorkerID) + (1|TrialName),
        control=glmerControl(optimizer='bobyqa', 
                             optCtrl=list(maxfun=2e6)))

mod_place_nointeract_gender_interact = 
  glmer(data = dat_bytrial_succ %>% filter(!is.na(Gender)), family=poisson,
        I(NPlacements-1) ~ Gender*Hands + ZAge*AgeCategory + ZMRT + (1|WorkerID) + (1|TrialName),
        control=glmerControl(optimizer='bobyqa', 
                             optCtrl=list(maxfun=2e6)))

mod_place_prosthesis_base = 
  glmer(data = dat_bytrial_succ %>% filter(!is.na(Prosthesis_Use)), family=poisson,
        I(NPlacements-1) ~ Prosthesis_Use*AgeCategory + ZAge*AgeCategory + ZMRT + (1|WorkerID) + (1|TrialName),
        control=glmerControl(optimizer='bobyqa', 
                             optCtrl=list(maxfun=2e6)))

mod_place_prosthesis_none = 
  glmer(data = dat_bytrial_succ %>% filter(!is.na(Prosthesis_Use)), family=poisson,
        I(NPlacements-1) ~ ZAge*AgeCategory + ZMRT + (1|WorkerID) + (1|TrialName),
        control=glmerControl(optimizer='bobyqa', 
                             optCtrl=list(maxfun=2e6)))

mod_place_prosthesis_nointeract = 
  glmer(data = dat_bytrial_succ %>% filter(!is.na(Prosthesis_Use)), family=poisson,
        I(NPlacements-1) ~ Prosthesis_Use + ZAge*AgeCategory + ZMRT + (1|WorkerID) + (1|TrialName),
        control=glmerControl(optimizer='bobyqa', 
                             optCtrl=list(maxfun=2e6)))



#' ### Time to solution
#' 

mod_tts_null =
  lmer(data = dat_bytrial_succ,
       LastPlace ~ 1 + (1|WorkerID) + (1|TrialName))

mod_tts_nullcov = 
  lmer(data = dat_bytrial_succ,
       LastPlace ~ ZAge*AgeCategory + ZMRT + (1|WorkerID) + (1|TrialName))

mod_tts_full = 
  lmer(data = dat_bytrial_succ,
       LastPlace ~ Hands*AgeCategory + ZAge*AgeCategory + ZMRT + (1|WorkerID) + (1|TrialName))

mod_tts_nomotor = 
  lmer(data = dat_bytrial_succ,
       LastPlace ~ Hands*AgeCategory + ZAge*AgeCategory + (1|WorkerID) + (1|TrialName))

mod_tts_nocov = 
  lmer(data = dat_bytrial_succ,
       LastPlace ~ Hands*AgeCategory + (1|WorkerID) + (1|TrialName))

mod_tts_nointeract = 
  lmer(data = dat_bytrial_succ,
       LastPlace ~ Hands + ZAge*AgeCategory + ZMRT + (1|WorkerID) + (1|TrialName))

mod_tts_noagecont = 
  lmer(data = dat_bytrial_succ,
       LastPlace ~ Hands + AgeCategory + ZMRT + (1|WorkerID) + (1|TrialName))

mod_tts_nointeract_adult = 
  lmer(data = dat_bytrial_succ %>% filter(AgeCategory=='adult'),
       LastPlace ~ Hands + Age + ZMRT + (1|WorkerID) + (1|TrialName))

mod_tts_nocov_adult = 
  lmer(data = dat_bytrial_succ %>% filter(AgeCategory=='adult'),
       LastPlace ~ Hands + (1|WorkerID) + (1|TrialName))

mod_tts_nointeract_kid = 
  lmer(data = dat_bytrial_succ %>% filter(AgeCategory=='kids'),
       LastPlace ~ Hands + Age + ZMRT + (1|WorkerID) + (1|TrialName))

mod_tts_nocov_kid = 
  lmer(data = dat_bytrial_succ %>% filter(AgeCategory=='kids'),
       LastPlace ~ Hands + (1|WorkerID) + (1|TrialName))

mod_tts_nointeract_laterality_base =
  lmer(data = dat_bytrial_succ %>% filter(Laterality != "Ambidextrous"),
       LastPlace ~ Hands + ZAge*AgeCategory + ZMRT + (1|WorkerID) + (1|TrialName))

mod_tts_nointeract_laterality_main = 
  lmer(data = dat_bytrial_succ %>% filter(Laterality != "Ambidextrous"),
       LastPlace ~ Laterality + Hands + ZAge*AgeCategory + ZMRT + (1|WorkerID) + (1|TrialName))

mod_tts_nointeract_laterality_interact = 
  lmer(data = dat_bytrial_succ %>% filter(Laterality != "Ambidextrous"),
       LastPlace ~ Hands*Laterality + ZAge*AgeCategory + ZMRT + (1|WorkerID) + (1|TrialName))

mod_tts_nointeract_device_base =
  lmer(data = dat_bytrial_succ %>% filter(Device != "other"),
       LastPlace ~ Hands + ZAge*AgeCategory + ZMRT + (1|WorkerID) + (1|TrialName))

mod_tts_nointeract_device_main = 
  lmer(data = dat_bytrial_succ %>% filter(Device != "other"),
       LastPlace ~ Device + Hands + ZAge*AgeCategory + ZMRT + (1|WorkerID) + (1|TrialName))

mod_tts_nointeract_device_interact = 
  lmer(data = dat_bytrial_succ %>% filter(Device != "other"),
       LastPlace ~ Hands*Device + ZAge*AgeCategory + ZMRT + (1|WorkerID) + (1|TrialName))

mod_tts_nointeract_gender_base =
  lmer(data = dat_bytrial_succ %>% filter(!is.na(Gender)),
       LastPlace ~ Hands + ZAge*AgeCategory + ZMRT + (1|WorkerID) + (1|TrialName))

mod_tts_nointeract_gender_main = 
  lmer(data = dat_bytrial_succ %>% filter(!is.na(Gender)),
       LastPlace ~ Gender + Hands + ZAge*AgeCategory + ZMRT + (1|WorkerID) + (1|TrialName))

mod_tts_nointeract_gender_interact = 
  lmer(data = dat_bytrial_succ %>% filter(!is.na(Gender)),
       LastPlace ~ Gender*Hands + ZAge*AgeCategory + ZMRT + (1|WorkerID) + (1|TrialName))


mod_tts_prosthesis_base = 
  lmer(data = dat_bytrial_succ %>% filter(!is.na(Prosthesis_Use)),
        LastPlace ~ Prosthesis_Use*AgeCategory + ZAge*AgeCategory + ZMRT + (1|WorkerID) + (1|TrialName),
        control=lmerControl(optimizer='bobyqa', 
                             optCtrl=list(maxfun=2e6)))

mod_tts_prosthesis_none = 
  lmer(data = dat_bytrial_succ %>% filter(!is.na(Prosthesis_Use)),
        LastPlace ~ ZAge*AgeCategory + ZMRT + (1|WorkerID) + (1|TrialName),
        control=lmerControl(optimizer='bobyqa', 
                             optCtrl=list(maxfun=2e6)))

mod_tts_prosthesis_nointeract = 
  lmer(data = dat_bytrial_succ %>% filter(!is.na(Prosthesis_Use)),
       LastPlace ~ Prosthesis_Use + ZAge*AgeCategory + ZMRT + (1|WorkerID) + (1|TrialName),
       control=lmerControl(optimizer='bobyqa', 
                           optCtrl=list(maxfun=2e6)))


#' ### Time to first placement

mod_ttf_null =
  lmer(data = dat_bytrial_succ,
       FirstPlace ~ 1 + (1|WorkerID) + (1|TrialName))

mod_ttf_nullcov = 
  lmer(data = dat_bytrial_succ,
       FirstPlace ~ ZAge*AgeCategory + ZMRT + (1|WorkerID) + (1|TrialName))

mod_ttf_full = 
  lmer(data = dat_bytrial_succ,
       FirstPlace ~ Hands*AgeCategory + ZAge*AgeCategory + ZMRT + (1|WorkerID) + (1|TrialName))

mod_ttf_nomotor = 
  lmer(data = dat_bytrial_succ,
       FirstPlace ~ Hands*AgeCategory + ZAge*AgeCategory + (1|WorkerID) + (1|TrialName))

mod_ttf_nocov = 
  lmer(data = dat_bytrial_succ,
       FirstPlace ~ Hands*AgeCategory + (1|WorkerID) + (1|TrialName))

mod_ttf_nointeract = 
  lmer(data = dat_bytrial_succ,
       FirstPlace ~ Hands + ZAge*AgeCategory + ZMRT + (1|WorkerID) + (1|TrialName))

mod_ttf_noagecont = 
  lmer(data = dat_bytrial_succ,
       FirstPlace ~ Hands + AgeCategory + ZMRT + (1|WorkerID) + (1|TrialName))

mod_ttf_nointeract_adult = 
  lmer(data = dat_bytrial_succ %>% filter(AgeCategory=='adult'),
       FirstPlace ~ Hands + Age + ZMRT + (1|WorkerID) + (1|TrialName))

mod_ttf_nocov_adult = 
  lmer(data = dat_bytrial_succ %>% filter(AgeCategory=='adult'),
       FirstPlace ~ Hands + (1|WorkerID) + (1|TrialName))

mod_ttf_nointeract_kid = 
  lmer(data = dat_bytrial_succ %>% filter(AgeCategory=='kids'),
       FirstPlace ~ Hands + Age + ZMRT + (1|WorkerID) + (1|TrialName))

mod_ttf_nocov_kid = 
  lmer(data = dat_bytrial_succ %>% filter(AgeCategory=='kids'),
       FirstPlace ~ Hands + (1|WorkerID) + (1|TrialName))

mod_ttf_nointeract_laterality_base =
  lmer(data = dat_bytrial_succ %>% filter(Laterality != "Ambidextrous"),
       FirstPlace ~ Hands + ZAge*AgeCategory + ZMRT + (1|WorkerID) + (1|TrialName))

mod_ttf_nointeract_laterality_main =
  lmer(data = dat_bytrial_succ %>% filter(Laterality != "Ambidextrous"),
       FirstPlace ~ Laterality + Hands + ZAge*AgeCategory + ZMRT + (1|WorkerID) + (1|TrialName))

mod_ttf_nointeract_laterality_interact =
  lmer(data = dat_bytrial_succ %>% filter(Laterality != "Ambidextrous"),
       FirstPlace ~ Laterality*Hands + ZAge*AgeCategory + ZMRT + (1|WorkerID) + (1|TrialName))

mod_ttf_nointeract_device_base =
  lmer(data = dat_bytrial_succ %>% filter(Device != "other"),
       FirstPlace ~ Hands + ZAge*AgeCategory + ZMRT + (1|WorkerID) + (1|TrialName))

mod_ttf_nointeract_device_main =
  lmer(data = dat_bytrial_succ %>% filter(Device != "other"),
       FirstPlace ~ Device + Hands + ZAge*AgeCategory + ZMRT + (1|WorkerID) + (1|TrialName))

mod_ttf_nointeract_device_interact =
  lmer(data = dat_bytrial_succ %>% filter(Device != "other"),
       FirstPlace ~ Device*Hands + ZAge*AgeCategory + ZMRT + (1|WorkerID) + (1|TrialName))

mod_ttf_nointeract_gender_base =
  lmer(data = dat_bytrial_succ %>% filter(!is.na(Gender)),
       FirstPlace ~ Hands + ZAge*AgeCategory + ZMRT + (1|WorkerID) + (1|TrialName))

mod_ttf_nointeract_gender_main =
  lmer(data = dat_bytrial_succ %>% filter(!is.na(Gender)),
       FirstPlace ~ Gender + Hands + ZAge*AgeCategory + ZMRT + (1|WorkerID) + (1|TrialName))

mod_ttf_nointeract_gender_interact =
  lmer(data = dat_bytrial_succ %>% filter(!is.na(Gender)),
       FirstPlace ~ Gender*Hands + ZAge*AgeCategory + ZMRT + (1|WorkerID) + (1|TrialName))


mod_ttf_prosthesis_base = 
  lmer(data = dat_bytrial_succ %>% filter(!is.na(Prosthesis_Use)),
       FirstPlace ~ Prosthesis_Use*AgeCategory + ZAge*AgeCategory + ZMRT + (1|WorkerID) + (1|TrialName),
       control=lmerControl(optimizer='bobyqa', 
                           optCtrl=list(maxfun=2e6)))

mod_ttf_prosthesis_none = 
  lmer(data = dat_bytrial_succ %>% filter(!is.na(Prosthesis_Use)),
       FirstPlace ~ ZAge*AgeCategory + ZMRT + (1|WorkerID) + (1|TrialName),
       control=lmerControl(optimizer='bobyqa', 
                           optCtrl=list(maxfun=2e6)))

mod_ttf_prosthesis_nointeract = 
  lmer(data = dat_bytrial_succ %>% filter(!is.na(Prosthesis_Use)),
       FirstPlace ~ Prosthesis_Use + ZAge*AgeCategory + ZMRT + (1|WorkerID) + (1|TrialName),
       control=lmerControl(optimizer='bobyqa', 
                           optCtrl=list(maxfun=2e6)))


#' ### Time between subsequent placements
#' 

mod_tbtwn_null =
  lmer(data = dat_bytrial_succ %>% filter(!is.na(AvgTimeDiff)),
       AvgTimeDiff ~ 1 + (1|WorkerID) + (1|TrialName))

mod_tbtwn_nullcov = 
  lmer(data = dat_bytrial_succ %>% filter(!is.na(AvgTimeDiff)),
       AvgTimeDiff ~ ZAge*AgeCategory + ZMRT + (1|WorkerID) + (1|TrialName))

mod_tbtwn_full = 
  lmer(data = dat_bytrial_succ %>% filter(!is.na(AvgTimeDiff)),
       AvgTimeDiff ~ Hands*AgeCategory + ZAge*AgeCategory + ZMRT + (1|WorkerID) + (1|TrialName))

mod_tbtwn_nomotor = 
  lmer(data = dat_bytrial_succ %>% filter(!is.na(AvgTimeDiff)),
       AvgTimeDiff ~ Hands*AgeCategory + ZAge*AgeCategory + (1|WorkerID) + (1|TrialName))

mod_tbtwn_nocov = 
  lmer(data = dat_bytrial_succ %>% filter(!is.na(AvgTimeDiff)),
       AvgTimeDiff ~ Hands*AgeCategory + (1|WorkerID) + (1|TrialName))

mod_tbtwn_nointeract = 
  lmer(data = dat_bytrial_succ %>% filter(!is.na(AvgTimeDiff)),
       AvgTimeDiff ~ Hands + ZAge*AgeCategory + ZMRT + (1|WorkerID) + (1|TrialName))

mod_tbtwn_noagecont = 
  lmer(data = dat_bytrial_succ %>% filter(!is.na(AvgTimeDiff)),
       AvgTimeDiff ~ Hands + AgeCategory + ZMRT + (1|WorkerID) + (1|TrialName))

mod_tbtwn_nointeract_adult = 
  lmer(data = dat_bytrial_succ %>% filter(!is.na(AvgTimeDiff), AgeCategory=='adult'),
       AvgTimeDiff ~ Hands + Age + ZMRT + (1|WorkerID) + (1|TrialName))

mod_tbtwn_nocov_adult = 
  lmer(data = dat_bytrial_succ %>% filter(!is.na(AvgTimeDiff), AgeCategory=='adult'),
       AvgTimeDiff ~ Hands + (1|WorkerID) + (1|TrialName))

mod_tbtwn_nointeract_kid = 
  lmer(data = dat_bytrial_succ %>% filter(!is.na(AvgTimeDiff), AgeCategory=='kids'),
       AvgTimeDiff ~ Hands + Age + ZMRT + (1|WorkerID) + (1|TrialName))

mod_tbtwn_nocov_kid = 
  lmer(data = dat_bytrial_succ %>% filter(!is.na(AvgTimeDiff), AgeCategory=='kids'),
       AvgTimeDiff ~ Hands + (1|WorkerID) + (1|TrialName))

mod_tbtwn_nointeract_laterality_base = 
  lmer(data = dat_bytrial_succ %>% filter(!is.na(AvgTimeDiff), Laterality != "Ambidextrous"),
       AvgTimeDiff ~ Hands + ZAge*AgeCategory + ZMRT + (1|WorkerID) + (1|TrialName))

mod_tbtwn_nointeract_laterality_main = 
  lmer(data = dat_bytrial_succ %>% filter(!is.na(AvgTimeDiff), Laterality != "Ambidextrous"),
       AvgTimeDiff ~ Laterality + Hands + ZAge*AgeCategory + ZMRT + (1|WorkerID) + (1|TrialName))

mod_tbtwn_nointeract_laterality_interact = 
  lmer(data = dat_bytrial_succ %>% filter(!is.na(AvgTimeDiff), Laterality != "Ambidextrous"),
       AvgTimeDiff ~ Laterality*Hands + ZAge*AgeCategory + ZMRT + (1|WorkerID) + (1|TrialName))

mod_tbtwn_nointeract_device_base = 
  lmer(data = dat_bytrial_succ %>% filter(!is.na(AvgTimeDiff), Device != "other"),
       AvgTimeDiff ~ Hands + ZAge*AgeCategory + ZMRT + (1|WorkerID) + (1|TrialName))

mod_tbtwn_nointeract_device_main = 
  lmer(data = dat_bytrial_succ %>% filter(!is.na(AvgTimeDiff), Device != "other"),
       AvgTimeDiff ~ Device + Hands + ZAge*AgeCategory + ZMRT + (1|WorkerID) + (1|TrialName))

mod_tbtwn_nointeract_device_interact = 
  lmer(data = dat_bytrial_succ %>% filter(!is.na(AvgTimeDiff), Device != "other"),
       AvgTimeDiff ~ Device*Hands + ZAge*AgeCategory + ZMRT + (1|WorkerID) + (1|TrialName))

mod_tbtwn_nointeract_gender_base = 
  lmer(data = dat_bytrial_succ %>% filter(!is.na(AvgTimeDiff), !is.na(Gender)),
       AvgTimeDiff ~ Hands + ZAge*AgeCategory + ZMRT + (1|WorkerID) + (1|TrialName))

mod_tbtwn_nointeract_gender_main = 
  lmer(data = dat_bytrial_succ %>% filter(!is.na(AvgTimeDiff), !is.na(Gender)),
       AvgTimeDiff ~ Gender + Hands + ZAge*AgeCategory + ZMRT + (1|WorkerID) + (1|TrialName))

mod_tbtwn_nointeract_gender_interact = 
  lmer(data = dat_bytrial_succ %>% filter(!is.na(AvgTimeDiff), !is.na(Gender)),
       AvgTimeDiff ~ Gender*Hands + ZAge*AgeCategory + ZMRT + (1|WorkerID) + (1|TrialName))


mod_tbtwn_prosthesis_base = 
  lmer(data = dat_bytrial_succ %>% filter(!is.na(AvgTimeDiff), !is.na(Prosthesis_Use)),
       AvgTimeDiff ~ Prosthesis_Use*AgeCategory + ZAge*AgeCategory + ZMRT + (1|WorkerID) + (1|TrialName),
       control=lmerControl(optimizer='bobyqa', 
                           optCtrl=list(maxfun=2e6)))

mod_tbtwn_prosthesis_none = 
  lmer(data = dat_bytrial_succ %>% filter(!is.na(AvgTimeDiff), !is.na(Prosthesis_Use)),
       AvgTimeDiff ~ ZAge*AgeCategory + ZMRT + (1|WorkerID) + (1|TrialName),
       control=lmerControl(optimizer='bobyqa', 
                           optCtrl=list(maxfun=2e6)))

mod_tbtwn_prosthesis_nointeract = 
  lmer(data = dat_bytrial_succ %>% filter(!is.na(AvgTimeDiff), !is.na(Prosthesis_Use)),
       AvgTimeDiff ~ Prosthesis_Use + ZAge*AgeCategory + ZMRT + (1|WorkerID) + (1|TrialName),
       control=lmerControl(optimizer='bobyqa', 
                           optCtrl=list(maxfun=2e6)))



#' ### Average Y offset from dynamic object

mod_avgy_null =
  lmer(data = dat_bytrial_succ,
       AvgAbsYOffset ~ 1 + (1|WorkerID) + (1|TrialName))

mod_avgy_nullcov = 
  lmer(data = dat_bytrial_succ,
       AvgAbsYOffset ~ ZAge*AgeCategory + ZMRT + (1|WorkerID) + (1|TrialName))

mod_avgy_full = 
  lmer(data = dat_bytrial_succ,
       AvgAbsYOffset ~ Hands*AgeCategory + ZAge*AgeCategory + ZMRT + (1|WorkerID) + (1|TrialName))

mod_avgy_nointeract = 
  lmer(data = dat_bytrial_succ,
       AvgAbsYOffset ~ Hands + ZAge*AgeCategory + ZMRT + (1|WorkerID) + (1|TrialName))

mod_avgy_noagecont = 
  lmer(data = dat_bytrial_succ,
       AvgAbsYOffset ~ Hands + AgeCategory + ZMRT + (1|WorkerID) + (1|TrialName))

mod_avgy_nointeract_adult = 
  lmer(data = dat_bytrial_succ %>% filter(AgeCategory=='adult'),
       AvgAbsYOffset ~ Hands + Age + ZMRT + (1|WorkerID) + (1|TrialName))

mod_avgy_nointeract_kid = 
  lmer(data = dat_bytrial_succ %>% filter(AgeCategory=='kids'),
       AvgAbsYOffset ~ Hands + Age + ZMRT + (1|WorkerID) + (1|TrialName))

mod_avgy_nointeract_laterality_base =
  lmer(data = dat_bytrial_succ %>% filter(Laterality != "Ambidextrous"),
       AvgAbsYOffset ~ Hands + ZAge*AgeCategory + ZMRT + (1|WorkerID) + (1|TrialName))

mod_avgy_nointeract_laterality_main =
  lmer(data = dat_bytrial_succ %>% filter(Laterality != "Ambidextrous"),
       AvgAbsYOffset ~ Laterality + Hands + ZAge*AgeCategory + ZMRT + (1|WorkerID) + (1|TrialName))

mod_avgy_nointeract_laterality_interact =
  lmer(data = dat_bytrial_succ %>% filter(Laterality != "Ambidextrous"),
       AvgAbsYOffset ~ Laterality*Hands + ZAge*AgeCategory + ZMRT + (1|WorkerID) + (1|TrialName))

mod_avgy_nointeract_device_base =
  lmer(data = dat_bytrial_succ %>% filter(Device != "other"),
       AvgAbsYOffset ~ Hands + ZAge*AgeCategory + ZMRT + (1|WorkerID) + (1|TrialName))

mod_avgy_nointeract_device_main =
  lmer(data = dat_bytrial_succ %>% filter(Device != "other"),
       AvgAbsYOffset ~ Device + Hands + ZAge*AgeCategory + ZMRT + (1|WorkerID) + (1|TrialName))

mod_avgy_nointeract_device_interact =
  lmer(data = dat_bytrial_succ %>% filter(Device != "other"),
       AvgAbsYOffset ~ Device*Hands + ZAge*AgeCategory + ZMRT + (1|WorkerID) + (1|TrialName))

mod_avgy_nointeract_gender_base =
  lmer(data = dat_bytrial_succ %>% filter(!is.na(Gender)),
       AvgAbsYOffset ~ Hands + ZAge*AgeCategory + ZMRT + (1|WorkerID) + (1|TrialName))

mod_avgy_nointeract_gender_main =
  lmer(data = dat_bytrial_succ %>% filter(!is.na(Gender)),
       AvgAbsYOffset ~ Gender + Hands + ZAge*AgeCategory + ZMRT + (1|WorkerID) + (1|TrialName))

mod_avgy_nointeract_gender_interact =
  lmer(data = dat_bytrial_succ %>% filter(!is.na(Gender)),
       AvgAbsYOffset ~ Gender*Hands + ZAge*AgeCategory + ZMRT + (1|WorkerID) + (1|TrialName))

# Further details: does this differ by level?
mod_avgy_level_diff = 
  lmer(data = dat_bytrial_succ,
       AvgAbsYOffset ~ Hands*AgeCategory + ZAge*AgeCategory + ZMRT + (1|WorkerID) + TrialName*AgeCategory)


#' ### Average X offset from dynamic object

mod_avgx_null =
  lmer(data = dat_bytrial_succ,
       AvgAbsXOffset ~ 1 + (1|WorkerID) + (1|TrialName))

mod_avgx_nullcov = 
  lmer(data = dat_bytrial_succ,
       AvgAbsXOffset ~ ZAge*AgeCategory + ZMRT + (1|WorkerID) + (1|TrialName))

mod_avgx_full = 
  lmer(data = dat_bytrial_succ,
       AvgAbsXOffset ~ Hands*AgeCategory + ZAge*AgeCategory + ZMRT + (1|WorkerID) + (1|TrialName))

mod_avgx_nointeract = 
  lmer(data = dat_bytrial_succ,
       AvgAbsXOffset ~ Hands + ZAge*AgeCategory + ZMRT + (1|WorkerID) + (1|TrialName))

mod_avgx_noagecont = 
  lmer(data = dat_bytrial_succ,
       AvgAbsXOffset ~ Hands + AgeCategory + ZMRT + (1|WorkerID) + (1|TrialName))

mod_avgx_nointeract_adult = 
  lmer(data = dat_bytrial_succ %>% filter(AgeCategory=='adult'),
       AvgAbsXOffset ~ Hands + Age + ZMRT + (1|WorkerID) + (1|TrialName))

mod_avgx_nointeract_kid = 
  lmer(data = dat_bytrial_succ %>% filter(AgeCategory=='kids'),
       AvgAbsXOffset ~ Hands + Age + ZMRT + (1|WorkerID) + (1|TrialName))

mod_avgx_nointeract_laterality_base =
  lmer(data = dat_bytrial_succ %>% filter(Laterality != "Ambidextrous"),
       AvgAbsXOffset ~ Hands + ZAge*AgeCategory + ZMRT + (1|WorkerID) + (1|TrialName))

mod_avgx_nointeract_laterality_main =
  lmer(data = dat_bytrial_succ %>% filter(Laterality != "Ambidextrous"),
       AvgAbsXOffset ~ Laterality + Hands + ZAge*AgeCategory + ZMRT + (1|WorkerID) + (1|TrialName))

mod_avgx_nointeract_laterality_interact =
  lmer(data = dat_bytrial_succ %>% filter(Laterality != "Ambidextrous"),
       AvgAbsXOffset ~ Laterality*Hands + ZAge*AgeCategory + ZMRT + (1|WorkerID) + (1|TrialName))

mod_avgx_nointeract_device_base =
  lmer(data = dat_bytrial_succ %>% filter(Device != "other"),
       AvgAbsXOffset ~ Hands + ZAge*AgeCategory + ZMRT + (1|WorkerID) + (1|TrialName))

mod_avgx_nointeract_device_main =
  lmer(data = dat_bytrial_succ %>% filter(Device != "other"),
       AvgAbsXOffset ~ Device + Hands + ZAge*AgeCategory + ZMRT + (1|WorkerID) + (1|TrialName))

mod_avgx_nointeract_device_interact =
  lmer(data = dat_bytrial_succ %>% filter(Device != "other"),
       AvgAbsXOffset ~ Device*Hands + ZAge*AgeCategory + ZMRT + (1|WorkerID) + (1|TrialName))

mod_avgx_nointeract_gender_base =
  lmer(data = dat_bytrial_succ %>% filter(!is.na(Gender)),
       AvgAbsXOffset ~ Hands + ZAge*AgeCategory + ZMRT + (1|WorkerID) + (1|TrialName))

mod_avgx_nointeract_gender_main =
  lmer(data = dat_bytrial_succ %>% filter(!is.na(Gender)),
       AvgAbsXOffset ~ Gender + Hands + ZAge*AgeCategory + ZMRT + (1|WorkerID) + (1|TrialName))

mod_avgx_nointeract_gender_interact =
  lmer(data = dat_bytrial_succ %>% filter(!is.na(Gender)),
       AvgAbsXOffset ~ Gender*Hands + ZAge*AgeCategory + ZMRT + (1|WorkerID) + (1|TrialName))


#' ### Proportion of "transtiions" between clusters

mod_transition_null =
  lmer(data = dat_bytrial_succ,
       Transition ~ 1 + (1|WorkerID) + (1|TrialName))

mod_transition_nullcov = 
  lmer(data = dat_bytrial_succ,
       Transition ~ ZAge*AgeCategory + ZMRT + (1|WorkerID) + (1|TrialName))

mod_transition_full = 
  lmer(data = dat_bytrial_succ,
       Transition ~ Hands*AgeCategory + ZAge*AgeCategory + ZMRT + (1|WorkerID) + (1|TrialName))

mod_transition_nointeract = 
  lmer(data = dat_bytrial_succ,
       Transition ~ Hands + ZAge*AgeCategory + ZMRT + (1|WorkerID) + (1|TrialName))

mod_transition_noagecont = 
  lmer(data = dat_bytrial_succ,
       Transition ~ Hands + AgeCategory + ZMRT + (1|WorkerID) + (1|TrialName))

mod_transition_nointeract_adult = 
  lmer(data = dat_bytrial_succ %>% filter(AgeCategory=='adult'),
       Transition ~ Hands + Age + ZMRT + (1|WorkerID) + (1|TrialName))

mod_transition_nointeract_kid = 
  lmer(data = dat_bytrial_succ %>% filter(AgeCategory=='kids'),
       Transition ~ Hands + Age + ZMRT + (1|WorkerID) + (1|TrialName))

mod_transition_nointeract_laterality_base =
  lmer(data = dat_bytrial_succ %>% filter(Laterality != "Ambidextrous"),
       Transition ~ Hands + ZAge*AgeCategory + ZMRT + (1|WorkerID) + (1|TrialName))

mod_transition_nointeract_laterality_main =
  lmer(data = dat_bytrial_succ %>% filter(Laterality != "Ambidextrous"),
       Transition ~ Laterality + Hands + ZAge*AgeCategory + ZMRT + (1|WorkerID) + (1|TrialName))

mod_transition_nointeract_laterality_interact =
  lmer(data = dat_bytrial_succ %>% filter(Laterality != "Ambidextrous"),
       Transition ~ Laterality*Hands + ZAge*AgeCategory + ZMRT + (1|WorkerID) + (1|TrialName))

mod_transition_nointeract_device_base =
  lmer(data = dat_bytrial_succ %>% filter(Device != "other"),
       Transition ~ Hands + ZAge*AgeCategory + ZMRT + (1|WorkerID) + (1|TrialName))

mod_transition_nointeract_device_main =
  lmer(data = dat_bytrial_succ %>% filter(Device != "other"),
       Transition ~ Device + Hands + ZAge*AgeCategory + ZMRT + (1|WorkerID) + (1|TrialName))

mod_transition_nointeract_device_interact =
  lmer(data = dat_bytrial_succ %>% filter(Device != "other"),
       Transition ~ Device*Hands + ZAge*AgeCategory + ZMRT + (1|WorkerID) + (1|TrialName))

mod_transition_nointeract_gender_base =
  lmer(data = dat_bytrial_succ %>% filter(!is.na(Gender)),
       Transition ~ Hands + ZAge*AgeCategory + ZMRT + (1|WorkerID) + (1|TrialName))

mod_transition_nointeract_gender_main =
  lmer(data = dat_bytrial_succ %>% filter(!is.na(Gender)),
       Transition ~ Gender + Hands + ZAge*AgeCategory + ZMRT + (1|WorkerID) + (1|TrialName))

mod_transition_nointeract_gender_interact =
  lmer(data = dat_bytrial_succ %>% filter(!is.na(Gender)),
       Transition ~ Gender*Hands + ZAge*AgeCategory + ZMRT + (1|WorkerID) + (1|TrialName))

# For exploratory x/y interaction requested by Tamar

dat_xy = dat_bytrial_succ %>% 
  rename(XOff = AvgAbsXOffset, YOff = AvgAbsYOffset) %>% 
  gather(Direction, Offset, XOff, YOff, -WorkerID, -TrialName, -AgeCategory, -Hands, -ZAge, -ZMRT)

mod_dir_interaction = 
  lmer(data = dat_xy,
       Offset ~ Direction*(Hands + ZAge*AgeCategory) + ZMRT + (1|WorkerID) + (1|TrialName))
mod_dir_nointeract = 
  lmer(data = dat_xy,
       Offset ~ Direction+(Hands + ZAge*AgeCategory) + ZMRT + (1|WorkerID) + (1|TrialName))
anova(mod_dir_nointeract, mod_dir_interaction)
car::leveneTest(residuals(mod_dir_interaction), dat_xy$Direction)

#' ## Motor test setup
#' 
#' Similar to the games analysis, we compare null models, models with no hand X age interaction, and full models for 
#' both median RT and pixel error on the motor test

mod_motor_rt_null =
  lm(data = dat_bysubj, MedianMotorRT ~ AgeCategory*Age)

mod_motor_rt_nointeract = 
  lm(data = dat_bysubj, MedianMotorRT ~ Hands + AgeCategory*Age)

mod_motor_rt_full = 
  lm(data = dat_bysubj, MedianMotorRT ~ Hands*AgeCategory + AgeCategory*Age)

mod_motor_err_null =
  lm(data = dat_bysubj, MedianMotorErr ~ AgeCategory*Age)

mod_motor_err_nointeract = 
  lm(data = dat_bysubj, MedianMotorErr ~ Hands + AgeCategory*Age)

mod_motor_err_full = 
  lm(data = dat_bysubj, MedianMotorErr ~ Hands*AgeCategory + AgeCategory*Age)

mod_motor_err_kids = 
  lm(data = dat_bysubj %>% filter(AgeCategory == 'kids'), MedianMotorErr ~ Hands + Age)


#' ## Categorization model set-up
dat_adult_comp = read.csv("data/first_comparisons_1H_2H_adults.csv") %>% 
  mutate(Group = factor(ifelse(Emp == 'emp1', 'DL', 'TL'))) %>% 
  select( -Emp) %>% 
  group_by(TrialName, Participant, Group) %>% 
  summarize(Probability = mean(Probability)) %>% 
  ungroup %>% 
  mutate(Participant = factor(paste(as.character(Group), as.character(Participant), sep='_'))) %>% 
  filter(TrialName != "Spiky")

dat_agg_adult_comp = dat_adult_comp %>% 
  group_by(TrialName) %>% 
  summarize(AvgClass = mean(Probability),
            LowClass = quantile(Probability, .025),
            HighClass = quantile(Probability, .975))

dat_kid_comp = read.csv("data/first_comparisons_1H_2H_kids.csv") %>% 
  mutate(Group = factor(ifelse(Emp == 'emp1', 'DL', 'TL'))) %>% 
  select( -Emp) %>% 
  group_by(TrialName, Participant, Group) %>% 
  summarize(Probability = mean(Probability)) %>% 
  ungroup %>% 
  mutate(Participant = factor(paste(as.character(Group), as.character(Participant), sep='_'))) %>% 
  filter(TrialName != "Spiky")

dat_agg_kid_comp = dat_kid_comp %>% 
  group_by(TrialName) %>% 
  summarize(AvgClass = mean(Probability),
            LowClass = quantile(Probability, .025),
            HighClass = quantile(Probability, .975))

dat_tl_comp = read.csv('data/first_comparisons_2H.csv') %>% 
  mutate(Group = factor(ifelse(Emp == 'emp1', 'Adult', 'Child'))) %>% 
  select( -Emp) %>% 
  group_by(TrialName, Participant, Group) %>% 
  summarize(Probability = mean(Probability)) %>% 
  ungroup %>% 
  mutate(Participant = factor(paste(as.character(Group), as.character(Participant), sep='_'))) %>% 
  filter(TrialName != "Spiky")

dat_agg_tl_comp = dat_tl_comp %>% 
  group_by(TrialName) %>% 
  summarize(AvgClass = mean(Probability),
            LowClass = quantile(Probability, .025),
            HighClass = quantile(Probability, .975))

dat_dl_comp = read.csv('data/first_comparisons_1H.csv') %>% 
  mutate(Group = factor(ifelse(Emp == 'emp1', 'Adult', 'Child'))) %>% 
  select( -Emp) %>% 
  group_by(TrialName, Participant, Group) %>% 
  summarize(Probability = mean(Probability)) %>% 
  ungroup %>% 
  mutate(Participant = factor(paste(as.character(Group), as.character(Participant), sep='_'))) %>% 
  filter(TrialName != "Spiky")

dat_agg_dl_comp = dat_dl_comp %>% 
  group_by(TrialName) %>% 
  summarize(AvgClass = mean(Probability),
            LowClass = quantile(Probability, .025),
            HighClass = quantile(Probability, .975))


dat_1hkidsvall_comp = read.csv('data/first_comparisons_kids_eitheror.csv') %>% 
  group_by(TrialName, Participant) %>% 
  summarize(Probability = mean(Probability)) %>% 
  ungroup %>% 
  mutate(Participant = factor(Participant))

dat_agg_1hkidsvall_comp = dat_1hkidsvall_comp %>% 
  group_by(TrialName) %>% 
  summarize(AvgP2hK = mean(Probability),
            LowP2hK = quantile(Probability, .025),
            HighP2hK = quantile(Probability, .975))

#' Function to get the fitted confidence interval for a particular trial out of a model comparison (controlling for individual variability)
get_comp_cis = function(tname, d) {
  d$TrialName = relevel(d$TrialName, tname)
  #mod = lmer(data=d, Probability ~ TrialName + (1|WorkerID))
  mod = lm(data=d, Probability ~ TrialName)
  #fe = fixef(mod)['(Intercept)']
  fe = coef(mod)['(Intercept)']
  ci = confint(mod)["(Intercept)",]
  return(c(fe, ci))
}

#' Function to apply above function to all trials
get_all_comp_cis = function(d) {
  cis = NULL
  for (tnm in unique(d$TrialName)) {
    cis = rbind(cis, c("TrialName" = tnm, get_comp_cis(tnm, d)))
  }
  cis = data.frame(cis)
  names(cis) = c("TrialName", "AvgClass", "LowClass", "HighClass")
  cis$AvgClass = as.numeric(as.character(cis$AvgClass))
  cis$LowClass = as.numeric(as.character(cis$LowClass))
  cis$HighClass = as.numeric(as.character(cis$HighClass))
  return(cis)
}

dat_adult_comp_cis = get_all_comp_cis(dat_adult_comp)
dat_kids_comp_cis = get_all_comp_cis(dat_kid_comp)
dat_1hkidsvall_comp_cis = get_all_comp_cis(dat_1hkidsvall_comp)
dat_tl_comp_cis = get_all_comp_cis(dat_tl_comp)
dat_dl_comp_cis = get_all_comp_cis(dat_dl_comp)

dcc_compares = dat_adult_comp_cis %>% select(TrialName, Adult=AvgClass) %>%
  merge(dat_kids_comp_cis %>% select(TrialName, Kid=AvgClass))
ggplot(dcc_compares, aes(x=Adult, y=Kid)) + geom_point() + theme_bw()

#' Build the models -- subtract .5 from probability to test intercept==0 same as chance
mod_comp_adult = lmer(data = dat_adult_comp, I(Probability - 0.5) ~ TrialName + (1|Participant),
                      contrasts = list(TrialName = contr.sum(unique(dat_adult_comp$TrialName))))
mod_comp_kids = lmer(data = dat_kid_comp, I(Probability - 0.5) ~ TrialName + (1|Participant),
                      contrasts = list(TrialName = contr.sum(unique(dat_adult_comp$TrialName))))

mod_comp_1hkidsvall = lmer(data = dat_1hkidsvall_comp, I(Probability - 0.5) ~ TrialName + (1|Participant),
                      contrasts = list(TrialName = contr.sum(unique(dat_adult_comp$TrialName))))

mod_comp_tl = lmer(data = dat_tl_comp, I(Probability - 0.5) ~ TrialName + (1|Participant),
                     contrasts = list(TrialName = contr.sum(unique(dat_adult_comp$TrialName))))
mod_comp_dl = lmer(data = dat_dl_comp, I(Probability - 0.5) ~ TrialName + (1|Participant),
                   contrasts = list(TrialName = contr.sum(unique(dat_adult_comp$TrialName))))

######################
#+ Main paper results ----------------------

#' # Main paper results
#' 
#' ## Group characteristics
#' 
#' Summaries of motor errors and RTs across adult/kid participants

dat_bysubj %>% group_by(AgeCategory) %>% 
  summarize(AvgMotRT = mean(MedianMotorRT), SDMotRT = sd(MedianMotorRT),
            AvgMotErr = mean(MedianMotorErr), SDMotErr = sd(MedianMotorErr),
            N = length(MedianMotorErr)) %>% 
  mutate(SEMotRT = SDMotRT / sqrt(N), SEMotErr = SDMotErr / sqrt(N)) %>% 
  mutate(RT95Low = AvgMotRT - 1.96*SEMotRT, RT95High = AvgMotRT + 1.96*SEMotRT,
         Err95Low = AvgMotErr - 1.96*SEMotErr, Err95High = AvgMotErr + 1.96*SEMotErr)

#' Comparison of 1h/2h RTs -- statistical test
#' 
anova(mod_motor_rt_null, mod_motor_rt_nointeract)

#' Comparison of 1h/2h RTs -- parameters
get_parameter_ci(mod_motor_rt_nointeract, 'Hands1')

#' Comparison of 1h/2h errors -- statistical test
anova(mod_motor_err_null, mod_motor_err_nointeract)

#' Comparison of 1h/2h errors -- parameters
get_parameter_ci(mod_motor_err_nointeract, 'Hands1')

#' Test for RT interaction
anova(mod_motor_rt_nointeract, mod_motor_rt_full)

#' Test for error interaction
anova(mod_motor_err_nointeract, mod_motor_err_full)

#' Test for kids performance improving with age
Anova(mod_motor_err_kids)
summary(mod_motor_err_kids)

#' ## Contributions to overall performance

#' ### Paragraph 1: accuracy
#'
#' Overall accuracy by age category
dat_bytrial %>% group_by(AgeCategory) %>% summarize(AvgAcc = mean(SuccessByOnline))

#' For tests, a Type-2 ANOVA can pick out the overall age differences, hand differences, and hand X age interaction
Anova(mod_accuracy_full, type=2)

#' However, for testing continuous age, we need to consider both child/adult slopes
anova(mod_accuracy_noagecont, mod_accuracy_nointeract)

#' Parameter estimates from the model for kids + adults
get_parameter_ci(mod_accuracy_nointeract_kid, 'Age')
get_parameter_ci(mod_accuracy_nointeract_adult, 'Age')

#' ### Paragraph 2: time to solve
#' 
#' For tests, use Type-2 ANOVA for main effects + interaction
Anova(mod_tts_full, type=2)

#' For continuous age, consider both slopes
anova(mod_tts_noagecont, mod_tts_nointeract)

#' Parameter estimates for the modeled kids + adults + tests
get_parameter_ci(mod_tts_nointeract_adult, 'Age')
Anova(mod_tts_nointeract_adult, type=2)

get_parameter_ci(mod_tts_nointeract_kid, 'Age')
Anova(mod_tts_nointeract_kid, type=2)

#' ## Differences in embodiment
#' 
#' ### Paragraph 1: Number of actions, time between placements
#' 
#' Comparing number of actions
Anova(mod_place_nointeract, type=2)
1/exp(get_parameter_ci(mod_place_nointeract, 'Hands1'))

#' First action
Anova(mod_ttf_nointeract, type=2)
get_parameter_ci(mod_ttf_nointeract, 'Hands1')

#' Between actions
Anova(mod_tbtwn_nointeract, type=2)
get_parameter_ci(mod_tbtwn_nointeract, 'Hands1')

#' Age and Hands X AgeCat interactions
Anova(mod_place_full, type=2)
Anova(mod_ttf_full, type=2)
Anova(mod_tbtwn_full, type=2)

#' ### P1 footnote: separate adult/kid differences
#'
#' First placement - kids
Anova(mod_ttf_nointeract_kid, type=2)
get_parameter_ci(mod_ttf_nointeract_kid, 'Hands1')

#' Between placements - kids
Anova(mod_tbtwn_nointeract_kid, type=2)
get_parameter_ci(mod_tbtwn_nointeract_kid, 'Hands1')

#' First placement - adults
Anova(mod_ttf_nointeract_adult, type=2)
get_parameter_ci(mod_ttf_nointeract_adult, 'Hands1')

#' Between placements - adult
Anova(mod_tbtwn_nointeract_adult, type=2)
get_parameter_ci(mod_tbtwn_nointeract_adult, 'Hands1')

#' Placements - adult
Anova(mod_place_nointeract_adult, type=2)
1/exp(get_parameter_ci(mod_place_nointeract_adult, 'Hands1'))

#' Placements - kids
Anova(mod_place_nointeract_kid, type=2)
1/exp(get_parameter_ci(mod_place_nointeract_kid, 'Hands1'))

#' ## Differences over development
#' 
#' ### Paragraph 2: kids vs. adults
summary(mod_comp_tl) # Compare intercept: this is vs. 50%
summary(mod_comp_dl)


#' ### Paragraph 3: comparison of positions
#' 
#' Differences in x- & y-position
dat_bysubj %>% group_by(AgeCategory,Hands) %>% summarize(AvgYDiff = mean(AvgAbsYOffset), AvgXDiff = mean(AvgAbsXOffset))

#' Test of age category overall
Anova(mod_avgy_nointeract, type=2)

#' Test of continuous age
anova(mod_avgy_noagecont, mod_avgy_nointeract)

#' Estimate of kids improvement over years
get_parameter_ci(mod_avgy_nointeract_kid, "Age")

#' Test of x-position age category differences
Anova(mod_avgx_nointeract, type=2)

#' ### Paragraph 4: proportion of transtiions -- kids vs adults
dat_bytrial_succ %>% group_by(AgeCategory) %>% summarize(AvgTransitions = mean(Transition))
Anova(mod_transition_nointeract, type=2)


#' ## Materials and methods
#' 
#' ### Participants
#' 
#' Participant counts analyzed separately, as loaded data includes the exclusions
#' 
#' Age groupings
dat_bysubj %>% group_by(AgeCategory, Hands) %>% summarize(AvgAge = mean(Age), SDAge = sd(Age))

#' IQ counts, range, and analysis
dat_bysubj %>% filter(AgeCategory=='kids') %>% group_by(Hands) %>% summarize(N=length(IQ_BPVS), WithIQ=sum(!is.na(IQ_BPVS)))

dat_bysubj %>% filter(!is.na(IQ_BPVS)) %>% with(range(IQ_BPVS))
dat_bysubj %>% filter(!is.na(IQ_Ravens)) %>% with(range(IQ_Ravens))

dat_bysubj %>% filter(!is.na(IQ_BPVS)) %>% group_by(Hands) %>% summarize(Avg_BPVS = mean(IQ_BPVS), Avg_Ravens = mean(IQ_Ravens))

t.test(data=dat_bysubj %>% filter(!is.na(IQ_BPVS)), IQ_BPVS~Hands)
t.test(data=dat_bysubj %>% filter(!is.na(IQ_Ravens)), IQ_Ravens~Hands)

#' ### Statistical tests
#' 
#' Correlation between motor RT and error
with(dat_bysubj, cor(MedianMotorErr, MedianMotorRT))

#####################
#+ Plots ---------------------------

#' # Plots
#' 
#' ## Figure 3
plt_fig3_motorage = ggplot(data=dat_bysubj %>% transform_category_labels() %>% 
                         mutate(Exp = factor(paste(AgeCategory, Hands, sep=' w/ '))), 
                       aes(x=Age, y=MedianMotorRT, group=Exp, color=Exp)) +
  geom_smooth(method='lm') +
  geom_point() +
  xlab("Age (years)") + 
  ylab("Median Motor Test RT (s)") +
  theme_bw()
if(isTRUE(getOption('kintr.inprogress'))) {print(plt_fig3_motorage)}

#' ## Figure 4
plt_fig4_accuracy = ggplot(data=dat_plotagg %>% transform_category_labels(use_long=T),
                           aes(x=AgeCategory, y=Avg_Accuracy, group=Hands, fill=Hands,
                               ymax=Avg_Accuracy+SE_Accuracy, ymin=Avg_Accuracy-SE_Accuracy)) +
  geom_bar(stat='identity', position='dodge') +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_discrete(name = "Embodiment") +
  geom_linerange(position=position_dodge(.9), size=1.5) +
  ylab("Solution Rate") + xlab('') +
  theme_bw() + theme(legend.position = 'bottom')

plt_fig4_actions = ggplot(data=dat_plotagg %>% transform_category_labels(use_long=T),
                           aes(x=AgeCategory, y=Avg_Placements_oS, group=Hands, fill=Hands,
                               ymax=Avg_Placements_oS+SE_Placements_oS, ymin=Avg_Placements_oS-SE_Placements_oS)) +
  geom_bar(stat='identity', position='dodge') +
  geom_linerange(position=position_dodge(.9), size=1.5) +
  scale_fill_discrete(name = "Embodiment") +
  ylab("Attempts to Solution") + xlab('') +
  theme_bw() + theme(legend.position = 'bottom')

plt_fig4_tts = ggplot(data=dat_plotagg %>% transform_category_labels(use_long=T),
                          aes(x=AgeCategory, y=Avg_LastPlace_oS, group=Hands, fill=Hands,
                              ymax=Avg_LastPlace_oS+SE_LastPlace_oS, ymin=Avg_LastPlace_oS-SE_LastPlace_oS)) +
  geom_bar(stat='identity', position='dodge') +
  geom_linerange(position=position_dodge(.9), size=1.5) +
  scale_fill_discrete(name = "Embodiment") +
  ylab("Time to Solution (s)") + xlab('') +
  theme_bw() + theme(legend.position = 'bottom')

plt_fig4_ttf = ggplot(data=dat_plotagg %>% transform_category_labels(use_long=T),
                      aes(x=AgeCategory, y=Avg_FirstPlace_oS, group=Hands, fill=Hands,
                          ymax=Avg_FirstPlace_oS+SE_FirstPlace_oS, ymin=Avg_FirstPlace_oS-SE_FirstPlace_oS)) +
  geom_bar(stat='identity', position='dodge') +
  geom_linerange(position=position_dodge(.9), size=1.5) +
  scale_fill_discrete(name = "Embodiment") +
  ylab("Time to First Attempt (s)") + xlab('') +
  theme_bw() + theme(legend.position = 'bottom')

plt_fig4_tbtwn = ggplot(data=dat_plotagg %>% transform_category_labels(use_long=T),
                      aes(x=AgeCategory, y=Avg_TimeBetween_oS, group=Hands, fill=Hands,
                          ymax=Avg_TimeBetween_oS+SE_TimeBetween_oS, ymin=Avg_TimeBetween_oS-SE_TimeBetween_oS)) +
  geom_bar(stat='identity', position='dodge') +
  geom_linerange(position=position_dodge(.9), size=1.5) +
  scale_fill_discrete(name = "Embodiment") +
  ylab("Time Between Attempts (s)") + xlab('') +
  theme_bw() + theme(legend.position = 'bottom')


#' ## Figure 5
plt_fig5_ydist_notused_bygroup = ggplot(data=dat_plotagg %>% transform_category_labels(use_long=T),
                        aes(x=AgeCategory, y=Avg_YOff_oS, group=Hands, fill=Hands,
                            ymax=Avg_YOff_oS+SE_YOff_oS, ymin=Avg_YOff_oS-SE_YOff_oS)) +
  geom_bar(stat='identity', position='dodge') +
  geom_linerange(position=position_dodge(.9), size=1.5) +
  scale_fill_discrete(name = "Embodiment") +
  ylab("Vertical Distance to Object (px)") + xlab('') +
  theme_bw()

plt_fig5_ydist = ggplot(data=dat_plotagg_byagecat %>% mutate(Hands='both') %>% transform_category_labels(use_long=T),
                        aes(x=AgeCategory, y=Avg_YOff_oS, fill=AgeCategory,
                            ymax=Avg_YOff_oS+SE_YOff_oS, ymin=Avg_YOff_oS-SE_YOff_oS)) +
  geom_bar(stat='identity') +
  geom_linerange(size=1.5) +
  ylab("Vertical Distance to Object (px)") + xlab('') +
  theme_bw()

plt_fig5_transition = ggplot(data=dat_plotagg_byagecat %>% mutate(Hands='both') %>% transform_category_labels(use_long=T),
                             aes(x=AgeCategory, y=Avg_Transition, fill=AgeCategory,
                                 ymax=Avg_Transition+SE_Transition, ymin=Avg_Transition-SE_Transition)) +
  geom_bar(stat='identity') +
  scale_y_continuous(labels = scales::percent) +
  geom_linerange(size=1.5) +
  ylab("Probability of switching") + xlab('') +
  theme_bw()

plt_fig5_comp_tl = ggplot(data=dat_tl_comp_cis,
                          aes(x=TrialName, y=AvgClass, ymin=LowClass, ymax=HighClass, fill=TrialName)) +
  geom_bar(stat='identity') +
  geom_hline(yintercept=0.5, linetype='dashed') +
  geom_linerange() +
  scale_y_continuous(labels = scales::percent, limits=c(0,1)) +
  xlab('') + ylab('Prob. correct class.') +
  theme_bw() + 
  theme(legend.position = 'none',
        axis.text.x = element_text(angle=45, hjust=1))

plt_fig5_comp_dl = ggplot(data=dat_dl_comp_cis,
                          aes(x=TrialName, y=AvgClass, ymin=LowClass, ymax=HighClass, fill=TrialName)) +
  geom_bar(stat='identity') +
  geom_hline(yintercept=0.5, linetype='dashed') +
  geom_linerange() +
  scale_y_continuous(labels = scales::percent, limits=c(0,1)) +
  xlab('') + ylab('Prob. correct class.') +
  theme_bw() + 
  theme(legend.position = 'none',
        axis.text.x = element_text(angle=45, hjust=1))

#' ## Supplement

#' Accuracy by age
plt_sup_accXage = ggplot(data=dat_bysubj %>% transform_category_labels() %>% 
                           mutate(AgeCategory = relevel(AgeCategory, "Child")),
                         aes(x=Age, y=AccuracyByOnline, group=Hands, color=Hands)) +
  geom_smooth(method='lm') +
  geom_point() +
  theme_bw() +
  scale_y_continuous(labels = scales::percent, limits=c(0,1)) +
  xlab("Age (years)") +
  ylab("Solution Rate") +
  facet_grid(. ~ AgeCategory, scales='free_x')

#' Recreation of main results without success conditioning
#' 
plt_sup_nsc_actions = ggplot(data=dat_plotagg %>% transform_category_labels(),
                             aes(x=AgeCategory, y=Avg_Placements, group=Hands, fill=Hands,
                                 ymax=Avg_Placements+SE_Placements, ymin=Avg_Placements-SE_Placements)) +
  geom_bar(stat='identity', position='dodge') +
  geom_linerange(position=position_dodge(.9), size=1.5) +
  scale_fill_discrete(name = "Embodiment") +
  ylab("Total Actions") + xlab('') +
  theme_bw()


#' Comparing motor results
plt_sup_motorerr_age = ggplot(data=dat_bysubj %>% transform_category_labels() %>% 
                             mutate(Exp = factor(paste(AgeCategory, Hands, sep=' '))), 
                           aes(x=Age, y=MedianMotorErr, group=Exp, color=Exp)) +
  geom_smooth(method='lm') +
  geom_point() +
  xlab("Age (years)") + 
  ylab("Median Motor Test Error (px)") +
  theme_bw()

plt_sup_motorcomp = ggplot(data=dat_bysubj %>% transform_category_labels() %>% 
                                mutate(Exp = factor(paste(AgeCategory, Hands, sep=' '))), 
                              aes(x=MedianMotorErr, y=MedianMotorRT, group=Exp, color=Exp)) +
  #geom_smooth(method='lm') +
  geom_point() +
  xlab("Median Motor Test Error (px)") +
  ylab("Median Motor Test RT (s)") +
  theme_bw()


if(isTRUE(getOption('kintr.inprogress'))) {print(plt_fig3_motorage)}

#+ Saving plots ---------------

# Only run through the script, not when building HTML
if (!isTRUE(getOption('knitr.in.progress'))) {
  savePlot = function(plt, name, w, h) {
    plt = plt + theme(axis.title = element_text(family='Helvetica', size=14),
                      axis.text = element_text(family='Helvetica', size=12),
                      legend.text = element_text(family='Helvetica', size=12),
                      legend.title = element_text(family='Helvetica', size=14))
    fnm = paste('figures/', name, '.png', sep='')
    ggsave(fnm, plt, units='in', width=w, height=h, dpi=300, device=png())
  }
  
  plt_fig3_motorage %>% savePlot('f3_motorage', 6, 4)
  
  plt_fig4_accuracy %>% savePlot('f4_accuracy', 4, 5)
  plt_fig4_actions %>% savePlot('f4_actions', 4, 5)
  plt_fig4_tts %>% savePlot('f4_ttsolve', 4, 5)
  plt_fig4_ttf %>% savePlot('f4_ttfirst', 4, 5)
  plt_fig4_tbtwn %>% savePlot('f4_tbetween', 4, 5)
  
  (plt_fig4_accuracy + theme(legend.position='none')) %>% savePlot('thin_f4_accuracy', 3, 5)
  (plt_fig4_actions + theme(legend.position='none'))  %>% savePlot('thin_f4_actions', 3, 5)
  (plt_fig4_tts + theme(legend.position='none'))  %>% savePlot('thin_f4_ttsolve', 3, 5)
  (plt_fig4_ttf + theme(legend.position='none')) %>% savePlot('thin_f4_ttfirst', 3, 5)
  (plt_fig4_tbtwn + theme(legend.position='none'))  %>% savePlot('thin_f4_tbetween', 3, 5)
  
  
  
  plt_fig4_accuracy %>% savePlot('f4_for_legend', 8, 5)
  
  plt_fig5_ydist %>% savePlot('f5_ydist', 4, 5)
  plt_fig5_transition %>% savePlot('f5_transition', 4, 5)
  (plt_fig5_ydist + theme(legend.position='none')) %>% savePlot('wide_f5_ydist', 4, 5)
  (plt_fig5_ydist + theme(legend.position='none')) %>% savePlot('thin_f5_ydist', 2.5, 5)
  (plt_fig5_transition + theme(legend.position='none')) %>% savePlot('thin_f5_transition', 3, 5)
  (plt_fig5_transition + theme(legend.position='none')) %>% savePlot('wide_f5_transition', 4, 5)
  
  plt_fig5_comp_tl %>% savePlot('f5_comp_tl', 6, 3)
  plt_fig5_comp_dl %>% savePlot('f5_comp_dl', 6, 3)
  
  plt_sup_accXage %>% savePlot('sup_accbyage', 6, 4)
  plt_sup_motorcomp %>% savePlot('sup_motorcomp', 6, 4)
  plt_sup_motorerr_age %>% savePlot('sup_motorerr_age', 6, 4)
}
######################
#+ Supplement ----------------------

#' # Supplementary material
#' 
#' ## No covariates
#' 

#' Accuracy
Anova(mod_accuracy_nocov,type=2)

#' Time to solve
Anova(mod_tts_nocov,type=2)

#' Number of actions
Anova(mod_place_nocov,type=2)

#' First action
Anova(mod_ttf_nocov, type=2)

#' Between actions
Anova(mod_tbtwn_nocov, type=2)

#' Kids placements by age
with(dat_bysubj %>% filter(AgeCategory == 'kids'),
     cor(AvgPlaceOnSuccess, Age))

#' ## Additional statistical tests
#' 
#' ### Gender
#' 
#' Number of participants excluded b/c gender wasn't recorded
dat_bysubj %>% group_by(AgeCategory, Hands, Gender) %>% count

#' Overall stats
dat_bysubj %>% group_by(Gender) %>%
  summarize(Acc = mean(AccuracyByOnline),
            Place = mean(AvgPlaceOnSuccess),
            TTSucc = mean(AvgLastPlaceOnSuccess),
            TTFirst = mean(AvgFirstPlaceOnSuccess),
            TBtwn = mean(AvgTimeBetweenOnSuccess)
            )

dat_bysubj %>% group_by(Gender, Hands) %>%
  summarize(Acc = mean(AccuracyByOnline),
            Place = mean(AvgPlaceOnSuccess),
            TTSucc = mean(AvgLastPlaceOnSuccess),
            TTFirst = mean(AvgFirstPlaceOnSuccess),
            TBtwn = mean(AvgTimeBetweenOnSuccess)
  )

#' Impact on accuracy
anova(mod_accuracy_nointeract_gender_base, mod_accuracy_nointeract_gender_main)
anova(mod_accuracy_nointeract_gender_main, mod_accuracy_nointeract_gender_interact)

#' Impact on placements to success
anova(mod_place_nointeract_gender_base, mod_place_nointeract_gender_main)
anova(mod_place_nointeract_gender_main, mod_place_nointeract_gender_interact)

#' Impact on tts
anova(mod_tts_nointeract_gender_base, mod_tts_nointeract_gender_main)
anova(mod_tts_nointeract_gender_main, mod_tts_nointeract_gender_interact)

#' Impact on ttf
anova(mod_ttf_nointeract_gender_base, mod_ttf_nointeract_gender_main)
anova(mod_ttf_nointeract_gender_main, mod_ttf_nointeract_gender_interact)

#' Impact on tbtwn
anova(mod_tbtwn_nointeract_gender_base, mod_tbtwn_nointeract_gender_main)
anova(mod_tbtwn_nointeract_gender_main, mod_tbtwn_nointeract_gender_interact)

#' ### Input device
#' 
#' Number of participants excluded b/c device was "other"
dat_bysubj %>%
  merge(dat_bytrial %>% select(WorkerID, Device) %>% unique) %>% 
  group_by(AgeCategory, Hands, Device) %>% count

#' Overall stats
dat_bysubj %>% 
  merge(dat_bytrial %>% select(WorkerID, Device) %>% unique) %>% 
  group_by(Device) %>%
  summarize(Acc = mean(AccuracyByOnline),
            Place = mean(AvgPlaceOnSuccess),
            TTSucc = mean(AvgLastPlaceOnSuccess),
            TTFirst = mean(AvgFirstPlaceOnSuccess),
            TBtwn = mean(AvgTimeBetweenOnSuccess)
  )

dat_bysubj %>% 
  merge(dat_bytrial %>% select(WorkerID, Device) %>% unique) %>% 
  group_by(Device, Hands) %>%
  summarize(Acc = mean(AccuracyByOnline),
            Place = mean(AvgPlaceOnSuccess),
            TTSucc = mean(AvgLastPlaceOnSuccess),
            TTFirst = mean(AvgFirstPlaceOnSuccess),
            TBtwn = mean(AvgTimeBetweenOnSuccess)
  )

#' Impact on accuracy
anova(mod_accuracy_nointeract_device_base, mod_accuracy_nointeract_device_main)
anova(mod_accuracy_nointeract_device_main, mod_accuracy_nointeract_device_interact)

#' Impact on placements to success
anova(mod_place_nointeract_device_base, mod_place_nointeract_device_main)
anova(mod_place_nointeract_device_main, mod_place_nointeract_device_interact)

#' Impact on tts
anova(mod_tts_nointeract_device_base, mod_tts_nointeract_device_main)
anova(mod_tts_nointeract_device_main, mod_tts_nointeract_device_interact)

#' Impact on ttf
anova(mod_ttf_nointeract_device_base, mod_ttf_nointeract_device_main)
anova(mod_ttf_nointeract_device_main, mod_ttf_nointeract_device_interact)

#' Impact on tbtwn
anova(mod_tbtwn_nointeract_device_base, mod_tbtwn_nointeract_device_main)
anova(mod_tbtwn_nointeract_device_main, mod_tbtwn_nointeract_device_interact)

#' ### Hand laterality
#' 
#' Number of participants excluded b/c they are ambidextrous
dat_bysubj %>%
  merge(dat_bytrial %>% select(WorkerID, Laterality) %>% unique) %>% 
  group_by(AgeCategory, Hands, Laterality) %>% count

#' Overall stats
dat_bysubj %>% 
  merge(dat_bytrial %>% select(WorkerID, Laterality) %>% unique) %>% 
  group_by(Laterality) %>%
  summarize(Acc = mean(AccuracyByOnline),
            Place = mean(AvgPlaceOnSuccess),
            TTSucc = mean(AvgLastPlaceOnSuccess),
            TTFirst = mean(AvgFirstPlaceOnSuccess),
            TBtwn = mean(AvgTimeBetweenOnSuccess)
  )

dat_bysubj %>% 
  merge(dat_bytrial %>% select(WorkerID, Laterality) %>% unique) %>% 
  group_by(Laterality, Hands) %>%
  summarize(Acc = mean(AccuracyByOnline),
            Place = mean(AvgPlaceOnSuccess),
            TTSucc = mean(AvgLastPlaceOnSuccess),
            TTFirst = mean(AvgFirstPlaceOnSuccess),
            TBtwn = mean(AvgTimeBetweenOnSuccess)
  )

#' Impact on accuracy
anova(mod_accuracy_nointeract_laterality_base, mod_accuracy_nointeract_laterality_main)
anova(mod_accuracy_nointeract_laterality_main, mod_accuracy_nointeract_laterality_interact)

#' Impact on placements to success
anova(mod_place_nointeract_laterality_base, mod_place_nointeract_laterality_main)
anova(mod_place_nointeract_laterality_main, mod_place_nointeract_laterality_interact)

#' Impact on tts
anova(mod_tts_nointeract_laterality_base, mod_tts_nointeract_laterality_main)
anova(mod_tts_nointeract_laterality_main, mod_tts_nointeract_laterality_interact)

#' Impact on ttf
anova(mod_ttf_nointeract_laterality_base, mod_ttf_nointeract_laterality_main)
anova(mod_ttf_nointeract_laterality_main, mod_ttf_nointeract_laterality_interact)

#' Impact on tbtwn
anova(mod_tbtwn_nointeract_laterality_base, mod_tbtwn_nointeract_laterality_main)
anova(mod_tbtwn_nointeract_laterality_main, mod_tbtwn_nointeract_laterality_interact)

#' ### Prosthesis usage
#' 
#' Number of participants excluded b/c they are ambidextrous

dat_bysubj %>%
  filter(Hands=='1h') %>% 
  merge(dat_bytrial %>% select(WorkerID, Prosthesis_Use, AgeCategory) %>% unique) %>% 
  group_by(AgeCategory, Hands, Prosthesis_Use) %>% count


#' Overall stats
dat_bysubj %>% 
  merge(dat_bytrial %>% filter(Hands=='1h') %>% 
          select(WorkerID, Prosthesis_Use) %>% unique) %>% 
  group_by(Prosthesis_Use) %>%
  summarize(Acc = mean(AccuracyByOnline),
            Place = mean(AvgPlaceOnSuccess),
            TTSucc = mean(AvgLastPlaceOnSuccess),
            TTFirst = mean(AvgFirstPlaceOnSuccess),
            TBtwn = mean(AvgTimeBetweenOnSuccess)
  )

dat_bysubj %>% 
  merge(dat_bytrial %>% filter(Hands=='1h') %>% 
          select(WorkerID, Prosthesis_Use) %>% unique) %>% 
  group_by(Prosthesis_Use, AgeCategory) %>%
  summarize(Acc = mean(AccuracyByOnline),
            Place = mean(AvgPlaceOnSuccess),
            TTSucc = mean(AvgLastPlaceOnSuccess),
            TTFirst = mean(AvgFirstPlaceOnSuccess),
            TBtwn = mean(AvgTimeBetweenOnSuccess)
  )

#' Impact on accuracy
anova(mod_accuracy_prosthesis_base, mod_accuracy_prosthesis_nointeract)
anova(mod_accuracy_prosthesis_nointeract, mod_accuracy_prosthesis_none)

#' Impact on placements to success
anova(mod_place_prosthesis_base, mod_place_prosthesis_nointeract)
anova(mod_place_prosthesis_nointeract, mod_place_prosthesis_none)

#' Impact on tts
anova(mod_tts_prosthesis_base, mod_tts_prosthesis_nointeract)
anova(mod_tts_prosthesis_nointeract, mod_tts_prosthesis_none)

#' Impact on ttf
anova(mod_ttf_prosthesis_base, mod_ttf_prosthesis_nointeract)
anova(mod_ttf_prosthesis_nointeract, mod_ttf_prosthesis_none)

#' Impact on tbtwn
anova(mod_tbtwn_prosthesis_base, mod_tbtwn_prosthesis_nointeract)
anova(mod_tbtwn_prosthesis_nointeract, mod_tbtwn_prosthesis_none)





#' ## Action classification
#' 
#' ### Comparing TL/DL actions
#' 
#' Adults
summary(mod_comp_adult) # Looking at (Intercept)==0 test (due to model spec. this is comparing to 50%)

compstats_adult_coef = summary(mod_comp_adult)$coefficients
compstats_adult_pest_mean = compstats_adult_coef['(Intercept)','Estimate'] + .5
compstats_adult_pest_sd = compstats_adult_coef['(Intercept)','Std. Error']
compstats_adult_pest_df = compstats_adult_coef['(Intercept)','df']
compstats_adult_pest_diff = qt(.975, compstats_adult_pest_df) * compstats_adult_pest_sd
compstats_adult_pest_range = compstats_adult_pest_mean + c(-compstats_adult_pest_diff, compstats_adult_pest_diff)
print(compstats_adult_pest_range)

#' Kids
summary(mod_comp_kids) # Looking at (Intercept)==0 test (due to model spec. this is comparing to 50%)

compstats_kids_coef = summary(mod_comp_kids)$coefficients
compstats_kids_pest_mean = compstats_kids_coef['(Intercept)','Estimate'] + .5
compstats_kids_pest_sd = compstats_kids_coef['(Intercept)','Std. Error']
compstats_kids_pest_df = compstats_kids_coef['(Intercept)','df']
compstats_kids_pest_diff = qt(.975, compstats_kids_pest_df) * compstats_kids_pest_sd
compstats_kids_pest_range = compstats_kids_pest_mean + c(-compstats_kids_pest_diff, compstats_kids_pest_diff)
print(compstats_kids_pest_range)

#' ### Making the table
tabluate_classifications = function(d, cname) {
  pct = function(n) {return(paste(round(n*100,0),"%", sep=''))}
  ndat = d %>% mutate(Txt = paste(pct(AvgClass), " [", pct(LowClass), ", ", pct(HighClass), "]", sep="")) %>% 
    select(TrialName, !!quo_name(cname) := Txt)
  return(ndat)
}

class_table = 
  tabluate_classifications(dat_adult_comp_cis, "Adult: TL vs DL") %>% 
  merge(tabluate_classifications(dat_kids_comp_cis, "Children: TL vs DL")) %>% 
  merge(tabluate_classifications(dat_tl_comp_cis, "TL: Adult vs Children")) %>% 
  merge(tabluate_classifications(dat_dl_comp_cis, "DL: Adult vs Children"))

print(class_table)

# For ease of LaTexing, run the line below:
# print(xtable::xtable(class_table), include.rownames=F)

#' ## Non-success-conditioned analysis
#' 
#' Here we test for the main studied effects looking at all trials, not just the successful ones
#' 
#' ### Placements
#' 
mod_nsc_place_null = 
  glmer(data = dat_bytrial, family=poisson,
        I(NPlacements-1) ~ 1 + (1|WorkerID) + (1|TrialName))

mod_nsc_place_nullcov = 
  glmer(data = dat_bytrial, family=poisson,
        I(NPlacements-1) ~ ZAge*AgeCategory + ZMRT + (1|WorkerID) + (1|TrialName))

mod_nsc_place_full =
  glmer(data = dat_bytrial, family=poisson,
        I(NPlacements-1) ~ Hands*AgeCategory + ZAge*AgeCategory + ZMRT + (1|WorkerID) + (1|TrialName),
        control=glmerControl(optimizer='bobyqa', 
                             optCtrl=list(maxfun=2e6)))

mod_nsc_place_nointeract = 
  glmer(data = dat_bytrial, family=poisson,
        I(NPlacements-1) ~ Hands + ZAge*AgeCategory + ZMRT + (1|WorkerID) + (1|TrialName),
        control=glmerControl(optimizer='bobyqa', 
                             optCtrl=list(maxfun=2e6)))

mod_nsc_place_noagecond = 
  glmer(data = dat_bytrial, family=poisson,
        I(NPlacements-1) ~ Hands + AgeCategory + ZMRT + (1|WorkerID) + (1|TrialName),
        control=glmerControl(optimizer='bobyqa', 
                             optCtrl=list(maxfun=2e6)))

mod_nsc_place_nointeract_adult = 
  glmer(data = dat_bytrial %>% filter(AgeCategory == 'adult'), family=poisson,
        I(NPlacements-1) ~ Hands + Age + ZMRT + (1|WorkerID) + (1|TrialName),
        control=glmerControl(optimizer='bobyqa', 
                             optCtrl=list(maxfun=2e6)))

mod_nsc_place_nointeract_kid = 
  glmer(data = dat_bytrial %>% filter(AgeCategory == 'kids'), family=poisson,
        I(NPlacements-1) ~ Hands + Age + ZMRT + (1|WorkerID) + (1|TrialName),
        control=glmerControl(optimizer='bobyqa', 
                             optCtrl=list(maxfun=2e6)))

#' Are there age differences or hand differences?
Anova(mod_nsc_place_full, type=2)
1/exp(get_parameter_ci(mod_nsc_place_nointeract, 'Hands1'))


#' Why do we no longer get an effect? First, test for "persistence" as the proportion of total placements to success placements
dat_bysubj %>%
  mutate(PlaceRatio = AvgPlacements / AvgPlaceOnSuccess) %>% 
  group_by(AgeCategory, Hands) %>% 
  summarize(AvgPR = mean(PlaceRatio))

mod_nsc_placeratio_full =
  lm(data = dat_bysubj %>% mutate(PlaceRatio = AvgPlacements / AvgPlaceOnSuccess),
     PlaceRatio ~ AgeCategory*Hands + AgeCategory*Age)
Anova(mod_nsc_placeratio_full,type=2)
  

#' ### Time to success
#' 
mod_nsc_tts_null =
  lmer(data = dat_bytrial,
       LastPlace ~ 1 + (1|WorkerID) + (1|TrialName))

mod_nsc_tts_nullcov = 
  lmer(data = dat_bytrial,
       LastPlace ~ ZAge*AgeCategory + ZMRT + (1|WorkerID) + (1|TrialName))

mod_nsc_tts_full = 
  lmer(data = dat_bytrial,
       LastPlace ~ Hands*AgeCategory + ZAge*AgeCategory + ZMRT + (1|WorkerID) + (1|TrialName))

mod_nsc_tts_nointeract = 
  lmer(data = dat_bytrial,
       LastPlace ~ Hands + ZAge*AgeCategory + ZMRT + (1|WorkerID) + (1|TrialName))

mod_nsc_tts_noagecont = 
  lmer(data = dat_bytrial,
       LastPlace ~ Hands + AgeCategory + ZMRT + (1|WorkerID) + (1|TrialName))

mod_nsc_tts_nointeract_adult = 
  lmer(data = dat_bytrial %>% filter(AgeCategory == 'adult'),
       LastPlace ~ Hands + Age + ZMRT + (1|WorkerID) + (1|TrialName))

mod_nsc_tts_nointeract_kid = 
  lmer(data = dat_bytrial %>% filter(AgeCategory == 'kids'),
       LastPlace ~ Hands + Age + ZMRT + (1|WorkerID) + (1|TrialName))


#' Are there age differences or hand differences?
Anova(mod_nsc_tts_full, type=2)
anova(mod_nsc_tts_noagecont, mod_nsc_tts_nointeract)
get_parameter_ci(mod_nsc_tts_nointeract, 'Hands1')

#' Get parameter estimates for age
get_parameter_ci(mod_nsc_tts_nointeract_adult, 'Age')
get_parameter_ci(mod_nsc_tts_nointeract_kid, 'Age')

#' ### Time to first action
#' 
mod_nsc_ttf_null =
  lmer(data = dat_bytrial,
       FirstPlace ~ 1 + (1|WorkerID) + (1|TrialName))

mod_nsc_ttf_nullcov = 
  lmer(data = dat_bytrial,
       FirstPlace ~ ZAge*AgeCategory + ZMRT + (1|WorkerID) + (1|TrialName))

mod_nsc_ttf_full = 
  lmer(data = dat_bytrial,
       FirstPlace ~ Hands*AgeCategory + ZAge*AgeCategory + ZMRT + (1|WorkerID) + (1|TrialName))

mod_nsc_ttf_nointeract = 
  lmer(data = dat_bytrial,
       FirstPlace ~ Hands + ZAge*AgeCategory + ZMRT + (1|WorkerID) + (1|TrialName))

mod_nsc_ttf_noagecont = 
  lmer(data = dat_bytrial,
       FirstPlace ~ Hands + AgeCategory + ZMRT + (1|WorkerID) + (1|TrialName))

#' Are there age differences or hand differences?
Anova(mod_nsc_ttf_full, type=2)
anova(mod_nsc_ttf_noagecont, mod_nsc_ttf_nointeract)
get_parameter_ci(mod_nsc_ttf_nointeract, 'Hands1')



#' ### Time between actions
#' 
mod_nsc_tbtwn_null =
  lmer(data = dat_bytrial %>% filter(!is.na(AvgTimeDiff)),
       AvgTimeDiff ~ 1 + (1|WorkerID) + (1|TrialName))

mod_nsc_tbtwn_nullcov = 
  lmer(data = dat_bytrial %>% filter(!is.na(AvgTimeDiff)),
       AvgTimeDiff ~ ZAge*AgeCategory + ZMRT + (1|WorkerID) + (1|TrialName))

mod_nsc_tbtwn_full = 
  lmer(data = dat_bytrial %>% filter(!is.na(AvgTimeDiff)),
       AvgTimeDiff ~ Hands*AgeCategory + ZAge*AgeCategory + ZMRT + (1|WorkerID) + (1|TrialName))

mod_nsc_tbtwn_nointeract = 
  lmer(data = dat_bytrial %>% filter(!is.na(AvgTimeDiff)),
       AvgTimeDiff ~ Hands + ZAge*AgeCategory + ZMRT + (1|WorkerID) + (1|TrialName))

mod_nsc_tbtwn_noagecond = 
  lmer(data = dat_bytrial %>% filter(!is.na(AvgTimeDiff)),
       AvgTimeDiff ~ Hands + AgeCategory + ZMRT + (1|WorkerID) + (1|TrialName))

#' Are there age differences or hand differences?
Anova(mod_nsc_tbtwn_full, type=2)
get_parameter_ci(mod_nsc_tbtwn_nointeract, 'Hands1')


Anova(mod_place_nointeract, type=2)



#+ Additional tests -------

#' Testing Bayes Factor of interactions

BFpack::BF(glm(data = dat_bytrial, family=binomial, SuccessByOnline ~ Hands*AgeCategory + ZAge*AgeCategory + ZMRT))

BFpack::BF(glm(data = dat_bytrial_succ, family=poisson,
           I(NPlacements-1) ~ Hands*AgeCategory + ZAge*AgeCategory + ZMRT))
