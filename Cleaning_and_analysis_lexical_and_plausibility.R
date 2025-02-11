

########################################################################
#**********************************************************************#
# Author: Clara Louise Lopes  -                                        #
#                                                                      #
# Date started: September, 2021
# This is the code that was created and used for the analyses that 
# comprise the paper entitled "On the Verge of Plausible: Understanding
# Contributions of Parafoveal & Foveal Plausibility Effects During 
# Reading" . If you have questions about the code or our analysis 
# pipeline, I am available at clara.lopes@utah.edu

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#----------------------------------------------------------------------#
#- - - Eye Movement Data Cleaning, Wrangling, Viz, Analysis Pipeline - #
#  
# *********************************************************************#
########################################################################


#--Reading in necessary packages--#

library(readr)
library(ggplot2)
library(lme4)
library(Rmisc)

library(doBy)

# Interest Area Report - Loading in the Data 
setwd("C:/Users/clara/OneDrive/Desktop/JEP-HPP 2022 Submission/OSF Scripts and Data")

#data <- read.csv("45_PP_Trim.csv", na.strings=".")
data <-  <- read_csv("C:/Users/clara/OneDrive/Desktop/EmoPrev/Dissertation Proposal/Pilot Data For Presentation/Pilots_1_2_ForProposal.csv")
View(Pilots_1_2_ForProposal)

########################################################################
#**********************************************************************#
#  Renaming & Filtering out target words only                          #
#**********************************************************************#
########################################################################

names(data)[1]<- "Subject" 
unique(data$Subject)
# N=48

#******************************************************************************#
################################################################################
# Note: When loading in the data, we need to make sure that there are 
# no "." or FALSE present - this is how DataViewer spits out the files
# Due to experimenter error, a handful of trials (listed below) need 
# to be removed from the dataframe before analysis is complete. These trials
# contain extra characters that we suspect may have changed some of the boundary
# trials. For this reason and prior to analyzing data, we decided to remove 
#these, which resulted in less than .05% data loss.
#This has all been described in detail in the manuscript.
################################################################################
################################################################################
# Now, with manually identifying / removing which stimuli had issues in each of 
# the lists so I can create a new dataframe 
#w/o it 'demorgan's' style - (i.e. "is.not !") 
################################################################################
# Following plaus stimuli had issues, so removing them from all conditions/lists

cleaneye <- data[which(!(data$item %in% c(2,11,16,22,62,100,220,226,10,19,
                                          63,97,171,187,
                                          42,137,161,173,190,43,86,211))),]

## Now we need to select target words only and scrap other words

CleanEye<-subset(cleaneye, cleaneye$IA_ID == cleaneye$indexofia)

#how many trials of each kind do we have 

table(CleanEye$Subject, CleanEye$condition) #<- this is how many trials per condition per subject

#This is how many trials per condition per item 

#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$#
#
# Loading in necessary packages & functions    #
#
################################################

library(dplyr)
library(ggplot2)

########################################################################
# *********************************************************************#                                                                      #
# Creating fixation variables (filtering non-progressive eye movements)#
# *********************************************************************#                                                                      #
########################################################################

# First, make acopy of CleanEYE called IA_report_combined 

IA_report_combined <- CleanEye


########################################################################
# *********************************************************************#
# Now,finish processing the fixation measures of interest, 
# and make sure to trim super short/long fixations 
# *********************************************************************#
########################################################################


IA_report_combined$ffd<- ifelse(IA_report_combined$IA_FIRST_FIX_PROGRESSIVE==1, IA_report_combined$IA_FIRST_FIXATION_DURATION, NA)

#adding sfd - Need to double check on SR Support Forum - definition from SR Forum
#The duration of a fixation on a target region, 
#provided that region is fixated one time only, 
#and that the one fixation does not occur after fixations on words further along in the text.
IA_report_combined$sfd<- ifelse(IA_report_combined$IA_FIRST_FIX_PROGRESSIVE==1& IA_report_combined$IA_FIXATION_COUNT==1, IA_report_combined$IA_FIRST_FIXATION_DURATION,  NA)


IA_report_combined$gd<- ifelse(IA_report_combined$IA_FIRST_FIX_PROGRESSIVE==1, IA_report_combined$IA_FIRST_RUN_DWELL_TIME, NA)


IA_report_combined$rpd<- ifelse(IA_report_combined$IA_FIRST_FIX_PROGRESSIVE==1, IA_report_combined$IA_REGRESSION_PATH_DURATION, NA)


IA_report_combined$reReading<-IA_report_combined$rp-IA_report_combined$gd


#
IA_report_combined$trt<-IA_report_combined$IA_DWELL_TIME

IA_report_combined$refixProb<- ifelse(IA_report_combined$IA_FIRST_RUN_FIXATION_COUNT== 1, 0, 1)


#To trim based on individual (subject) quantiles:

#FFD

maxFFD<-aggregate(IA_report_combined$ffd,list(IA_report_combined$Subject,IA_report_combined$condition), quantile, p =0.997, na.rm = TRUE) #find max FFD for each subject

colnames(maxFFD)<-c("Subject", "condition", "maxFFD") #change names for merging

IA_report_combined2<-merge(IA_report_combined, maxFFD, by = c("Subject","condition"), all.x = TRUE) #merge

IA_report_combined2$ffd_trim<- ifelse(IA_report_combined2$ffd > IA_report_combined2$maxFFD,
                                      
                                      NA, IA_report_combined2$ffd) #trim

#SFD

maxsfd<-aggregate(IA_report_combined$sfd,list(IA_report_combined$Subject,IA_report_combined$condition), quantile, p =0.997, na.rm = TRUE) #find max sfd for each subject

colnames(maxsfd)<-c("Subject", "condition", "maxsfd") #change names for merging

IA_report_combined3<-merge(IA_report_combined2, maxsfd, by = c("Subject","condition"), all.x = TRUE) #merge

IA_report_combined3$sfd_trim<- ifelse(IA_report_combined3$sfd > IA_report_combined3$maxsfd,
                                      
                                      NA, IA_report_combined3$sfd) #trim

#GD
maxGD<-aggregate(IA_report_combined$gd,list(IA_report_combined$Subject,IA_report_combined$condition), quantile, p =0.997, na.rm = TRUE) #find max GD for each subject

colnames(maxGD)<-c("Subject", "condition", "maxGD") #change names for merging

IA_report_combined4<-merge(IA_report_combined3, maxGD, by = c("Subject","condition"), all.x = TRUE) #merge

IA_report_combined4$GD_trim<- ifelse(IA_report_combined4$gd > IA_report_combined4$maxGD,
                                     
                                     NA, IA_report_combined4$gd) #trim

#rpd

maxrpd<-aggregate(IA_report_combined$rpd,list(IA_report_combined$Subject,IA_report_combined$condition), quantile, p =0.997, na.rm = TRUE) #find max rpd for each subject

colnames(maxrpd)<-c("Subject", "condition", "maxrpd") #change names for merging

IA_report_combined5<-merge(IA_report_combined4, maxrpd, by = c("Subject","condition"), all.x = TRUE) #merge

IA_report_combined5$rpd_trim<- ifelse(IA_report_combined5$rpd > IA_report_combined5$maxrpd,
                                      
                                      NA, IA_report_combined5$rpd) #trim

#reReading

maxreReading<-aggregate(IA_report_combined$reReading,list(IA_report_combined$Subject,IA_report_combined$condition), quantile, p =0.997, na.rm = TRUE) #find max reReading for each subject

colnames(maxreReading)<-c("Subject", "condition", "maxreReading") #change names for merging

IA_report_combined6<-merge(IA_report_combined5, maxreReading, by = c("Subject","condition"), all.x = TRUE) #merge

IA_report_combined6$reReading_trim<- ifelse(IA_report_combined6$reReading > IA_report_combined6$maxreReading,
                                            
                                            NA, IA_report_combined6$reReading) #trim


#trt

maxtrt<-aggregate(IA_report_combined$trt,list(IA_report_combined$Subject,IA_report_combined$condition), quantile, p =0.997, na.rm = TRUE) #find max trt for each subject

colnames(maxtrt)<-c("Subject", "condition", "maxtrt") #change names for merging

IA_report_combined7<-merge(IA_report_combined6, maxtrt, by = c("Subject","condition"), all.x = TRUE) #merge

IA_report_combined7$trt_trim<- ifelse(IA_report_combined7$trt > IA_report_combined7$maxtrt,
                                      
                                      NA, IA_report_combined7$trt) #trim
################################################################################
#
# Continuing to subset, all measures we want to include are the 
#     following 6: 
#  ...   First Fixation duration
#        Gaze Duration 
#        Go-Past time (aka Regression Path Duration)
#        Probability of regressing out of a target
#        Probability of Skipping the target word 
#        Probability of first-pass refixations on target  ....
#
################################################################################

################################################################################
library(plyr)

nodup<- IA_report_combined7 
################################################################################

# Plotting just our conditions of interest 

################################################################################

Plauseye<-nodup[which(nodup$condition == "PP" | nodup$condition == "PI"
                      | nodup$condition == "IP"| nodup$condition == "II"),]
#subsetting 
freqeye<-nodup[which(nodup$condition == "High/High" | nodup$condition == "High/Low"
                     | nodup$condition == "Low/High"| nodup$condition == "Low/Low"),]

both<- inner_join(Plauseye2, Freqeye2, by=c("Subject" & "Condition"))

#####################################################################################
#  Now to run some preliminary stats                   ##############################
#####################################################################################

library(lme4)
library(afex)
library(emmeans)

#################################################################
# :: All Frequency Analyses below::
# ---------------------------------------------------------------
#################################################################

#################################################################
# Making the factors for frequency match the plausibility pipeline
# ---------------------------------------------------------------
#################################################################

as.factor(freqeye$condition)


#Levels: High/High High/Low Low/High Low/Low

Freqeye2<-freqeye


Freqeye2$PreviewVal <- ifelse(Freqeye2$condition == "Low/High" | Freqeye2$condition == "High/Low","Invalid", "Valid")
as.factor(Freqeye2$PreviewVal)

# as. factor == Levels Invalid, Valid 
#moving the conditions column next to my new Preview Validity column
require(dplyr)
Freqeye2 <- Freqeye2 %>% relocate(condition, .before = PreviewVal)

Freqeye2$TargetFreq <- ifelse(Freqeye2$condition == "Low/Low" | Freqeye2$condition == "High/Low","InFreq","Freq")

as.factor(Freqeye2$TargetFreq)

# releveling so Infreq is first
Freqeye2$TargetFreq <- factor(Freqeye2$TargetFreq, levels=c('InFreq','Freq'))

############################################################################
# First Fixation  - -
############################################################################
# Model 1 (Preview Validity & Foveal Freqibility as predictors - w/o interaction) <- fit is singular 
#ffd.mod1<- mixed(ffd_trim~TargetFreq+PreviewVal+ (1|Subject)+(1|item)
#                 , data = Freqeye2, method = "LRT")
#summary(ffd.mod1)

# Keep it Maximal Approach
# ran the model with all possible random slopes (keep it maximal) using a step-down
# approach based on the Variance and correlation matricies 
# in this case, taking out TargetFreq, Preview Val and their interaction from 
# random slope of subject & as well as their interaction, and the final model 
# allowed for Target Freq and Preview Val to be kept in (without warnings)
# Thus the final model: 

freq.ffd.mod1<- mixed(ffd_trim~ TargetFreq*PreviewVal+
                        (1|Subject) +
                        (1|item),
                      data = Freqeye2, method = "LRT",
                      control = lmerControl(optCtrl = list(maxfun = 1e6)),
                      expand_re = TRUE)

summary(freq.ffd.mod1)
#chisquare
freq.ffd.mod1

#spits out raw estimates - 
summary(freq.ffd.mod1)

# Now, Log Transform ffd_trim & re-run the model (sensitivity test)
# this is just to make sure our effects pan out


freq.ffd.mod2<- mixed(log(ffd_trim)~ TargetFreq*PreviewVal+
                        (1|Subject) +
                        (1|item),
                      data = Freqeye2, method = "LRT",
                      control = lmerControl(optCtrl = list(maxfun = 1e6)),
                      expand_re = TRUE)

freq.ffd.mod2

#Calculate Marginal means 
freq.ffd.emmeans <- emmeans(freq.ffd.mod1, specs = c("TargetFreq","PreviewVal"))
emmeans::emm_options(lmer.df = "asymptotic")

freq.ffd.emmeans


# Do a pair-wise comparison to see what is driving the main effect
pairs(freq.ffd.emmeans, adjust="none")


############################################################################
# Gaze Duration  - - 
############################################################################


freq.gd.mod1<- mixed(log(GD_trim)~ TargetFreq*PreviewVal+
                       (1 |Subject) +
                       (1 + PreviewVal||item),
                     data = Freqeye2, method = "LRT",
                     control = lmerControl(optCtrl = list(maxfun = 1e6)),
                     expand_re = TRUE)

summary(freq.gd.mod1)
freq.gd.mod1


freq.gd.mod2<- mixed(GD_trim~ TargetFreq*PreviewVal+
                       (1 |Subject) +
                       (1 + PreviewVal||item),
                     data = Freqeye2, method = "LRT",
                     control = lmerControl(optCtrl = list(maxfun = 1e6)),
                     expand_re = TRUE)
freq.gd.mod2

summary(freq.gd.mod2)

#Calculate Marginal means 
freq.gd.emmeans <- emmeans(freq.gd.mod2, specs = c("TargetFreq", "PreviewVal"))

# Do a pair-wise comparison to see what is driving the main effect
pairs(freq.gd.emmeans)

############################################################################
# Regression Path Duration -   - - 
############################################################################


freq.rpd.mod1<- mixed(rpd_trim~ TargetFreq*PreviewVal+
                        (1|Subject) +
                        (1|item),
                      data = Freqeye2, method = "LRT",
                      control = lmerControl(optCtrl = list(maxfun = 1e6)),
                      expand_re = TRUE)
freq.rpd.mod1

summary(freq.rpd.mod1)

freq.rpd.mod2<- mixed(log(rpd_trim)~ TargetFreq*PreviewVal+
                        (1|Subject) +
                        (1|item),
                      data = Freqeye2, method = "LRT",
                      control = lmerControl(optCtrl = list(maxfun = 1e6)),
                      expand_re = TRUE)
freq.rpd.mod2

#Calculate Marginal means 



freq.rpd.emmeans <- emmeans(freq.rpd.mod1,  specs = c ("TargetFreq", "PreviewVal"))

# Do a pair-wise comparison to see what is driving the main effect
pairs(freq.rpd.emmeans, adjust="none")


############################################################################
#  Single Fix   - - 
############################################################################
# Model would not converge with random slopes for Subject and Item
freq.sfd.mod1<- mixed(sfd_trim~ TargetFreq*PreviewVal+
                        (1|Subject) +
                        (1|item),
                      data = Freqeye2, method = "LRT",
                      control = lmerControl(optCtrl = list(maxfun = 1e6)),
                      expand_re = TRUE)

summary(freq.sfd.mod1)
freq.sfd.mod1


freq.sfd.mod2<- mixed(log(sfd_trim)~ TargetFreq*PreviewVal+
                        (1|Subject) +
                        (1|item),
                      data = Freqeye2, method = "LRT",
                      control = lmerControl(optCtrl = list(maxfun = 1e6)),
                      expand_re = TRUE)

freq.sfd.mod2


#Calculate Marginal means 
freq.sfd.emmeans <- emmeans(freq.sfd.mod1, specs = c ("TargetFreq", "PreviewVal"))

# Do a pair-wise comparison to see what is driving the main effect
pairs(freq.sfd.emmeans, adjust="none")

############################################################################
#  Total Reading Time   - - 
############################################################################

freq.trt.mod1<- mixed(trt_trim~ TargetFreq*PreviewVal+
                        (1|Subject) +
                        (1|item),
                      data = Freqeye2, method = "LRT",
                      control = lmerControl(optCtrl = list(maxfun = 1e6)),
                      expand_re = TRUE)

summary(freq.trt.mod1)
freq.trt.mod1

freq.trt.mod2<- mixed(log(trt_trim)~ TargetFreq*PreviewVal+
                        (1|Subject) +
                        (1|item),
                      data = Freqeye2, method = "LRT",
                      control = lmerControl(optCtrl = list(maxfun = 1e6)),
                      expand_re = TRUE)
(freq.trt.mod2)



#Calculate Marginal means 
freq.trt.emmeans <- emmeans(freq.trt.mod1, specs = c ("TargetFreq", "PreviewVal"))

# Do a pair-wise comparison to see what is driving the main effect
pairs(freq.trt.emmeans, adjust="none")

################################################################################
# p(based) measures - re:word skipping, regressing out of target, refixating
################################################################################


Freqeye2$PreviewFreq <- ifelse(Freqeye2$condition == "Low/High" | Freqeye2$condition == "Low/Low","Infrequent", "Frequent")
as.factor(Freqeye2$PreviewFreq)


mod1.probofskip <- mixed(IA_SKIP~PreviewFreq+(1|Subject)+(1|item), data = Freqeye2, 
                         family=binomial(logit),method = "LRT",
                         control = glmerControl(optCtrl = list(maxfun = 1e6)),
                         expand_re = TRUE)

summary(mod1.probofskip)
mod1.probofskip

skip.emmeans <- emmeans(mod1.probofskip, specs = c ("PreviewFreq"))

# Do a pair-wise comparison to see what is driving the main effect
pairs(skip.emmeans, adjust="none")



# Probability of Regressing out of Target  (IA_REGRESSION_OUT) ???

freq.mod1.regOUT <- mixed(IA_REGRESSION_OUT~TargetFreq*PreviewVal+(1|Subject)+(1|item), data = Freqeye2, 
                          family=binomial(logit),method = "LRT",
                          control = glmerControl(optCtrl = list(maxfun = 1e6)),
                          expand_re = TRUE)


summary(freq.mod1.regOUT)
(freq.mod1.regOUT)


freq.regOUT.emmeans <- emmeans(freq.mod1.regOUT, specs = c ("TargetFreq", "PreviewVal"))

# Do a pair-wise comparison to see what is driving the main effect
pairs(freq.regOUT.emmeans, adjust="none")

# Probability of First-Pass Refixations on Target (IA_FIRST_FIXATION_PREVIOUS_FIX_IA)

freq.mod1.probofrefix <- mixed(refixProb~TargetFreq*PreviewVal+(1|Subject)+(1|item), data = Freqeye2, 
                               family=binomial(logit),method = "LRT",
                               control = glmerControl(optCtrl = list(maxfun = 1e6)),
                               expand_re = TRUE)
freq.mod1.probofrefix
summary (freq.mod1.probofrefix)

freq.refix.emmeans <- emmeans(freq.mod1.probofrefix, specs = c ("TargetFreq", "PreviewVal"))

# Do a pair-wise comparison to see what is driving the main effect
pairs(freq.refix.emmeans, adjust="none")



######################################################################
# :: All Plausibility Analyses are repeated in the same style below::
# ---------------------------------------------------------------
######################################################################

as.factor(Plauseye$condition)


#levels II IP PI PP

#Creating Preview Validity objects  

Plauseye2<-Plauseye


Plauseye2$PreviewVal <- ifelse(Plauseye2$condition == "IP" | Plauseye2$condition == "PI","Invalid", "Valid")
as.factor(Plauseye2$PreviewVal)

# as. factor == Levels Invalid, Valid 
#moving the conditions column next to my new Preview Validity column
require(dplyr)
Plauseye2 <- Plauseye2 %>% relocate(condition, .before = PreviewVal)

Plauseye2$TargetPlaus <- ifelse(Plauseye2$condition == "II" | Plauseye2$condition == "PI","Implaus","Plaus")

as.factor(Plauseye2$TargetPlaus)


############################################################################
# First Fixation  - -
############################################################################
# Model 1 (Preview Validity & Foveal Plausibility as predictors - w/o interaction) <- fit is singular 
#ffd.mod1<- mixed(ffd_trim~TargetPlaus+PreviewVal+ (1|Subject)+(1|item)
#                 , data = Plauseye2, method = "LRT")
#summary(ffd.mod1)

# Keep it Maximal Approach
# ran the model with all possible random slopes (keep it maximal) using a step-down
# approach based on the Variance and correlation matricies 
# in this case, taking out TargetPlaus, Preview Val and their interaction from 
# random slope of subject & as well as their interaction, and the final model 
# allowed for Target Plaus and Preview Val to be kept in (without warnings)
# Thus the final model: 

ffd.mod1<- mixed(ffd_trim~ TargetPlaus*PreviewVal+
                   (1|Subject) +
                   (1|item),
                 data = Plauseye2, method = "LRT",
                 control = lmerControl(optCtrl = list(maxfun = 1e6)),
                 expand_re = TRUE)
ffd.mod1


# Now, Log Transform ffd_trim & re-run the model (sensitivity test)
# this is just to make sure our effects pan out


ffd.mod2<- mixed(log(ffd_trim)~ TargetPlaus*PreviewVal+
                   (1|Subject) +
                   (1|item),
                 data = Plauseye2, method = "LRT",
                 control = lmerControl(optCtrl = list(maxfun = 1e6)),
                 expand_re = TRUE)


#Calculate Marginal means 
ffd.emmeans <- emmeans(ffd.mod1, specs = c("TargetPlaus","PreviewVal"))
emmeans::emm_options(lmer.df = "asymptotic")

ffd.emmeans

# Do a pair-wise comparison to see what is driving the main effect
pairs(ffd.emmeans, adjust="none")


############################################################################
# Gaze Duration  - - 
############################################################################


gd.mod1<- mixed(log(GD_trim)~ TargetPlaus*PreviewVal+
                  (1 |Subject) +
                  (1 + PreviewVal||item),
                data = Plauseye2, method = "LRT",
                control = lmerControl(optCtrl = list(maxfun = 1e6)),
                expand_re = TRUE)
gd.mod1


gd.mod2<- mixed(GD_trim~ TargetPlaus*PreviewVal+
                  (1 |Subject) +
                  (1 + PreviewVal||item),
                data = Plauseye2, method = "LRT",
                control = lmerControl(optCtrl = list(maxfun = 1e6)),
                expand_re = TRUE)
gd.mod2
# Model 2 (Same predictors, with interaction) <- fit is singular!
#gd.mod2 <- mixed(gd~TargetPlaus*PreviewVal+(1|Subject)+(1|item),data = Plauseye2, method = "LRT")
#summary(gd.mod2)


#Calculate Marginal means 
gd.emmeans <- emmeans(gd.mod1, specs = c("TargetPlaus", "PreviewVal"))

# Do a pair-wise comparison to see what is driving the main effect
pairs(gd.emmeans)

############################################################################
# Regression Path Duration -   - - 
############################################################################


rpd.mod1<- mixed(rpd_trim~ TargetPlaus*PreviewVal+
                   (1|Subject) +
                   (1 + PreviewVal||item),
                 data = Plauseye2, method = "LRT",
                 control = lmerControl(optCtrl = list(maxfun = 1e6)),
                 expand_re = TRUE)
rpd.mod1

rpd.mod2<- mixed(log(rpd_trim)~ TargetPlaus*PreviewVal+
                   (1|Subject) +
                   (1 + PreviewVal||item),
                 data = Plauseye2, method = "LRT",
                 control = lmerControl(optCtrl = list(maxfun = 1e6)),
                 expand_re = TRUE)
rpd.mod2

#Calculate Marginal means 
rpd.emmeans <- emmeans(rpd.mod1,  specs = c ("TargetPlaus", "PreviewVal"))

# Do a pair-wise comparison to see what is driving the main effect
pairs(rpd.emmeans, adjust="none")


############################################################################
#  Single Fix   - - 
############################################################################
# Model would not converge with random slopes for Subject and Item
sfd.mod1<- mixed(sfd_trim~ TargetPlaus*PreviewVal+
                   (1+TargetPlaus|Subject) +
                   (1|item),
                 data = Plauseye2, method = "LRT",
                 control = lmerControl(optCtrl = list(maxfun = 1e6)),
                 expand_re = TRUE)
sfd.mod1


sfd.mod2<- mixed(log(sfd_trim)~ TargetPlaus*PreviewVal+
                   (1+TargetPlaus|Subject) +
                   (1|item),
                 data = Plauseye2, method = "LRT",
                 control = lmerControl(optCtrl = list(maxfun = 1e6)),
                 expand_re = TRUE)
sfd.mod2


#Calculate Marginal means 
sfd.emmeans <- emmeans(sfd.mod1, specs = c ("TargetPlaus", "PreviewVal"))

# Do a pair-wise comparison to see what is driving the main effect
pairs(sfd.emmeans, adjust="none")

############################################################################
#  Total Reading Time   - - 
############################################################################

trt.mod1<- mixed(trt_trim~ TargetPlaus*PreviewVal+
                   (1+TargetPlaus+PreviewVal||Subject) +
                   (1 + PreviewVal||item),
                 data = Plauseye2, method = "LRT",
                 control = lmerControl(optCtrl = list(maxfun = 1e6)),
                 expand_re = TRUE)
trt.mod1

trt.mod2<- mixed(log(trt_trim)~ TargetPlaus*PreviewVal+
                   (1+TargetPlaus+PreviewVal||Subject) +
                   (1 + PreviewVal||item),
                 data = Plauseye2, method = "LRT",
                 control = lmerControl(optCtrl = list(maxfun = 1e6)),
                 expand_re = TRUE)
(trt.mod2)



#Calculate Marginal means 
trt.emmeans <- emmeans(trt.mod1, specs = c ("TargetPlaus", "PreviewVal"))

# Do a pair-wise comparison to see what is driving the main effect
pairs(trt.emmeans, adjust="none")


Plauseye2$PreviewPlaus <- ifelse(Plauseye2$condition == "IP" | Plauseye2$condition == "II","Implausible", "Plausible")
as.factor(Plauseye2$PreviewPlaus)


mod1.probofskip <- mixed(IA_SKIP~PreviewPlaus+(1|Subject)+(1|item), data = Plauseye2, 
                         family=binomial(logit),method = "LRT",
                         control = glmerControl(optCtrl = list(maxfun = 1e6)),
                         expand_re = TRUE)

mod1.probofskip

skip.emmeans <- emmeans(mod1.probofskip, specs = c ("TargetPlaus", "PreviewVal"))

# Do a pair-wise comparison to see what is driving the main effect
pairs(skip.emmeans, adjust="none")


# Probability of Regressing out of Target  (IA_REGRESSION_OUT) ???

mod1.regOUT <- mixed(IA_REGRESSION_OUT~TargetPlaus*PreviewVal+(1|Subject)+(1|item), data = Plauseye2, 
                     family=binomial(logit),method = "LRT",
                     control = glmerControl(optCtrl = list(maxfun = 1e6)),
                     expand_re = TRUE)



(mod1.regOUT)


regOUT.emmeans <- emmeans(mod1.regOUT, specs = c ("TargetPlaus", "PreviewVal"))

# Do a pair-wise comparison to see what is driving the main effect
pairs(regOUT.emmeans, adjust="none")

# Probability of First-Pass Refixations on Target (IA_FIRST_FIXATION_PREVIOUS_FIX_IA)

mod1.probofrefix <- mixed(refixProb~TargetPlaus*PreviewVal+(1|Subject)+(1|item), data = Plauseye2, 
                          family=binomial(logit),method = "LRT",
                          control = glmerControl(optCtrl = list(maxfun = 1e6)),
                          expand_re = TRUE)
mod1.probofrefix

refix.emmeans <- emmeans(mod1.probofrefix, specs = c ("TargetPlaus", "PreviewVal"))

# Do a pair-wise comparison to see what is driving the main effect
pairs(refix.emmeans, adjust="none")

##########################################################
# Code for generating means and st. error tables 
##########################################################

###***** First load the summarySE function using ddply() function
###*and the summarizeBY ()
###*and aggregate()
###*
library(plyr)
library(doBy)


#Frequency items (1) 
# Means and SD Table Data making (Freq then Plaus datasets)

# using dplyr

library(dplyr)

#Making a function for standard error of the mean

std.error <- function(x) sd(x)/sqrt(length(x))


#Maybe I need to separate out each measure, and then get the means and Std.Error 
# for each condition (Freq = "High/High" , "Low/Low", "High/Low", "Low/High")

#starting with sfd
#subsetting first 

sfd_freq<-result <- Freqeye2 %>% select("Subject", "condition", "sfd_trim")

# Now, to aggregate across subjects to get mean conditions 
ss$se<- aggregate(sfd_trim~condition, data=sfd_freq,mean)
# Consulted Brennan and he helped me add the SE column which is here: 

Fsfd<-aggregate(sfd_trim~condition, data=sfd_freq, FUN = function(x) c(mean = mean(x), se = std.error(x)))

#now to do the subsetting for the rest of the freq data set 

ffd_freq<-result <- Freqeye2 %>% select("Subject", "condition", "ffd_trim")
GD_freq<-result <- Freqeye2 %>% select("Subject", "condition", "GD_trim")
rpd_freq<-result <- Freqeye2 %>% select("Subject", "condition", "rpd_trim")
trt_freq<-result <- Freqeye2 %>% select("Subject", "condition", "trt_trim")


# getting means and se for each measure 

Fffd<-aggregate(ffd_trim~condition, data=ffd_freq, FUN = function(x) c(mean = mean(x), se = std.error(x)))
FGDd<-aggregate(GD_trim~condition, data=GD_freq, FUN = function(x) c(mean = mean(x), se = std.error(x)))
Frpd<-aggregate(rpd_trim~condition, data=rpd_freq, FUN = function(x) c(mean = mean(x), se = std.error(x)))
Ftrt<-aggregate(trt_trim~condition, data=trt_freq, FUN = function(x) c(mean = mean(x), se = std.error(x)))
#@@@@@@@@@@@@@@@ Doing this now for the probability based (saccadic) measures for frequency 

skip_freq<-result <- Freqeye2 %>% select("Subject", "condition", "IA_SKIP")
regout_freq<-result <- Freqeye2 %>% select("Subject", "condition","IA_REGRESSION_OUT")
refix_freq<-result <- Freqeye2 %>% select("Subject", "condition", "refixProb")

# getting means and se for each measure 

Fskip<-aggregate(IA_SKIP~condition, data=skip_freq, FUN = function(x) c(mean = mean(x), se = std.error(x)))
Fregout<-aggregate(IA_REGRESSION_OUT~condition, data=regout_freq, FUN = function(x) c(mean = mean(x), se = std.error(x)))
Frefix<-aggregate(refixProb~condition, data=refix_freq, FUN = function(x) c(mean = mean(x), se = std.error(x)))

####################################################################################################################
# Now to do all the same that is above, for the Plausibility dataset)
####################################################################################################################


sfd_plaus<-result <- Plauseye2 %>% select("Subject", "condition", "sfd_trim")

# Now, to aggregate across subjects to get mean conditions 
ss$se<- aggregate(sfd_trim~condition, data=sfd_plaus,mean)
# Consulted Brennan and he helped me add the SE column which is here: 

psfd<-aggregate(sfd_trim~condition, data=sfd_plaus, FUN = function(x) c(mean = mean(x), se = std.error(x)))

#now to do the subsetting for the rest of the plaus data set 

ffd_plaus<-result <- Plauseye2 %>% select("Subject", "condition", "ffd_trim")
GD_plaus<-result <- Plauseye2 %>% select("Subject", "condition", "GD_trim")
rpd_plaus<-result <- Plauseye2 %>% select("Subject", "condition", "rpd_trim")
trt_plaus<-result <- Plauseye2 %>% select("Subject", "condition", "trt_trim")

# getting means and se for each measure 

pffd<-aggregate(ffd_trim~condition, data=ffd_plaus, FUN = function(x) c(mean = mean(x), se = std.error(x)))
pGDd<-aggregate(GD_trim~condition, data=GD_plaus, FUN = function(x) c(mean = mean(x), se = std.error(x)))
prpd<-aggregate(rpd_trim~condition, data=rpd_plaus, FUN = function(x) c(mean = mean(x), se = std.error(x)))
ptrt<-aggregate(trt_trim~condition, data=trt_plaus, FUN = function(x) c(mean = mean(x), se = std.error(x)))
#@@@@@@@@@@@@@@@ Doing this now for the probability based (saccadic) measures for plausuency 

skip_plaus<-result <- Plauseye2 %>% select("Subject", "condition", "IA_SKIP")
regout_plaus<-result <- Plauseye2 %>% select("Subject", "condition", "IA_REGRESSION_OUT")
refix_plaus<-result <- Plauseye2 %>% select("Subject", "condition", "refixProb")

# getting means and se for each measure 

pskip<-aggregate(IA_SKIP~condition, data=skip_plaus, FUN = function(x) c(mean = mean(x), se = std.error(x)))
pregout<-aggregate(IA_REGRESSION_OUT~condition, data=regout_plaus, FUN = function(x) c(mean = mean(x), se = std.error(x)))
prefix<-aggregate(refixProb~condition, data=refix_plaus, FUN = function(x) c(mean = mean(x), se = std.error(x)))

