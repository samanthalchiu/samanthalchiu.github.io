library(lme4)
library(readxl)
rm(list = ls())

# setwd("C:\\Users\\rmmcmurr\\Documents\\research\\NonLanguage\\audcat")
setwd("L:\\Experiments\\Current\\TalkerShift\\Blocked2Day\\results\\")
#ds <- read.table("L:\\Experiments\\Current\\audcat\\Exp 5B - 2cats replication\\results\\analysis\\ds.xlsx",header = TRUE, sep="\t")
ds <- read_excel("fullresults.xlsx", sheet = "trialdata")

#convert subject to a factor
ds$prolific_participant_id <- as.factor(ds$prolific_participant_id)
ds$condition <- as.factor(ds$condition)
ds$phase     <- as.factor(ds$phase)
ds$item      <- as.factor(ds$item)
ds$talker    <- as.factor(ds$talker)
ds$TalkerStatus <- as.factor(ds$TalkerStatus)
ds$TalkerDirection <- as.factor(ds$TalkerDirection)
#ds$pass_HC_check <- as.factor(ds$pass_HC_check)
ds$response <- as.factor(ds$response)


#rename trial and sub id
colnames(ds)[colnames(ds) == "count_trial"] <- "trial"
colnames(ds)[colnames(ds) == "prolific_participant_id"] <- "subject"

#delete some columsn we don't need
ds$...1 <-NULL
ds$date_startdate <-NULL
ds$correct <- NULL

#### EXCLUSIONS

#drop ds where VOTstep is none (those are background questions)
ds <- ds[ ds$vot_step != "None", ]
#got a couple of trials where the response wasn't recorded
ds <- ds[  !is.na(ds$p_response), ]

#exclude the subjects we need to exclude
#ds <- ds[ ds$pass_HC_check == 1, ]
ds <- ds[!(ds$TalkerDirection == "Exclude"), ]

#### RECODE VARIABLES

##recode VOT as a number
ds$vot <- as.numeric(ds$vot_step)
ds$vot <- ds$vot - mean(ds$vot)

#now construct numeric codes for the b/w subject IVs
ds$CC_trainedtalker  <- (ds$trainedtalker == "M") -.5 #male = 0.5, female = -0.5
ds$CC_traineddirection  <- (ds$trained_direction == "L") -.5 #left = 0.5, right = -0.5
ds$CC_TalkerStatus  <- (ds$TalkerStatus == "Untrained") -.5 #trained = -0.5, untrained = 0.5
ds$CC_session <- (ds$session == 2) - .5 #day 1 = -0.5, day 2 = 0.5
ds$CC_fulltraineddirection <- (ds$fullTrainDirection == "L->R") -.5 #L -> R = 0.5, R -> L = -0.5

#### MODELS
# 
# resultsA <- glmer( p_response ~ CC_trainedtalker*CC_traineddirection*CC_TalkerStatus*vot + (1 | subject), data=ds, family = binomial)
# resultsB <- glmer( p_response ~ CC_trainedtalker*CC_traineddirection*CC_TalkerStatus*vot + (1 | subject)+ (1 | item), data=ds, family = binomial)
# resultsC <- glmer( p_response ~ CC_trainedtalker*CC_traineddirection*CC_TalkerStatus*vot + (1 + vot || subject)+ (1 | item), data=ds, family = binomial)
# resultsD <- glmer( p_response ~ CC_trainedtalker*CC_traineddirection*CC_TalkerStatus*vot + (1 + vot || subject)+ (1 + vot || item), data=ds, family = binomial)
# resultsE <- glmer( p_response ~ CC_trainedtalker*CC_traineddirection*CC_TalkerStatus*vot + (vot + TalkerStatus || subject) + (vot +TalkerStatus + CC_trainedtalker + CC_traineddirection + CCTalkerStatus||item), data=ds, family = binomial)
# 
# anova(resultsA, resultsB, resultsC, resultsD)

## Day 1 models 
#sesh1.0 <- glmer(p_response ~ CC_trainedtalker*CC_traineddirection*CC_TalkerStatus*vot, data = subset(ds,session == 1 & !phase == "train"), family = binomial) 
#can we even have a model without random effects???? 

sesh1.1 <- glmer(p_response ~ CC_fulltraineddirection*CC_TalkerStatus*CC_trainedtalker + CC_fulltraineddirection*CC_TalkerStatus*vot + (1 | subject), data = subset(ds,session == 1 & !phase == "train"), family = binomial, control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun=2e5))) 
sesh1.2 <- glmer(p_response ~ CC_fulltraineddirection*CC_TalkerStatus*CC_trainedtalker + CC_fulltraineddirection*CC_TalkerStatus*vot + (1 | subject) + (1 | item), data = subset(ds,session == 1& !phase == "train"), family = binomial, control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun=2e5))) 
sesh1.3 <- glmer(p_response ~ CC_fulltraineddirection*CC_TalkerStatus*CC_trainedtalker + CC_fulltraineddirection*CC_TalkerStatus*vot + (1 + vot || subject)+ (1 | item), data = subset(ds,session == 1 & !phase == "train"), family = binomial, control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun=2e5))) 
sesh1.4 <- glmer(p_response ~ CC_fulltraineddirection*CC_TalkerStatus*CC_trainedtalker + CC_fulltraineddirection*CC_TalkerStatus*vot + (1 + vot || subject)+ (1 + vot || item), data = subset(ds,session == 1 & !phase == "train"), family = binomial, control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun=2e5))) 
sesh1.5 <- glmer(p_response ~ CC_fulltraineddirection*CC_TalkerStatus*CC_trainedtalker + CC_fulltraineddirection*CC_TalkerStatus*vot + (vot + CC_TalkerStatus || subject) + (1 + vot || item), data = subset(ds,session == 1 & !phase == "train"), family = binomial, control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun=2e5))) 
sesh1.6 <- glmer(p_response ~ CC_fulltraineddirection*CC_TalkerStatus*CC_trainedtalker + CC_fulltraineddirection*CC_TalkerStatus*vot + (vot + CC_TalkerStatus || subject) + (vot  + CC_trainedtalker ||item), data = subset(ds,session == 1& !phase == "train"), family = binomial, control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun=2e5))) 
sesh1.7 <- glmer(p_response ~ CC_fulltraineddirection*CC_TalkerStatus*CC_trainedtalker + CC_fulltraineddirection*CC_TalkerStatus*vot + (vot + CC_TalkerStatus || subject) + (vot  + CC_trainedtalker + CC_fulltraineddirection||item), data = subset(ds,session == 1 & !phase == "train"), family = binomial, control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun=2e5))) 
sesh1.8 <- glmer(p_response ~ CC_fulltraineddirection*CC_TalkerStatus*CC_trainedtalker + CC_fulltraineddirection*CC_TalkerStatus*vot + (vot + CC_TalkerStatus || subject) + (vot + CC_trainedtalker + CC_fulltraineddirection + CC_TalkerStatus||item), data = subset(ds,session == 1 & !phase == "train"), family = binomial, control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun=2e5))) 

## Day 2 models 
sesh2.1 <- glmer(p_response ~ CC_fulltraineddirection*CC_TalkerStatus*CC_trainedtalker + CC_fulltraineddirection*CC_TalkerStatus*vot + (1 | subject), data = subset(ds,session == 2 & !phase == "train"), family = binomial, control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun=2e5))) 
sesh2.2 <- glmer(p_response ~ CC_fulltraineddirection*CC_TalkerStatus*CC_trainedtalker + CC_fulltraineddirection*CC_TalkerStatus*vot + (1 | subject) + (1 | item), data = subset(ds,session == 2 & !phase == "train"), family = binomial, control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun=2e5))) 
sesh2.3 <- glmer(p_response ~ CC_fulltraineddirection*CC_TalkerStatus*CC_trainedtalker + CC_fulltraineddirection*CC_TalkerStatus*vot + (1 + vot || subject)+ (1 | item), data = subset(ds,session == 2 & !phase == "train"), family = binomial, control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun=2e5))) 
sesh2.4 <- glmer(p_response ~ CC_fulltraineddirection*CC_TalkerStatus*CC_trainedtalker + CC_fulltraineddirection*CC_TalkerStatus*vot + (1 + vot || subject)+ (1 + vot || item), data = subset(ds,session == 2 & !phase == "train"), family = binomial, control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun=2e5))) 
sesh2.5 <- glmer(p_response ~ CC_fulltraineddirection*CC_TalkerStatus*CC_trainedtalker + CC_fulltraineddirection*CC_TalkerStatus*vot + (vot + CC_TalkerStatus || subject) + (1 + vot || item), data = subset(ds,session == 2 & !phase == "train"), family = binomial, control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun=2e5))) 
sesh2.6 <- glmer(p_response ~ CC_fulltraineddirection*CC_TalkerStatus*CC_trainedtalker + CC_fulltraineddirection*CC_TalkerStatus*vot + (vot + CC_TalkerStatus || subject) + (vot  + CC_trainedtalker ||item), data = subset(ds,session == 2 & !phase == "train"), family = binomial, control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun=2e5))) 
sesh2.7 <- glmer(p_response ~ CC_fulltraineddirection*CC_TalkerStatus*CC_trainedtalker + CC_fulltraineddirection*CC_TalkerStatus*vot + (vot + CC_TalkerStatus || subject) + (vot  + CC_trainedtalker + CC_fulltraineddirection||item), data = subset(ds,session == 2 & !phase == "train"), family = binomial, control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun=2e5))) 
sesh2.8 <- glmer(p_response ~ CC_fulltraineddirection*CC_TalkerStatus*CC_trainedtalker + CC_fulltraineddirection*CC_TalkerStatus*vot + (vot + CC_TalkerStatus || subject) + (vot + CC_trainedtalker + CC_fulltraineddirection + CC_TalkerStatus||item), data = subset(ds,session == 2 & !phase == "train"), family = binomial, control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun=2e5))) 

sink("outputday1day2.txt")

anova(sesh1.1, sesh1.2, sesh1.3, sesh1.4, sesh1.5, sesh1.6, sesh1.7, sesh1.8)
anova(sesh2.1, sesh2.2, sesh2.3, sesh2.4, sesh2.5, sesh2.6, sesh2.7, sesh2.8)

sink()





#### models sesh*.5 are the best

## Day 1 vs Day 2 models 
sesh3.1 <- glmer(p_response ~ CC_fulltraineddirection*CC_session*CC_trainedtalker + CC_fulltraineddirection*CC_session*vot + (1 | subject), data = subset(ds,phase == "train"), family = binomial, control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun=2e5)))
sesh3.2 <- glmer(p_response ~ CC_fulltraineddirection*CC_session*CC_trainedtalker + CC_fulltraineddirection*CC_session*vot + (1 | subject) + (1 | item), data = subset(ds,phase == "train"), family = binomial, control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun=2e5)))
sesh3.3 <- glmer(p_response ~ CC_fulltraineddirection*CC_session*CC_trainedtalker + CC_fulltraineddirection*CC_session*vot + (1 + vot || subject)+ (1 | item), data = subset(ds,phase == "train"), family = binomial, control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun=2e5)))
sesh3.4 <- glmer(p_response ~ CC_fulltraineddirection*CC_session*CC_trainedtalker + CC_fulltraineddirection*CC_session*vot + (1 + vot || subject)+ (1 + vot || item), data = subset(ds,phase == "train"), family = binomial, control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun=2e5)))
sesh3.5 <- glmer(p_response ~ CC_fulltraineddirection*CC_session*CC_trainedtalker + CC_fulltraineddirection*CC_session*vot + (vot + CC_session || subject) + (1 + vot || item), data = subset(ds,phase == "train"), family = binomial, control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun=2e5))) 
sesh3.6 <- glmer(p_response ~ CC_fulltraineddirection*CC_session*CC_trainedtalker + CC_fulltraineddirection*CC_session*vot + (vot + CC_session || subject) + (vot + CC_trainedtalker ||item), data = subset(ds,phase == "train"), family = binomial, control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun=2e5)))
sesh3.7 <- glmer(p_response ~ CC_fulltraineddirection*CC_session*CC_trainedtalker + CC_fulltraineddirection*CC_session*vot + (vot + CC_session|| subject) + (vot + CC_trainedtalker + CC_fulltraineddirection ||item), data = subset(ds,phase == "train"), family = binomial, control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun=2e5)))
sesh3.8 <- glmer(p_response ~ CC_fulltraineddirection*CC_session*CC_trainedtalker + CC_fulltraineddirection*CC_session*vot + (vot + CC_session + CC_trainedtalker|| subject) + (vot + CC_trainedtalker + CC_fulltraineddirection ||item), data = subset(ds,phase == "train"), family = binomial, control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun=2e5))) 
sesh3.9 <- glmer(p_response ~ CC_fulltraineddirection*CC_session*CC_trainedtalker + CC_fulltraineddirection*CC_session*vot + (vot + CC_session + CC_trainedtalker || subject) + (vot + CC_trainedtalker + CC_fulltraineddirection + CC_session||item), data = subset(ds,phase == "train"), family = binomial, control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun=2e5))) 
#sesh3.10 <- glmer(p_response ~ CC_fulltraineddirection*CC_session*CC_trainedtalker + CC_fulltraineddirection*CC_session*vot + (vot + CC_session + CC_trainedtalker|| subject) + (vot + CC_trainedtalker + CC_fulltraineddirection + CC_session||item), data = subset(ds,phase == "train"), family = binomial, control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun=2e5))) 
# do i need CC_trainedtalker for subject now too? 

sesh4.1 <- glmer(p_response ~ CC_fulltraineddirection*CC_session*CC_trainedtalker + CC_fulltraineddirection*CC_session*CC_TalkerStatus*vot + (1 | subject), data = subset(ds,!phase == "train"), family = binomial, control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun=2e5)))
sesh4.2 <- glmer(p_response ~ CC_fulltraineddirection*CC_session*CC_trainedtalker + CC_fulltraineddirection*CC_session*CC_TalkerStatus*vot + (1 | subject) + (1 | item), data = subset(ds,!phase == "train"), family = binomial, control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun=2e5)))
sesh4.3 <- glmer(p_response ~ CC_fulltraineddirection*CC_session*CC_trainedtalker + CC_fulltraineddirection*CC_session*CC_TalkerStatus*vot + (1 + vot || subject)+ (1 | item), data = subset(ds,!phase == "train"), family = binomial, control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun=2e5)))
sesh4.4 <- glmer(p_response ~ CC_fulltraineddirection*CC_session*CC_trainedtalker + CC_fulltraineddirection*CC_session*CC_TalkerStatus*vot + (1 + vot || subject)+ (1 + vot || item), data = subset(ds,!phase == "train"), family = binomial, control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun=2e5)))
sesh4.5 <- glmer(p_response ~ CC_fulltraineddirection*CC_session*CC_trainedtalker + CC_fulltraineddirection*CC_session*CC_TalkerStatus*vot + (vot + CC_session || subject) + (1 + vot || item), data = subset(ds,!phase == "train"), family = binomial, control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun=2e5))) 
sesh4.6 <- glmer(p_response ~ CC_fulltraineddirection*CC_session*CC_trainedtalker + CC_fulltraineddirection*CC_session*CC_TalkerStatus*vot + (vot + CC_session || subject) + (vot + CC_trainedtalker ||item), data = subset(ds,!phase == "train"), family = binomial, control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun=2e5)))
sesh4.7 <- glmer(p_response ~ CC_fulltraineddirection*CC_session*CC_trainedtalker + CC_fulltraineddirection*CC_session*CC_TalkerStatus*vot + (vot + CC_session|| subject) + (vot + CC_trainedtalker + CC_fulltraineddirection ||item), data = subset(ds,!phase == "train"), family = binomial, control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun=2e5)))
sesh4.8 <- glmer(p_response ~ CC_fulltraineddirection*CC_session*CC_trainedtalker + CC_fulltraineddirection*CC_session*CC_TalkerStatus*vot + (vot + CC_session + CC_trainedtalker|| subject) + (vot + CC_trainedtalker + CC_fulltraineddirection ||item), data = subset(ds,!phase == "train"), family = binomial, control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun=2e5))) 
sesh4.9 <- glmer(p_response ~ CC_fulltraineddirection*CC_session*CC_trainedtalker + CC_fulltraineddirection*CC_session*CC_TalkerStatus*vot + (vot + CC_session + CC_trainedtalker || subject) + (vot + CC_trainedtalker + CC_fulltraineddirection + CC_session||item), data = subset(ds,!phase == "train"), family = binomial, control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun=2e5))) 
#sesh3.10 <- glmer(p_response ~ CC_fulltraineddirection*CC_session*CC_trainedtalker + CC_fulltraineddirection*CC_session*vot + (vot + CC_session + CC_trainedtalker|| subject) + (vot + CC_trainedtalker + CC_fulltraineddirection + CC_session||item), data = subset(ds,phase == "train"), family = binomial, control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun=2e5))) 

sink("outputtrain.2.txt")
anova(sesh3.1, sesh3.2, sesh3.3, sesh3.4, sesh3.5, sesh3.6, sesh3.7, sesh3.8, sesh3.9)
sink()

sink("outputtestxsession.txt")
anova(sesh4.1, sesh4.2, sesh4.3, sesh4.4, sesh4.5, sesh4.6, sesh4.7, sesh4.8, sesh4.9)
sink()

sink("fixedeffect_testsession.txt")
summary(sesh4.5)
#summary(sesh2.5)
sink()

sink("fixedeffect_test.txt")
summary(sesh1.5)
summary(sesh2.5)
sink()


#### sesh3.7 is best

sink("fixedeffect_train.txt")
summary(sesh3.7)
sink()

# full train direction: L -> R = 0.5, R -> L = -0.5
#trainedtalker: M = 0.5, F = -0.5
sesh1.5LR <- glmer(p_response ~ CC_TalkerStatus*CC_trainedtalker*vot + (vot + CC_TalkerStatus || subject) + (1 + vot || item), data = subset(ds,session == 1 & !phase == "train" & CC_fulltraineddirection == 0.5), family = binomial, control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun=2e5))) 
sesh1.5RL <- glmer(p_response ~ CC_TalkerStatus*CC_trainedtalker*vot + (vot + CC_TalkerStatus || subject) + (1 + vot || item), data = subset(ds,session == 1 & !phase == "train" & CC_fulltraineddirection == -0.5), family = binomial, control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun=2e5))) 

sink("day1interactions.txt")
summary(sesh1.5LR)
summary(sesh1.5RL)
sink()


#talker status x train direction 
#talker status x gender 

#L -> R = 0.5, R -> L = -0.5
sesh2.5LR <- glmer(p_response ~ CC_TalkerStatus*vot*CC_trainedtalker + (vot + CC_TalkerStatus || subject) + (1 + vot || item), data = subset(ds,session == 2 & !phase == "train" &  CC_fulltraineddirection == 0.5), family = binomial, control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun=2e5))) 
sesh2.5RL <- glmer(p_response ~ CC_TalkerStatus*vot*CC_trainedtalker + (vot + CC_TalkerStatus || subject) + (1 + vot || item), data = subset(ds,session == 2 & !phase == "train"&  CC_fulltraineddirection == -0.5), family = binomial, control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun=2e5))) 

sesh2.5M <- glmer(p_response ~ CC_fulltraineddirection*CC_TalkerStatus*vot + (vot + CC_TalkerStatus || subject) + (1 + vot || item), data = subset(ds,session == 2 & !phase == "train" & CC_trainedtalker == 0.5), family = binomial, control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun=2e5))) 
sesh2.5F <- glmer(p_response ~ CC_fulltraineddirection*CC_TalkerStatus*vot + (vot + CC_TalkerStatus || subject) + (1 + vot || item), data = subset(ds,session == 2 & !phase == "train" & CC_trainedtalker == -0.5), family = binomial, control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun=2e5))) 

sink("day2interactions.txt")
summary(sesh2.5LR)
summary(sesh2.5RL)
sink()

sesh3.7LR <- glmer(p_response ~ CC_session*vot + (vot + CC_session|| subject) + (vot + CC_trainedtalker + CC_fulltraineddirection ||item), data = subset(ds,phase == "train" &  CC_fulltraineddirection == 0.5), family = binomial, control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun=2e5)))
sesh3.7RL <- glmer(p_response ~ CC_session*vot + (vot + CC_session|| subject) + (vot + CC_trainedtalker + CC_fulltraineddirection ||item), data = subset(ds,phase == "train" &  CC_fulltraineddirection == -0.5), family = binomial, control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun=2e5)))


sink("traininteraction.txt")
summary(sesh3.7LR)
summary(sesh3.7RL)
sink()


