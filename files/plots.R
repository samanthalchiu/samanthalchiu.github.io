rm(list=ls(all=TRUE))

#update packages
update.packages()

library(tidyverse)
library(grid)
library(ggplot2)
library(readxl)

ds <- read_xlsx('C:\\Users\\saman\\OneDrive\\Documents\\TS_data.xlsx',sheet = "data")
ds <- na.omit(ds)
#subset by not excluded 
ds <- subset(ds, !Include == "Exclude")
colnames(ds)[colnames(ds) == "count_trial"] <- "trial"
colnames(ds)[colnames(ds) == "prolific_participant_id"] <- "subject"
colnames(ds)[colnames(ds) == "condition number"] <- "cond"
#drop ds where VOTstep is none (those are background questions)
ds <- ds[ ds$vot_step != "None", ]
#got a couple of trials where the response wasn't recorded
ds <- ds[  !is.na(ds$p_response), ]
ds$p_response <- as.numeric(ds$p_response)
ds$vot_step <- as.factor(ds$vot_step)
ds$TalkerStatus <- as.factor(ds$TalkerStatus)
ds$fullTrainDirection <- as.factor(ds$fullTrainDirection)
ds$trainedtalker <- as.factor(ds$trainedtalker)
ds$session <- as.factor(ds$session)

day1ds <- subset(ds, session == 1 & !phase == "train")
day2ds <- subset(ds, session == 2 & !phase == "train")
trainds <- subset(ds, phase == "train")
testds <- subset(ds, !phase == "train")

#lets check how many ppl per condition just to make sure its right. 
condds <- subset(ds,,c("cond","subject"))
condds <- unique(condds)
condds$cond <- as.factor(condds$cond)
summary(condds)

############################### MAIN ANALYSES ##########################
#### plot 1: subset day 1, VOT x prop p response, split by talker status and talker direction

plot1ds <- subset(day1ds,, c("TalkerStatus", "fullTrainDirection", "vot_step", "p_response"))
#colnames(plot1ds)[colnames(plot1ds) == "fullTrainDirection"] <- "Training Direction"
g1 <- ggplot(plot1ds, aes(x = factor(vot_step, levels = c("3","4","5","6","7","8","9","10")), y = p_response, group = interaction(fullTrainDirection,TalkerStatus))) + 
  stat_summary(fun = "mean", geom = "line", size = 1.25, show.legend = TRUE, aes(color = fullTrainDirection, linetype = TalkerStatus))
g2 <- g1 + theme_bw() + scale_x_discrete(name = "VOT Step") + scale_y_continuous(name = "Proportion /p/ response")
g3 <- g2 + labs(caption = expression(paste(italic('Figure 3. '),"Proportion of voiceless responses to trained and untrained talkers in session 1.")))
g4 <- g3 + theme(plot.caption = element_text(hjust = 0,size = 12)) 

print(g4)

#ggsave("results/plot1.g",
#  width = 12, height = 6, dpi = 300) 

#### plot 2: subset day 2, VOT x prop p response, split by talker status and talker direction

plot2ds <- subset(day2ds,, c("TalkerStatus", "fullTrainDirection", "vot_step", "p_response"))
g1 <- ggplot(plot2ds, aes(x = factor(vot_step, levels = c("3","4","5","6","7","8","9","10")), y = p_response, group = interaction(fullTrainDirection,TalkerStatus))) + 
  stat_summary(fun = "mean", geom = "line", size = 1.25, show.legend = TRUE, aes(color = fullTrainDirection, linetype = TalkerStatus))
g2 <- g1 + theme_bw() + scale_x_discrete(name = "VOT Step") + scale_y_continuous(name = "Proportion /p/ response")
#g3 <- g2 + labs(c("TalkerStatus", "Training Direction"))


print(g2)
#need to add colors and legend 
#ggsave("results/plot2.png",
#       width = 12, height = 6, dpi = 300) 

#### ANOTHER PLOT: Session 2, subset by gender, split by talker status and direction 

plot2ds <- subset(day2ds,, c("trainedtalker","TalkerStatus", "fullTrainDirection", "vot_step", "p_response"))
g1 <- ggplot(plot2ds, aes(x = factor(vot_step, levels = c("3","4","5","6","7","8","9","10")), y = p_response, group = interaction(fullTrainDirection,TalkerStatus))) + 
  stat_summary(fun = "mean", geom = "line", size = 1.25, show.legend = TRUE, aes(color = fullTrainDirection, linetype = TalkerStatus)) + facet_wrap(~ trainedtalker)
g2 <- g1 + theme_bw() + scale_x_discrete(name = "VOT Step") + scale_y_continuous(name = "Proportion /p/ response")
g3 <- g2 + theme(plot.caption = element_text(hjust = 0,size = 12),panel.grid.major = element_blank(), panel.grid.minor = element_blank()) 
#g3 <- g2 + labs(c("TalkerStatus", "Training Direction"))

print(g3)
#### plot 3: subset train data, VOT x prop p response, split by session and talker direction 

plot3ds <- subset(trainds,, c("session", "fullTrainDirection", "vot_step", "p_response"))
g1 <- ggplot(plot3ds, aes(x = factor(vot_step, levels = c("1","2","3","4","5","6","7","8","9","10","11","12")), y = p_response, group = interaction(fullTrainDirection,session))) + 
  stat_summary(fun = "mean", geom = "line", size = 1.25, show.legend = TRUE, aes(color = fullTrainDirection, linetype = session))
g2 <- g1 + theme_bw() + scale_x_discrete(name = "VOT Step") + scale_y_continuous(name = "Proportion /p/ response")
#g3 <- g2 + labs(c("TalkerStatus", "Training Direction"))


plot4ds <- subset(testds,, c("session", "fullTrainDirection", "TalkerStatus", "vot_step",  "p_response"))
g1 <- ggplot(plot4ds, aes(x = factor(vot_step, levels = c("1","2","3","4","5","6","7","8","9","10","11","12")), y = p_response, group = fullTrainDirection )) + 
  stat_summary(fun = "mean", geom = "line", size = 1.25, show.legend = TRUE, aes(color = fullTrainDirection, linetype = TalkerStatus)) + facet_wrap(~ session+ TalkerStatus )
g2 <- g1 + theme_bw() + scale_x_discrete(name = "VOT Step") + scale_y_continuous(name = "Proportion /p/ response") 
g3 <- g2 + 
  label =
    c(italic('Figure 2A-D: '),"Proportion of voiceless response to trained and untrained talkers in session 1 and session 2. 
    Panel A, top left: Response to trained talker in session 1. Panel B, top right: Response to untrained talker in session 1. 
    Panel C, bottom left: Response to trained talker in session 2. Panel D, bottom right: Response to untrained talker in session 2.", 
  hjust = 0)
  
  
  #labs(caption = expression(atop(italic('Figure 2A-D: '),paste("Proportion of voiceless response to trained and untrained talkers in session 1 and session 2. Panel A, top left: Response to trained talker in session 1. Panel B, top right: Response to untrained talker in session 1. Panel C, bottom left: Response to trained talker in session 2. Panel D, bottom right: Response to untrained talker in session 2."))))
g4 <- g3 + theme(plot.caption = element_text(hjust = 0,size = 12)) #+ wrap_labs(g3, "normal")

print(g4)

#g3 <- g2 + labs(c("TalkerStatus", "Training Direction"))


