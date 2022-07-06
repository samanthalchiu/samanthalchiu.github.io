rm(list=ls(all=TRUE))

#update.packages()

#devtools::install_github("tidyverse/tidyverse")

#library(tidyverse)
setwd('L:\\Experiments\\Current\\L2VAS\\results\\')
ds<- read.table('L:\\Experiments\\Current\\L2VAS\\results\\LanguageQuestionnair_DATA_2022-07-04_1513.csv',header = TRUE, sep = ",")

#ds$record_id <- as.factor(ds$?..record_id)

#remove NULL or incomplete participants 
ds = ds[-which(is.na(ds$difficulties) == TRUE),]

# do lextale results 

lextalecols <- c("partid", "wordscorrect","nonwordscorrect","score")
lextaledata <- subset(ds, select=lextalecols)
#find lextale questions to remove
rmlextalecols <-seq(5,71,by=1)
#remove them
ds<- ds[-rmlextalecols]

#remove timestamp and progress complete columns
rmextracols <- c(grep("complete",colnames(ds)),grep("timestamp",colnames(ds)),grep("age_months",colnames(ds)))
ds<-ds[-rmextracols]

democols <- c(grep("partid",colnames(ds)),seq(4,grep("numlanguages",colnames(ds)),by=1),seq(grep("country_of_origin",colnames(ds)),grep("langtraveluse1",colnames(ds)),by=1),
              seq(grep("elementarylanguage",colnames(ds)),grep("doctorboth",colnames(ds)),by=1),seq(grep("language_testyn",colnames(ds)),grep("testscore2",colnames(ds)),by=1),grep("language_learning_skill",colnames(ds)) )
LHQcols <- c(grep("partid",colnames(ds)),seq(4,grep("comment_box_dialects",colnames(ds)),by=1))
LHQcols <- c(grep("partid",colnames(ds)),setdiff(c(4:length(LHQcols)),democols))
SLScols <- c(grep("partid",colnames(ds)),seq(grep("ego_trans",colnames(ds)),length(ds),by=1))
LHQcolnames<- colnames(ds)[LHQcols[2:length(LHQcols)]]
SLScolnames<- colnames(ds)[SLScols[2:length(SLScols)]]

######## CLEAN LHQ DATA #######
#remove additional demo information from LHQ 

demods <- subset(ds,select=democols)
demods <- demods[ ,colSums(is.na(demods)) != nrow(demods)]

##### turn LHQ into long data #####

LHQds <- subset(ds,select=LHQcols)
#reformat into long form 
LHQds <- reshape(data=LHQds, idvar="partid", timevar = "question",
                         varying = LHQcolnames,
                         v.name="response",
                         times=LHQcolnames,
                         new.row.names = 1:10000000,
                         direction="long")

LHQds<- na.omit(LHQds)
LHQds <-LHQds[-which(LHQds$response==""),]
#LHQds <-LHQds[-which(LHQds$partid==""),]

#subset by all questions that contain race & have a response 
fixds <- subset(LHQds, grepl("___", LHQds[,"question"]))
#extract the name of the question (extract first part before the _ )
fixds$newquestion<- sub("___.*","",fixds$question)
#extract the response (response goes from binary 1 or 0 to the actual race)
fixds$newresponse <- sub(".*___","",fixds$question)
#get rid of old questions
fixds$question <- NULL
fixds$response <- NULL 
#rename the columns
colnames(fixds) <- c("partid","question","response")
#remove all the race questions from full dataset 
LHQds <- subset(LHQds, !grepl("___", LHQds[,"question"]))
#replace them with the new ones 
LHQds <- rbind(LHQds,fixds)

LHQds$langnum <- as.numeric(gsub(".*?([0-9]+).*", "\\1", LHQds$question))
LHQds$variable <- gsub("[^a-zA-Z]", "", LHQds$question)

sublist <- unique(LHQds$partid)
varlist <- unique(LHQds$variable)
langnumlist <- unique(LHQds$langnum)
langnumlist <- langnumlist[!is.na(langnumlist)]

LHQds$question <- NULL

##### REFORMAT LHQ INTO WIDE DATA ######

LHQds <- reshape(data=LHQds,idvar=c("partid","langnum"),
                          v.names = "response",
                          timevar = "variable",
                          direction="wide")
#now remove "response." from the column names 
colnames(LHQds) <- sub(".*response.","",colnames(LHQds))


#randsubdata <- subset(LHQds, LHQds$partid == "5da21ef67284fa0010e1e11e", )


######## CLEAN SLS DATA #######

SLSds <- subset(ds,select=SLScols)
SLSds <- reshape(data=SLSds, idvar="partid", timevar = "question",
                 varying = SLScolnames,
                 v.name="response",
                 times=SLScolnames,
                 new.row.names = 1:100000000,
                 direction="long")

SLSds<- na.omit(SLSds)
#SLSds <-SLSds[-which(SLSds$response==""),]
#SLSds <-SLSds[-which(SLSds$partid==""),]

######## REMOVE IDENTIFIERS #######

#name name columns; the first argument is "name" 15 times, the second argument is 1:15 as a character; this combines the two with no delim
namecols <- paste(rep("name",times = 15), as.character(1:15),sep = "")
#subset data by questions that do not contain any of the exact strings from namecols 
SLSds <- subset(SLSds, !question %in% namecols)

##### FIX RACE QUESTIONS ######

#subset by all questions that contain race & have a response 
raceds <- subset(SLSds, grepl("race", SLSds[,"question"]) & response == 1)
#extract the name of the question (extract first part before the _ )
raceds$newquestion<- sub("_.*","",raceds$question)
#extract the response (response goes from binary 1 or 0 to the actual race)
raceds$newresponse <- sub(".*_","",raceds$question)
#get rid of old questions
raceds$question <- NULL
raceds$response <- NULL 
#rename the columns
colnames(raceds) <- c("partid","question","response")
#remove all the race questions from full dataset 
SLSds <- subset(SLSds, !grepl("race", SLSds[,"question"]))
#replace them with the new ones 
SLSds <- rbind(SLSds,raceds)

demofname = paste('demoLHQdata','csv',sep = ".")
lexfname = paste('lextaleresults','csv',sep = ".")
LHQfname = paste('LHQresults','csv',sep = ".")
SLSfname = paste('SLSresults','csv',sep = ".")

write.csv(demods, demofname)
write.csv(lextaledata, lexfname)
write.csv(LHQds, LHQfname)
write.csv(SLSds, SLSfname)

