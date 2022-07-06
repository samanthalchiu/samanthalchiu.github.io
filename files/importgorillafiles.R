#combine gorilla data
#
#need csv files
rm(list=ls(all=TRUE))
#set directories
#stemwd <- ("\\\\iowa.uiowa.edu\\shared\\ResearchData\\rdss_rmmcmurr\\McMurrayLab\\Experiments\\Current\\audcat\\Exp 5 - 2cats\\results\\2 - sort by task\\")
stemwd <- ("L:\\Experiments\\Current\\L2VAS\\results\\datacleaning\\gorilla data\\")
setwd(stemwd)
# creates 3 different directories
#stemwds <- paste(stemwd,foldernames, sep="")
destinationwd <- ("L:\\Experiments\\Current\\L2VAS\\results\\")

#make list of audVAS files 
audgfiles <- c('qv9s','skcx','35i5','vevw')
#get list of files in directory 
list <- list.files(stemwd,pattern="data_exp")
#find indexes of files we need here
audfileidx <- c(which(grepl(audgfiles[1],list, fixed=TRUE),arr.ind = TRUE), which(grepl(audgfiles[2],list, fixed=TRUE), arr.ind = TRUE),which(grepl(audgfiles[3],list, fixed=TRUE),arr.ind = TRUE),which(grepl(audgfiles[4],list, fixed=TRUE),arr.ind = TRUE))
#find the names of those files 
audlist <- list[audfileidx]
#read them all in
fulldata <- lapply(audlist, FUN=read.csv)
#combine them
data1 <- do.call("rbind", fulldata[lengths(fulldata)==59])
colnames(data1)[grep("leftpic",colnames(data1))] <- "leftresp"
colnames(data1)[grep("rightpic",colnames(data1))] <- "rightresp"
data2 <- do.call("rbind", fulldata[lengths(fulldata)==66])

data2A <-subset(data2,leftresp == 'leftrespA')
data2B <-subset(data2,leftresp == 'leftrespB')

data2A$leftresp <- data2A$leftrespA
data2A$rightresp <- data2A$rightrespA

data2B$leftresp <- data2B$leftrespB
data2B$rightresp <- data2B$rightrespB

data2 <- rbind(data2A,data2B)

# list of columns we need 
audcolsToKeep <- c("Participant.Public.ID","itemset","step","leftresp","rightresp","Response","Reaction.Time","Local.Date")
#subset by columns we need and rows that have the endValue response in them
dataToKeep1 = subset(data1, Zone.Type == "response_slider_endValue" & display == "task", select = audcolsToKeep)
dataToKeep2 = subset(data2, Zone.Type == "response_slider_endValue" & display == "task", select = audcolsToKeep)


dataToKeep <- rbind(dataToKeep1,dataToKeep2)

dataToKeep$adjtrial <- as.integer(substr(dataToKeep$rightresp, start = 1, stop = 2) == substr(dataToKeep$itemset, start = 1, stop = 2))
dataToAdj = subset(dataToKeep,adjtrial == 1)
dataToKeep = subset(dataToKeep, !adjtrial == 1)
dataToAdj$adjresp <- 100 - as.integer(dataToAdj$Response)
dataToKeep$adjresp <- dataToKeep$Response

dataToKeep <- rbind(dataToKeep,dataToAdj)
audcolsToKeep.2 <- c("Participant.Public.ID","itemset","step","adjresp","Reaction.Time","Local.Date")
dataToKeep <- subset(dataToKeep, select = audcolsToKeep.2)

#save it 
date <- Sys.Date()
audfilename <- paste("audVASdata", date, "csv", sep=".")

#check if this filename exists already 
list <- list.files(destinationwd,pattern=audfilename)

if (length(list) == 0){
  
} else {
  num2add <- toString(length(list)+1)
  audfilename <- paste("audVASdata", "csv", sep=".")
}
setwd(destinationwd)
write.csv(dataToKeep, audfilename)

########## VISVAS DATA ##########

setwd(stemwd)
visgfile <- 'gsnm'
list <- list.files(stemwd,pattern="data_exp")
visfileidx <- which(grepl(visgfile,list, fixed=TRUE),arr.ind = TRUE)
#read data in 
vislist <- list[visfileidx]
visdata <- lapply(vislist, FUN=read.csv)
visdata1 <- do.call("rbind", visdata[lengths(visdata)==58])
visdata2 <- do.call("rbind", visdata[lengths(visdata)==60])
viscolsToKeep <- c("Participant.Public.ID","step","leftresp","rightresp","Response","Reaction.Time","Local.Date")
visdataToKeep1 = subset(visdata1, Zone.Type == "response_slider_endValue" & display == "task", select = viscolsToKeep)
visdataToKeep2 = subset(visdata2, Zone.Type == "response_slider_endValue" & display == "task", select = viscolsToKeep)

visdataToKeep <- rbind(visdataToKeep1,visdataToKeep2)

#save it 
date <- Sys.Date()
visfilename <- paste("visVASdata", "csv", sep=".")

#check if this filename exists already 
list <- list.files(destinationwd, pattern=visfilename)

if (length(list) == 0){
  
} else {
  num2add <- toString(length(list)+1)
  visfilename <- paste("visVASdata", "csv", sep=".")
}
setwd(destinationwd)
write.csv(visdataToKeep, visfilename)