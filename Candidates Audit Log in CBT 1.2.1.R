## Series of Time Stamps and Time Lengths extraction from multiple Raw-Text Files saved in a folder.
## Insert the inside folder location path where all the files are saved.
path<-readline()

gsub("\\\\","/",path)
setwd(path)

## List of all the files saved inside the folder.
AuditFiles<-list.files(path, pattern = "log$")

## Packages Should be Installed and Loaded
library(stringr)
library(chron)
library(dplyr)
library(readr)

## Make NULL lists and NULL vectors for every object inside the for loop
TFrRpt<-list();TSpRpt<-list();input_text<-c();rt<-list();navi<-list();lastQid<-list();Qid<-list();date<-list()
Time<-list();testend<-list();LastTimePoint<-list();TimePoint<-list();TimeFrame<-list()
timeform<-list();TimeLength<-list();TimeFrameLen<-list();TimeIntr<-list();TimeSumPerQn0<-list()
TimeSumPerQn1<-list();TimeSumPerQn<-list();TFrRptName<-c();TSpRptName<-c()

## Load the raw-text files
for(j in 1:length(AuditFiles))
{
rt[[j]]<-read_file(AuditFiles[j])

## Split the text in respect to the string 'navigated'. 
navi[[j]]<-str_split(rt[[j]],"navigated",simplify = T)

## Extract the last Qid attempted by the candidate.
lastQid[[j]]<-word(str_split(navi[[j]][length(navi[[j]])],"ID",simplify = T)[6],2)
Qid[[j]]<-c(word(navi[[j]][2:length(navi[[j]])],5),lastQid[[j]], "End")

## Date and time extraction of attempt of an item by the candidate in the event/file
date[[j]]<-rep(word(str_split(rt[[j]],"At",simplify = T)[2],3),length(Qid[[j]]))
Time[[j]]<-function(x){
t<-c()
for(i in 2:length(navi[[j]])){
t[i]<-word(str_split(x[i],"Time",simplify = T)[2],4)
}
return(t)
}

## Identify the end time stamp for a test taker and extract the time series.
testend[[j]]<-str_split(rt[[j]],"End Exam",simplify = T)
LastTimePoint[[j]]<-word(str_split(testend[[j]][2],"Time",simplify = T)[2],4)
TimePoint[[j]]<-c(Time[[j]](navi[[j]])[!Time[[j]](navi[[j]]) %in% NA],rep(LastTimePoint[[j]],2))
TimeFrame[[j]]<-data.frame(Qid=Qid[[j]], TimePoint=TimePoint[[j]])
timeform[[j]] <- chron(times=TimeFrame[[j]]$TimePoint)

## Calculate the time length per item spent by a candidate and construct the data frame.
TimeLength[[j]]<-chron(times=c(00:00:00,diff(timeform[[j]])))
DATE<-as.character(word(str_split(rt[[j]],"At",simplify = T)[2],3))
TimeFrameLen[[j]]<-cbind(TimeFrame[[j]],TimeLength=TimeLength[[j]])
TimeIntr[[j]]<-data.frame(Qid=TimeFrame[[j]]$Qid,Time=TimeLength[[j]])
TimeSumPerQn0[[j]]<-TimeIntr[[j]]%>%
group_by(Qid)%>%
summarise(Total.Time=sum(Time))
TimeSumPerQn1[[j]]<-TimeSumPerQn0[[j]]%>%
filter(Qid != "null")%>%
filter(Qid != "End")

## Construct the individual data frames per event/file for time stamps and time spent per Qn.
TimeSumPerQn1[[j]]$Date<-rep(word(str_split(rt[[j]],"At",simplify = T)[2],3),length(TimeSumPerQn1[[j]]$Qid))
TimeSumPerQn[[j]]<-TimeSumPerQn1[[j]][,c("Qid","Total.Time")]
TFrRpt[[j]]<-TimeFrameLen[[j]]
TSpRpt[[j]]<-TimeSumPerQn[[j]]
input_text[j]<-strsplit(AuditFiles[j],"\\.")[[1]][1]
TFrRptName[j]<-paste(input_text[j],"-(",DATE,")-Time Frame for Movements bw Qns",sep="")
TSpRptName[j]<-paste(input_text[j],"-(",DATE,")-Time Spent per Qn",sep="")
}
names(TFrRpt)<-TFrRptName
names(TSpRpt)<-TSpRptName

## Create a folder where to save outputs
dir.create(file.path(path,"Audit Log Time Analysis Reports"), showWarnings = FALSE)
OutPath<-paste(path,"\\Audit Log Time Analysis Reports\\",sep="")

## Save the output .csv files in the newly created folder
for(k in names(TSpRpt)){
write.csv(TSpRpt[[k]], paste0(OutPath, k,".csv"))
}
for(k in names(TFrRpt)){
write.csv(TFrRpt[[k]], paste0(OutPath, k,".csv"))
}
