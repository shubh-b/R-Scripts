## Series of Time Stamps and Time Lengths extraction from a single Raw-Text File
## Insert the location of the file
path<-readline()

gsub("\\\\","/",path)

## Insert the file name
input<-readline()

text<-paste(".log")
input_text<-paste(input,text,sep="")
setwd(path)

## Packages Should be Installed and Loaded
library(stringr)
library(chron)
library(dplyr)
library(readr)

## Load the raw-text file
rt<-read_file(input_text)

## Split the text in respect to the string 'navigated'. 
navi<-str_split(rt,"navigated",simplify = T)

## Extract the last Qid attempted by the candidate.
lastQid<-word(str_split(navi[length(navi)],"ID",simplify = T)[6],2)
Qid<-c(word(navi[2:length(navi)],5),lastQid, "End")

## Date and time extraction of attempt of an item by the candidate in the event
date<-rep(word(str_split(rt,"At",simplify = T)[2],3),length(Qid))
Time<-function(x){
t<-c()
for(i in 2:length(navi)){
t[i]<-word(str_split(x[i],"Time",simplify = T)[2],4)
}
return(t)
}

## Identify the end time stamp for a test taker and extract the time series.
testend<-str_split(rt,"End Exam",simplify = T)
LastTimePoint<-word(str_split(testend[2],"Time",simplify = T)[2],4)
TimePoint<-c(Time(navi)[!Time(navi) %in% NA],rep(LastTimePoint,2))
TimeFrame<-data.frame(Qid, date, TimePoint)
timeform <- chron(times=TimeFrame$TimePoint)

## Calculate the time length per item spent by a candidate and construct the data frame. 
TimeLength<-chron(times=c(00:00:00,diff(timeform)))
TimeFrameLen<-cbind(TimeFrame,TimeLength)
TimeIntr<-data.frame(Qid=TimeFrame$Qid,Time=TimeLength)
TimeSumPerQn0<-TimeIntr%>%
group_by(Qid)%>%
summarise(Total.Time=sum(Time))
TimeSumPerQn1<-TimeSumPerQn0%>%
filter(Qid != "null")%>%
filter(Qid != "End")

## Construct the individual data frames for time stamps and time spent per Qn.
TimeSumPerQn1$Date<-rep(word(str_split(rt,"At",simplify = T)[2],3),length(TimeSumPerQn1$Qid))
TimeSumPerQn<-TimeSumPerQn1[,c("Qid","Date","Total.Time")]
TimeSpentOutPut<-paste(input,"-Time Spent per Qn.csv",sep="")
TimeFrameOutPut<-paste(input,"-Time Frame for Movements bw Qns.csv",sep="")

## Export the Out Put
write.csv(TimeSumPerQn,TimeSpentOutPut)
write.csv(TimeFrameLen,TimeFrameOutPut)
