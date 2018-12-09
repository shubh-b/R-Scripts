## Item Analysis Report generation from a single Event's Response Sheet of Test-Takers.
fileName<-readline()

path<-readline()

gsub("\\\\","/",path)
setwd(path)

packages = c("dplyr","readxl","CTT")
package.check <- lapply(packages, FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
        install.packages(x, dependencies = TRUE)
        library(x, character.only = TRUE)
    }
})

input<-paste(fileName,".xlsx",sep="")
myfile0<-read_excel(input,col_names = F)
myfile00<-myfile0%>%
	select(-c(1:12))

## Whole Data with QId
myfile00<-myfile00[-c(1:6),]		
colnames(myfile00)<-as.vector(myfile0[4,-c(1:12)],"character")

## Evaluate Difficulty Index
myfile<-myfile00[-1,]
myfilekeys<-as.vector(myfile00[1,],"character")
myscores<-score(myfile,myfilekeys,output.scored=TRUE)
xx<-myscores$scored
 attempted<-colSums(myfile!="X")
 corrected<-colSums(xx!=0)
 Difficulty.Index<-round((1-corrected/attempted),3)


names(Difficulty.Index)<-NULL
QId<-colnames(myfile00)
TopicName<-as.vector(myfile0[2,-(1:12)],"character")
SubTopic<-as.vector(myfile0[3,-(1:12)],"character")
QBM.Diffi<-as.vector(myfile0[6,-(1:12)],"character")

diff<-data.frame(QId,Difficulty.Index,TopicName,SubTopic,QBM.Diffi)


topics<-levels(factor(as.vector(myfile0[2,-(1:12)],"character")))
myfile_split0<-myfile0[-c(1,3,4,5,6),-c(1:12)]
colnames(myfile_split0)<-as.vector(myfile0[4,-c(1:12)],"character")


## Topic-Wise Split of the Data File and Separate Analysis for Biserial Correlation and Discrimination Index.
## Null lists of the objects

myfile_split00<-list();myfile_split<-list();myfile_split_keys<-list();myscores<-list()
x<-list();P<-list();score.level<-list();hni<-list();lni<-list();QId<-list()
discrim.index<-list();TopicOV<-list();biserial.corr<-list()

for(i in 1:length(topics)){
myfile_split00[[i]]<-myfile_split0[ , c(myfile_split0[1,]==topics[i])][-1,]
myfile_split[[i]]<-myfile_split00[[i]][-1,]
myfile_split_keys[[i]]<-as.vector(myfile_split00[[i]][1,],"character")

##Topic Wise Calculations:
myscores[[i]]<-score(myfile_split[[i]],myfile_split_keys[[i]],output.scored=TRUE)
x[[i]]<-myscores[[i]]$scored
P[[i]]<-myscores[[i]]$score
score.level[[i]]<-as.vector(quantile(P[[i]], c(0, 1/3, 2/3, 1)))
hni[[i]]<-length(P[[i]][P[[i]]>score.level[[i]][3]])
lni[[i]]<-length(P[[i]][P[[i]]<=score.level[[i]][2]])

Discrim<-function(x){
 x<-na.exclude(as.matrix(x))
 k<-ncol(x)
 N<-nrow(x)
 TOT<-apply(x,1,mean)
 tmpx<-cbind(x,TOT)[order(TOT),]
 tmpxU<-tmpx[(N+1-hni[[i]]):N,]
 tmpxL<-tmpx[lni[[i]]:1,]
 Ui<-apply(tmpxU,2,sum)
 Li<-apply(tmpxL,2,sum)
 DiscrimIndex<-round((Ui/hni[[i]] - Li/lni[[i]]),3)
 HigherPercent<-paste(round((Ui/hni[[i]])*100,2),"%",sep="")
 LowerPercent<-paste(round((Li/lni[[i]])*100,2),"%",sep="")
 Mat<-data.frame(DiscrimIndex)
 Discrim<- Mat[1:k,]
 return(Discrim)
 }
discrim.index[[i]]<-Discrim(x[[i]])
rownames(discrim.index[[i]])<-NULL
QId[[i]]<-colnames(myfile_split[[i]])
biserial.corr[[i]]<-round(reliability(x[[i]],itemal=TRUE)$bis,3)
TopicOV[[i]]<-data.frame(QId=QId[[i]],discrim.index=discrim.index[[i]],biserial.corr=biserial.corr[[i]])
}

BisDiscr<-do.call("rbind",TopicOV)

Merged.Report<-merge(diff,BisDiscr,by = "QId",sort=F)

File.Name<-rep(fileName,nrow(Merged.Report))
Event.Name<-rep(as.character(myfile0[8,3]),nrow(Merged.Report))
Event.Rel.Alpha<-rep(round(as.numeric(itemAnalysis(xx)[3]),3),nrow(Merged.Report))
No.Of.Candidates<-rep(as.numeric(itemAnalysis(xx)[2]),nrow(Merged.Report))

CTT.Report<-data.frame(QId=Merged.Report$QId, Diffi.Index=Merged.Report$Difficulty.Index, Discrim.Index=Merged.Report$discrim.index, Bis.Corr=Merged.Report$biserial.corr, Topic=Merged.Report$TopicName, SubTopic=Merged.Report$SubTopic, QBM.Diffi=Merged.Report$QBM.Diffi,Event.Rel.Alpha,No.Of.Candidates,Attempted.By.Candidates.No=attempted,Corrected.By.Candidates.No=corrected,File.Name,Event.Name)

Time<-gsub(":","~",Sys.time())
output<-paste("CTT.Report-",fileName," ",Time,".csv",sep="")
CTT.Report%>%
arrange(QId)%>%
write.csv(output)
