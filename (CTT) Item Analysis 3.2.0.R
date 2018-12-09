## Consolidated Item Analysis Report generation from multiple Event's Response Sheets of Test-Takers.
path<-readline()

gsub("\\\\","/",path)
setwd(path)

ABCDXFiles<-list.files(pattern = "xlsx$")

packages = c("dplyr","readxl","CTT")
package.check <- lapply(packages, FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
        install.packages(x, dependencies = TRUE)
        library(x, character.only = TRUE)
    }
})


## Initiate NULL Lists and NULL vectors.
fileName<-c();input<-list();myfile0<-list();myfile00<-list();myfile<-list()
myfilekeys<-list();myscores<-list();xx<-list();attempted<-list();corrected<-list()
Difficulty.Index<-list();QId<-list();TopicName<-list();SubTopic<-list()
QBM.Diffi<-list();diff<-list();topics<-list();myfile_split00<-list();myfile_split<-list();myfile_split_keys<-list()
myscores<-list();x<-list();P<-list();score.level<-list();hni<-list();lni<-list()
QId<-list();discrim.index<-list();TopicOV<-list();biserial.corr<-list();BisDiscr<-list();Merged.Report<-list()
CTT.Report<-list();myfile_split0<-list();File.Name<-list();Event.Name<-list()
Event.Rel<-list();Candidates<-list()


for(j in 1:length(ABCDXFiles)){

myfile0[[j]]<-read_excel(ABCDXFiles[j],col_names = F)
myfile00[[j]]<-myfile0[[j]]%>%
	select(-c(1:12))

## Whole Data with QId
myfile00[[j]]<-myfile00[[j]][-c(1:6),]		
colnames(myfile00[[j]])<-as.vector(myfile0[[j]][4,-c(1:12)],"character")

##Evaluate Difficulty Index
myfile[[j]]<-myfile00[[j]][-1,]
myfilekeys[[j]]<-as.vector(myfile00[[j]][1,],"character")
myscores[[j]]<-score(myfile[[j]],myfilekeys[[j]],output.scored=TRUE)
xx[[j]]<-myscores[[j]]$scored
 attempted[[j]]<-colSums(myfile[[j]]!="X")
 corrected[[j]]<-colSums(xx[[j]]!=0)
 Difficulty.Index[[j]]<-round((1-corrected[[j]]/attempted[[j]]),3)

names(Difficulty.Index[[j]])<-NULL
QId[[j]]<-colnames(myfile00[[j]])
TopicName[[j]]<-as.vector(myfile0[[j]][2,-(1:12)],"character")
SubTopic[[j]]<-as.vector(myfile0[[j]][3,-(1:12)],"character")
QBM.Diffi[[j]]<-as.vector(myfile0[[j]][6,-(1:12)],"character")

diff[[j]]<-data.frame(QId=QId[[j]],Difficulty.Index=Difficulty.Index[[j]],TopicName=TopicName[[j]],SubTopic=SubTopic[[j]],QBM.Diffi=QBM.Diffi[[j]])


topics[[j]]<-levels(factor(as.vector(myfile0[[j]][2,-(1:12)],"character")))
myfile_split0[[j]]<-myfile0[[j]][-c(1,3,4,5,6),-c(1:12)]
colnames(myfile_split0[[j]])<-as.vector(myfile0[[j]][4,-c(1:12)],"character")


##Topic-Wise Split of the Data File and Separate Analysis
##Null lists of the objects

myfile_split00[[j]]<-list();myfile_split[[j]]<-list();myfile_split_keys[[j]]<-list()
myscores[[j]]<-list();x[[j]]<-list();P[[j]]<-list();score.level[[j]]<-list();hni[[j]]<-list();lni[[j]]<-list()
QId[[j]]<-list();discrim.index[[j]]<-list();TopicOV[[j]]<-list();biserial.corr[[j]]<-list()

for(i in 1:length(topics[[j]])){
myfile_split00[[j]][[i]]<-myfile_split0[[j]][ , c(myfile_split0[[j]][1,]==topics[[j]][i])][-1,]
myfile_split[[j]][[i]]<-myfile_split00[[j]][[i]][-1,]
myfile_split_keys[[j]][[i]]<-as.vector(myfile_split00[[j]][[i]][1,],"character")

##Topic Wise Calculations:
myscores[[j]][[i]]<-score(myfile_split[[j]][[i]],myfile_split_keys[[j]][[i]],output.scored=TRUE)
x[[j]][[i]]<-myscores[[j]][[i]]$scored
P[[j]][[i]]<-myscores[[j]][[i]]$score
score.level[[j]][[i]]<-as.vector(quantile(P[[j]][[i]], c(0, 1/3, 2/3, 1)))
hni[[j]][[i]]<-length(P[[j]][[i]][P[[j]][[i]]>score.level[[j]][[i]][3]])
lni[[j]][[i]]<-length(P[[j]][[i]][P[[j]][[i]]<=score.level[[j]][[i]][2]])

Discrim<-function(x){
 x<-na.exclude(as.matrix(x))
 k<-ncol(x)
 N<-nrow(x)
 TOT<-apply(x,1,mean)
 tmpx<-cbind(x,TOT)[order(TOT),]
 tmpxU<-tmpx[(N+1-hni[[j]][[i]]):N,]
 tmpxL<-tmpx[lni[[j]][[i]]:1,]
 Ui<-apply(tmpxU,2,sum)
 Li<-apply(tmpxL,2,sum)
 DiscrimIndex<-round((Ui/hni[[j]][[i]] - Li/lni[[j]][[i]]),3)
 HigherPercent<-paste(round((Ui/hni[[j]][[i]])*100,2),"%",sep="")
 LowerPercent<-paste(round((Li/lni[[j]][[i]])*100,2),"%",sep="")
 Mat<-data.frame(DiscrimIndex)
 Discrim<- Mat[1:k,]
 return(Discrim)
 }
discrim.index[[j]][[i]]<-Discrim(x[[j]][[i]])
rownames(discrim.index[[j]][[i]])<-NULL
QId[[j]][[i]]<-colnames(myfile_split[[j]][[i]])
biserial.corr[[j]][[i]]<-round(reliability(x[[j]][[i]],itemal=TRUE)$bis,3)
TopicOV[[j]][[i]]<-data.frame(QId=QId[[j]][[i]],discrim.index=discrim.index[[j]][[i]],biserial.corr=biserial.corr[[j]][[i]])
}

BisDiscr[[j]]<-do.call("rbind",TopicOV[[j]])

Merged.Report[[j]]<-merge(diff[[j]],BisDiscr[[j]],by = "QId",sort=F)

File.Name[[j]]<-rep(strsplit(ABCDXFiles[j],"\\.")[[1]][1],nrow(Merged.Report[[j]]))
Event.Name[[j]]<-rep(as.character(myfile0[[j]][8,3]),nrow(Merged.Report[[j]]))
Event.Rel[[j]]<-rep(round(as.numeric(itemAnalysis(xx[[j]])[3]),3),nrow(Merged.Report[[j]]))
Candidates[[j]]<-rep(as.numeric(itemAnalysis(xx[[j]])[2]),nrow(Merged.Report[[j]]))

CTT.Report[[j]]<-data.frame(QId=Merged.Report[[j]]$QId, Diffi.Index=Merged.Report[[j]]$Difficulty.Index, Discrim.Index=Merged.Report[[j]]$discrim.index, Bis.Corr=Merged.Report[[j]]$biserial.corr, Topic=Merged.Report[[j]]$TopicName, SubTopic=Merged.Report[[j]]$SubTopic, QBM.Diffi=Merged.Report[[j]]$QBM.Diffi,Event.Rel.Alpha=Event.Rel[[j]],No.Of.Candidates=Candidates[[j]],Attempted.By.Candidates.No=attempted[[j]],Corrected.By.Candidates.No=corrected[[j]],File.Name=File.Name[[j]],Event.Name=Event.Name[[j]])
}
consol<-do.call("rbind",CTT.Report)

##Create a new folder to save the report
outfolder0<-paste("Consolidated CTT.Report From",length(ABCDXFiles),"Files",Sys.time())
outfolder<-paste(gsub(":","~",outfolder0),"IST")
dir.create(file.path(path,outfolder), showWarnings = FALSE)
OutPath<-paste(path,"\\",outfolder,"\\",sep="")


##Save the report in .csv format
output0<-paste("CTT.Report from ",length(ABCDXFiles)," Files Total ",nrow(consol)," Qns ",Sys.time()," IST.csv",sep="")
output<-paste(gsub(":","~",output0))
consol%>%
arrange(QId)%>%
write.csv(paste0(OutPath,output))
