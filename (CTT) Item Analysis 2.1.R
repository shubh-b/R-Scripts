fileName<-readline()

path<-readline()

gsub("\\\\","/",path)
setwd(path)
if(!require(readxl)){ 
message("Installing and loading `readxl` package.")
install.packages("readxl")
}
if(!require(CTT)){ 
message("Installing and loading `CTT` package.")
install.packages("CTT")
}
input<-paste(fileName,".xlsx",sep="")
output<-paste("Report_",fileName,".txt",sep="")
sink(output)
myfile0<-read_excel(input)
myfile<-myfile0[-1,]
myfilekeys<-as.vector(myfile0[1,],"character")
myscores<-score(myfile,myfilekeys,output.scored=TRUE)
x<-myscores$scored
P<-myscores$score
score.level<-as.vector(quantile(P, c(0, 1/3, 2/3, 1)))
hni<-length(P[P>score.level[3]])
lni<-length(P[P<=score.level[2]])

 attempted<-colSums(myfile!="X")
 corrected<-colSums(x!=0)
 difficulty<-round((1-corrected/attempted),3)
 biserial.corr<-round(reliability(x,itemal=TRUE)$bis,3)
 Rel.AfterDel<-round(reliability(x,itemal=TRUE)$alphaIfDeleted,3)

Discrim<-function(x){
 x<-na.exclude(as.matrix(x))
 k<-ncol(x)
 N<-nrow(x)
 TOT<-apply(x,1,mean)
 tmpx<-cbind(x,TOT)[order(TOT),]
 tmpxU<-tmpx[(N+1-hni):N,]
 tmpxL<-tmpx[lni:1,]
 Ui<-apply(tmpxU,2,sum)
 Li<-apply(tmpxL,2,sum)
 DiscrimIndex<-round((Ui/hni - Li/lni),3)
 HigherPercent<-paste(round((Ui/hni)*100,2),"%",sep="")
 LowerPercent<-paste(round((Li/lni)*100,2),"%",sep="")
 Mat<-data.frame(DiscrimIndex,HigherPercent,LowerPercent)
 Discrim<- Mat[1:k,]
 return(Discrim)
 }
discrim.index<-Discrim(x)

#Reliability of the event:
 Exam.Rel<-reliability(x,itemal=T)
 Exam.Rel

Item.Analysis<-data.frame(difficulty, biserial.corr, discrim.index, Rel.AfterDel)
gap<-paste("    ")
writeLines(c(gap,paste("::Item Analysis::")))
Item.Analysis

M<-paste("Highest no. of Items which were responded correctly by any candidate is ",max(P),".",sep="")
U<-paste("The number of upper ability candidates is ", hni, ", who gave correct response to >= ", (score.level[3]+1), " items.",sep="")
L<-paste("The number of lower ability candidates is ", lni, ", who gave correct response to <= ", score.level[2], " items.",sep="")
Discrim.Summary<-writeLines(c(gap,gap,"::Distractor Analysis::",gap,M,U,L,gap))

distractor.analysis(myfile,myfilekeys)
sink()
