library(CTT)
library(dplyr)
myfile<-read.csv("D:\\MeritTrac Assignments\\STUDIES\\CTT & IRT\\Item Analysis by R\\Analysis II.csv")
myfilekeys<-c("A","C","D","A","C","C","A","B","D","D")
myscores<-score(myfile,myfilekeys,output.scored=TRUE)
x<-myscores$scored
P<-myscores$score

TOT<-apply(x,1,sum)
probx<-data.frame(x,TOT)[order(TOT,decreasing=T),]
scores<-as.vector(data.frame(table(TOT))[,1])

xsubs<-list();No.Cndtes<-c();prob.freq<-list()
for(i in 1:length(scores))
{
xsubs[[i]]<-probx%>%
 filter(TOT==scores[i])%>%
 select(-TOT)
No.Cndtes[i]<-nrow(xsubs[[i]])
prob.freq[[i]]<-colSums(xsubs[[i]])/No.Cndtes[i]
}
Prob.Distr<-do.call("rbind", prob.freq)
Prob.Distr.Table<-data.frame(scores,No.Cndtes,Prob.Distr)[order(scores,decreasing=T),]

##Change the class of each column in z into numeric
#asNumeric <- function(x) as.numeric(as.character(x))
#factorsNumeric <- function(d) modifyList(d, lapply(d[, sapply(d, is.factor)],   
#                                                   asNumeric))
