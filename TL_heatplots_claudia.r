library(data.table)
library(ggplot2)
library(tidyr)
library(dplyr)
library(reshape2)
library(tidyverse)

setwd("//Users/claus/Desktop/JRC/tourism")
STL<-read.csv("CQ_TLsurvey.csv")
STL2<-melt(STL[,c(1:6)],id.vars=1:3,measure.vars= 4:6)
STL2$Scope<-as.factor(STL2$Scope)
STL2$N<-as.factor(STL2$N)
STL3<-melt(STL[,c(1,7:12)],id.vars=1,measure.vars= 2:7)
STL3$N<-as.factor(STL3$N)

STL4<-melt(STL[,c(1,13:18)],id.vars=1,measure.vars= 2:7)
STL4$N<-as.factor(STL4$N)



plotCH <- ggplot(STL2, aes(x = variable, y = reorder(N,desc(N)), fill = value)) +
  geom_tile() +
  # fte_theme() +
   
  scale_fill_gradient(low="#e6f598", high= "#d53e4f")+
  #coord_fixed()
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5), legend.title = element_blank(), legend.position="top", legend.direction="horizontal", legend.key.width=unit(2, "cm"), legend.key.height=unit(0.25, "cm")) +
  scale_x_discrete(labels=c("Reduced funding","Reduced community income", "Increased extraction"))+
  labs(x = "", y="Respondent", title = "Relevance of challenges for PAs in C19 crisis") #+
  #scale_fill_gradient(low = "yellow", high = "#d53e4f") #+scale_y_discrete(limit = labels(c(1:9)))
plotCH 


labIM=c("Reduced funding","Increased poaching", "Increased fishing", "Increased logging", "Increased other", "Reduced disturbance")
wrlabIM<-wrap.labels(labIM,10)

plotIM <- ggplot(STL3, aes(x = variable, y = reorder(N,desc(N)), fill = value)) +
  geom_tile() +
  scale_fill_gradient(low="#e6f598", high= "#d53e4f")+
  theme(axis.text.x = element_text(angle = 0, vjust = 0.6), legend.title = element_blank(), legend.position="top", legend.direction="horizontal", legend.key.width=unit(2, "cm"), legend.key.height=unit(0.25, "cm")) +
  labs(x = "", y="Respondent", title = "") +
  scale_x_discrete(labels=wrlabIM)
  #scale_fill_gradient(low = "yellow", high = "#377eb8") #+scale_y_discrete(limit = labels(c(1:9)))
plotIM 


labSF<-c("Reduced gov funding","Reduced donation", "Reduced dev/coop funding", "Reduced entrance fees", "Reduced concession income", "Reduced tourist spending")

wrlabS<-wrap.labels(labSF, 10)
plotSF <- ggplot(STL4, aes(x = variable, y = reorder(N,desc(N)), fill = value)) +
  geom_tile() +
  scale_fill_gradient(low="#e6f598", high= "#d53e4f")+
  theme(axis.text.x = element_text(angle = 0, vjust = 0.6), legend.title = element_blank(), legend.position="top", legend.direction="horizontal", legend.key.width=unit(2, "cm"), legend.key.height=unit(0.25, "cm")) +
  labs(x = "", y="Respondent", title = "") +
  scale_x_discrete(labels=wrlabS)
  #scale_fill_gradient(low = "yellow", high = "#377eb8") #+scale_y_discrete(limit = labels(c(1:9)))
plotSF


# Core wrapping function
wrap.it <- function(x, len)
{ 
  sapply(x, function(y) paste(strwrap(y, len), 
                              collapse = "\n"), 
         USE.NAMES = FALSE)
}


# Call this function with a list or vector
wrap.labels <- function(x, len)
{
  if (is.list(x))
  {
    lapply(x, wrap.it, len)
  } else {
    wrap.it(x, len)
  }
}

STL2$value<-as.factor(STL2$value)
STL2gr<-STL2%>%group_by(variable,value)%>%summarise(Percent=n())
STL2gr$value<-factor(STL2gr$value, levels=c(1,2,3,4,5),labels = c("Somewhat relevant", "Neutral","Relevant","Very relevant","Extremelyrelevant"))

lab1=c("Reduced funding","Reduced community income", "Increased extraction")

wrlab1<-wrap.labels(lab1, 10)
plotCHbar<-ggplot(STL2gr, aes(x = variable, y= Percent, fill = reorder(value,desc(value))))+
        geom_bar(position="fill", stat="identity")+
  labs(x = "", y="Share", title = "") +
  scale_fill_manual(values =c("red2","chocolate2","orange","blue2","cyan3","cyan1"))+
  #coord_fixed()
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5), axis.title.y = element_text(angle = 0, vjust = 0.5),legend.title = element_blank()) +
  scale_x_discrete(labels=wrlab1)
  
plotCHbar 
####
STL3$value<-as.factor(STL3$value)
STL3$value<-factor(STL3$value, levels=c(0,1,2,3,4,5),labels = c("Not relevant","Somewhat relevant", "Neutral","Relevant","Very relevant","Extremelyrelevant"))
STL3gr<-STL3%>%group_by(variable,value)%>%summarise(Percent=n())



lab=c("Reduced funding","Increased poaching", "Increased fishing", "Increased logging", "Increased other extraction", "Reduced disturbance")
wrlab<-wrap.labels(lab, 10)
plotIMbar<-ggplot(STL3gr, aes(x = variable, y= Percent, fill = reorder(value, desc(value))))+
  geom_bar(position="fill", stat="identity")+
  scale_fill_manual(values =c("red2","chocolate2","orange","blue2","cyan3","cyan1"))+
  #coord_fixed() 
  labs(x = "", y="Share", title = "")+
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5),axis.title.y = element_text(angle = 0, vjust = 0.5), legend.title = element_blank()) +
  scale_x_discrete(labels=wrlab)
  
plotIMbar

STL4$value<-as.factor(STL4$value)
STL4$value<-factor(STL4$value, levels=c(0,1,2,3,4,5),labels = c("Not relevant","Somewhat relevant", "Neutral","Relevant","Very relevant","Extremelyrelevant"))
STL4gr<-STL4%>%group_by(variable,value)%>%summarise(Percent=n())
lab2=c("Reduced Gov funding","Reduced donation", "Reduced dev/coop funding", "Reduced entrance fees", "Reduced concession income", "Reduced tourist spending")
wrlab2<-wrap.labels(lab2, 10)

plotSFbar<-ggplot(STL4gr, aes(x = variable, y= Percent, fill = reorder(value, desc(value))))+
  geom_bar(position="fill", stat="identity")+
  scale_fill_manual(values =c("red2","chocolate2","orange","blue2","cyan3","cyan1"))+
  #coord_fixed() 
  labs(x = "", y="Share", title = "")+
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5),axis.title.y = element_text(angle = 0, vjust = 0.5), legend.title = element_blank()) +
  scale_x_discrete(labels=wrlab2)

plotSFbar
