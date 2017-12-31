#medical research 

#import dataset 
alt_data<-read.csv(file.choose(),header=TRUE)
attach(alt_data) 
names(alt_data)

# import libaries 
library(ggplot2)
library(plyr)
library(dplyr)
# Age vs. ETIV
ggplot(alt_data,aes(x=Age,y=eTIV,colour=Group))+geom_point()+xlab("Patient Age")+
  ylab("eTIV")
# M.F. vs. ASF
ggplot(alt_data,aes(M.F,fill=Group))+
  geom_bar(position="dodge")+
  scale_fill_grey(start=0,end=1)+
  ggtitle("Females Are Less Demented Than Males")+
  xlab("Gender")+theme(plot.title = element_text(hjust = 0.5))

mean_asf_gender<-ddply(alt_data,~M.F,summarise,mean=mean(ASF),sd=sd(ASF))
ggplot(data=mean_asf_gender,aes(M.F,mean))+geom_bar(stat="identity")+
  xlab("gender")+ylab("Mean ASF")

#tukey's hypothesis test 
#is there a difference between ASF and gender?
tapply(alt_data$ASF,alt_data$M.F,mean) #1.26 F vs. 1.11 M
#anova test
model<-lm(alt_data$ASF~alt_data$M.F)
summary(model) #p-value <0.05 
anova(model)

#tukey test (see differences)
anova1<-aov(alt_data$ASF~alt_data$M.F)
posthoc<-TukeyHSD(x=anova1,'alt_data$M.F',conf.level = 0.95)
posthoc 
plot(anova1)

#alternative test for difference between males and females 
install.packages("agricolae")
library(agricolae)
HSD.test(model, 'alt_data$M.F')