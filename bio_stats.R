#9/6/18
library(psych)
library(DescTools)
library(zoo)
library(pscl)
library(plyr)
library(dplyr)
library(epitools)
library(dplyr)

#dataset 
meng=read.csv("abm.csv")

#fill nan with mean 
for(i in 1:ncol(meng)){
  meng[is.na(meng[,i]), i] <- mean(meng[,i], na.rm = TRUE)
}

hist(meng$age,    
     col="gray",  
     main="Patient Age Distribution", 
     xlab="Patient count")  

tab <- matrix(c(4,16,40,168),byrow=TRUE,nrow=2)
epitab(tab,method="riskratio")
levels(meng1$sex)

#contracting abm based on sex
male=subset(meng1,sex=="male")
female=subset(meng1,sex=="female")

male %>%
  group_by(abm) %>%
  summarise(n= n()) #118 (0),113 (1)

female %>%
  group_by(abm) %>%
  summarise(n=n()) # 85(0), 104(1)

#proportion table 
m=matrix(c(113,104,118,85),nrow=2)
dimnames(m)=list("Group"=c("Male","Female"),"abm table"=c("Yes","No"))
prop.table(m,margin=1)

#difference of proportions test 
prop.test(m)
p.out=prop.test(m)
p.out$estimate[1]-p.out$estimate[2] #0.061

##relative risk 
prop.out=prop.table(m,margin=1)
prop.out[1,1]/prop.out[2,1] #males 12% less likely to be diagnosed with abm

rr=riskratio(m)
rr1=riskratio(m,rev="c")
rr1$measure

rr11=riskratio(m,rev="b")
rr11$measure #males have between 0.74 to 1.07 times the risk of contracting the meng 
#meaning that the risk for meng in males is 12% lower than females 

#odds ratios ******************
#odds of meng in males 
prop.out[1,1]/prop.out[1,2] #0.96
#odds of meng in females 
prop.out[2,1]/prop.out[2,2] #1.22 

#approximation for values
MASS::fractions(prop.out[1,1]/prop.out[1,2])
MASS::fractions(prop.out[2,1]/prop.out[2,2])


