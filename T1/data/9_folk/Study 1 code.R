library(lm.beta)
library(lsr)
library(tidyr)
library(psych) 


Data<-read.csv("Study 1 Data.csv", header = TRUE) #reading in data
Data<-as_tibble(Data)


####Has social connection changed as a result of the COVID-19 Pandemic

mean(Data$SCAVERAGE.T1)
mean(Data$SCAVERAGE.T2)
sd(Data$SCAVERAGE.T1)
sd(Data$SCAVERAGE.T2)

#t.test 
t.test(Data$SCAVERAGE.T1, Data$SCAVERAGE.T2, alternative = "two.sided", paired = TRUE)


#Calculating Cohens d using pooled SD
d<-cohensD(Data$SCAVERAGE.T1,Data$SCAVERAGE.T2) 
d
psych::cohen.d.ci(d = d, n1 = 467, n2 = 467) #confidence interval (not accounting for paired-subjects design)




####Has social connectedness changed more for extraverts or introverts?

#regression analyses: T2 social connectedness predicted by Time 1 Social connectedness and extraversion
fit<-(lm(SCAVERAGE.T2~EXTRAVERSION+SCAVERAGE.T1, data = Data))
summary(fit)
fitbeta<-lm.beta::lm.beta(fit) #getting standardized coefficients
print(fitbeta)


#social connectedness difference score predicted by extraversion
fitNocontrol<-(lm(SCdiff~EXTRAVERSION, data = Data))
summary(fitNocontrol)
betafitNocontrol<-lm.beta::lm.beta(fitNocontrol)
print(betafitNocontrol)


#analyses looking at change in social connectedness for most introverted and extraverted participants

#Have most introverted participants experienced changes in connectedness
quantile(Data$EXTRAVERSION) #finding quartiles 
BottomQuarter<- subset(Data, EXTRAVERSION <= 3.41667) #Making subset of data with only participants in bottom quartile of extraversion
BottomQuarter
BottomQuarter<-as_tibble(BottomQuarter)
BottomQuarter

mean(BottomQuarter$SCAVERAGE.T2)
sd(BottomQuarter$SCAVERAGE.T2)
mean(BottomQuarter$SCAVERAGE.T1)
sd(BottomQuarter$SCAVERAGE.T1)

#t.test
t.test(BottomQuarter$SCAVERAGE.T1, BottomQuarter$SCAVERAGE.T2, alternative = "two.sided", paired = TRUE)

#calculating cohens d
d2<-cohensD(BottomQuarter$SCAVERAGE.T1,BottomQuarter$SCAVERAGE.T2)
d2
psych::cohen.d.ci(d = d2, n1 = 119, n2 = 119) #confidence interval

#Have most extraverted participants experienced changes in connectedness
quantile(Data$EXTRAVERSION) #finding quartiles
UpperQuarter<- subset(Data, EXTRAVERSION >= 4.8333) #Making subset of data with only participants in top quartile of extraversion
UpperQuarter<-as_tibble(UpperQuarter)
UpperQuarter

mean(UpperQuarter$SCAVERAGE.T1)
sd(UpperQuarter$SCAVERAGE.T1)
mean(UpperQuarter$SCAVERAGE.T2)
sd(UpperQuarter$SCAVERAGE.T2)

t.test(UpperQuarter$SCAVERAGE.T1, UpperQuarter$SCAVERAGE.T2, alternative = "two.sided", paired = TRUE)
d3<-cohensD(UpperQuarter$SCAVERAGE.T1,UpperQuarter$SCAVERAGE.T2)
d3
psych::cohen.d.ci(d = d3, n1 = 130, n2 = 130)



####Is the effect of change in social connection well-being different for extraverts and introverts?

#Regression analysis- does social connectedness differentially affect lethargy
fit2<-(lm(LethDiff~EXTRAVERSION +SCdiff+ EXTRAVERSION * SCdiff,data = Data))
summary(fit2)
fit2beta<-lm.beta::lm.beta(fit2) #getting standardized coefficients
print(fit2beta)


#Regression analysis (same as above but controlling for Time1 lethargy)
fit3<-(lm(LethDiff~EXTRAVERSION+SCdiff+ LETHAVERAGE.T1 + EXTRAVERSION * SCdiff ,data = Data))
summary(fit3)
fit3beta<-lm.beta::lm.beta(fit3) #getting standardized coefficients
print(fit3beta)

#Have people in general decreased in lethargy?

t.test(Data$LETHAVERAGE.T2, Data$LETHAVERAGE.T1, alternative = "two.sided", paired = TRUE)

d4<-cohensD(Data$LETHAVERAGE.T1,Data$LETHAVERAGE.T2)
d4
psych::cohen.d.ci(d = d4, n1 = 467, n2 = 467)


#Are changes in social connectedness related to changes in lethargy
cor.test(Data$SCdiff,Data$LethDiff)

#Supplemental Analysis 
fit4<-(lm(LethDiff~EXTRAVERSION+SCdiff+ LETHAVERAGE.T1 + SCAVERAGE.T1+ EXTRAVERSION * SCdiff ,data = Data))
summary(fit4)
fit4beta<-lm.beta::lm.beta(fit4) #getting standardized coefficients
print(fit4beta)




