getwd()
install.packages("rio")
install.packages("moments")
install.packages("PerformanceAnalytics")
install.packages("corrplot")
install.packages("car")
install.packages("stargazer")
library("corrplot")
library("PerformanceAnalytics")
library("car")
library("stargazer")
library("rio")
library("moments") 

mh.data=import("Mental Health 2012-2018.xlsx",sheet="Normalized")
colnames(mh.data)=tolower(make.names(colnames(mh.data)))
mh.2018=subset(mh.data, (year==2018))

#Univariate Analysis

hist(mh.2018$suicide.rate.per.100000.people)
hist(log(mh.2018$suicide.rate.per.100000.people))

##Preprocessing for only 2018 data 

illicit.drug.use.disorder = mh.2018$illicit.durg.use.disorder.12.17...+ mh.2018$illicit.durg.use.disorder.18.or.older..
pain.reliever.use.disorder = mh.2018$pain.reliever.use.disorder.12.17.. + mh.2018$pain.reliever.use.disorder.18.or.older
alcohol.use.disorder = mh.2018$alcohol.use.disorder.12.17.. + mh.2018$alcohol.use.disorder.18.or.older..
substance.use.disorder = mh.2018$substance.use.disorder.12.17.. + mh.2018$substance.use.disorder.18.or.older..
not.receiving.treatment.for.illicit.drug.use = mh.2018$not.receiving.treatment.for.illicit.drug.use.12.17.. + mh.2018$not.receiving.treatment.for.illicit.drug.use.18.or.older..
not.receiving.treatment.for.alcohol.use = mh.2018$not.receiving.treatment.for.alcohol.use.12.17.. + mh.2018$not.receiving.treatment.for.alcohol.use.18.or.older..
not.receiving.treatment.for.substance.use = mh.2018$not.receiving.treatment.for.substance.use.12.17.. + mh.2018$not.receiving.treatment.for.substance.use.18.or.older..
major.depressive.episode = mh.2018$major.depressive.episode.12.17.. + mh.2018$major.depressive.episode.18.or.older..
serious.mental.illness = mh.2018$serious.mental.illness.18.or.older..
any.mental.illness = mh.2018$any.mental.illness.18.or.older..
received.mental.health.services = mh.2018$received.mental.health.services.18.or.older..
had.serious.thoughts.of.suicide = mh.2018$had.serious.thoughts.of.suicide.18.or.older..

final.mh.2018 = data.frame(mh.2018$year, mh.2018$state, illicit.drug.use.disorder,pain.reliever.use.disorder,
                           alcohol.use.disorder,substance.use.disorder,not.receiving.treatment.for.illicit.drug.use,
                           not.receiving.treatment.for.alcohol.use, not.receiving.treatment.for.substance.use,
                           major.depressive.episode, serious.mental.illness, any.mental.illness, received.mental.health.services,
                           had.serious.thoughts.of.suicide)

final.mh.2018$gdp = mh.2018$gdp
final.mh.2018$unemployment.rate = mh.2018$unemployment.rate
final.mh.2018$gini.index = mh.2018$gini.index
final.mh.2018$divorce.rate = mh.2018$divorce.rate
final.mh.2018$poverty.rate = mh.2018$poverty.rate
final.mh.2018$suicide.rate = mh.2018$suicide.rate.per.100000.people
final.mh.2018$population = mh.2018$population

##
corrplot(cor(final.mh.2018[,3:21]),type="lower")

chart.Correlation(final.mh.2018[,3:9],histogram = TRUE) #perfomance analytics package
chart.Correlation(final.mh.2018[,10:21],histogram = TRUE)
cor(final.mh.2018[,3:12]) #correlation plot
pairs(final.mh.2018[,3:12]) #scatterplot matrix

# Model for Suicide Rate
m1 = lm(final.mh.2018$suicide.rate ~ alcohol.use.disorder + illicit.drug.use.disorder + had.serious.thoughts.of.suicide + major.depressive.episode
        + pain.reliever.use.disorder + substance.use.disorder + received.mental.health.services + serious.mental.illness + any.mental.illness + final.mh.2018$gdp 
        + final.mh.2018$unemployment.rate + final.mh.2018$gini.index + final.mh.2018$divorce.rate + final.mh.2018$poverty.rate)
summary(m1)
plot(m1)

# Model for Serious Mental Illness
m2 = lm(final.mh.2018$serious.mental.illness ~ alcohol.use.disorder + illicit.drug.use.disorder + had.serious.thoughts.of.suicide + major.depressive.episode
        + pain.reliever.use.disorder + substance.use.disorder + received.mental.health.services + any.mental.illness + final.mh.2018$gdp 
        + final.mh.2018$unemployment.rate + final.mh.2018$gini.index + final.mh.2018$divorce.rate + final.mh.2018$poverty.rate)
summary(m2)
plot(m2)

# Model for major.depressive.episode
m3 = lm(final.mh.2018$major.depressive.episode ~ alcohol.use.disorder + illicit.drug.use.disorder + had.serious.thoughts.of.suicide 
        + pain.reliever.use.disorder + substance.use.disorder + received.mental.health.services + serious.mental.illness + any.mental.illness + final.mh.2018$gdp 
        + final.mh.2018$unemployment.rate + final.mh.2018$gini.index + final.mh.2018$divorce.rate + final.mh.2018$poverty.rate)
summary(m3)
plot(m3)

# Model for had.serious.thoughts.of.suicide
m4 = lm(final.mh.2018$had.serious.thoughts.of.suicide ~ alcohol.use.disorder + illicit.drug.use.disorder + major.depressive.episode
        + pain.reliever.use.disorder + substance.use.disorder + received.mental.health.services + serious.mental.illness + any.mental.illness + final.mh.2018$gdp 
        + final.mh.2018$unemployment.rate + final.mh.2018$gini.index + final.mh.2018$divorce.rate + final.mh.2018$poverty.rate)
summary(m4)
plot(m4)


#Suiciderate
#Linearity
plot(final.mh.2018$suicide.rate,m4$fitted.values,pch=19,main="Suicide Rate Actual v. Fitted Values")
abline(0,1,col="red",lwd=3)
#Normality
qqnorm(m4$residuals,pch=19,main="Suicide Rate Normality Plot")
qqline(m4$residuals,col="red",lwd=3)
#Equality of variance
plot(m4$fitted.values,rstandard(m4),pch=19,
     main="Suicide Rate Standardized Residuals")
abline(0,0,col="red",lwd=3)

vif(m1)
stargazer(m1,m2,m3,m4,type = "text")

##########################################

#Preprocessing for multiple year dataset
library(lme4)             

illicit.drug.use.disorder = mh.data$illicit.durg.use.disorder.12.17...+ mh.data$illicit.durg.use.disorder.18.or.older..
pain.reliever.use.disorder = mh.data$pain.reliever.use.disorder.12.17.. + mh.data$pain.reliever.use.disorder.18.or.older
alcohol.use.disorder = mh.data$alcohol.use.disorder.12.17.. + mh.data$alcohol.use.disorder.18.or.older..
substance.use.disorder = mh.data$substance.use.disorder.12.17.. + mh.data$substance.use.disorder.18.or.older..
not.receiving.treatment.for.illicit.drug.use = mh.data$not.receiving.treatment.for.illicit.drug.use.12.17.. + mh.data$not.receiving.treatment.for.illicit.drug.use.18.or.older..
not.receiving.treatment.for.alcohol.use = mh.data$not.receiving.treatment.for.alcohol.use.12.17.. + mh.data$not.receiving.treatment.for.alcohol.use.18.or.older..
not.receiving.treatment.for.substance.use = mh.data$not.receiving.treatment.for.substance.use.12.17.. + mh.data$not.receiving.treatment.for.substance.use.18.or.older..
major.depressive.episode = mh.data$major.depressive.episode.12.17.. + mh.data$major.depressive.episode.18.or.older..
serious.mental.illness = mh.data$serious.mental.illness.18.or.older..
any.mental.illness = mh.data$any.mental.illness.18.or.older..
received.mental.health.services = mh.2018$received.mental.health.services.18.or.older..
had.serious.thoughts.of.suicide = mh.data$had.serious.thoughts.of.suicide.18.or.older..

final.mh.data = data.frame(illicit.drug.use.disorder,pain.reliever.use.disorder,
                           alcohol.use.disorder,substance.use.disorder,not.receiving.treatment.for.illicit.drug.use,
                           not.receiving.treatment.for.alcohol.use, not.receiving.treatment.for.substance.use,
                           major.depressive.episode, serious.mental.illness, any.mental.illness, received.mental.health.services,
                           had.serious.thoughts.of.suicide)
final.mh.data$state = mh.data$state
final.mh.data$year = mh.data$year
final.mh.data$gdp = mh.data$gdp
final.mh.data$unemployment.rate = mh.data$unemployment.rate
final.mh.data$gini.index = mh.data$gini.index
final.mh.data$divorce.rate = mh.data$divorce.rate
final.mh.data$poverty.rate = mh.data$poverty.rate
final.mh.data$suicide.rate = mh.data$suicide.rate.per.100000.people
final.mh.data$population = mh.data$population

# Fixed Effects Models

m5 = lm(final.mh.data$suicide.rate ~ alcohol.use.disorder + illicit.drug.use.disorder + had.serious.thoughts.of.suicide + major.depressive.episode
          + pain.reliever.use.disorder + substance.use.disorder + received.mental.health.services + serious.mental.illness + any.mental.illness + gdp 
          + unemployment.rate + gini.index + divorce.rate + poverty.rate 
          + year + state, data = final.mh.data)
summary(m5)

plot(m5)
shapiro.test(m5$res)                      # Shapiro-Wilks test of MV normality
bartlett.test(list(m5$res, m5$fit))       # Bartlett's test of homoskedasticity
bptest(m5)                                # Bresuch-Pagan test of homoskedasticity
vif(m5)                                   # VIF test of multicollinearity
library(lmtest)
dwtest(m5)  

# Random Effects Models (throwing errors)

m6 = lmer(final.mh.data$suicide.rate ~ alcohol.use.disorder + illicit.drug.use.disorder + had.serious.thoughts.of.suicide + major.depressive.episode
          + pain.reliever.use.disorder + substance.use.disorder + received.mental.health.services + serious.mental.illness + any.mental.illness + gdp 
          + unemployment.rate + gini.index + divorce.rate + poverty.rate 
          + (1 | year) + (1 | state), data = final.mh.data)
summary(m6)

fixef(m6)
ranef(m6)
coef(m6)


# FE and RE using PLM
install.packages("plm")
library(plm)
final.mh.data <- pdata.frame(final.mh.data, index=c("state", "year"))

pooled <- plm(final.mh.data$suicide.rate ~ alcohol.use.disorder + illicit.drug.use.disorder + had.serious.thoughts.of.suicide + major.depressive.episode
        + pain.reliever.use.disorder + substance.use.disorder + received.mental.health.services + serious.mental.illness + any.mental.illness + gdp 
        + unemployment.rate + gini.index + divorce.rate + poverty.rate 
        , data = final.mh.data, model ="pooling")
plmtest(pooled) 

fixed <- plm(final.mh.data$suicide.rate ~ alcohol.use.disorder + illicit.drug.use.disorder + had.serious.thoughts.of.suicide + major.depressive.episode
              + pain.reliever.use.disorder + substance.use.disorder + received.mental.health.services + serious.mental.illness + any.mental.illness + gdp 
              + unemployment.rate + gini.index + divorce.rate + poverty.rate 
              , data = final.mh.data, model ="within")
summary(fixed)                                       
fixef(fixed)                         
summary(fixef(fixed))

random <- plm(final.mh.data$suicide.rate ~ alcohol.use.disorder + illicit.drug.use.disorder + had.serious.thoughts.of.suicide + major.depressive.episode
             + pain.reliever.use.disorder + substance.use.disorder + received.mental.health.services + serious.mental.illness + any.mental.illness + gdp 
             + unemployment.rate + gini.index + divorce.rate + poverty.rate 
             , data = final.mh.data, model ="random")
summary(random)
ranef(random)

stargazer(pooled, fixed, random, type="text", single.row=TRUE)
pFtest(fixed, pooled)                     # F test for nested models: FE is better
phtest(fixed, random)                     # Hausman test: FE is better

# Multi-Level Poisson Model

p1 = glm(final.mh.data$suicide.rate ~ alcohol.use.disorder + illicit.drug.use.disorder + had.serious.thoughts.of.suicide + major.depressive.episode
        + pain.reliever.use.disorder + substance.use.disorder + received.mental.health.services + serious.mental.illness + any.mental.illness + gdp 
        + unemployment.rate + gini.index + divorce.rate + poverty.rate + state + year
        , family=poisson (link=log), data = final.mh.data)
summary(p1)
install.packages("AER")
library(AER)
dispersiontest(p1) 

