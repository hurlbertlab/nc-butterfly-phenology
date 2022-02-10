#load early date vs. temp and year data
library(plyr)
tempdat<-read.csv("data/temp.earlydate.uniquedate.triangle.static.4.months.filtered.csv")

#add species traits in
variables<-read.csv("data/species traits list.csv")
dat<-merge(variables,tempdat, by.x=c("species"),by.y=c('species'), all.x = T, all.y = T)

#setting voltinism as a factor
dat$voltinism<-as.factor(dat$voltinism)

#model creation
library(lme4) #for constructing mixed models
library(lmerTest) #for displaying p-values
library (car) #for Anova function

#trying out simple models. for fun. 
#species as interacting effect?
mod1 <- lm(jd~year*species, data=dat)
summary(mod1)
mod2 <- lm(jd~temp*species, data=dat)
summary(mod2)
#species as random effect?
mod3 <- lmer(jd  ~ year*temp + (1 | species), data=dat)
summary(mod3)
#adding in variables
#there are 4 variables of interest: voltinism, diettype, dietbreadth, overwinter
mod4 <- lmer(jd  ~ year*voltinism + (1 | species), data=dat)
summary(mod4)

Anova(mod3, type = 3, test.statistic =  "F") #(Anova from package car), 2 is appropriate for NS interactions


#example: year:diettype: response different for diff diettypes for diff years
#f-value: what does it mean? 
#species is a random variable, everything else is fixed effects. report f and df
#F subscript df, p=whatever
#don't need to report non-significance w/ values (didn't have an effect)

plot.design(earlydate~year+voltinism+diettype+dietbreadth+overwinter,data=dat)

#in the anova of the full mod,
#year (p<0.0001), temp (p<0.0001), voltinism (p=0.0024), diettype (p=0.0140), year:voltinism (P=0.0127) were significant

##code bit for model averaging
# frequentist model selection
library(MuMIn)

#fit all subsets of full model
fullmod_dredge<-dredge(fullmod) #take every combination and say which is the best statistically
#lowest AIC is best, and it's relative to the model
#top is the best model is the one dredge says is the best that you could run
#julian~volt+volt:year+diettype (could run the top two models and take things in and out and remove a factor or two 
#and compare them with them with a chi-square. if significant you can't drop what you dropped)


#fit all subsets of full model
list.good<-get.models(fullmod_dredge, subset = delta < 7) #get the top few
model1<-model.avg(list.good) #goes through good models and sees how many times a 
summary(model1) #returns relative 
coef(model1) #maybe less or more?
importance(model1) #number of models that that thing is important in with a proportion
#nail temp thing down and rerun with comparison of model with temp there or removed 

#used linear and mixed:effects
#temp, voltinism, year, overwintering, diettype

#temp, voltinism, year important in 100% of the models
#voltinism:year important in 83% of the models
#diettype important in 78% of the models



#model averaging for the set of best model (delta<7)
model1<-model.avg(get.models(fullmod_dredge, subset = delta < 7))
summary(model1) # returns relative variable importance

#best model
summary(get.models(fullmod_dredge, 1)[[1]])

##refitting model with only significant fixed effects + year:predictor interactions
bestmod<-lme(julian~voltinism+overwinter+migratory+dietbreadth, random=~1|species, method="ML", data=dat)

#fit all subsets of full model
bestmod_dredge<-dredge(bestmod)

#model averaging for the set of best model (delta<7)
model2<-model.avg(get.models(bestmod_dredge, subset = delta < 7))
summary(model2) # returns relative variable importance

#best model
summary(get.models(bestmod_dredge, 1)[[1]])
