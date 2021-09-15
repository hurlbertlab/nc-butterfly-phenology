#merging the full dataset and the calculated earlydate/temp values (without province for now)
setwd("~/Documents/Biology/butterfly paper 2016")
library(plyr)
tempdat<-read.csv("C:/Users/lhamo/Documents/Biology/butterfly paper 2016/data/fulldat.8months.NC.2016.csv")
#adjusted
tempdat<-read.csv("C:/Users/lhamo/Documents/Biology/butterfly paper 2016/data/fulldat.8months.NC.adjusted.2016.csv") #adjusted
#subsetted
tempdat<-subset(tempdat,species=="Nymphalis antiopa")
tempdat<-na.omit(tempdat)

#merge 2: adding the variables in 
variables<-read.csv("C:/Users/lhamo/Documents/Biology/butterfly paper 2016/data/species list 2017.csv")
drop<-c("Cname","county","observer","number","comments","month", "day","dateCalc","province","coid","julian","voltinismnotes","dietnotes")
drop2<-which(names(variables) %in% drop)
variables1<-variables[,-drop2]
variables2<- unique(variables1[,1:6])
dat<-merge(variables2,tempdat, by.x=c("species"),by.y=c('species'), all.x = T, all.y = T)

#setting voltinism as a factor
dat$voltinism<-as.factor(dat$voltinism)

dat<-na.omit(dat)

## LOOKING FOR YEAR EFFECT ACROSS ENTIRE DATASET (aka alldat) ##
# Required packages
library(nlme)

# defining full model (=most complex) to consider
fullmod<- lme(julian~year+temp+voltinism+diettype+dietbreadth+overwinter+year:voltinism+year:diettype+year:dietbreadth+year:overwinter, random=~1|species, method="ML", data=dat)
fullmod<- lme(earlydate~year+temp+voltinism+diettype+dietbreadth+overwinter+year:voltinism+year:diettype+year:dietbreadth+year:overwinter+temp:voltinism+temp:diettype+temp:dietbreadth+temp:overwinter, random=~1|species, method="ML", data=dat)
fullmod<-lme(temp*overwinter) #(shorthand)
summary(fullmod)
anova(fullmod)

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
