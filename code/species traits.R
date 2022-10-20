#load early date vs. temp and year data
library(plyr)
library(dplyr)
library(tidyr)
tempdat<-read.csv("data/year.temp.earlydate.uniquedate.triangle.cooksd.4.months.csv")

#add species traits in
variables<-read.csv("data/species traits list.csv")
dat<-merge(variables,tempdat, by.x=c("species"),by.y=c('species'), all.x = T, all.y = T)

#drop migratory or partially migratory spp (if not already removed)
dat<-dat[dat$migratory != "yes", ]   

#create a csv with the values for just the species of interest, if desired
write.csv(dat, "data/temp.earlydate.uniquedate.triangle.static.4.months.focalspecies.csv")

#model creation
library(lme4) #for constructing mixed models
library(lmerTest) #for displaying p-values
library (car) #for Anova function

#trying out simple models. for fun. 
#year
mod1 <- lm(year.slope~overwinter, data=dat, weights=(1/dat$sd.year.earlydate))
summary(mod1)
mod2 <- lm(year.slope~voltinism, data=dat, weights=(1/dat$sd.year.earlydate))
summary(mod2)
#temp
mod3 <- lm(temp.slope~overwinter, data=dat, weights=(1/dat$sd.temp.earlydate))
summary(mod3)
mod4 <- lm(temp.slope~voltinism, data=dat, weights=(1/dat$sd.temp.earlydate))
summary(mod4)
#multiple
mod5 <- lm(year.slope  ~ overwinter*voltinism, data=dat , weights=(1/dat$sd.temp.earlydate))
summary(mod5)
mod6 <- lm(temp.slope  ~ overwinter*voltinism, data=dat , weights=(1/dat$sd.temp.earlydate))
summary(mod6)

#model comparison
anova(mod2, mod5) #year, overwinter vs overwinter + voltinism
anova(mod4, mod6) #temp, overwinter vs overwinter + voltinism

#exploring the relationship between traits and phenology
#are multivoltine species appearing earlier in the year? Smaller mean early flight date phenology
#mean early date across all our different points as an explanatory
  #the earlier in the year you are, the more sensitive your response
  #interaction of earlydate with voltinism 
#year
mod7 <- lm(year.slope~voltinism*mean.year.earlydate, data=dat, weights=(1/dat$sd.year.earlydate))
summary(mod7)
Anova(mod7)
mod8 <- lm(year.slope~overwinter*mean.year.earlydate, data=dat, weights=(1/dat$sd.year.earlydate))
summary(mod8)
#temp
mod9 <- lm(temp.slope~voltinism*mean.temp.earlydate, data=dat, weights=(1/dat$sd.temp.earlydate))
summary(mod9) #significant one
Anova(mod9)
mod10 <- lm(temp.slope~overwinter*mean.temp.earlydate, data=dat, weights=(1/dat$sd.year.earlydate))
summary(mod10)
#interactions
mod11 <- lm(year.slope~voltinism*mean.year.earlydate+overwinter, data=dat, weights=(1/dat$sd.temp.earlydate))
summary(mod11)
mod12 <- lm(temp.slope~voltinism*mean.temp.earlydate+overwinter, data=dat, weights=(1/dat$sd.temp.earlydate))
summary(mod12) 
#voltinism on its own
mod13 <- lm(temp.slope~voltinism, data=dat, weights=(1/dat$sd.temp.earlydate))
summary(mod13) #significant one

#model comparison
anova(mod7, mod11) #year, overwinter vs overwinter + voltinism
anova(mod9, mod12) #temp, overwinter vs overwinter + voltinism
anova(mod13, mod9) 


#visulizing distribution of slopes by factors
library(ggplot2)
dat2 <- dat
#temp slope vs. voltinism violin plot
dat2$voltinism <- as.factor(dat2$voltinism)
dat3<-dat2[!(dat2$voltinism == 3.5 | dat2$voltinism == 5),] #use this version of the dataset
#if you want to drop the one 3.5 and 5 voltinism species
voltplot<- ggplot(dat3, aes(x=voltinism, y=temp.slope, fill=voltinism)) + 
                  geom_violin()+
                  xlab("Voltinism") + 
                  theme_classic(base_size = 18)+
                  stat_summary(fun = "mean",
                    geom = "crossbar", 
                    width = 0.5,
                    colour = "red")+
                  theme(legend.position = "none",
                         axis.title.y = element_blank())+
                  scale_fill_grey()+
                  geom_hline(yintercept=0,linetype=2)
voltplot

#temp slope vs. overwinter boxplot
#need to reorder overwintering treatment in an order that makes sense
dat2$overwinter <- factor(dat2$overwinter, levels=c("larvae", "pupae", "adults"))
winterplot<- ggplot(dat2, aes(x=overwinter, y=temp.slope, fill = overwinter)) + 
             geom_violin()+
              xlab("Overwintering stage") + 
              ylab("Phenological sensitivity")+
              theme_classic(base_size = 18)+
              stat_summary(fun = "mean",
                geom = "crossbar", 
                width = 0.5,
                colour = "red")+
              theme(legend.position = "none")+
              scale_fill_grey()+
              geom_hline(yintercept=0,linetype=2)
winterplot

earlydateplot<- ggplot(dat2, aes(x=overwinter, y=mean.temp.earlydate, fill = overwinter)) + 
  geom_violin()+
  xlab("Overwintering stage") + 
  ylab("Mean onset date")+
  theme_classic(base_size = 18)+
  stat_summary(fun = "mean",
               geom = "crossbar", 
               width = 0.5,
               colour = "red")+
  theme(legend.position = "none")+
  scale_fill_grey()
earlydateplot

#year slope plots
#year slope vs. voltinism boxplot
voltplot2<- ggplot(dat3, aes(x=voltinism, y=year.slope)) + 
                  geom_violin()+
                  stat_summary(fun.data=mean_sdl, 
                                geom="pointrange", color="red")+
                  xlab("Voltinism") + 
                  ylab("Earlydate vs. year slope")+
                  geom_hline(yintercept=0, linetype=2)+
                  theme_classic(base_size = 13.2)
voltplot2

#year slope vs. overwinter boxplot
winterplot2<- ggplot(dat2, aes(x=overwinter, y=year.slope)) + 
                     geom_violin()+
                     stat_summary(fun.data=mean_sdl, 
                     geom="pointrange", color="red")+
                     xlab("Overwinter") + 
                     ylab("Earlydate vs. year slope")+
                     geom_hline(yintercept=0, linetype=2)+
              theme_classic(base_size = 13.2)
winterplot2

#overall summary plot (requires reshaping the data)
tempslopes<-dat2[c("temp.slope")] #pull the temp slopes
tempslopes$type <- "temp" #add a column with slope type
tempslopes<-rename(tempslopes, "temp.slope"="slope") #rename slope column for rbinding
yearslopes<-dat2[c("year.slope")] #pull the year slopes
yearslopes$type <- "year" #add a column with slope type
yearslopes<-rename(yearslopes, "slope" = "year.slope") #rename slope column for rbinding
slopesum<-rbind(yearslopes,tempslopes) #you can bind them if you want them on the same graph
#create graph for temp
tempplot<- ggplot(tempslopes, aes(x=type, y=temp.slope)) + 
  geom_violin(fill="light grey")+
  xlab("All species") + 
  ylab("Phenological sensitivity")+
  stat_summary(fun = "mean",
               geom = "crossbar", 
               width = 0.5,
               colour = "red")+
  theme_classic(base_size = 18)+
  theme(legend.position = "none",
        axis.text.x = element_blank())+
  geom_hline(yintercept=0,linetype=2)
tempplot

#create cowplot
library(cowplot)
comboplot2<-plot_grid(tempplot, voltplot, winterplot,earlydateplot,
                     nrow=2,
                     ncol=2,
                     labels = c('A', 'B', 'C', 'D'),
                     label_size = 18, 
                     align = "hv",
                     vjust = 1)
save_plot("fig2.png",comboplot2, nrow = 2, ncol = 2)

#create graph for year
yearplot<- ggplot(yearslopes, aes(x=type, y=slope)) + 
  geom_violin()+
  stat_summary(fun.data=mean_sdl, 
               geom="pointrange", color="red")+
  xlab("Dependent variable") + 
  ylab("Earlydate vs. year slope")+
  geom_hline(yintercept=0, linetype=2)+
  theme_classic(base_size = 13.6)
yearplot

#Interaction plot
interactionplot <- ggplot(dat2, aes(x = mean.temp.earlydate, y = temp.slope, color = voltinism)) +
              theme_classic() +
              labs(x = "Mean onset date", y = "Phenological sensitivity", color = "voltinism")+
              geom_point(size = 3, aes(shape=overwinter)) +
              geom_smooth(method = "lm", aes(group = voltinism), se = F)+
  theme_classic(base_size = 18)
interactionplot
#dimensions = 600 x 420

#plot without interactions for supp image
interactionplot2 <- ggplot(dat2, aes(x = mean.temp.earlydate, y = temp.slope)) +
  theme_classic(base_size = 18) +
  labs(x = "Mean onset date", y = "Phenological sensitivity")+
  geom_point(color = 'black', size=4) +
  geom_point(data=dat2, aes( x=130.11111, y=-2.7488826,), color = 'darkturquoise', size = 6, shape=16) +
  geom_point(data=dat2, aes( x=78.61905, y=-0.4542952,), color = 'dark orange', size = 6, shape=16) +
  geom_smooth(method = "lm", se=F, color = 'dark grey', size=2)
interactionplot2
#dimensions = 600 x 420


#example: year:diettype: response different for diff diettypes for diff years
#f-value: what does it mean? 
#species is a random variable, everything else is fixed effects. report f and df
#F subscript df, p=whatever
#don't need to report non-significance w/ values (didn't have an effect)

plot.design(earlydate~year+voltinism+overwinter,data=dat)

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
