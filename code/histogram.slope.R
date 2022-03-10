#LOAD IN DATA
#merging the full dataset and the calculated earlydate/temp values (without province for now)
library(plyr)

#load temp/julian data. Any fulldat file may be substituted
#triangle
alldat<-read.csv("data/temp.earlydate.uniquedate.triangle.static.4.months.csv")

##############################################################################
#for loop excluding outliers using cooks D
#creating for loop (First I'll just try to get this to read the plots and put them in a pdf)
species<-unique(alldat$species)
pdf("julian.year.4months.static.unique.date.cooksd.pdf",width=10, height=8)
par(mfrow=c(2,3))

for (s in species) {
  df=alldat[alldat$species==s,]
  lm.sub=lm(df$jd~df$year)
  cooksD <- cooks.distance(lm.sub)
  influential <- as.numeric(names(cooksD)[(cooksD > (4/nrow(df)))])
  length_influential <- length(influential) 
  if (length_influential >= 1) {
    df_screen <- df[-influential, ]
    plot(df_screen$jd~df_screen$year, xlab='year', ylab='Early Date (julian)', main=paste(s))
    lm.sub_screen = lm(df_screen$jd~df_screen$year, xlab="year", ylab = "julian")
    abline(lm(df_screen$jd~df_screen$year))
    rsquared<-paste("R2=",format(summary(lm.sub_screen)$r.squared, digits=4))
    pvalue<-paste("p=", format(summary(lm.sub_screen)$coefficients[2,4]), digits=4)
  } else {
    plot(df$jd~df$year, xlab='year', ylab='Early Date (julian)', main=paste(s))
    lm.sub = lm(df$jd~df$year, xlab="year", ylab = "julian")
    abline(lm(df$jd~df$year))
    rsquared<-paste("R2=",format(summary(lm.sub)$r.squared, digits=4))
    pvalue<-paste("p=", format(summary(lm.sub)$coefficients[2,4]), digits=4)
  }
  legend("topright", bty="n", legend=c(rsquared,pvalue))
}
dev.off()

#############################################################################################
#an additional for loop for temp
pdf("julian.temp.4months.static.unique.date.cooksd.pdf",width=10, height=8)
par(mfrow=c(2,3))

for (s in species) {
  df=alldat[alldat$species==s,]
  lm.sub=lm(df$jd~df$temp)
  cooksD <- cooks.distance(lm.sub)
  influential <- as.numeric(names(cooksD)[(cooksD > (4/nrow(df)))])
  length_influential <- length(influential) 
  if (length_influential >= 1) {
    df_screen <- df[-influential, ]
    plot(df_screen$jd~df_screen$temp, xlab='temp', ylab='Early Date (julian)', main=paste(s))
    lm.sub_screen = lm(df_screen$jd~df_screen$temp, xlab="temp", ylab = "julian")
    abline(lm(df_screen$jd~df_screen$temp))
    rsquared<-paste("R2=",format(summary(lm.sub_screen)$r.squared, digits=4))
    pvalue<-paste("p=", format(summary(lm.sub_screen)$coefficients[2,4]), digits=4)
  } else {
    plot(df$jd~df$temp, xlab='temp', ylab='Early Date (julian)', main=paste(s))
    lm.sub = lm(df$jd~df$temp, xlab="temp", ylab = "julian")
    abline(lm(df$jd~df$temp))
    rsquared<-paste("R2=",format(summary(lm.sub)$r.squared, digits=4))
    pvalue<-paste("p=", format(summary(lm.sub)$coefficients[2,4]), digits=4)
  }
  legend("topright", bty="n", legend=c(rsquared,pvalue))
}
dev.off()



#############################################################################################
#subsetting by an arbitrary start date (I'm saying july 1, approx. 182)
#julian vs. year
pdf("julian.year.4months.static.unique.date.july1.pdf",width=10, height=8)
par(mfrow=c(2,3))

for (s in species) {
  df=alldat[alldat$species==s,]
  df2 <- df[(df$jd < 182) , ]
  lm.sub=lm(df2$jd~df2$year)
  plot(df2$jd~df2$year, xlab='year', ylab='Early Date (julian)', main=paste(s))
  abline(lm(df2$jd~df2$year))
  rsquared<-paste("R2=",format(summary(lm.sub)$r.squared, digits=4))
  pvalue<-paste("p=", format(summary(lm.sub)$coefficients[2,4]), digits=4)
  legend("topright", bty="n", legend=c(rsquared,pvalue))
}
dev.off()

#############################################################################################
#subsetting by an arbitrary start date (I'm saying july 1, approx. 182)
#julian vs. temp
pdf("julian.temp.4months.static.unique.date.july1.pdf",width=10, height=8)
par(mfrow=c(2,3))

for (s in species) {
  df=alldat[alldat$species==s,]
  df2 <- df[(df$jd < 182) , ]
  lm.sub=lm(df2$jd~df2$temp)
  plot(df2$jd~df2$year, xlab='temp', ylab='Early Date (julian)', main=paste(s))
  abline(lm(df2$jd~df2$temp))
  rsquared<-paste("R2=",format(summary(lm.sub)$r.squared, digits=4))
  pvalue<-paste("p=", format(summary(lm.sub)$coefficients[2,4]), digits=4)
  legend("topright", bty="n", legend=c(rsquared,pvalue))
}
dev.off()

###################################################################################

#generate a dataframe of species,slope,r-squared and p-value
output = data.frame(species = character(),
                    year.slope=numeric(),
                    year.rsquared = numeric(),
                    year.pvalue = numeric(),
                    temp.slope=numeric(),
                    temp.rsquared = numeric(),
                    temp.pvalue = numeric())

species<-unique(alldat$species)


for (s in species) {
  df<-alldat[alldat$species==s,]
  lm.sub.year<-lm(df$jd~df$year)
  lm.sub.temp<-lm(df$jd~df$temp)
  cooksD.year <- cooks.distance(lm.sub.year)
  cooksD.temp <- cooks.distance(lm.sub.temp)
  influential.year <- as.numeric(names(cooksD.year)[(cooksD.year > (4/nrow(df)))])
  influential.temp <- as.numeric(names(cooksD.temp)[(cooksD.temp > (4/nrow(df)))])
  length_influential.year <- length(influential.year)
  length_influential.temp <- length(influential.temp)
  if (length_influential.year >= 1) {
    df_screen.year <- df[-influential.year, ]
    lm.sub_screen.year = lm(df_screen.year$jd~df_screen.year$year)
  } else {
    lm.sub_screen.year = lm(df$jd~df$year)
  }
  if (length_influential.temp >= 1) {
    df_screen.temp <- df[-influential.temp, ]
    lm.sub_screen.temp = lm(df_screen.temp$jd~df_screen.temp$temp)
  } else {
    lm.sub_screen.year = lm(df$jd~df$temp)
  }
  year.slope<-summary(lm.sub_screen.year)$coefficients[2,1] 
  year.rsquared<-summary(lm.sub_screen.year)$r.squared
  year.pvalue<-summary(lm.sub_screen.year)$coefficients[2,4]
  temp.slope<-summary(lm.sub_screen.temp)$coefficients[2,1] 
  temp.rsquared<-summary(lm.sub_screen.temp)$r.squared
  temp.pvalue<-summary(lm.sub_screen.temp)$coefficients[2,4]
  tempoutput<- data.frame(species = s, 
                          year.slope=year.slope, 
                          year.rsquared = year.rsquared,
                          year.pvalue = year.pvalue,
                          temp.slope=temp.slope, 
                          temp.rsquared = temp.rsquared,
                          temp.pvalue = temp.pvalue)
  output<-rbind(output,tempoutput)
}

#create a csv that includes temperature and julian dates, if desired
write.csv(output, "data/year.temp.earlydate.uniquedate.triangle.cooksd.4.months.csv")

#################################################################################
#histograms and summary stats

mean(output$slope)
ok1<-subset(output, output$pvalue<0.05)
ok2<-subset(output,output$slope<0)

hist(output$slope, xlab="Slope (days/degree Celsius)", ylab="Julian date",main="")
abline(v=-0.5056561,col="red",lwd=2)

histogram1<-ggplot(output, aes(x=slope))+
  geom_histogram(binwidth=0.5)+
  abline(v=-0.5056561,col="red",lwd=2)+
  theme_classic()+theme(axis.line=element_line(colour="grey80"),axis.text=element_text(size=18), axis.title=element_text(size=18,face="bold"), title=element_text(size=18,face="bold")) +
  xlab("Slope (days/year)")+
  ylab("Julian date")

#a new better histogram
library(ggplot2)
    
    #year
    p <- ggplot(year.output, aes(year.output$slope)) 
    p <- p + geom_histogram(breaks=seq(-2.5, 2.5, by=0.5),fill="grey",col="darkgrey")
    p <- p + xlab("Slope (days/year)") + ylab("Julian date") 
    p <- p + geom_vline(xintercept = -0.5056561, col="red", lwd=1)
    
    #temp
    p <- ggplot(temp.output, aes(temp.output$slope)) 
    p <- p + geom_histogram(breaks=seq(-20, 20, by=3),fill="grey",col="darkgrey")
    p <- p + xlab("Slope (days/degree Celsius)") + ylab("Julian date") 
    p <- p + geom_vline(xintercept = -1.618129, col="red", lwd=1)
    
#triangle
    #year
      #11/44 negative, 10 significant, mean=0.6005109
    #temp
      #33/48 negative, 6 significant, mean=-4.426349
    #in a simple regression, there is no significant correlation b/w
      #year and temp
      

#binomial test to test whether number of negative slopes is greater 
binom.test(12, 44, 0.5, alternative="greater")

