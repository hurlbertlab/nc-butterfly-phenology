#LOAD IN DATA
#merging the full dataset and the calculated earlydate/temp values (without province for now)
library(plyr)

#load temp/julian data. Any fulldat file may be substituted
#triangle
alldat<-read.csv("C:/Users/lhamo/Documents/git/nc-butterfly-phenology/data/temp.earlydate.triangle.static.4.months.csv")
        
##############################################################################

#creating for loop (First I'll just try to get this to read the plots and put them in a pdf)
species<-unique(alldat$species)
pdf("julian.temp.4months.static.pdf",width=10, height=8)
par(mfrow=c(2,3))

for (s in species) {
  df=alldat[alldat$species==s,]
  lm.sub=lm(df$julian~df$temp,xlab="temp", ylab="julian", group=species)
  plot(df$julian~df$temp, xlab='temp', ylab='Early Date (julian)', main=paste(s))
  abline(lm(df$julian~df$temp))
  rsquared<-paste("R2=",format(summary(lm.sub)$r.squared, digits=4))
  pvalue<-paste("p=", format(summary(lm.sub)$coefficients[2,4]), digits=4)
  legend("topright", bty="n", legend=c(rsquared,pvalue))
}
dev.off()

#generate a dataframe of species,slope,r-squared and p-value
output = data.frame(species = character(),
                    slope=numeric(),
                    rsquared = numeric(),
                    pvalue = numeric())

species<-unique(alldat$species)

for (s in species) {
  df<-alldat[alldat$species==s,]
  lm.sub<-lm(df$julian~df$year)
  slope<-summary(lm.sub)$coefficients[2,1] 
  rsquared<-summary(lm.sub)$r.squared
  pvalue<-summary(lm.sub)$coefficients[2,4]
  tempoutput<- data.frame(species = s, slope=slope, rsquared = rsquared,
                          pvalue = pvalue)
  output<-rbind(output,tempoutput)
}

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

