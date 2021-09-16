#Finding the 10% and our 25% flight date for each species for each year
#and merging with temperature data generated from extract.temp.NC.R

#load approximation data
alldat<-read.csv("C:/Users/lhamo/Documents/git/nc-butterfly-phenology/data/bnc_thru2018.csv")

#add add labels to designate triangle observations
labels<-read.csv("C:/Users/lhamo/Documents/git/nc-butterfly-phenology/data/triangle_labels.csv")

#subset labels with lowercase county format
labels<-labels[c("county_lc","triangle")]

#rename label columns 
colnames(labels)<-c("county", "triangle")

#merge with approximation
dat<-merge(alldat,labels, by.x=c("county"),by.y=c("county"), all.x = F, all.y = T)

#subset data after 1990
dat <-subset(dat, year > 1989)

#subset triangle observations
dat2 <- subset(dat, triangle == "Y")

#select species to analyze
#summarize number of years of data per species
speciessummary<-aggregate(x = dat2$number, by = list(dat2$Cname, dat2$year), FUN = sum)
colnames(speciessummary)<-c("Cname", "year", "number") #name columns
#subset species/year pairs with a minimum of 30 observations
speciessummary2 <- speciessummary[ which(speciessummary$number >= 30), ]
#summarize further by counting number of available years for each species
library(plyr)
speciessummary3<-count(speciessummary2$Cname)
colnames(speciessummary3)<-c("Cname", "freq") #name columns
#select species for which there are 10 or more years of data
speciessummary4 <- speciessummary3[ which(speciessummary3$freq >= 10), ]
speciessummary4$minyear="Y"
#subset speciessummary with these species
trianglespp <- speciessummary4[c(1,3)] 

#merge approximation with designation of min data ("minyear") to subset species
dat3<-merge(dat2,trianglespp, by.x=c("Cname"),by.y=c("Cname"), all.x = F, all.y = T)

#load species trait data (includes scientific names)
traits<-read.csv("C:/Users/lhamo/Documents/git/nc-butterfly-phenology/data/species traits list.csv")

#merge traits list with approximation
dat4<-merge(traits, dat3, by.x=c("Cname"), by.y=c("Cname"), all.x = F, all.y = F)

#Changing column names to match the 2nd summary dataframe
alldat2<-dat4[c("county", "species", "number", "year", "jd")]

library(dplyr)

# First need to create a vector of dates for individuals
species<-unique(alldat2$species)
year=1990:2018

#create an empty output for the for loop to put values into
output = data.frame(species=character(),
                    year=integer(),
                    julian=numeric())
#-------------------------------------------------------

# Pulling out date associated with first 10 % individuals

#perform this function to return the 10 % date
dateXpct.ind = function(data, pct) {
  data = data[order(data$jd, decreasing = F), ]
  data$cumindividuals = cumsum(data$number)
  data$cumpct = data$cumindividuals/sum(data$number)
  #earliest date at which 'num' individuals have cumulatively been observed
  mindate = min(data$jd[data$cumpct >= pct])
  return(mindate)
}

#for loop to perform this for each species/year/region 
for (s in species){
    for (y in year){
      b=subset(alldat2, year==y & species==s)
      mindate<-dateXpct.ind(b,0.10)
      datoutput = data.frame(species = s, year = y, julian=mindate)
      output=rbind(output,datoutput)
  }
}

output2 <- output[!is.infinite(output$julian),] #note: many many Infs where there were not enough data
write.csv(output2,file="C:/Users/lhamo/Documents/git/nc-butterfly-phenology/data/10.percent.earlydate.triangle.csv")

#load temperature data
tempdat<-read.csv("C:/Users/lhamo/Documents/git/nc-butterfly-phenology/data/tempmean.4.months.triangle.static.2016.csv")

#average temperatures across all three triangle counties for each year (if desired)
tempdat2<-aggregate(temp~year, tempdat, FUN=mean)

#merge temperature data with earlydate
tempjulian = merge(tempdat2, output2, by.x = c('year'), by.y =c('year'), all.x = T, all.y = T)
tempjulian<-na.omit(tempjulian)

#create a csv that includes temperature and julian dates
write.csv(tempjulian, "C:/Users/lhamo/Documents/git/nc-butterfly-phenology/data/temp.earlydate.triangle.static.4.months.csv")

