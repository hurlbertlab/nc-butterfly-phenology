#FINDING THE 10 % and 25% FLIGHT DATE FOR EACH SPECIES FOR EACH YEAR
#AND MERGING THIS WITH THE TEMPERATURE DATA
#TO CREATE A NEW DOCUMENT WITH NEW PROXIES WITH WHICH TO RERUN MY ANALYSES
setwd("~/Documents/Biology/butterfly paper 2016")
alldat<-read.csv("C:/Users/lhamo/Documents/Biology/butterfly paper 2016/data/approximation_thru_2016.csv")

#adding province labels
labels<-read.csv("C:/Users/lhamo/Documents/Biology/BIOL 692H/fixing.province.names.2.24.2016.csv")

#subset labels (with lowercase county format)
labels<-labels[c("county_lc","triangle")]

#rename label columns 
colnames(labels)<-c("county", "triangle")


#merge with approximation
dat<-merge(alldat,labels, by.x=c("county"),by.y=c("county"), all.x = F, all.y = T)

#Changing column names to match the 2nd summary dataframe
colnames(dat)<-c("county","x","Cname","species","observer","number","comments","dateCalc","year","julian",
                 "voltinism","voltinismnotes","diettype","dietbreadth","dietnotes","migratory","overwinter",
                 "triangle")
alldat<-dat[c("county", "species", "number", "year", "julian", "triangle")]

#subset data after 1990
alldat <-subset(alldat, year > 1989)

#subset triangle observations
alldat2 <- subset(alldat, triangle == "Y")

library(dplyr)

# First need to create a vector of dates for individuals
species<-unique(alldat2$species)
year=1990:2016

#create an empty output for the for loop to put values into
output = data.frame(species=character(),
                    year=integer(),
                    julian=numeric())
#-------------------------------------------------------

# Pulling out date associated with first 10 % individuals

#perform this function to return the 10 % date
dateXpct.ind = function(data, pct) {
  data = data[order(data$julian, decreasing = F), ]
  data$cumindividuals = cumsum(data$number)
  data$cumpct = data$cumindividuals/sum(data$number)
  #earliest date at which 'num' individuals have cumulatively been observed
  mindate = min(data$julian[data$cumpct >= pct])
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
write.csv(output2,file="C:/Users/lhamo/Documents/Biology/butterfly paper 2016/data/10.percent.earlydate.triangle.csv")

#load temperature data
tempdat<-read.csv("C:/Users/lhamo/Documents/Biology/butterfly paper 2016/data/tempmean.4.months.triangle.static.2016.csv")

#average temperatures across all three triangle counties for each year (if desired)
tempdat2<-aggregate(temp~year, tempdat, FUN=mean)

#merge temperature data with earlydate
tempjulian = merge(tempdat2, output2, by.x = c('year'), by.y =c('year'), all.x = T, all.y = T)
tempjulian<-na.omit(tempjulian)

#create a csv that includes temperature and julian dates
write.csv(tempjulian, "C:/Users/lhamo/Documents/Biology/butterfly paper 2016/data/temp.earlydate.triangle.static.4.months.csv")

