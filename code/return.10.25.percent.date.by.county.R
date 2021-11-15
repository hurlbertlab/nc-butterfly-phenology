#Finding the 10% and our 25% flight date for each species for each year
#and merging with temperature data generated from extract.temp.NC.R

#load approximation data
alldat<-read.csv("C:/Users/lhamo/Documents/git/nc-butterfly-phenology/data/bnc_thru2020.csv")

#add add labels to designate triangle observations
labels<-read.csv("C:/Users/lhamo/Documents/git/nc-butterfly-phenology/data/triangle_labels.csv")

#subset labels with lowercase county format
labels<-labels[c("county_lc","triangle")]

#rename label columns 
colnames(labels)<-c("county", "triangle")

#merge labels with approximation
dat<-merge(alldat,labels, by.x=c("county"),by.y=c("county"), all.x = F, all.y = T)

#subset data after 1990
dat <-subset(dat, year > 1989)

#subset triangle observations
dat2 <- subset(dat, triangle == "Y")

#where number=0, change to "1"
dat2$number[dat2$number == 0] <- 1

#aggregate numbers by observer, species, year, and julian day
#(i.e. if an observer went out on the same day and counted the same spp
#it's counted as a single line of observation)
dat3<-aggregate(x = dat2$number, by = list(dat2$Cname, dat2$observer, dat2$jd, dat2$year), FUN = sum)
colnames(dat3)<-c("Cname", "observer", "jd", "year", "number") #rename columns

#create a column where any number >/= 1 is listed as "1"
#since we've already changed zeros to 1, this is every number
dat3$abundance <- 1

#select species to analyze. skip this if needed
#summarize number of years of data per species
speciessummary<-aggregate(x = dat3$number, by = list(dat3$Cname, dat3$year), FUN = sum)
colnames(speciessummary)<-c("Cname", "year", "abundance") #name columns
#subset species/year pairs with a minimum of 30 observations
speciessummary2 <- speciessummary[ which(speciessummary$abundance >= 30), ]
#summarize further by counting number of available years for each species
library(plyr)
speciessummary3<-count(speciessummary2$Cname)
colnames(speciessummary3)<-c("Cname", "freq") #name columns
#select species for which there are 10 or more years of data
speciessummary4 <- speciessummary3[ which(speciessummary3$freq >= 10), ]
speciessummary4$minyear="Y"
#subset speciessummary with these species
trianglespp <- speciessummary4[c(1,3)] 
colnames(trianglespp)<-c("Cname", "minyear") #name columns
#merge approximation with designation of min data ("minyear") to subset species
dat4<-merge(dat3,trianglespp, by.x=c("Cname"),by.y=c("Cname"), all.x = F, all.y = T)

#load species trait data (includes scientific names)
traits<-read.csv("C:/Users/lhamo/Documents/git/nc-butterfly-phenology/data/species traits list.csv")

#merge traits list with approximation
dat4<-merge(traits, dat3, by.x=c("Cname"), by.y=c("Cname"), all.x = F, all.y = F)

#subset column names to match the 2nd summary dataframe
alldat2<-dat4[c("species", "abundance", "number", "year", "jd")]

#filter out years that have less than 10 unique dates
library(dplyr)

goodspeciesyears <- alldat2 %>% 
  count(species, year) %>%
  right_join(alldat2 , by = c("species", "year")) %>%
  filter(n >= 10)

# First need to create a vector of dates for individuals
species<-unique(goodspeciesyears$species)
year=1990:2020

#create an empty output for the for loop to put values into
output = data.frame(species=character(),
                    year=integer(),
                    jd=numeric())
#-------------------------------------------------------

# Pulling out date associated with first 10 % individuals

#perform this function to return the 10 % date. 
#use the "abundance" column if interested in only unique date/observer data
dateXpct.ind = function(data, pct) {
  data = data[order(data$jd, decreasing = F), ]
  data$cumindividuals = cumsum(data$abundance)
  data$cumpct = data$cumindividuals/sum(data$abundance)
  #earliest date at which 'num' individuals have cumulatively been observed
  mindate = min(data$jd[data$cumpct >= pct])
  return(mindate)
}

#for loop to perform this for each species/year 
for (s in species){
    for (y in year){
      b=subset(goodspeciesyears, year==y & species==s)
      mindate<-dateXpct.ind(b,0.10)
      datoutput = data.frame(species = s, year = y, jd=mindate)
      output=rbind(output,datoutput)
  }
}

output2 <- output[!is.infinite(output$jd),] #note: many many Infs where there were not enough data
write.csv(output2,file="C:/Users/lhamo/Documents/git/nc-butterfly-phenology/data/10.percent.earlydate.uniquedate.triangle.csv")

########################################################
#load and merge in temperature data

#load temperature data
tempdat<-read.csv("C:/Users/lhamo/Documents/git/nc-butterfly-phenology/data/tempmean.4.months.triangle.static.2020.csv")

#average temperatures across all three triangle counties for each year (if desired)
tempdat2<-aggregate(temp~year, tempdat, FUN=mean)

#merge temperature data with earlydate
tempjulian = merge(tempdat2, output2, by.x = c('year'), by.y =c('year'), all.x = T, all.y = T)
tempjulian<-na.omit(tempjulian)

#create a csv that includes temperature and julian dates
write.csv(tempjulian, "C:/Users/lhamo/Documents/git/nc-butterfly-phenology/data/temp.earlydate.uniquedate.triangle.static.4.months.filtered.csv")

#-------------------------------------------------------------
#to visualize differences in sampling effort between years
#make a for loop that generates the following:
#for EACH species, for EACH year, a scatterplot  
#of number of observations vs. julian date

#first, load and prep the data
#in this case, we're using alldat2

#aggregate number or abundance by species, year, and julian date
alldat3<-aggregate(abundance ~ species+year+jd, data = goodspeciesyears, FUN = sum)

#merge calculated earlydate into alldat3
#this will allow us to represent it on our graphs using an abline
#first create a unique species.year column in alldat3 and tempjulian
alldat3$speciesyear <- do.call(paste, c(alldat3[1:2], sep = ""))  # Apply do.call & paste
  col_order <- c("species", "year","jd","temp") #reorder columns
  tempjulian <- tempjulian[, col_order]
tempjulian$speciesyear <- do.call(paste, c(tempjulian[1:2], sep = ""))  # Apply do.call & paste
#rename columns to indicate which one is earlydate
colnames(tempjulian)<-c("species", "year", "earlydate", "temp", "speciesyear") #rename columns
#merge by speciesyear
alldat4<-merge(alldat3,tempjulian, by.x=c("speciesyear"),by.y=c("speciesyear"), all.x = F, all.y = F)
  #rename columns of merged dat
  colnames(alldat4)<-c("speciesyear", "species", "year", "jd", "abundance",
                         "species.y", "year.y", "earlydate", "temp")
  alldat4<-alldat4[c("speciesyear", "species", "year", "jd", "abundance","earlydate", "temp")]   #subset desired columns

# Need to create a vector of species and dates for individuals
species<-unique(alldat4$species)

#generate empty pdf to fill 
pdf("numbersightings.peryear.perspecies.uniquedate.pdf",width=10, height=8)
par(mfrow=c(3,5), mar=c(3,1,3,1), oma=c(4,4,3,0))


#for loop that returns number vs. julian date per year per species
for (s in species) {
  df1<-subset(alldat4, species==s)
  year<-sort(unique(df1$year))
  for (y in year){
    df2<-subset(df1, year==y & species==s)
    plot(df2$jd,df2$abundance, xlab='', ylab='', cex=df2$abundance, pch=16,
         xlim=c(1,365), yaxt='n', cex.axis=1)
    legend("topleft", legend=y, bty='n')
    abline(v=df2$earlydate,col="red",lwd=2)
    
    if (y == year[1] | y == year[16])
    {
      mtext(s,3, outer=TRUE)
      mtext("julian day", 1, outer=TRUE)
      mtext("abundance", 2, outer=TRUE)
    }
    
    if (y == year[length(year)])
    {
      par(mfrow=c(3,5))
    }
  }
}
dev.off()

#try with ggplot instead to scale points:
for (s in species) {
  df1<-subset(alldat4, species==s)
  year<-sort(unique(df1$year))
  for (y in year){
    df2<-subset(df1, year==y & species==s)
    print(ggplot(df2, aes(x = jd, y = abundance)) +
            geom_point(aes(size=abundance))+
            labs(title = paste(s,y),
                 x="julian", y = "abundance")+
            geom_vline(xintercept = df2$earlydate, color="red")+
            theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                  panel.background = element_blank(), axis.line = element_line(colour = "black")))
  }
}
dev.off()

#cex to mess with the color or symbol size by number of observations


#this is me checking that each species has at least 10 "good" years (at least 10 distinct dates)
speciessummary<-aggregate(x = alldat4$abundance, by = list(alldat4$speciesyear), FUN = sum)
#subset species/year pairs with a minimum of 30 observations
speciessummary2 <- speciessummary[ which(speciessummary$x >= 10), ]

