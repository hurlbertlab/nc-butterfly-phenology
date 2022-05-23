#Finding the 10% flight date for each species for each year
#and merging with temperature data generated from extract.temp.NC.R

library(dplyr)
library(tidyr)
library(readxl)

#load approximation data
alldat<-read.csv("data/bnc_thru2020.csv")

#add labels to designate triangle observations
labels<-read.csv("data/triangle_labels.csv")

labels<-labels %>%
  select(county_lc, triangle) %>% #subset labels with lowercase county format
  rename(county = county_lc) #rename columns

#merge labels with approximation
dat<-merge(alldat,labels, by.x=c("county"),by.y=c("county"), all.x = F, all.y = T)

dat <- dat %>% 
  filter(year > 1989, triangle == "Y") %>% #subset years after 1989 and triangle data
  mutate(number = replace(number, number == 0, 1)) %>% #where number=0, change to "1"
  group_by(Cname, observer, jd, year) %>%
  summarise(number = sum(number)) #aggregate by Cname, observer, jd, year

#create a column where any number >/= 1 is listed as "1"
#since we've already changed zeros to 1, this is every number
dat$abundance <- 1

#load species trait data (includes scientific names)
traits<-read.csv("data/species traits list.csv")

#merge traits list with approximation
dat2<-merge(traits, dat, by.x=c("Cname"), by.y=c("Cname"), all.x = F, all.y = F)

#subset column names to match the 2nd summary dataframe
dat3<-dat2 %>%
  select("species", "abundance", "number", "year", "jd") 

#Lump Ceratina and Erynnis under the genus level
#replace both "Celastrina ladon" and "Celastrina neglecta" with "Celastrina spp"
dat3 <- dat3 %>% replace(.=="Celastrina ladon", "Celastrina spp")
dat3 <- dat3 %>% replace(.=="Celastrina neglecta", "Celastrina spp")
#replace both "juvenalis" and "horatius" with "spp" 
dat3 <- dat3 %>% replace(.=="Erynnis juvenalis", "Erynnis spp")
dat3 <- dat3 %>% replace(.=="Erynnis horatius", "Erynnis spp")

#Monarch is migratory, while ocola and red admiral are partly migratory
#remove those here, if desired
dat3<-dat3[dat3$species != "Danaus plexippus", ] 
dat3<-dat3[dat3$species != "Panoquina ocola", ]   
dat3<-dat3[dat3$species != "Vanessa atalanta", ] 
dat3<-dat3[dat3$species != "Junonia coenia", ]

#Chlosyne nycteis and Nastra lherminier have fewer than 10 "good" years
#remove them here
dat3<-dat3[dat3$species != "Chlosyne nycteis", ] 
dat3<-dat3[dat3$species != "Nastra lherminier", ]

#filter out years that have less than 10 unique dates
goodspeciesyears <- dat3 %>% 
  count(species, year) %>%
  right_join(dat3, by = c("species", "year")) %>%
  filter(n >= 10)

#visualising distrubtion of Legrand data
library(ggplot2)
datehist<-ggplot(dat3, aes(x=year)) + 
  geom_histogram(color="black", fill="white")
datehist

#-------------------------------------------------------

# First need to create a vector of dates for individuals
species<-unique(goodspeciesyears$species)
year=1990:2020

#create an empty output for the for loop to put values into
output = data.frame(species=character(),
                    year=integer(),
                    jd=numeric())

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
      datoutput = data.frame(species = s, year = y, jd=mindate, sdjd = sddat)
      output=rbind(output,datoutput)
  }
}

output2 <- output[!is.infinite(output$jd),] #note:many Infs where there were not enough data

########################################################
#load and merge in temperature data

#load temperature data
tempdat<-read.csv("data/tempmean.4.months.triangle.static.2020.csv")

#average temperatures across all three triangle counties for each year (if desired)
tempdat2<-aggregate(temp~year, tempdat, FUN=mean)

#merge temperature data with earlydate
tempjulian = merge(tempdat2, output2, by.x = c('year'), by.y =c('year'), all.x = T, all.y = T)
tempjulian<-na.omit(tempjulian)

#create a csv that includes temperature and julian dates, if desired
write.csv(tempjulian, "data/temp.earlydate.uniquedate.triangle.static.4.months.csv")


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

#need to omit species that have fewer than 10 years of data
  uniquespeciesyears<-unique(alldat4[c("species", "year")])  
  tt <- table(uniquespeciesyears$species)
alldat5 <- alldat4[alldat4$species %in% names(tt[tt > 9]), ]
  
uniquespeciesyears2<-unique(alldat5[c("species", "year")])  
tt2 <- table(uniquespeciesyears2$species)

# Need to create a vector of species and dates for individuals
species<-unique(alldat4$species)
species<-unique(alldat5$species)

#generate empty pdf to fill 
pdf("numbersightings.peryear.perspecies.uniquedate.pdf",width=10, height=8)
par(mfrow=c(3,5), mar=c(2,1,2,1), oma=c(4,4,3,0))


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
    
    if(length(year) > 15)
      {
      labels = ifelse(y == year[16], TRUE, FALSE) 
    }
    if(y == year[1] | labels) 
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


#this is me checking that each species has at least 10 "good" years (at least 10 distinct dates)
speciessummary<-aggregate(x = alldat4$abundance, by = list(alldat4$speciesyear), FUN = sum)
#subset species/year pairs with a minimum of 10 observations
speciessummary2 <- speciessummary[ which(speciessummary$x >= 10), ]


