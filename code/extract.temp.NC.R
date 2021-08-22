#####LOADING SPATIAL DATA, CHECKING MAP PROJECTIONS

##x-values- temperature
setwd("~/Biology/butterfly paper 2016/temp data/1990")
files=list.files()
library(raster)

# Read in single raster layer from january
files=list.files()
numfiles=length(files)
jan90<-raster(files[8])

#read in county boundary file
library(rgdal)
state=readOGR("C:/Users/lhamo/Documents/Biology/butterfly paper 2016/boundarydata","cb_2014_us_state_500k")
NC<-state[state$NAME=="North Carolina",]

#check map projections
NC_geog = spTransform(NC, crs(jan90)) 
plot(jan90)
plot(NC_geog, add=T)
proj4string(NC_geog)

#crop data and plot
jan90NC = crop(jan90, NC_geog)
plot(jan90NC)
plot(NC_geog,add=T)

#####READING IN ALL APPROPRIATE TEMP FILES AND FINDING NCTEMPMEAN

mean.earlydate<-read.csv("C:/Users/lhamo/Documents/Biology/butterfly paper 2016/data/earlymonth.2016.csv") 
#for adjusted months use 
mean.earlydate<-read.csv("C:/Users/lhamo/Documents/Biology/butterfly paper 2016/data/arrivalmonth.2016.csv") 
species = unique(mean.earlydate$species)
years = 1990:2016


#for loop to read in all the files
output = data.frame(county = character(),
                    species = character(),
                    year = integer(),
                    temp = numeric())

# Specify months of climate data to get
numMonths = 4 # You can change this here if you decide to use a longer or shorter window

for (s in species) { # add a species loop to pull out species-specific arrival month
  arrivMonth = mean.earlydate$arrivalMonth[mean.earlydate$species == s]
  monthsToGet1 = max(1, (arrivMonth - (numMonths - 1))):arrivMonth
  monthsToGet2<- if (arrivMonth < numMonths) {
    (arrivMonth + (12 - numMonths + 1)):12
  }
  monthsText1 = paste("0", monthsToGet1, sep = "")
  monthsText2 = sapply(monthsToGet2, function(x) {
    if (nchar(monthsToGet2)[monthsToGet2 == x] == 1) { paste("0", x, sep = "") 
    } else if (nchar(monthsToGet2)[monthsToGet2 == x] == 2) { paste(x) }})
  
  # Now you should have a character string of months that you want to extract
  # called monthsText1 which are the early months in the year c("01", "02", "03", ...)
  # and monthsText2 which are the later months, e.g., c("09", "10", "11", "12").
  
  # Get temperature data across years for the 8-month period up to and including
  # the arrival month for species s
  for(y in years){
    filenames.y<- paste("C:/Users/lhamo/Documents/Biology/butterfly paper 2016/temp data/", y, "/PRISM_tmean_stable_4kmM2_", y, monthsText1, "_bil.bil", sep="")
    filenames.prevy <-filenames<- paste("C:/Users/lhamo/Documents/Biology/butterfly paper 2016/temp data/", y-1, "/PRISM_tmean_stable_4kmM2_", y-1, monthsText2, "_bil.bil", sep="")
    filenames1 = c(filenames.y, filenames.prevy)
    filenames <- filenames1[ !grepl("C:/Users/lhamo/Documents/Biology/butterfly paper 2016/temp data/", y, "/PRISM_tmean_stable_4kmM2_", y, "_bil.bil", filenames1) ]
    temp_allmonths<-stack(filenames)
    tmean = calc(temp_allmonths, mean)
    tempmean = extract(tmean, NC_geog, fun=mean)
    tempoutput = data.frame(county = NC_geog@data$NAME, species = s, year = y, temp = tempmean)
    output = rbind(output, tempoutput)
  } # end of year loop
} # end of species loop

# The output dataframe has 4 columns: the county name, species name, year, and mean temperature
# for that 8-month window for that year.

#for output from adjusted arrival months
write.csv(output, file="C:/Users/lhamo/Documents/Biology/butterfly paper 2016/data/tempmean.8.months.adjusted.2016.csv")
#save output from for loop
write.csv(output, file="C:/Users/lhamo/Documents/Biology/butterfly paper 2016/data/tempmean.8.months.2016.csv")

#merge with province labels
labels <-read.csv("C:/Users/lhamon/Dropbox/NC butterfly project/NCbutterflies.65species.June2015.csv")
labels<-labels[c("county","province")]
#note: the above labels are lowercase, and include counties that are further subdivided by region
#these counties are: burke, harnett, mcdowell, moore, polk, richmond, rutherford, wilkes
#for now, assigned a singular region based on entity region map. 
#the following province labels are the ones that have been cleaned up.
labels<-read.csv("C:/Users/lhamon/Documents/Documents/Biology/BIOL 692H/fixing.province.names.2.24.2016.csv")
dat<-merge(output,labels, by.x=c("county"),by.y=c("county"), all.x = T, all.y = T)

#aggregate data by region
#mean the county temp values FOR each species, FOR each year, FOR each province
aggdat <-aggregate(dat, by=list(dat$species,dat$year,dat$province), FUN=mean, na.rm=TRUE)
mountain.group<-subset(aggdat,aggdat$Group.3=="M")
piedmont.group<-subset(aggdat,aggdat$Group.3=="P")
coast.group<-subset(aggdat,aggdat$Group.3=="C")

#clean the above files
mountains<-mountain.group[c("Group.1","Group.3","year","temp")]
colnames(mountains)<-c("species","province","year","temp")

piedmont<-piedmont.group[c("Group.1","Group.3","year","temp")]
colnames(piedmont)<-c("species","province","year","temp")

coast<-coast.group[c("Group.1","Group.3","year","temp")]
colnames(coast)<-c("species","province","year","temp")


#####ADD JULIAN DATE AND MERGE

#load earlydat data. Note that the province labels here are according to their original subdivisions, so that should be changed
earlydate<-read.csv("C:/Users/lhamon/Documents/Documents/Biology/BIOL 692H/data/10.percent.by.province.2.17.2016.csv")
earlydate<-read.csv("C:/Users/lhamo/Documents/Biology/butterfly paper 2016")

#remove whole-state temperature values
earlydate<-earlydate[c("species","year","province","earlydate")]

#subset by province
M.earlydate<-subset(earlydate,province=="M")
P.earlydate<-subset(earlydate,province=="P")
C.earlydate<-subset(earlydate,province=="C")

#merge earlydate and tempdat for each region
M.tempjulian = merge(mountains, M.earlydate, by.x = c('species','year'), by.y =c('species','year'), all.x = T, all.y = T)
M.tempjulian<-na.omit(M.tempjulian)
M.tempjulian<-M.tempjulian[c("species","year","province.x","temp","earlydate")]
colnames(M.tempjulian)<-c("species","year","province","temp","earlydate")

P.tempjulian = merge(piedmont, P.earlydate, by.x = c('species','year'), by.y =c('species','year'), all.x = T, all.y = T)
P.tempjulian<-na.omit(P.tempjulian)
P.tempjulian<-P.tempjulian[c("species","year","province.x","temp","earlydate")]
colnames(P.tempjulian)<-c("species","year","province","temp","earlydate")

C.tempjulian = merge(coast, C.earlydate, by.x = c('species','year'), by.y =c('species','year'), all.x = T, all.y = T)
C.tempjulian<-na.omit(C.tempjulian)
C.tempjulian<-C.tempjulian[c("species","year","province.x","temp","earlydate")]
colnames(C.tempjulian)<-c("species","year","province","temp","earlydate")

#save as csv for each region
write.csv(M.tempjulian,file="C:/Users/lhamon/Documents/Documents/Biology/BIOL 692H/data/mountain.fulldat.2.24.2016.csv")
write.csv(P.tempjulian,file="C:/Users/lhamon/Documents/Documents/Biology/BIOL 692H/data/piedmont.fulldat.2.24.2016.csv")
write.csv(C.tempjulian,file="C:/Users/lhamon/Documents/Documents/Biology/BIOL 692H/data/coast.fulldat.2.24.2016.csv")

