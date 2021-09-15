#This script extracts temperature per year for the triangle 
#using a static window

#1) load spatial data, check map projections

##x-values- temperature
files=list.files("C:/Users/lhamo/Documents/git/nc-butterfly-phenology/data/temp data/1990/")
library(raster)

# Read in single raster layer from january
files=list.files("C:/Users/lhamo/Documents/git/nc-butterfly-phenology/data/temp data/1990/", full.names=TRUE)
numfiles=length(files)
jan90<-raster(files[8])

#read in state boundary file
library(rgdal)
#read in county boundary file
county=readOGR("C:/Users/lhamo/Documents/git/nc-butterfly-phenology/data" , "COUNTYBOUNDARYSHORELINE")
#subset the triangle
r <- c(10, 16, 17)
triangle <- county[r,]

#check map projections
#state 
NC_geog <- spTransform(NC, crs(jan90, asText=FALSE)) 
plot(jan90)
plot(NC_geog, add=T)
proj4string(NC_geog)
#triangle
triangle_geog <- spTransform(triangle, crs(jan90, asText=FALSE)) 
plot(jan90)
plot(triangle_geog, add=T)
proj4string(triangle_geog)

#crop data and plot
#state
jan90NC = crop(jan90, NC_geog)
plot(jan90NC)
plot(NC_geog,add=T)
#triangle
jan90triangle = crop(jan90, triangle_geog)
plot(jan90triangle)
plot(triangle_geog, add=T)

######################################################################################
#Jump here if you want to use a static window for calculating temperature
#(specifically, March-June)

#for adjusted months use 
years = 1990:2016

#for loop to read in all the files
output = data.frame(county = character(),
                    year = integer(),
                    temp = numeric())

#for loop extracts average temp from March-June
for(y in years){
  filenames.march<- paste("C:/Users/lhamo/Documents/Biology/butterfly paper 2016/temp data/", y, "/PRISM_tmean_stable_4kmM2_", y, "03", "_bil.bil", sep="")
  filenames.april<- paste("C:/Users/lhamo/Documents/Biology/butterfly paper 2016/temp data/", y, "/PRISM_tmean_stable_4kmM2_", y, "04", "_bil.bil", sep="")
  filenames.may<- paste("C:/Users/lhamo/Documents/Biology/butterfly paper 2016/temp data/", y, "/PRISM_tmean_stable_4kmM2_", y, "05", "_bil.bil", sep="")
  filenames.june<- paste("C:/Users/lhamo/Documents/Biology/butterfly paper 2016/temp data/", y, "/PRISM_tmean_stable_4kmM2_", y, "06", "_bil.bil", sep="")
  filenames1 = c(filenames.march, filenames.april, filenames.may, filenames.june)
  filenames <- filenames1[ !grepl("C:/Users/lhamo/Documents/Biology/butterfly paper 2016/temp data/", y, "/PRISM_tmean_stable_4kmM2_", y, "_bil.bil", filenames1) ]
  temp_allmonths<-stack(filenames)
  tmean = calc(temp_allmonths, mean)
  tempmean = extract(tmean, triangle_geog, fun=mean)
  tempoutput = data.frame(county = triangle_geog@data$NAME, year = y, temp = tempmean)
  output = rbind(output, tempoutput)
} # end of year loop

# The output dataframe has 3 columns: county, year, and mean temperature for that window

#save output
write.csv(output, file="C:/Users/lhamo/Documents/git/nc-butterfly-phenology/data/tempmean.4.months.triangle.static.2016.csv")

