library(dplyr)
library(tidyr)
library(readxl)

#load triangle temperature data
tempdat<-read.csv("data/tempmean.4.months.triangle.static.2020.csv")

#mean temp across counties by years
tempdat<-tempdat %>%
  group_by(year) %>%
  summarise(tempmean = mean(temp))

########################################################################################

#load number of records of approximation data
#load approximation data
recorddat<-read.csv("data/bnc_thru2020.csv")

#add labels to designate triangle observations
labels<-read.csv("data/triangle_labels.csv")

labels<-labels %>%
  select(county_lc, triangle) %>% #subset labels with lowercase county format
  rename(county = county_lc) #rename columns

#merge labels with approximation
recorddat<-merge(recorddat,labels, by.x=c("county"),by.y=c("county"), all.x = F, all.y = T)

recorddat <- recorddat %>% 
  filter(year > 1989, triangle == "Y") %>% #subset years after 1989 and triangle data
  mutate(number = replace(number, number == 0, 1)) %>% #where number=0, change to "1"
  group_by(Cname, observer, jd, year) %>%
  summarise(tempmean = sum(number)) #aggregate by Cname, observer, jd, year

#create a column where any number >/= 1 is listed as "1"
#since we've already changed zeros to 1, this is every number
recorddat$abundance <- 1

#sum records by year
recorddat<-recorddat %>%
  group_by(year) %>%
  summarise(sumrecords = sum(abundance))

#merge record dat with temp dat
dat2<-merge(recorddat, tempdat, by.x=c("year"), by.y=c("year"), all.x = F, all.y = F)

#save this dat
write.csv(dat2, file="data/record.temp.summary.csv")


#########################################################################################3
#line chart with 2 y-axes

library(ggplot2)

#max record w/in year = 3125
#max temp = 18.63774
3125/18.63774

p1<-ggplot(dat2, aes(year, tempmean)) + 
        geom_line(colour = "chartreuse4", size = 1.5)+
        theme_classic()

p2<-ggplot(dat2, aes(year, sumrecords)) + 
        geom_line(colour = "darkmagenta", size = 1.5)+
        theme_classic()

coeff = 168
ggplot(dat2, aes(x=year)) +
  geom_line( aes(y=tempmean), color="red") + 
  geom_line( aes(y=sumrecords / coeff), color="darkmagenta") +
  scale_y_continuous(
    # Features of the first axis
    name = "Temperature (°C)", limits = c(0,20),
    # Add a second axis and specify its features
    sec.axis = sec_axis(~.*coeff, name="Number of Records"),
  )+
  xlab("Year")+
  theme_classic()
