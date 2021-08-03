library(dplyr)

bnc <- read.csv("data/bnc_thru2018.csv")


yearcounty = bnc %>% count(county, year)

countyTotals = bnc %>%
  filter( year >= 1990) %>%
  count(county) %>% arrange(desc(n)) %>%
  filter(n > 5000)

yc90 = yearcounty %>% 
  filter(year >= 1990,
         county %in% countyTotals$county)


  
colors = rainbow(9)

plot(c(1990, 2020), c(0, 2000), xlab = "Year", ylab = "Number of records", type = 'n')
for (i in 1:length(unique(yc90$county))) {
  
  tmp = yearcounty %>%
    filter(year >= 1990,
           county == unique(yc90$county)[i])
  points(tmp$year, tmp$n, type = 'l', col = colors[i])
  rm(tmp)
}
legend("topleft", legend = unique(yc90$county), lty = 'solid', col = colors)


