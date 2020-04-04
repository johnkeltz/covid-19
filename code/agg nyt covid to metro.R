#code does the following:
  #pulls nyt county covid-19 data
  #aggregates to metro
  #merges in populations
  #stack metro and county data to allow toggle
  #formats for visual in tableau

#####pull county case and deaths data from nytimes github- https://github.com/nytimes/covid-19-data
##data refreshes mid-day with previous day's data
#they ask to link to this page: https://www.nytimes.com/interactive/2020/us/coronavirus-us-cases.html
#install.packages("RCurl")
library(RCurl)
nyt.data <- getURL("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv")
county.counts <- read.csv(text = nyt.data)

county.counts$county <- as.character(county.counts$county)
county.counts$state <- as.character(county.counts$state)
county.counts$date <- as.character.Date(county.counts$date)

#######add county-metro crosswalk
#original file downloaded from here: https://www.census.gov/geographies/reference-files/time-series/demo/metro-micro/delineation-files.html
cw.data <- getURL("https://raw.githubusercontent.com/johnkeltz/covid-19/master/data/census_county_msa_crosswalk_sept_2018.csv")
crosswalk <- read.csv(text = cw.data)

#convert fips to number
crosswalk$fips <- as.numeric(crosswalk$FIPS.State.Code)*1000 + as.numeric(crosswalk$FIPS.County.Code)
keeps <- c("CBSA.Title","fips")
crosswalk <- crosswalk[keeps]
colnames(crosswalk) <- c("region","fips")

crosswalk$region <- as.character(crosswalk$region)

#failed merges
#crosswalk, no nyt-> no confirmed cases
#nyt, no crosswalk -> small counties, not part of a micro or macro, "unknown county", kansas city and new york city
county.counts <- merge(county.counts,crosswalk,by=c("fips"),all.x = TRUE)

#fix new york city and kansas city
#they span multiple counties and the nyt groups the counties into one observation
county.counts$region <- ifelse(county.counts$county=="New York City","New York-Newark-Jersey City, NY-NJ-PA",county.counts$region)
county.counts$region <- ifelse(county.counts$county=="Kansas City","Kansas City, MO-KS",county.counts$region)

#change states to abbreviations to be consistent with region names next step
county.counts$state.ab <- state.abb[match(county.counts$state,state.name)]
# 
# #assign county name to region if region is missing
# #include marker for later
# county.counts$no.msa <- ifelse(is.na(county.counts$region),1,0)
# county.counts$region <- ifelse(is.na(county.counts$region),paste(county.counts$county,"County,",county.counts$state.ab,sep=" "),county.counts$region)

######add county populations
#data originally from here: https://www.census.gov/data/datasets/time-series/demo/popest/2010s-counties-total.html
pops.data <- getURL("https://raw.githubusercontent.com/johnkeltz/covid-19/master/data/county%20populations.csv")
pops <- read.csv(text = pops.data)

pops$fips <- pops$STATE*1000 + pops$COUNTY

pop.keeps <- c("fips","POPESTIMATE2019")
pops <- pops[pop.keeps]

county.counts <- merge(county.counts,pops,by=c("fips"),all.x = TRUE)

#drop unknown counties before aggregating to metro... might want to report/visualize these later
county.counts <- county.counts[which(county.counts$county!="Unknown"),]

#aggregate to metro and micro area
metro.cases <- aggregate(county.counts$cases, by=list(county.counts$region,county.counts$date),FUN=sum , na.rm=TRUE)
colnames(metro.cases) <- c("region","date","cases")

metro.deaths <- aggregate(county.counts$deaths, by=list(county.counts$region,county.counts$date),FUN=sum , na.rm=TRUE)
colnames(metro.deaths) <- c("region","date","deaths")

metro <- merge(metro.cases,metro.deaths,by=c("region","date"))

#######add metro populations...
#source: https://www.census.gov/data/tables/time-series/demo/popest/2010s-total-metro-and-micro-statistical-areas.html
met.pop.data <- getURL("https://raw.githubusercontent.com/johnkeltz/covid-19/master/data/metropolitan-population-2019.csv")
metro.pop <- read.csv(text = met.pop.data)

mic.pop.data <- getURL("https://raw.githubusercontent.com/johnkeltz/covid-19/master/data/micropolitan-population-2019.csv")
micro.pop <- read.csv(text = mic.pop.data)


area.pops <- rbind(metro.pop,micro.pop)
colnames(area.pops) <- c("region","population")

#merging on name, but both files come from census.gov and use same names
metro <- merge(metro,area.pops,by=c("region"),all.x=TRUE)

metro$type <- "Metropolitan / Micropolitan Area"

#stack with county

county <- county.counts
county$region <- paste(county$county,county$state.ab,sep=", ")

county.keeps <- c("region", "date", "cases", "deaths", "POPESTIMATE2019")
county <- county[county.keeps]
colnames(county) <- c("region", "date", "cases", "deaths", "population")

county$type <- "County"

metro.county <- rbind(metro,county)

#add days since x cases, y deaths? (not yet completed, right now calculating in tableau)
#91-dicov uses 20 cases, 10 deaths, 1 case/million, 1 death per million...
#might have to use smaller cutoffs because dealing with smaller areas

#add current totals to historical records for easier filtering in tableau
metro.county <- transform(metro.county, deaths.total=ave(deaths,region,FUN=max))

metro.county <- transform(metro.county, cases.total=ave(cases,region,FUN=max))

#sort
metro.county <- metro.county[order(metro.county$region,metro.county$date),]

#make main state column
#metro areas that span multiple states have the main state first
#grab second and third digits after comma...
metro.county$state.ab <- substr(metro.county$region,regexpr(",",metro.county$region)+2,regexpr(",",metro.county$region)+3)

#convert state abreviation to full name
metro.county$state <- state.name[match(metro.county$state.ab,state.abb)]

#region short name for simple graph labels
#everything before first hyphen or comma
metro.county$region.ab <- substr(metro.county$region,1,regexpr(",|-",metro.county$region)-1)

metro.county$region.ab.st <- paste(metro.county$region.ab, metro.county$state.ab, sep=", ")

#add populations for new york city and kansas city 
#special case- only two city geographies in NYT data set
metro.county$population <- ifelse(metro.county$region == "New York City, NY", 8700000, metro.county$population)
metro.county$population <- ifelse(metro.county$region == "Kansas City, MO", 491918, metro.county$population)

#output for tableau
write.csv(metro.county,"C:/Users/john.keltz/Desktop/Projects/Coronavirus/Data/metro county stacked.csv",row.names = FALSE)


