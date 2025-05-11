library(httr)
library(keyring)
library(jsonlite)
library(MMWRweek)
library(tidyverse)
library(lubridate)
library(usmap)
plot_usmap(regions="state")

# Region Code 
library(maps)
library(grid)

#load us state map data
us_state_map = map_data("state");

#map each state to a division
us_state_map$division[us_state_map$region %in% c("connecticut", "maine", "massachusetts", "new hampshire", "rhode island", "vermont")] <- 1
us_state_map$division[us_state_map$region %in% c("new jersey","new york","puerto rico","virgin islands")] <- 2
us_state_map$division[us_state_map$region %in% c("delaware","district of columbia","maryland","pennsylvania","virginia","west virginia")] <- 3
us_state_map$division[us_state_map$region %in% c("alabama","florida","georgia","kentucky","mississippi","north carolina","south carolina","tennessee")] <- 4
us_state_map$division[us_state_map$region %in% c("illinois","indiana","michigan","minnesota","ohio","wisconsin")] <- 5
us_state_map$division[us_state_map$region %in% c("arkansas","louisiana","new mexico","oklahoma","texas")] <- 6
us_state_map$division[us_state_map$region %in% c("iowa","kansas","missouri","nebraska")] <- 7
us_state_map$division[us_state_map$region %in% c("colorado","montana","north dakota","south dakota","utah","wyoming")] <- 8
us_state_map$division[us_state_map$region %in% c("arizona","california","hawaii","nevada")] <- 9
us_state_map$division[us_state_map$region %in% c("alaska","idaho","oregon","washington")] <- 10

#create a dummy variable that counts the number of states in each division
divisions.subtotal <- ddply(us_state_map, .(division), summarize, NumberOfStates=length(unique(region)))


key_set(service= "essence", username="hsmith01")

#kr <- keyring::backend_env$new()
#kr$set(service = "essence", username = "hsmith01")

# MJ Data 2019-2021
#data pulled: 02/07/22



#March 13, 2019- Jan 22, 2022
url_2 <-"https://essence.syndromicsurveillance.org/nssp_essence/api/tableBuilder?dqCOVHasBeenEThreeYear=40&endDate=31Jan2022&ccddCategory=marijuana%20v3&percentParam=ccddCategory&geographySystem=hospitaldhhsregion&ddInformativeAvgWeeklyPercentThreeYearOperator=gte&datasource=va_hospdreg&detector=nodetectordetector&startDate=30Dec2018&timeResolution=daily&hasBeenE=1&medicalGroupingSystem=essencesyndromes&userId=3049&ddInformativeAvgWeeklyPercentThreeYear=70&dqCOVHasBeenEThreeYearOperator=lte&aqtTarget=TableBuilder&rowFields=timeResolution&rowFields=ageNCHS&rowFields=geographyhospitaldhhsregion&columnField=ccddCategory"

api_response <- GET(url_2,
                    authenticate(key_list("essence")[1,2],
                                 key_get("essence",
                                         key_list("essence")[1,2])))

api_response_json <- content(api_response, as= "text") #parse error if not key_set/logged in
cannabis_region <- fromJSON(api_response_json)

#write.csv(cannabis_region, file = "C:/Users/pqw6/OneDrive - CDC/Cannabis SyS Paper/cannabis_region.csv")

cannabis_region <- read.csv("C:/Users/pqw6/OneDrive - CDC/Cannabis SyS Paper/cannabis_region.csv")

##############
##############
#Data Cleaning


can_sys_region <- cannabis_region %>%
  select(!contains("raw")) %>%
  dplyr::rename(
    data_count = numerator, 
    all_count = denominator, 
    percent = count
  ) %>%
  #  separate(timeResolution, c("year", "week"), sep = "-") %>%
  mutate(
    date = ymd(timeResolution)
  )%>% 
  #  mutate(date = MMWRweek2Date(year, week)) %>%
  select(
    date,
    ageNCHS,
    data_count, 
    all_count, 
    percent,
    geographyhospitaldhhsregion
  )%>%
  mutate(row = row_number())%>%
  filter(ageNCHS=="00-10"|ageNCHS=="11-14"|ageNCHS=="15-24")%>%
  select(-row)%>%
  mutate(region= recode (geographyhospitaldhhsregion,
                         "Region 1" = 1,
                         "Region 2" = 2,
                         "Region 3" = 3,
                         "Region 4" = 4,
                         "Region 5" = 5,
                         "Region 6" = 6,
                         "Region 7" = 7,
                         "Region 8" = 8,
                         "Region 9" = 9,
                         "Region 10" = 10,
                         "OTHER_REGION" = 0))

head(can_sys_region)


region_2019 <- can_sys_region[can_sys_region$date > "2019-03-12" & can_sys_region$date < "2020-03-13", ]

region_2020 <- can_sys_region[can_sys_region$date > "2020-03-12" & can_sys_region$date < "2021-03-13", ]

region_2021 <- can_sys_region[can_sys_region$date > "2021-03-12" , ] #thru available data on data pull date.

#Aggregate Sums
region_2019_sum <- aggregate(region_2019$data_count, by=list(ageNCHS=region_2019$ageNCHS,region=region_2019$region), FUN=sum, na.rm=TRUE)
region_2020_sum <- aggregate(region_2020$data_count, by=list(ageNCHS=region_2020$ageNCHS,region=region_2020$region), FUN=sum, na.rm=TRUE)
region_2021_sum <- aggregate(region_2021$data_count, by=list(ageNCHS=region_2021$ageNCHS,region=region_2021$region), FUN=sum, na.rm=TRUE)


#aggregate(region_2020$data_count, by=list(ageNCHS=region_2020$ageNCHS), FUN=sum, na.rm=TRUE)
#aggregate(region_2021$data_count, by=list(ageNCHS=region_2021$ageNCHS), FUN=sum, na.rm=TRUE)

us_state_map.mod_2019 <- merge(x=us_state_map, y=region_2019_sum, all=TRUE, by.x="division", by.y="region")
us_state_map.mod_2019 = arrange(us_state_map.mod_2019, order);
us_state_map.mod_2019$division = as.factor(us_state_map.mod_2019$division)

us_state_map.mod_2020 <- merge(x=us_state_map, y=region_2020_sum, all=TRUE, by.x="division", by.y="region")
us_state_map.mod_2020 = arrange(us_state_map.mod_2020, order);
us_state_map.mod_2020$division = as.factor(us_state_map.mod_2020$division)

us_state_map.mod_2021 <- merge(x=us_state_map, y=region_2021_sum, all=TRUE, by.x="division", by.y="region")
us_state_map.mod_2021 = arrange(us_state_map.mod_2021, order);
us_state_map.mod_2021$division = as.factor(us_state_map.mod_2021$division)


#THEME
theme_1 <- theme(panel.background = element_rect(fill = "transparent", colour = NA),
                 plot.background = element_rect(fill = "transparent", colour = NA),
                 axis.ticks.x = element_line(size = 0.25),
                 panel.grid.major.x = element_blank(),
                 panel.grid.major.y = element_line(size = 0.5, linetype = "dashed"),
                 panel.grid.minor = element_blank(),
                 legend.position = "bottom")


#plot a map of each division
map <- ggplot()
map_2019 = map + geom_polygon(data=us_state_map.mod_2019, aes(x=long, y=lat, group=group, fill=x)) + theme_1 + facet_wrap(~ageNCHS, scales= "free")
map_2019

map_2020 = map + geom_polygon(data=us_state_map.mod_2020, aes(x=long, y=lat, group=group, fill=x)) + theme_1 + facet_wrap(~ageNCHS, scales= "free")
map_2020

map_2021 = map + geom_polygon(data=us_state_map.mod_2021, aes(x=long, y=lat, group=group, fill=x)) + theme_1 + facet_wrap(~ageNCHS, scales= "free")
map_2021


