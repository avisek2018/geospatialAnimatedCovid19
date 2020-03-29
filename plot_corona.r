#Special Thanks to - https://d4tagirl.com/2017/05/how-to-plot-animated-maps-with-gganimate
#Corona Data - https://www.ecdc.europa.eu/en/publications-data/download-todays-data-geographic-distribution-covid-19-cases-worldwide
#Country Code and Coordinate JSON - https://gist.github.com/sindresorhus/1341699

#Load the Libraries
library(tidyverse)
library(readxl)
library(jsonlite)
library(maps)
library(ggthemes)
library(lubridate)
library(gganimate)
library(gifski)
library(av)

#Read the Corona data from
#European Centre for Disease Prevention and Control (ECDC)
corona_df <- read_excel('C:/Corona/COVID-19-2020-03-28.xlsx')
#Verify the data
head(corona_df)

#Read the Country Code, latitude and longitude
cjson <- fromJSON('C:/Corona/countrycode-latlong-array.json')

#Make a dataframe from the JSON

#Custom Fucntion
convertList <- function(x){
  lat <<- append(lat, unlist(x[1]))
  long <<- append(long, unlist(x[2]))
}

#Get the country names
countryNames <- unlist(attributes(cjson))
names(countryNames) <- ""

#Extract latitude and longitude from the list
lat <<- c()
long <<- c()

lapply(cjson, convertList)

#Generate the Dataframe
djson <- data.frame(as.character(countryNames), as.numeric(lat), as.numeric(long))
colnames(djson) <- c('GeoId', 'lat', 'long')
djson$GeoId <- toupper(as.character(djson$GeoId))

#Join both the data sets
corona_df <- corona_df %>% 
  rename("GeoId" = "geoId", "DateRep" = "dateRep", "Cases" = "cases") %>%
  inner_join(djson)


#Generate the static map

cum_corona_df <- corona_df %>% 
  group_by(GeoId, long, lat) %>% 
  summarise(cases = sum(Cases))

world <- ggplot() +
  borders("world", colour = "gray85", fill = "gray80") +
  theme_map() 

map <- world +
  geom_point(aes(x = long, y = lat, size = cases*5),
             data = cum_corona_df, 
             colour = 'red', alpha = .5) +
  scale_size_continuous(range = c(1, 8), 
                        breaks = c(250, 500, 750, 1000, 
                                   5000, 10000, 20000, 30000,
                                   40000, 50000, 60000, 70000,
                                   80000, 90000, 100000)) +
  labs(size = 'cases')
map



#Generate animated map

#Generate the Cumulative Cases
cumsum_corona_df <- corona_df %>% 
  arrange(GeoId, DateRep) %>% 
  group_by(GeoId) %>%
  mutate("cumsum_cases" = cumsum(Cases)) %>% 
  arrange(GeoId, DateRep, desc(cumsum_cases))
#Convert DataTime to Date
cumsum_corona_df$DateRep <- as.Date(cumsum_corona_df$DateRep)
#Create a dataset for each days for each GeoId
corona_points_fin <- tibble(
  DateRep = rep(seq(as.Date('2019-12-31'), 
                    as.Date('2020-03-28'),
                by = 'days'), 
                length(unique(cumsum_corona_df$GeoId)))
  )
#Add the GeoId to the Data set
corona_points_fin$GeoId <- rep(unique(cumsum_corona_df$GeoId),
                              length(unique(corona_points_fin$DateRep)))
#Combine the both to get Lat and Long
corona_points_fin <- corona_points_fin %>% 
  inner_join(djson)
#Do a right join to get the missing GeoId for Missing Days
cumsum_corona_df <- cumsum_corona_df %>% 
  right_join(corona_points_fin)

#Validate data for couple of fields
cumsum_corona_df[cumsum_corona_df$DateRep == 
                  as.Date("2020-03-02"),] %>% 
  select(GeoId,countriesAndTerritories, DateRep, cumsum_cases, lat, long) %>% 
  arrange(desc(cumsum_cases))

cumsum_corona_df[cumsum_corona_df$DateRep == 
                   as.Date("2020-03-28"),] %>% 
  select(GeoId,countriesAndTerritories, DateRep, cumsum_cases, lat, long) %>% 
  arrange(desc(cumsum_cases))

#Generate the Static Map 1st
static_map <- world +
  geom_point(aes(x = long, y = lat, size = cumsum_cases, group = GeoId), 
               data = cumsum_corona_df, colour = 'purple', alpha = .5) + 
  scale_size_continuous(range = c(1, 16), 
                        breaks = c(250, 500, 750, 1000, 
                                   5000, 10000, 20000, 30000,
                                   40000, 50000, 60000, 70000,
                                   80000, 90000, 100000)) + 
  theme_minimal() +
  theme( plot.title = element_text(size = 30, hjust=0.5, face="bold", colour="grey", vjust= -1), 
         plot.subtitle = element_text(size = 20, hjust=0.5, face="italic", color="grey"), 
         plot.caption = element_text(size = 20, hjust = 0.5, face="italic", color="red"), 
         axis.ticks.y = element_blank(), 
         axis.text.y = element_blank(),
         plot.margin = margin(1,1,1,4, "cm")
  ) +
  labs(size = 'Cumulative Cases')

#Creating the final animation
dynamic_map<-static_map + 
  transition_states(states = DateRep, 
                    transition_length = 4, 
                    state_length = 1) + 
  ease_aes('linear') +
  labs(title = 'Date : {closest_state}', 
       caption = "Data Source: European Centre for Disease Prevention and Control\n Author:Avisek Choudhury")

#Rendering the animation for gif
final_animation_map<-animate(dynamic_map, 100,
                             fps = 20, duration = 60, 
                             width = 1200, 
                             height = 800, 
                             renderer = gifski_renderer())

#Rendering the animation for mp4
animate(dynamic_map, 100,
        fps = 20,duration = 60, 
        width = 1200, height = 800, 
        renderer = ffmpeg_renderer())

#Saving the animation
anim_save('CoronaDynamicMap_Avisek.gif',
          animation = final_animation_map, 
          path = 'C:/Corona/')

#create mp4 file
animate(dynamic_map, fps=30,
        duration = 80, 
        width = 1200, height = 800,
        renderer=av_renderer('C:/Corona/CoronaDynamicMap_Avisek.mp4'))

