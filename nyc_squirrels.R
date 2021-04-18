#########################################################
# idea from:
  # https://www.springboard.com/blog/15-fun-datasets-to-analyze/

# Data Analysis
# New York City Squirrel Census
# 
# Yep, you read that right. A native New Yorker data enthusiast and
# over 300 volunteers counted and observed the squirrels living in 
# the city—all to gather an immense amount of data that can be found here. 
# 
# A skill within data analysis involves asking the right questions, 
# and this dataset can be a great tool to study and come up with questions 
# that can be answered with this squirrel census. 
# Some might include their most frequented bodega trash cans, 
# most popular coat patterns, or where they summer.

#########################################################

library(readr)
library(dplyr)
library(lubridate) # to use function month
library(ggplot2)
library(ggthemes)

##### 1) load the dataset
nyc_squirrels <- read.csv('nyc_squirrels.csv')

##### 2) check str, stats, NAs, data types...
str(nyc_squirrels)

# 'data.frame':	3023 obs. of  36 variables:
# $ long                                      : num  -74 -74 -74 -74 -74 ...
# $ lat                                       : num  40.8 40.8 40.8 40.8 40.8 ...
# $ unique_squirrel_id                        : Factor w/ 3018 levels "10A-AM-1006-01",..: 1913 1903 1298 2566 2046 1644 2678 1747 2707 1531 ...
# $ hectare                                   : Factor w/ 339 levels "01A","01B","01C",..: 292 291 14 40 306 258 52 271 56 246 ...
# $ shift                                     : Factor w/ 2 levels "AM","PM": 2 2 1 2 1 1 2 2 1 2 ...
# $ date                                      : int  CHANGE DATA TYPE
# $ hectare_squirrel_number                   : int  3 3 3 5 1 2 2 3 9 14 ...
# $ age                                       : Factor w/ 3 levels "?","Adult","Juvenile": CHECK FOR NAs, UNIFY "?" AND NAs
# $ primary_fur_color                         : Factor w/ 3 levels "Black","Cinnamon",..: NA 3 2 3 NA 3 3 3 3 3 ...
# $ highlight_fur_color                       : Factor w/ 10 levels "Black","Black, Cinnamon",..: NA 5 NA NA NA 5 NA 5 NA NA ...
# $ combination_of_primary_and_highlight_color: Factor w/ 22 levels "+","Black+","Black+Cinnamon",..: 1 20 8 15 1 20 15 20 15 15 ...
# $ color_notes                               : Factor w/ 135 levels "!!!","\"Brown\" written in as Primary",..: NA NA NA NA NA NA NA NA NA 89 ...
# $ location                                  : Factor w/ 2 levels "Above Ground",..: NA 2 1 1 1 2 2 2 2 NA ...
# $ above_ground_sighter_measurement          : Factor w/ 41 levels "0","1","10","100",..: NA 41 26 21 NA 41 41 41 41 NA ...
# $ specific_location                         : Factor w/ 304 levels "\"FIELD\"","(white oak)",..: NA NA NA NA NA NA NA NA NA NA ...
# $ running                                   : logi  FALSE TRUE FALSE FALSE FALSE FALSE ...
# $ chasing                                   : logi  FALSE FALSE FALSE FALSE FALSE FALSE ...
# $ climbing                                  : logi  FALSE FALSE TRUE TRUE FALSE FALSE ...
# $ eating                                    : logi  FALSE FALSE FALSE FALSE FALSE FALSE ...
# $ foraging                                  : logi  FALSE FALSE FALSE FALSE FALSE FALSE ...
# $ other_activities                          : Factor w/ 307 levels "#7 & #8 chased each other up diff tree",..: NA NA NA NA 286 307 NA NA NA NA ...
# $ kuks                                      : logi  FALSE FALSE FALSE FALSE TRUE FALSE ...
# $ quaas                                     : logi  FALSE FALSE FALSE FALSE FALSE FALSE ...
# $ moans                                     : logi  FALSE FALSE FALSE FALSE FALSE FALSE ...
# $ tail_flags                                : logi  FALSE FALSE FALSE FALSE FALSE FALSE ...
# $ tail_twitches                             : logi  FALSE FALSE FALSE FALSE FALSE FALSE ...
# $ approaches                                : logi  FALSE FALSE FALSE FALSE FALSE FALSE ...
# $ indifferent                               : logi  FALSE FALSE TRUE FALSE FALSE FALSE ...
# $ runs_from                                 : logi  FALSE TRUE FALSE TRUE FALSE FALSE ...
# $ other_interactions                        : Factor w/ 197 levels "(me)","acknowledged",..: NA 111 NA NA NA NA NA NA NA NA ...
# $ lat_long                                  : Factor w/ 3023 levels "POINT (-73.9497217674555 40.796517007214)",..: 293 401 2697 2554 656 350 2114 765 2728 701 ...
# $ zip_codes                                 : int  NA NA NA NA NA NA NA NA NA NA ...
# $ community_districts                       : int  19 19 19 19 19 19 19 19 19 19 ...
# $ borough_boundaries                        : int  4 4 4 4 4 4 4 4 4 4 ...
# $ city_council_districts                    : int  19 19 19 19 19 19 19 19 19 19 ...
# $ police_precincts                          : int  13 13 13 13 13 13 13 13 13 13 ...

########## This dataset contains data that could be separated in 4 subsets based on category:
########## location, features, behavior and time. I will base my analysis on these 4 categories of information. 
########## XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

########## from str(nyc_squirrels):
########## At first sight, I find that I need to change nyc_squirrels$date's data type from integer to date

### change nyc_squirrels$date data type to character
nyc_squirrels$date <- as.character(nyc_squirrels$date)

### define function add_slash: takes the date in format: "10142018" (class Character)
### and returns it in format: "10/14/2018" (class Character) 
add_slash <- function(x){
  day <- substr(x, 3, 4)
  month <- substr(x, 1, 2)
  year <- substr(x, 5, 8)
  
  return <- paste(month, day, year, sep="/")
}
### change each date value with apply add_slash
new_date <- apply(nyc_squirrels[,6,drop=F], 1, add_slash)  # >>> nyc_squirrels[,6,drop=F] >>> here 'drop=F' prevents R from changing the dimensions of the object
nyc_squirrels$date <- new_date

### finally, change data type from 'Character' to 'Date'
nyc_squirrels$date <- as.Date(nyc_squirrels$date, "%m/%d/%Y")
class(nyc_squirrels$date) # "Date"

########## For nyc_squirrels$age, I need to unify formats, "?" + NAs = NAs
class(nyc_squirrels$age) # factor, Levels: ? Adult Juvenile
summary(nyc_squirrels$age)
#        ?    Adult Juvenile     NA's 
#        4     2568      330      121 

### First I tried this line, it will change '?' to NA, but it doesn't change the factor's level
# nyc_squirrels[nyc_squirrels$age == "?" ] <- NA

### So, I need to change the factor level from "?" to NA
levels(nyc_squirrels$age)[levels(nyc_squirrels$age) == '?'] <- NA
summary(nyc_squirrels$age)
#    Adult Juvenile     NA's 
#     2568      330      125 

### @@@@@@@@@@@@@@@@ R Markdown >>> I'm here!
### find which columns have NA values, and count them
na_totals <- colSums(is.na(nyc_squirrels))  # I could also use: sapply(nyc_squirrels, function(x) sum(is.na(x)))
index <- na_totals != 0
cols_w_na <- na_totals[index]
cols_w_na
# age                primary_fur_color              highlight_fur_color 
# 125                               55                             1086 
# color_notes                         location above_ground_sighter_measurement 
# 2841                               64                              114 
# specific_location                 other_activities               other_interactions 
# 2547                             2586                             2783 
# zip_codes 
# 3014 

########## highlight_fur_color has many NAs but it's informative, I'll keep it.

########## I see that there are a few columns that have very little information:
########## I choose to keep the ones related to location and behavior because they may still be useful:
# nyc_squirrels$other_activities
# nyc_squirrels$other_interactions
# nyc_squirrels$specific_location

########## I drop 'color_notes' (it's not of much use) and 'zip_codes' (it has only 9 non-NA values):
# nyc_squirrels$color_notes 
# nyc_squirrels$zip_codes
nyc_squirrels <- select(nyc_squirrels, -c(color_notes, zip_codes)) # column names without ""!

########## I order to analize the squirrels behavior vs time of year, I chose to create 4 bins 
########## corresponding to the 4 seasons.
########## Using climate data for New York City from:
########## https://en.wikipedia.org/wiki/New_York_City#Climate
########## I assigned the different months to seasons using average high and average low temps as follows:
##########  1  2  3  4  5  6  7  8  9  10  11  12
##########  w  w  w  g  g  s  s  s  f  f   f   w

season <- case_when(month(nyc_squirrels$date) == 1 ~ 'Winter',
                    month(nyc_squirrels$date) == 2 ~ 'Winter',
                    month(nyc_squirrels$date) == 3 ~ 'Winter',
                    month(nyc_squirrels$date) == 4 ~ 'Spring',
                    month(nyc_squirrels$date) == 5 ~ 'Spring',
                    month(nyc_squirrels$date) == 6 ~ 'Summer',
                    month(nyc_squirrels$date) == 7 ~ 'Summer',
                    month(nyc_squirrels$date) == 8 ~ 'Summer',
                    month(nyc_squirrels$date) == 9 ~ 'Fall',
                    month(nyc_squirrels$date) == 10 ~ 'Fall',
                    month(nyc_squirrels$date) == 11 ~ 'Fall',
                    month(nyc_squirrels$date) == 12 ~ 'Winter')

nyc_squirrels$season <- season   # Now I have one more column

########## 
########## With that I would complete the data wrangling step of my analysis.
########## 

########## Now I'll start answering some questions:

# features:
#   1.# what coat + highlight is the most frequent? what species is that?
#   2.# is there a correlation between age and fur color sighted? what color are the juvenile squirrels?
#   3.# for color and location >>> is it correlated? i.e., are cinnamon squirrels more
#     # frequently seen in the park and grey squirrels in the city or viceversa or
#     # is it unrelated? >>> that would mean different behaviors would be associated
#     # with the species
#   4.# fur color vs behavior >>> heatmap
#   5.# age vs behavior >>> heatmap


########## #   1.# what coat + highlight is the most frequent? what species is that?

### First, I create a subdf called features_df:
features_df <- subset(nyc_squirrels, select=c('age', 
                                              'primary_fur_color', 
                                              'highlight_fur_color', 
                                              'combination_of_primary_and_highlight_color'))

### Then, check for missing data:
missing_feat <- is.na(features_df)
summary(missing_feat)
# age          primary_fur_color highlight_fur_color combination_of_primary_and_highlight_color
# Mode :logical   Mode :logical     Mode :logical       Mode :logical                             
# FALSE:2898      FALSE:2968        FALSE:1937          FALSE:3023                                
# TRUE :125       TRUE :55          TRUE :1086

# There are 55 NA values in "primary_fur_color", but none in "combination_of_primary_and_highlight_color"
# why is that? what color is takes as default?

prim_color_na <- which(is.na(features_df$primary_fur_color))  # indexes of NA values for primary_fur_color

default_colors <- features_df$combination_of_primary_and_highlight_color[prim_color_na]
summary(default_colors)
# There is no "default color", for the combination_of_primary_and_highlight_color column, 
# when data was not available, instead of NA there is a '+':
prim_color_na_df <- features_df %>% filter((is.na(features_df$primary_fur_color)))

# ##### the next 2 lines don't work, WHY???
# nyc_squirrels[nyc_squirrels$combination_of_primary_and_highlight_color == "+" ] <- NA
# features_df[features_df$combination_of_primary_and_highlight_color == "+" ] <- NA

########## #   BACK TO: 1.# what coat + highlight is the most frequent? what species is that?

### 2. Descriptive Statistics
summary(features_df)
# age         primary_fur_color     highlight_fur_color     combination_of_primary_and_highlight_color
# Adult   :2568   Black   : 103     Cinnamon       : 767     Gray+               :895                  
# Juvenile: 330   Cinnamon: 392     White          : 585     Gray+Cinnamon       :752                  
# NA's    : 125   Gray    :2473     Cinnamon, White: 268     Gray+White          :489                  
#                 NA's    :  55     Gray           : 170     Gray+Cinnamon, White:265                  
#                                   Gray, White    :  59     Cinnamon+Gray       :162                  
#                                   (Other)        :  88     Cinnamon+White      : 94                  
#                                   NA's           :1086     (Other)             :366 

# Gray is the most frequent fur color. To see this more clearly, let's make a plot:

##### drop NA values from features_df$primary_fur_color:
features_df_fur <- features_df %>% filter(!is.na(primary_fur_color))
### plot primary_fur_color frequencies
p <- ggplot(data=features_df_fur, aes(x=primary_fur_color, fill=primary_fur_color)) + 
  geom_bar() + 
  scale_fill_manual(values=c("#000000", "#D2691E", "#808080")) + 
  labs(x="Primary Fur Color", y="Count") +
  ggtitle("Primary Fur Color") +
  theme(legend.title = element_blank()) +
  theme(legend.position="none") +  # is this line working? I don't think so...
  theme_hc()

p

# black=#000000
# cinnamon=#D2691E
# gray=#808080

# from:
# https://www.nycgovparks.org/programs/rangers/wildlife-management/squirrels
# The majority of squirrels in New York City are eastern grey squirrels.
# Even the darker grey or black or brown-rusty colored squirrels are all eastern grey squirrels! 
# Squirrels are mostly active during the daytime.

########## 2
#   2.# what color are the juvenile squirrels? is there a correlation between age and fur color sighted? 

# extract data for young squirrels
young <- features_df %>% filter(age == 'Juvenile')
# drop NAs
young <- young %>% filter(!is.na(primary_fur_color))
### plot primary_fur_color frequencies
p <- ggplot(data=young, aes(x=primary_fur_color, fill=primary_fur_color)) + 
  geom_bar() + 
  scale_fill_manual(values=c("#000000", "#D2691E", "#808080")) + 
  labs(x="Primary Fur Color", y="Count") +
  ggtitle("Juvenile Squirrels Primary Fur Color") +
  theme(legend.title = element_blank()) +
  theme(legend.position="none") +  # is this line working? I don't think so...
  theme_hc()

p
summary(features_df_fur$primary_fur_color)
# Black Cinnamon     Gray 
# 103      392     2473 

summary(young$primary_fur_color)
# Black Cinnamon     Gray 
# 8       58      256 

# 8/322
# 58/322
# 256/322
# 
# 103/2968
# 398/2968
# 2473/2968

# The color frequencies for young squirrels is very similar to the overall group
# plot adults? should I include this or should I remove #2 completely? it doesn't add anything...

########## let's have a look at the highlights:
### remove NAs
features_df_fur_h <- features_df %>% filter(!is.na(highlight_fur_color))

p <- ggplot(data=features_df_fur_h, aes(x=highlight_fur_color, fill=highlight_fur_color)) + 
  geom_bar() + 
  labs(x="Highlight Colors", y="Count") +
  ggtitle("Fur Highlight Colors") +
  theme(legend.title = element_blank()) +
  coord_flip() +
  theme_hc()

p
# OK! Now I have an interesting plot! But I need to rename a few things, otherwise all labels
# in it will look too confusing >>> NEEDS MORE WORK!


########## and what about the combinations? let's take a look:
p <- ggplot(data=features_df_fur, aes(x=combination_of_primary_and_highlight_color,
                                      fill=combination_of_primary_and_highlight_color)) + 
  geom_bar() + 
  labs(x="Color Combinations", y="Count") +
  ggtitle("Primary and Highlight Color Combined") +
  theme(legend.title = element_blank()) +
  theme(legend.position="none") +  # is this line working? I don't think so...
  coord_flip() +
  theme_hc()

p
# OK! Now I have an interesting plot! But I need to rename a few things, otherwise all labels
# in it will look too confusing >>> NEEDS MORE WORK!

###### for the colors "Black+Gray, White" there's no bar in the plot
###### I want to see how many have those colors (or if there's a mistake)
which(features_df$combination_of_primary_and_highlight_color == 'Black+Gray, White')
# out: 1345  >>> index of the row I'm looking for, there's only 1!

features_df[1345,]   # no issues here, it's just 1 squirrel
#         age      primary_fur_color    highlight_fur_color   combination_of_primary_and_highlight_color
# 1345 Juvenile             Black         Gray, White           Black+Gray, White
                        
which(features_df$combination_of_primary_and_highlight_color == 'Cinnamon+Black, White')
# out: 1198 2761 2801   # only 3 squirrels in this case




### 3. age
summary(features_df$age)
# Adult Juvenile     NA's 
#     2568      330      125
# I see that 84% of sighted squirrels where adults and only 10% where juvenile

# create bar plot to show age frequencies:
p <- ggplot(data=features_df, aes(x=age)) +
  geom_bar() +
  theme_hc()

p




########## 

#   3.# for color and location >>> is it correlated? i.e., are cinnamon squirrels more
#     # frequently seen in the park and grey squirrels in the city or viceversa or
#     # is it unrelated? >>> that would mean different behaviors would be associated
#     # with the species???

# Create location_df 
location_df <- subset(nyc_squirrels, select=c('long', 'lat', 'lat_long',
                                              'hectare', 'location', 
                                              'above_ground_sighter_measurement', 
                                              'community_districts', 
                                              'borough_boundaries', 
                                              'city_council_districts', 
                                              'police_precincts'))

names(location_df)
# "long"  "lat"   "lat_long"                         

# "hectare"
# ID tag, which is derived from the hectare grid used to divide
# and count the park area. One axis that runs predominantly north-to-south is numerical
# (1-42), and the axis that runs predominantly east-to-west is roman characters (A-I).

# "location"
# Value is either "Ground Plane" or "Above Ground." Sighters were instructed to
# indicate the location of where the squirrel was when first sighted.

# "above_ground_sighter_measurement"
# For squirrel sightings on the ground plane, 
# fields were populated with a value of “FALSE.”

# "community_districts"              "borough_boundaries"              
# "city_council_districts"           "police_precincts" 

summary(location_df$hectare) # this is pointless! ...or maybe not?... NOT AT ALL!!!
# 14D     32E     14E     01B     07H     13D     13E     03D     04C     33E     02C 
# 32      30      28      27      26      25      24      22      22      22      21 
# 03B     03F     08I     10G     12F     30B     36I     38C     05C     07F     16E 
# 21      21      21      21      21      21      21      21      20      20      20 
# 35A     09B     16D     32C     04D     08B     15G     33B     02B     02F     03E 
# 20      19      19      19      18      18      18      18      17      17      17 
# 15F     20F     38E     40B     01D     05E     07G     09A     09I     22C     41B 
# 17      17      17      17      16      16      16      16      16      16      16 
# 02A     06A     07B     08H     09H     10F     14F     15E     22F     32D     32F 
# 15      15      15      15      15      15      15      15      15      15      15 
# 37G     40C     12E     15D     17E     32A     33I     35C     37E     40D     03H 
# 15      15      14      14      14      14      14      14      14      14      13 
# 03I     11B     11D     11H     15I     17F     20B     01C     02E     04E     05F 
# 13      13      13      13      13      13      13      12      12      12      12 
# 06C     08D     09F     12G     14H     18I     21B     21D     22B     33D     36F 
# 12      12      12      12      12      12      12      12      12      12      12 
# 38F     01A     03A     04A     04B     05D     07E     11E     14B     18C     21F 
# 12      11      11      11      11      11      11      11      11      11      11 
# (Other) 
# 1423 

class(location_df$hectare) # "factor"

most_freq_h <- summary(nyc_squirrels$hectare)
most_freq_h[1:10]
# 14D 32E 14E 01B 07H 13D 13E 03D 04C 33E 
# 32  30  28  27  26  25  24  22  22  22   >>> use this numbers as markers, 
### get the lat-long for each sighting, 
### then place on map. What can be found at this spots? Are these the most/least visited areas of the park?
### are there any behaviours associated with theses spots? (for example, are they eating here?)

# the values in the 'hectare' column are of class 'factor':
class(nyc_squirrels$hectare[1]) # "factor"

# create filters to get each one of the top 10 locations:
hect_14D <- nyc_squirrels$hectare == '14D'
hect_32E <- nyc_squirrels$hectare == '32E'
hect_14E <- nyc_squirrels$hectare == '14E'
hect_01B <- nyc_squirrels$hectare == '01B'
hect_07H <- nyc_squirrels$hectare == '07H'
hect_13D <- nyc_squirrels$hectare == '13D'
hect_13E <- nyc_squirrels$hectare == '13E'
hect_03D <- nyc_squirrels$hectare == '03D'
hect_04C <- nyc_squirrels$hectare == '04C'
hect_33E <- nyc_squirrels$hectare == '33E'

# create df with only hectare, lat and long columns for each hectare value:
locations_coord_14D <- nyc_squirrels %>%
  select(hectare, lat, long) %>%
  filter(hect_14D)
locations_coord_32E <- nyc_squirrels %>%
  select(hectare, lat, long) %>%
  filter(hect_32E)
# add to top 10 locations df:
top_10_coordinates <- rbind(locations_coord_14D, locations_coord_32E)

locations_coord_14E <- nyc_squirrels %>%
  select(hectare, lat, long) %>%
  filter(hect_14E)
top_10_coordinates <- rbind(top_10_coordinates, locations_coord_14E)

locations_coord_01B <- nyc_squirrels %>%
  select(hectare, lat, long) %>%
  filter(hect_01B)
top_10_coordinates <- rbind(top_10_coordinates, locations_coord_01B)

locations_coord_13D <- nyc_squirrels %>%
  select(hectare, lat, long) %>%
  filter(hect_13D)
top_10_coordinates <- rbind(top_10_coordinates, locations_coord_13D)

locations_coord_13E <- nyc_squirrels %>%
  select(hectare, lat, long) %>%
  filter(hect_13E)
top_10_coordinates <- rbind(top_10_coordinates, locations_coord_13E)

locations_coord_03D <- nyc_squirrels %>%
  select(hectare, lat, long) %>%
  filter(hect_03D)
top_10_coordinates <- rbind(top_10_coordinates, locations_coord_03D)

locations_coord_04C <- nyc_squirrels %>%
  select(hectare, lat, long) %>%
  filter(hect_04C)
top_10_coordinates <- rbind(top_10_coordinates, locations_coord_04C)

# and the last one:
locations_coord_33E <- nyc_squirrels %>%
  select(hectare, lat, long) %>%
  filter(hect_33E)
top_10_coordinates <- rbind(top_10_coordinates, locations_coord_33E)

    # this is an answer to: where were the squirrels more frequently seen?
# so now I have the top 10 locations where squirrels where sighted, let's plot them:

ny_map_top_10 <- leaflet(data=top_10_coordinates) %>%
  addTiles() %>%  # Add default OpenStreetMap map tiles
  addMarkers(lng=~long, lat=~lat, popup= ~as.character(hectare))
ny_map_top_10

top_10_spots <- rbind(locations_coord_01B[1,], locations_coord_03D[1,])
top_10_spots <- rbind(top_10_spots, locations_coord_04C[1,])
top_10_spots <- rbind(top_10_spots, locations_coord_07H[1,])
top_10_spots <- rbind(top_10_spots, locations_coord_13D[1,])
top_10_spots <- rbind(top_10_spots, locations_coord_13E[1,])
top_10_spots <- rbind(top_10_spots, locations_coord_14D[1,])
top_10_spots <- rbind(top_10_spots, locations_coord_14E[1,])
top_10_spots <- rbind(top_10_spots, locations_coord_32E[1,])
top_10_spots <- rbind(top_10_spots, locations_coord_33E[1,])

ny_top_10_spots <- leaflet(data=top_10_spots) %>%
  addTiles() %>%  # Add default OpenStreetMap map tiles
  addMarkers(lng=~long, lat=~lat, popup= ~as.character(hectare))
ny_top_10_spots  # @@@@@@@@@@@@@ ADD COLORS!!!

########## READ ABOUT THE TOP 10 LOCATIONS:
############ ARE THESE SPOTS FULL OF TURISTS?
############ ARE THERE RESTAURANTS/EASY FOOD SPOTS THERE? ETC

### check this out:
# https://www.jessesadler.com/post/geocoding-with-r/


############################# Intermezzo: maps! ################################ 

# https://rstudio.github.io/leaflet/
# https://rstudio.github.io/leaflet/map_widget.html

################################################################################ 

# New York City's Central Park Coordinates: 
# 40.785091° N, -73.968285° W

install.packages("leaflet")
library(leaflet)

############################# 
### SUPER BASIC NYC MAP, REMOVE LATER:
ny_map <- leaflet() %>%
  addTiles() %>%  # Add default OpenStreetMap map tiles
  addMarkers(lng=-73.968285, lat=40.785091, popup="NYC")
ny_map  # Print the map
############################# 

### plot location of all squirrels sighted, with markers showing all sightings
ny_map <- leaflet(data=nyc_squirrels) %>%
  addTiles() %>%  # Add default OpenStreetMap map tiles
  addMarkers(lng=~long, lat=~lat, popup= ~as.character(shift))
ny_map


### plot location of all squirrels sighted, markers show shift (am/pm)
color_icons <- icons(
  iconUrl = ifelse(as.character(nyc_squirrels$shift) == "AM",
                   "/home/alina/Learning_to_Code/My_Projects/NYC_squirrels/icons8-blue_marker-40.png",
                   "/home/alina/Learning_to_Code/My_Projects/NYC_squirrels/icons8-red_marker-40.png"),
  iconWidth = 20, iconHeight = 45   # 38 / 95
)

# /home/alina/Learning_to_Code/My_Projects/NYC_squirrels/icons8-blue_marker-40.png
# /home/alina/Learning_to_Code/My_Projects/NYC_squirrels/icons8-red_marker-40.png

ny_map <- leaflet(data=nyc_squirrels) %>%
  addTiles() %>%
  addMarkers(lng=~long, lat=~lat, icon=color_icons)
ny_map # this is crashing my RStudio! I need to find better icons!


# found new icons at
# https://icons8.com/icons/set/marker--static


### I'm here!!! :)
# https://rstudio.github.io/leaflet/markers.html

# change colors, am/pm
# juvenile/adult
# behavior...





behavior_df <- subset(nyc_squirrels, select=c('running', 'chasing', 'climbing',
                                              'eating', 'foraging', 
                                              'kuks', 'quaas', 'moans', 
                                              'tail_flags', 'tail_twitches', 
                                              'approaches', 'indifferent', 
                                              'runs_from'))
##### I dropped 2 columns, 'other_activities' and 'other_interactions',
##### they are not of much use, mostly NA values.


time_df <- subset(nyc_squirrels, select=c('shift', 'date'))
summary(time_df)  #  AM:1347 - PM:1676





  







# plot:
#   bar plot:
#     coat colors (bar=color)
#     most frequent sightings
#     different behaviors <<<<<<<<<< I'm HERE
#   histogram:
#     age
#   heatmap:
#     behavior (avoid/approach) vs age

# features:
#   # what coat + highlight is the most frequent? what species is that?
#   # is there a correlation between age and fur color sighted?
#   # for color and location >>> is it correlated? i.e., are cinnamon squirrels more
#   # frequently seen in the park and grey squirrels in the city or viceversa or
#   # is it unrelated? >>> that would mean different behaviors would be associated
#   # with the species
#   # fur color vs behavior >>> heatmap
#   # age vs behavior >>> heatmap


# time:
#   # change date format
#   # binning >>> spring, summer, fall, winter
#   # am/pm + 4 seasons >>>
#   # at what time where the squirrels more frequently sighted? did it depend on the season?


# location:
#   # where were the squirrels most frequently sighted?:
#     # park/city - am most frequent location/pm most frequent location
#     # does it depend on the season? on the time of day?
#     # what behaviors are associated with the different locations?:
#       # kuks + etc vs location
#       # approach/avoid humans vs location
#       # above ground vs location





# behavior:
#   # which behaviors were recorded? which ones were more frequent? when? (am/pm - month)
#   # behavior vs city/park
#   # where do the squirrels eat? can I find that in this dataset?
  
  
  
  
  









################ themes...
# theme_fivethirtyeight() # >>> this theme changes the x and y labels, and takes the column name for the x label
# theme_economist() # >>> this theme places the legend above, doesn't look good either
# theme_tufte() # >>> maybe?... Times New Roman fonts...
# theme_stata() # this one gives too much importance to the label...
# theme_hc() # this one is a keeper! :) BUT I need to change the legend's name...

# https://www.datanovia.com/en/blog/ggplot-themes-gallery/


# theme_tufte(): a minimalist theme
# theme_economist(): theme based on the plots in the economist magazine
# theme_stata(): theme based on stata graph schemes.
# theme_hc(): theme based on Highcharts JS



################ ################ ################ ################ 
################ ################ ################ ################ 
################ ################ ################ ################ 

# when trying to knit for the first time, I got this error message:
#   Error: pandoc version 1.12.3 or higher is required and was not found (see the help page ?rmarkdown::pandoc_available).
# Execution halted
# 
# 
# >pandoc -v
# Error: object 'pandoc' not found
# > install.packages(pandoc)
# Error in install.packages : object 'pandoc' not found
# > Sys.getenv("RSTUDIO_PANDOC")
# [1] "/home/alina/anaconda3/envs/r-env/bin/pandoc"
# > pandoc_available()
# Error in pandoc_available() : could not find function "pandoc_available"
# > pandoc_version()
# Error in pandoc_version() : could not find function "pandoc_version"
# 
# 
# from:
#   https://stackoverflow.com/questions/28432607/pandoc-version-1-12-3-or-higher-is-required-and-was-not-found-r-shiny
# > Sys.getenv("RSTUDIO_PANDOC")
# [1] "/home/alina/anaconda3/envs/r-env/bin/pandoc"
# 
# Sys.setenv(RSTUDIO_PANDOC="--- insert directory here ---")
# 
# Sys.setenv(RSTUDIO_PANDOC="/home/alina/anaconda3/envs/r-env/bin/pandoc")
# >>> I DID NOT TRY THIS, GOT SCARED...
# 
# from:
#   https://stackoverflow.com/questions/43215191/rmarkdown-not-working-due-to-pandoc/49058294
# sudo apt-get install pandoc >>> I tried this within r-env
# AND IT WORKED!!! :)



