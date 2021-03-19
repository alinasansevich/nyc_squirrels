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
library(ggplot2)
library(dplyr)

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
# class(nyc_squirrels$date) # "Date"

########## For nyc_squirrels$age, I need to unify formats, "?" + NAs = NAs
class(nyc_squirrels$age) # factor, Levels: ? Adult Juvenile
summary(nyc_squirrels$age)
#        ?    Adult Juvenile     NA's 
#        4     2568      330      121 

### First I tried this line, it will change '?' to NA, but it doesn't change the factor's level
nyc_squirrels[nyc_squirrels$age == "?" ] <- NA

### So, I need to change the factor level from "?" to NA
levels(nyc_squirrels$age)[levels(nyc_squirrels$age) == '?'] <- NA
summary(nyc_squirrels$age)
#    Adult Juvenile     NA's 
#     2568      330      125 


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


########## I'M HERE!!!!!!!!!!!!!!!!!!
########## 
########## Continue with binning: for nyc_squirrels$date, create 4 bins == 4 seasons
########## 
########## Do I need any dummy_variables? I think not, but check..
########## 
########## With that I would complete the data wrangling step of my analysis.
########## 

library(lubridate)

seasons <- c('Spring', 'Summer', 'Fall', 'Winter')

class(nyc_squirrels$date[1])

month(nyc_squirrels$date[1])

# seasons = []
# 
# for i in range(len(nyc_squirrels)):
#   if month(nyc_squirrels$date[i]) in [3, 4, 5]:
#   seasons.append('Spring')
# if month(nyc_squirrels$date[i]) in [6, 7, 8]:
#   seasons.append('Summer')
# if month(nyc_squirrels$date[i]) in [9, 10, 11]:
#   seasons.append('Fall')
# if month(nyc_squirrels$date[i]) in [12, 1, 2]:
#   seasons.append('Winter')
# 
# cbind(nyc_squirrels, seasons)


# features:
#   # what coat + highlight is the most frequent? what species is that?
#   # is there a correlation between age and fur color sighted?
#   # for color and location >>> is it correlated? i.e., are cinnamon squirrels more
#   # frequently seen in the park and grey squirrels in the city or viceversa or
#   # is it unrelated? >>> that would mean different behaviors would be associated
#   # with the species
#   # fur color vs behavior >>> heatmap
#   # age vs behavior >>> heatmap








# I create 4 subsets based on category: location, features, behavior and time.
location_df <- subset(nyc_squirrels, select=c('long', 'lat', 'lat_long',
                                              'hectare', 'location', 
                                              'above_ground_sighter_measurement', 
                                              'community_districts', 
                                              'borough_boundaries', 
                                              'city_council_districts', 
                                              'police_precincts')) 
##### I dropped 2 columns: 'specific_location' and zip_codes', 
##### because they have mostly NA values.


features_df <- subset(nyc_squirrels, select=c('age', 
                                              'primary_fur_color', 
                                              'highlight_fur_color', 
                                              'combination_of_primary_and_highlight_color'))
##### I dropped 'color_notes' column because it's not of much use, mostly NA values.


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



### ToDo: remove NA values and run the summaries again >>> Y/N?

summary(location_df)

summary(behavior_df)

#########################################################
### I start working with the features_df:

### 1. Check for missing data:
missing_feat <- is.na(features_df)
summary(missing_feat)
# age          primary_fur_color highlight_fur_color
# Mode :logical   Mode :logical     Mode :logical      
# FALSE:2898      FALSE:2968        FALSE:1937         
# TRUE :125       TRUE :55          TRUE :1086         
# combination_of_primary_and_highlight_color
# Mode :logical                             
# FALSE:3023  
### I find missing values in all columns,
### but (at least for now) I choose not to drop anything.

### 2. Descriptive Statistics
summary(features_df)
# age       primary_fur_color      highlight_fur_color
# ?       :   0   Black   : 103     Cinnamon       : 767    
# Adult   :2568   Cinnamon: 392     White          : 585    
# Juvenile: 330   Gray    :2473     Cinnamon, White: 268    
# NA's    : 125   NA's    :  55     Gray           : 170    
#                                   Gray, White    :  59    
#                                   (Other)        :  88    
#                                   NA's           :1086    
#  combination_of_primary_and_highlight_color
#  Gray+               :895                  
#  Gray+Cinnamon       :752                  
#  Gray+White          :489                  
#  Gray+Cinnamon, White:265                  
#  Cinnamon+Gray       :162                  
#  Cinnamon+White      : 94                  
#  (Other)             :366  


### 3. age
# I see that 84% of sighted squirrels where adults and only 10% where juvenile
features_df[features_df == "?" ] <- NA  # change '?' to NA
summary(features_df$age)
# create bar plot to show age frequencies:
library(ggthemes)
p <- ggplot(data=features_df, aes(x=age)) +
  geom_bar(color="black", fill="#000000") +
  theme_hc()

p
### 4. coat colors

df[!is.na(df$B), ]

##### drop NA values from features_df$primary_fur_color:
features_df_fur_na <- features_df %>% filter(!is.na(primary_fur_color))
### plot primary_fur_color frequencies
p <- ggplot(data=features_df_fur_na, aes(x=primary_fur_color, fill=primary_fur_color)) + 
  geom_bar() + 
  scale_fill_manual(values=c("#000000", "#D2691E", "#808080")) + 
  labs(x="Primary Fur Color", y="Count") +
  theme(legend.position="none") + # + labs(fill = "Primary Fur Color")
  theme_hc() ##### ToDo: change the legend's name! Or remove legend!

p
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


# black=#000000
# cinnamon=#D2691E
# gray=#808080

  





### ToDo: remove NA values
ggplot(data=features_df, aes(x=highlight_fur_color)) + geom_bar(color="black", fill="grey") # rearrange variables, change colors
ggplot(data=features_df, aes(x=primary_fur_color)) + geom_bar(color="black", fill="beige")

ggplot(data=behavior_df, aes(x=age)) + geom_bar(color="black", fill="blue")

behavior_freq <- summary(behavior_df)
behavior_freq

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
  
  
  
  
  












