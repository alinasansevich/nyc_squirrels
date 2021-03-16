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

#library(readr) ?????????????   XXXXXXXXX Do I need this?
library(ggplot2)
library(dplyr) # I need dplyr to use filter function


nyc_squirrels <- read.csv('nyc_squirrels.csv')

# Change nyc_squirrels$date data type from integer to date:

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


# features:
#   # what coat + highlight is the most frequent? what species is that?
#   # is there a correlation between age and fur color sighted?
#   # for color and location >>> is it correlated? i.e., are cinnamon squirrels more
#   # frequently seen in the park and grey squirrels in the city or viceversa or
#   # is it unrelated? >>> that would mean different behaviors would be associated
#   # with the species
#   # fur color vs behavior >>> heatmap
#   # age vs behavior >>> heatmap


# behavior:
#   # which behaviors were recorded? which ones were more frequent? when? (am/pm - month)
#   # behavior vs city/park
#   # where do the squirrels eat? can I find that in this dataset?
  
  
  
  
  












