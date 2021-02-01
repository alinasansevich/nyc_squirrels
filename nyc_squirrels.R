#library(readr) ?????????????

nyc_squirrels <- read.csv('nyc_squirrels.csv')

View(nyc_squirrels)
nrow(nyc_squirrels)
ncol(nyc_squirrels)

col_names <- colnames(nyc_squirrels)
class(col_names)
# I create subsets of column names by category:
location <- c('long', 'lat', 'lat_long', 'hectare', 'location', 'above_ground_sighter_measurement', 'specific_location', 'zip_codes', 'community_districts', 'borough_boundaries', 'city_council_districts', 'police_precincts')
features <- c('age', 'primary_fur_color', 'highlight_fur_color', 'combination_of_primary_and_highlight_color', 'color_notes')
behavior <- c('running', 'chasing', 'climbing', 'eating', 'foraging', 'other_activities', 'kuks', 'quaas', 'moans', 'tail_flags', 'tail_twitches', 'approaches', 'indifferent', 'runs_from', 'other_interactions')

# Now, I create new dataframes, subsets from nyc_squirrels:
location_df <- subset(nyc_squirrels, select=c('long', 'lat', 'lat_long', 'hectare', 'location', 'above_ground_sighter_measurement', 'community_districts', 'borough_boundaries', 'city_council_districts', 'police_precincts')) # I removed 2 columns: 'specific_location' and zip_codes'
features_df <- subset(nyc_squirrels, select=c('age', 'primary_fur_color', 'highlight_fur_color', 'combination_of_primary_and_highlight_color')) # I remove the 'color_notes' columns because it's not of much use
behavior_df <- subset(nyc_squirrels, select=c('running', 'chasing', 'climbing', 'eating', 'foraging', 'kuks', 'quaas', 'moans', 'tail_flags', 'tail_twitches', 'approaches', 'indifferent', 'runs_from')) # I removed 2 columns: 'other_activities' and 'other_interactions'

head(location_df)

### ToDo: remove NA values and run the summaries again

summary(location_df)
summary(features_df)
summary(behavior_df)

######## change col type
######## plot a few cols :P :D

### I start working with the features_df:

# histogram:
library(ggplot2)

### ToDo: remove NA values
ggplot(data=features_df, aes(x=age)) + geom_bar(color="black", fill="blue")
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