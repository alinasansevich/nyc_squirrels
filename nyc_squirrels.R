#library(readr) ?????????????

nyc_squirrels <- read.csv('nyc_squirrels.csv')

View(nyc_squirrels)
nrow(nyc_squirrels)
ncol(nyc_squirrels)

colnames(nyc_squirrels)

# I create subsets of column names by category:
location <- c('long', 'lat', 'lat_long', 'hectare', 'location', 'above_ground_sighter_measurement', 'specific_location', 'zip_codes', 'community_districts', 'borough_boundaries', 'city_council_districts', 'police_precincts')
features <- c('age', 'primary_fur_color', 'highlight_fur_color', 'combination_of_primary_and_highlight_color', 'color_notes')
behavior <- c('running', 'chasing', 'climbing', 'eating', 'foraging', 'other_activities', 'kuks', 'quaas', 'moans', 'tail_flags', 'tail_twitches', 'approaches', 'indifferent', 'runs_from', 'other_interactions')

# Now, I create new dataframes, subsets from nyc_squirrels:
location_df <- subset(nyc_squirrels, select=c('long', 'lat', 'lat_long', 'hectare', 'location', 'above_ground_sighter_measurement', 'specific_location', 'zip_codes', 'community_districts', 'borough_boundaries', 'city_council_districts', 'police_precincts'))
features_df <- subset(nyc_squirrels, select=c('age', 'primary_fur_color', 'highlight_fur_color', 'combination_of_primary_and_highlight_color', 'color_notes'))
behavior_df <- subset(nyc_squirrels, select=c('running', 'chasing', 'climbing', 'eating', 'foraging', 'other_activities', 'kuks', 'quaas', 'moans', 'tail_flags', 'tail_twitches', 'approaches', 'indifferent', 'runs_from', 'other_interactions'))

head(location_df)

summary(location_df)
summary(features_df)
summary(behavior_df)

######## change col type
######## plot a few cols :P :D

### I start working with the features_df:

# histogram:
library(ggplot2)

df <- data.frame(features_df$age)
age_summary <- summary(df)
df2 <- data.frame(age_summary)
### I'm HERE!
ggplot(df2, aes(x=age_summary)) + geom_histogram(color="black", fill="blue") # NOT WORKING

age_summary

