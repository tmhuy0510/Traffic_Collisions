
# Load packages -----------------------------------------------------------

library(tidyverse)
library(modelr)
library(lubridate)
library(broom)
library(infer)
library(leaflet)
library(flexdashboard)

# Set seed ----------------------------------------------------------------

set.seed(12345)

# Load data -------------------------------------------------------------

tc <- read_csv("./Traffic_Collisions.csv")
################################### Give the download link

# Understand the data -----------------------------------------------------

## Check the dimension

View(tc)
#################################### Use kable()

# Number of rows: 21127
# Number of columns: 21

## Check NAs

tc %>% 
  map_dbl(~ sum(is.na(.))) %>% 
  enframe() %>% 
  filter(value > 0)

# Number of NAs:
# ROAD_LOCATION_1:          4
# ROAD_LOCATION_2:          9068
# ROAD_CONFIGURATION:       626
# COLLISION_CONFIGURATION:  897
# NON_FATAL_INJURY:         18287
# FATAL_INJURY:             21081

## Check distinct values of unique identifiers

tc %>% 
  select(OBJECTID:CASE_FILE_NUMBER) %>% 
  map_dbl(n_distinct) %>% 
  enframe()

# Number of distinct items of the columns which are likely to be
# unique identifiers:
# OBJECTID:           21127 = Number of rows
# COLLISION_SK:       21127 = Number of rows
# CASE_FILE_NUMBER:   20319, which probably means some people were
#                     involved in traffic collisions more than once

## Check if there is any missing date of a year

# This requires ACCIDENT_DATE to be modified so the checking will be
#   done in the next part

## Check distinct values of character-columns

tc %>% 
  select(ROAD_LOCATION_1:INTERSECTION_RELATED) %>% 
  map_dbl(n_distinct) %>% 
  enframe()

# Number of distinct values for character-columns:
# ROAD_LOCATION_1:          7685 including NA
# ROAD_LOCATION_2:          2492 including NA
# ROAD_CONFIGURATION:       12 including NA
# COLLISION_CONFIGURATION:  17 including NA
# NON_FATAL_INJURY:         2 including NA and 'Yes'
# FATAL_INJURY:             2 including NA and 'Yes'
# 7 last columns:           2 including 'N' and 'Y'

## Check categories of character-columns with at most 
## 20 different items

tc %>% 
  select(ROAD_CONFIGURATION:INTERSECTION_RELATED) %>% 
  map(unique)

# ROAD_CONFIGURATION:       12 including NA
# COLLISION_CONFIGURATION:  17 including NA
# NON_FATAL_INJURY:         2 including NA and 'Yes'
# FATAL_INJURY:             2 including NA and 'Yes'
# 7 last columns:           2 including 'N' and 'Y'

# This can be automated by: 
# (1) filtering the code at line 35 with value <= 20
# (2) using semi-join the filtered and origin tc

## Examine NAs
## NON_FATAL_INJURY and FATAL_INJURY:
## 'Yes' cannot be the value of both columns

tc %>% filter(FATAL_INJURY == 'Yes', NON_FATAL_INJURY == 'Yes')

# The result is an empty data frame
# 'Yes' in one column means 'No' in the other
# Change NAs to 'No' in NON_FATAL_INJURY and FATAL_INJURY in the
#   next part

## COLLISION_CONFIGURATION and ROAD_CONFIGURATION:

# NAs are among different configuration of collision and road
# Change NAs to 'Not specified' in COLLISION_CONFIGURATION and 
#   ROAD_CONFIGURATION in the next part

## ROAD_LOCATION_2
## This column means intersecting road
## NAs may represent incidents not related to an intersection but
##   this must be checked

tc %>% filter(is.na(ROAD_LOCATION_2), INTERSECTION_RELATED == 'Y')

# The result is a data frame with 2027 rows
# Change NAs to 'Not specified' in ROAD_LOCATION_2

## ROAD_LOCATION_1
## There are only 4 NAs in this column

# Change NAs using the latitude and longitude of incidents and
#   ROAD_LOCATION_2 otherwise change NAs to 'Not specified'

## Check duplicated rows

## After excluding OBJECTID, COLLISION_SK and CASE_FILE_NUMBER

tc %>% 
  select(X, Y, ACCIDENT_DATE:INTERSECTION_RELATED) %>% 
  duplicated() %>% 
  sum()

# There are 15 duplicated rows which might represent incidents in
#   which at least 2 people were at fault
# This may be useful when compared with the spots with the highest 
#   number of incidents ##########################################
##################################################################


## After excluding OBJECTID and COLLISION_SK

tc %>% 
  select(X, Y, CASE_FILE_NUMBER:INTERSECTION_RELATED) %>% 
  duplicated() %>% 
  sum()

# There are none duplicated rows

### Rename the name of each column
### Change type of ACCIDENT_DATE
### Change NAs to 'No' in NON_FATAL_INJURY and FATAL_INJURY
###   and emphasize this change
### Change NAs to 'Not specified' in COLLISION_CONFIGURATION and 
###   ROAD_CONFIGURATION in the next part
### Change NAs to 'Not specified' in ROAD_LOCATION_2
### Change NAs using the latitude and longitude of incidents and
###   ROAD_LOCATION_2 otherwise change NAs to 'Not specified' in
###   ROAD_LOCATION_1
### Add 'year', 'month', 'day', 'day of week', 'holiday' columns
### Check if there is any missing date of a year

### Notes on some columns:
### ACCIDENT_DATE:
### The fact that daylight savings started and ended corresponds to 
### 03:00:00 and 03:59:59

# Clean the data -------------------------------------------------------

## Rename the name of each column
## Change type of ACCIDENT_DATE
## Add 'year', 'month', 'day', 'day of week' columns
tc <- tc %>% 
  rename_with(str_to_lower) %>% 
  mutate(date = accident_date %>% str_extract('^.{10}') %>% ymd(),
         year = year(date),
         month = month(date),
         day = day(date),
         day_of_week = wday(date, label = T))

## Add 'holiday' column
#####################################################################
# It is better to create another dataset related to holidays and
#   then join two tables

## Check if there is any missing date of a year

max(tc$date) - min(tc$date) + 1
n_distinct(tc$date)

# The results are the same so there are none missing dates

## Change NAs to 'No' in NON_FATAL_INJURY and FATAL_INJURY
##   and emphasize this change
## Change NAs to 'Not specified' in COLLISION_CONFIGURATION and 
##   ROAD_CONFIGURATION in the next part
## Change NAs to 'Not specified' in ROAD_LOCATION_2

tc <- tc %>% 
  mutate(non_fatal_injury = replace_na(non_fatal_injury, 'No'),
         fatal_injury = replace_na(fatal_injury, 'No'),
         collision_configuration = replace_na(collision_configuration, 'No specified'),
         road_configuration = replace_na(road_configuration, 'No specified'),
         road_location_2 = replace_na(road_location_2, 'No specified'))

## Change NAs using the latitude and longitude of incidents and
##   ROAD_LOCATION_2 otherwise change NAs to 'Not specified' in
##   ROAD_LOCATION_1

tc %>% filter(road_location_1 %>% is.na()) %>% glimpse()

# Using Google Maps with x and y columns, NAs can be replaced by 
#   'Gottingen St', 'Circassion Dr', 'Old Guysborough Rd', 
#   'Osborne Ave' for objectid of 2026, 4170, 4478 and 11536 
#   respectively
# The abbreviations like 'St' and 'Dr' comply with those used in the
#   dataset

tc <- tc %>% 
  mutate(road_location_1 = case_when(
    objectid == 2026 ~ 'Gottingen St',
    objectid == 4170 ~ 'Circassion Dr',
    objectid == 4478 ~ 'Old Guysborough Rd',
    objectid == 11536 ~ 'Osborne Ave',
    TRUE ~ road_location_1))

## Note: It must be noted that replacing NAs in fatal_injury and 
##   non_fatal_injury is based on the assumption that there were
##   none unreported, unspecified or delayed non-fatal and fatal
##   injuries. Therefore, incidents with 'No' values in both columns 
##   mean there were no injuries at all

## Check if there is any Na in the dataset

tc %>% is.na() %>% sum()

# The result is 0

## Use str_to_title() function to make the content format of 
##   each character-column uniform

tc <- tc %>% 
  mutate(across(road_location_1:collision_configuration, 
                str_to_title))

## Add 'road_name' column which contains the road name without the
##   number in front

road_name <- str_split(tc$road_location_1, 
                       '^\\d*\\b', 
                       simplify = T)[, 2] %>% str_trim()

# Split (1) '123 Abc Rd' into '123' and 'Abc Rd'
#       (2) 'Abc Rd' into '' and ' Abc Rd'
# Take the 2nd element
# Trim space in the case of (2)

# There is a pitfall for the above approach. For example,
#   '123e Abc Rd' is split into '' and '123e Abc Rd'. Taking the
#   2nd element still gives the road name with the number in front

road_name %>% 
  str_starts('\\d') %>% 
  sum()

# The result is 31 which is the number of cases falling into this
#   pitfall. Let's check these 31 cases

tc$road_location_1[road_name %>% str_starts('\\d')]

# Indeed, all of them have the format of '123e Abc Rd' with 3
#   exceptions under the format of '123 456' which is highly likely
#   to represent data incorrectly entered
# Because 31 is too small when compared to 21127, these 31 rows will 
#   be dropped

tc <- tc %>% 
  mutate(road_name = road_name) %>% 
  filter(!str_starts(road_name, '\\d'))

View(tc)
#################################### Use kable()

# Perform EDA -------------------------------------------------------------

## Questions
## Total number of daily incidents:
##    Year-month-day              (done)
##    Year                        (done)
##    Month                       (done)
##    Day of week                 (done)
##    ## These can be further split into sub-categories
## Distribution of number of incidents:
##    Year-month-day              (done)
## Total number of incidents:
##    Road name                   (done)
##    Road configuration          (done)
##    Collision configuration     (done)
##    Young demographic           (done)
##    Pedestrian collisions       (same as YOUTH)
##    Aggressive driving          (same as YOUTH)
##    Distracted driving          (same as YOUTH)
##    Impaired driving            (same as YOUTH)
##    Bicycle collisions          (same as YOUTH)
##    Intersection related        (same as YOUTH)
##    ## These can be further split into sub-timelines
## Distribution of incidents on map

## Relation between:
##    Injury and Time                       (done)
##    Injury and Youth                      (done)
##    Injury and Impaired-driving           (same as YOUTH)
##    Injury and collision_configuration    (done)
##    Injury and road_configuration         (done)
##    Injury and intersection_related       (same as YOUTH)
##    Youth and Aggressive driving          (done)
##    Youth and Impaired driving            (done)
##    Youth and collision_configuration     (same as previous)
##    Youth and road_configuration          (same as previous)

## Model:
##    Number of incidents Vs. Day of week and holiday

## Hypothesis test:
##    Injury and Youth
##    Youth and aggressive driving

## Daily incidents:
##    Year-month-day
##    Year-month
##    Year
##    Month
##    Day of week
##    ## These can be further split into sub-categories:


## Daily incidents: Year-month-day

##### Use UI graph 

tc %>% 
  count(date) %>% 
  ggplot(aes(x = date, y = n)) + 
  geom_line(size = 1) + 
  geom_smooth(se = F)

tc %>% 
  filter(year == 2021) %>% 
  count(date) %>%
  ggplot(aes(x = date, y = n)) + 
  geom_line(size = 1) + 
  geom_smooth(se = F) +
  scale_x_date(date_breaks = '1 month', date_labels = '%b')

# Comments:
# In 2018:
#   Spike in Jan, Mar and Nov-Dec
# In 2019:
#   Spike in Jan-Mar and Nov
# In 2020:
#   Spike in Jan-Feb and Nov
#   Dip in Apr
# In 2021:
#   Spike in Jan and Mar
#   End on 2021-11-30
# In Total:
#   Spike in Jan-March and Nov-Dec in 2018-2020
#   Spike in Jan-Mar in 2021
#   End on 2021-11-30
#   Dip in Apr in 2020

## Daily incidents: Year-month

tc %>% 
  count(date, year, month) %>% 
  group_by(year, month) %>% 
  summarize(n_mean = mean(n)) %>% 
  ggplot(aes(x = make_date(year = year, month = month),
             y = n_mean)) +
  geom_line(size = 1)

# Comments:
# Dip in Mar-Apr
# Spike in Nov-Dec
# Max in Dec-2019
# Min in Mar-2020
# End on 2021-11-30

## Daily incidents: Year

tc %>% 
  count(date, year) %>% 
  group_by(year) %>% 
  summarize(n_mean = mean(n)) %>% 
  ggplot(aes(x = as_factor(year), y = n_mean)) +
  labs(x = NULL, y = 'Average number of daily incidents') +
  geom_col()

tc %>% 
  count(date, year) %>% 
  ggplot(aes(x = as_factor(year),
             y = n)) +
  geom_boxplot() +
  stat_summary(fun = mean, color = 'darkred') +
  labs(x = NULL, y = 'Average number of daily incidents') +
  coord_flip()

# Comments:
# 2018-2019: Slightly increase ********* Growth in population
# 2019-2020: Sharply decrease ********* COVID-19
# 2020-2021: Slightly increase ********* Growth in population

## Daily incidents: Month

tc %>% 
  count(date, month) %>% 
  group_by(month) %>% 
  summarize(n_mean = mean(n)) %>% 
  ggplot(aes(x = as_factor(month), y = n_mean)) +
  labs(x = NULL, y = 'Average number of daily incidents') +
  geom_col() 

tc %>% 
  count(date, month) %>% 
  ggplot(aes(x = as_factor(month),
             y = n)) +
  geom_boxplot() +
  stat_summary(fun = mean, color = 'darkred') +
  labs(x = NULL, y = 'Average number of daily incidents') +
  coord_flip()
  
# Comments:
# 3-8: Daily incidents < 15
# 9-2: Daily incidents > 15
# Max in 12
# Min in 4

## Daily incidents: Day of Week

tc %>%
  count(date, day_of_week) %>% 
  group_by(day_of_week) %>% 
  summarize(n_mean = mean(n)) %>% 
  ggplot(aes(x = fct_relevel(day_of_week, 'Sun', after = Inf), 
             y = n_mean)) +
  labs(x = NULL, y = 'Average number of daily incidents') +
  geom_col() 

tc %>%
  count(date, day_of_week) %>% 
  ggplot(aes(x = fct_relevel(day_of_week, 'Sun', after = Inf), 
             y = n)) +
  geom_boxplot() +
  stat_summary(fun = mean, color = "darkred") +
  labs(x = NULL, y = 'Average number of daily incidents') +
  coord_flip()

# Comments:
# Weekends have lower number of daily incidents
# Weekdays have higher number of daily incidents
# Max in Fri
# Min in Sun

######### Pay attention to why there is 1 significant outlier on Fri
######## link: https://halifax.citynews.ca/local-news/drivers-asked-to-use-caution-on-highway-103-3287739

## Distribution of number of incidents:
##    Year-month-day

daily_tc = tc %>% 
  group_by(date) %>% 
  summarize(
    n = n(),
    non_fatal_injury = sum(non_fatal_injury == 'Yes'),
    fatal_injury = sum(fatal_injury == 'Yes'),
    injury = non_fatal_injury + fatal_injury,
    non_injury = n - injury
  ) %>% 
  mutate(day_of_week = wday(date, label = TRUE))

daily_tc %>% 
  ggplot(aes(n)) + 
  geom_histogram(binwidth = 3, boundary = 0, color = 'white') +
  scale_x_continuous(breaks = seq(0, 90, by = 9))

daily_tc %>% 
  .$n %>% 
  quantile()

daily_tc %>% 
  select(date, day_of_week, n) %>% 
  arrange(desc(n)) %>% 
  head()

daily_tc %>% 
  select(date, day_of_week, n) %>% 
  arrange(desc(n)) %>% 
  tail()
############ Create tables for them

# Comments:
# Bin of 12-15 has the highest number of counts
# Skewed positively
# 5-number summary
# 6 days with highest number of daily incidents
# 6 days with lowest number of daily incidents

## Non-injury and injury vs Month and DOW
daily_tc_long = daily_tc %>%
  select(-n) %>% 
  pivot_longer(non_fatal_injury:non_injury, 
               names_to = 'type_of_injury',
               values_to = 'count') %>% 
  mutate(month = month(date, label = TRUE),
         day_of_week_2 = case_when(
           day_of_week %in% c('Sun', 'Sat') ~ 'Weekend',
           TRUE ~ 'Weekday'))

# Month
daily_tc_long %>% 
  filter(type_of_injury %in% c('injury', 'non_injury')) %>% 
  group_by(month, type_of_injury) %>%
  summarize(n = sum(count)) %>% 
  ggplot(aes(x = month, y = n)) +
  geom_col(aes(fill = type_of_injury), position = 'fill') +
  coord_flip()

########################## If involved in accident, risk of injury is
########################## same for each month

# DOW (day_of_week) and Weekend/Weekday (day_of_week_2)
daily_tc_long %>% 
  filter(type_of_injury %in% c('injury', 'non_injury')) %>% 
  group_by(day_of_week, type_of_injury) %>%
  summarize(n = sum(count)) %>% 
  ggplot(aes(x = fct_relevel(day_of_week, 'Sun', after = Inf), 
             y = n)) +
  geom_col(aes(fill = type_of_injury), position = 'fill') +
  coord_flip()

daily_tc_long %>% 
  filter(type_of_injury %in% c('injury', 'non_injury')) %>% 
  group_by(day_of_week_2, type_of_injury) %>%
  summarize(n = sum(count)) %>% 
  ggplot(aes(x = day_of_week_2, 
             y = n)) +
  geom_col(aes(fill = type_of_injury), position = 'fill') +
  coord_flip()

########################## If involved in accident, risk of injury is
########################## same for each dow/weekdays and weekdays

## Total number of incidents:
##    Road name
##    Road configuration
##    Collision configuration
##    Young demographic
##    Pedestrian collisions
##    Aggressive driving
##    Distracted driving
##    Impaired driving
##    Bicycle collisions
##    Intersection related
##    ## These can be further split into sub-timelines

##    Road name

## Based on incidents
tc %>% count(road_name) %>% arrange(desc(n)) %>% slice(1:20)

## Based on injury
tc %>% 
  filter(non_fatal_injury == 'Yes' | fatal_injury == 'Yes') %>% 
  count(road_name) %>% 
  arrange(desc(n)) %>% 
  slice(1:10)

## Based on fatal-injury
tc %>% 
  filter(fatal_injury == 'Yes') %>% 
  count(road_name) %>% 
  arrange(desc(n)) %>% 
  slice(1:10)

# Comment: Hwy 103 has highest number of fatal but not on the
#   other 2 lists

##    Road configuration

## Based on incidents
tc %>% filter(road_configuration != 'No Specified') %>% 
  count(road_configuration) %>% arrange(desc(n)) 

## Based on injury
tc %>% 
  filter(road_configuration != 'No Specified') %>% 
  filter(non_fatal_injury == 'Yes' | fatal_injury == 'Yes') %>% 
  count(road_configuration) %>% 
  arrange(desc(n))

## Based on fatal-injury
tc %>% 
  filter(road_configuration != 'No Specified') %>% 
  filter(fatal_injury == 'Yes') %>% 
  count(road_configuration) %>% 
  arrange(desc(n))

# Comment: Top 4 remain on all lists

##    Collision configuration

## Based on incidents
tc %>% filter(collision_configuration != 'No Specified') %>% 
  count(collision_configuration) %>% arrange(desc(n)) 

## Based on injury
tc %>% 
  filter(collision_configuration != 'No Specified') %>% 
  filter(non_fatal_injury == 'Yes' | fatal_injury == 'Yes') %>% 
  count(collision_configuration) %>% 
  arrange(desc(n))

## Based on fatal-injury
tc %>% 
  filter(collision_configuration != 'No Specified') %>% 
  filter(fatal_injury == 'Yes') %>% 
  count(collision_configuration) %>% 
  arrange(desc(n))

# Comment: 
#   Single Vehicle is the most dangerous
#   Rear-end is the most frequent


##    Young demographic

tc %>% count(young_demographic)

tc %>% filter(non_fatal_injury == 'Yes' | fatal_injury == 'Yes') %>% 
  count(young_demographic)

tc %>% filter(fatal_injury == 'Yes') %>% 
  count(young_demographic)

tc %>% 
  filter(non_fatal_injury == 'Yes' | fatal_injury == 'Yes') %>% 
  count(young_demographic) %>%
  mutate(percent = n/sum(n) * 100,
         ypos = cumsum(percent) - 0.5*percent) %>% 
  ggplot(aes(x = '', y = percent)) +
  geom_bar(aes(fill = fct_rev(young_demographic)), 
           stat = 'identity',
           color = 'white') +
  scale_y_continuous(name = NULL, breaks = NULL) +
  scale_x_discrete(name = NULL) +
  scale_fill_discrete(name = NULL,
                      labels = c('People under 25 years old',
                                 "People at or over 25 years old")) +
  coord_polar(theta = 'y') +
  geom_text(aes(y = ypos, 
                label = str_c(round(percent, 1), '%')), color = "white", size=5) +
  theme_void() +
  guides(fill = guide_legend(nrow = 1)) +
  theme(legend.position = 'bottom')


# Comment: Young is safer

################# Test hypo between young and injury

##    Pedestrian collisions
##    Aggressive driving
##    Distracted driving
##    Impaired driving
##    Bicycle collisions
##    Intersection related

# These are following same suit as Young

##    Youth and Aggressive driving
##    Youth and Impaired driving


tc %>% count(young_demographic, agressive_driving)


tc %>% count(young_demographic, impaired_driving)

tc_map <- tc %>% 
  select(lat = wgs84_lat_coord, 
         lng = wgs84_lon_coord) %>% 
  leaflet() %>% 
  addTiles() %>% 
  addMarkers(clusterOptions = markerClusterOptions())

tc_map

tc %>% 
  filter(year == 2021, month == 1, day == 22) %>% 
  select(lat = wgs84_lat_coord, 
         lng = wgs84_lon_coord) %>% 
  leaflet() %>% 
  addTiles() %>% 
  addMarkers(clusterOptions = markerClusterOptions())


snow_date = ymd(c('2021-01-02',
                  '2021-01-22', 
                  '2021-03-19',
                  '2020-11-03', '2020-02-10', '2020-01-16',
                  '2019-11-28',
                  '2018-03-08', '2018-12-07', '2018-11-16'))


daily_tc_data = daily_tc %>% 
  select(date, n, day_of_week) %>% 
  mutate(
    term = case_when(
      month(date) >= 3 & month(date) <= 8 ~ 'Spring-Summer',
      TRUE ~ 'Fall-Winter'),
    month = as_factor(month(date)),
    storm = case_when(
      date %in% snow_date ~ 'Yes',
      TRUE ~ 'No')
    )

model1 = lm(n ~ day_of_week, data = daily_tc_data)
model2 = lm(n ~ day_of_week + storm, data = daily_tc_data)
model3 = lm(n ~ day_of_week + storm + month, data = daily_tc_data)

daily_tc_data %>% 
  gather_residuals(without = model1, 
                   with_storm = model2, 
                   with_storm_month = model3) %>%
  filter(year(date) == 2021) %>% 
  ggplot(aes(date, resid)) +
  geom_line() + 
  geom_smooth(se = F)+
  facet_wrap(~ model, nrow = 3) +
  geom_hline(yintercept = 0, size = 2, colour = "white")



tc %>% 
  filter(year == 2018) %>% 
  count(date) %>%
  ggplot(aes(x = date, y = n)) + 
  geom_line(size = 1)

tc %>% 
  filter(year == 2019) %>% 
  count(date) %>%
  arrange(desc(n))


# >= 30
### '2021-01-02', '2021-01-22', 2021-03-19
### '2020-11-03', '2020-02-10', '2020-01-16', snowy storm
### '2019-11-28', snow storm
### 2018-03-08, '2018-12-07', 2018-11-16 storm



## Test if risk of being injured is same for weekday and weekend

data_test_1 = daily_tc %>% 
  mutate(risk_of_injury = injury/n,
         day_of_week = case_when(
           day_of_week %in% c('Sun', 'Sat') ~ 'Weekend',
           TRUE ~ 'Weekday'
         )) %>%
  select(day_of_week, risk_of_injury) 

data_test_1 %>% 
  ggplot(aes(x = day_of_week, y = risk_of_injury)) +
  geom_boxplot()

# Set alpha = 0.05

# distracted and impaired vs injury

tc_agressive = tc %>% 
  filter(agressive_driving == 'Y',
         distracted_driving == 'N',
         impaired_driving == 'N') %>% 
  mutate(injury = case_when(
    non_fatal_injury == 'Yes' | fatal_injury == 'Yes' ~ 'Yes',
    TRUE ~ 'No'))

tc_distracted = tc %>% 
  filter(agressive_driving == 'N',
         distracted_driving == 'Y',
         impaired_driving == 'N') %>% 
  mutate(injury = case_when(
    non_fatal_injury == 'Yes' | fatal_injury == 'Yes' ~ 'Yes',
    TRUE ~ 'No'))

tc_impaired = tc %>% 
  filter(agressive_driving == 'N',
         distracted_driving == 'N',
         impaired_driving == 'Y') %>% 
  mutate(injury = case_when(
    non_fatal_injury == 'Yes' | fatal_injury == 'Yes' ~ 'Yes',
    TRUE ~ 'No'))

imp_dis = tibble(
  condition = c(rep('impaired', nrow(tc_impaired)),
                rep('distracted', nrow(tc_distracted))),
  injury = c(tc_impaired$injury, tc_distracted$injury)) %>% 
  sample_n(size = nrow(tc_impaired) + nrow(tc_distracted))
  
obs_diff_prop = imp_dis %>% 
  specify(injury ~ condition, success = 'Yes') %>% 
  calculate(stat = 'diff in props', 
            order = c('impaired', 'distracted'))

null_distribution = imp_dis %>% 
  specify(injury ~ condition, success = 'Yes') %>% 
  hypothesize(null = 'independence') %>% 
  generate(reps = 10000, type = 'permute') %>% 
  calculate(stat = 'diff in props', 
            order = c('impaired', 'distracted'))

boot_distribution = imp_dis %>% 
  specify(injury ~ condition, success = 'Yes') %>%
  generate(reps = 10000, type = 'bootstrap') %>% 
  calculate(stat = 'diff in props', 
            order = c('impaired', 'distracted'))

null_distribution %>% visualize() + shade_p_value(obs_diff_prop, direction = "right")

boot_distribution %>% visualize()

ci_percentile = boot_distribution %>% get_ci(level = 0.95, type = 'percentile')

boot_distribution %>% visualize() + shade_ci(ci_percentile)

# Weekday and weekend vs risk of injury

weekday_weekend = daily_tc %>%
  mutate(
    day_of_week_2 = case_when(
      day_of_week %in% c('Sun', 'Sat') ~ 'Weekend',
      TRUE ~ 'Weekday'),
    risk_of_injury = injury/n*100) %>% 
  select(day_of_week_2, risk_of_injury)

boot_distribution_2 = weekday_weekend %>% 
  specify(risk_of_injury ~ day_of_week_2) %>% 
  generate(reps = 10000, type = 'bootstrap') %>% 
  calculate(stat = 'diff in means', order = c('Weekend', 'Weekday'))

boot_distribution_2 %>% visualize()

ci_percentile_2 = boot_distribution_2 %>% get_ci()

boot_distribution_2 %>% visualize() + shade_ci(ci_percentile_2)

obs_diff_means = weekday_weekend %>% 
  specify(risk_of_injury ~ day_of_week_2) %>%
  calculate(stat = 'diff in means', order = c('Weekend', 'Weekday'))
