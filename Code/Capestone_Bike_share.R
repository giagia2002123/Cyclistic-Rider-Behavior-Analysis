library(tidyverse)  #helps wrangle data
library(lubridate)  #helps wrangle date attributes
library(ggplot2)  #helps visualize data
library(plotrix)

getwd() #displays your working directory
setwd("/Users/USER/Desktop/MitCourses/GoogleDataAnalytics/GDA_CaseStudy/Data/csv")

#=====================
# STEP 1: COLLECT DATA
#=====================
# Upload Divvy datasets (csv files) here
raw_202101 <- read.csv("202101-divvy-tripdata.csv")
raw_202102 <- read.csv("202102-divvy-tripdata.csv")
raw_202103 <- read.csv("202103-divvy-tripdata.csv")
raw_202104 <- read.csv("202104-divvy-tripdata.csv")
raw_202105 <- read.csv("202105-divvy-tripdata.csv")
raw_202106 <- read.csv("202106-divvy-tripdata.csv")
raw_202107 <- read.csv("202107-divvy-tripdata.csv")
raw_202108 <- read.csv("202108-divvy-tripdata.csv")
raw_202109 <- read.csv("202109-divvy-tripdata.csv")
raw_202110 <- read.csv("202110-divvy-tripdata.csv")
raw_202111 <- read.csv("202111-divvy-tripdata.csv")
raw_202112 <- read.csv("202112-divvy-tripdata.csv")

# Checking the data
str(raw_202101)
str(raw_202102)
str(raw_202103)
str(raw_202104)
str(raw_202105)
str(raw_202106)
str(raw_202107)
str(raw_202108)
str(raw_202109)
str(raw_202110)
str(raw_202111)
str(raw_202112)

#====================================================
# STEP 2: WRANGLE DATA AND COMBINE INTO A SINGLE FILE
#====================================================
# Compare column names each of the files
# While the names don't have to be in the same order, they DO need to match perfectly before we can use a command to join them into one file
colnames(raw_202101)
colnames(raw_202102)
colnames(raw_202103)
colnames(raw_202104)
colnames(raw_202105)
colnames(raw_202106)
colnames(raw_202107)
colnames(raw_202108)
colnames(raw_202109)
colnames(raw_202110)
colnames(raw_202111)
colnames(raw_202112)

# Convert ride_id and rideable_type to character so that they can stack correctly
raw_202101 <-  mutate(raw_202101, ride_id = as.character(ride_id)
                   ,rideable_type = as.character(rideable_type)) 
raw_202102 <-  mutate(raw_202102, ride_id = as.character(ride_id)
                   ,rideable_type = as.character(rideable_type)) 
raw_202103 <-  mutate(raw_202103, ride_id = as.character(ride_id)
                   ,rideable_type = as.character(rideable_type)) 
raw_202104 <-  mutate(raw_202104, ride_id = as.character(ride_id)
                      ,rideable_type = as.character(rideable_type)) 
raw_202105 <-  mutate(raw_202105, ride_id = as.character(ride_id)
                      ,rideable_type = as.character(rideable_type)) 
raw_202106 <-  mutate(raw_202106, ride_id = as.character(ride_id)
                      ,rideable_type = as.character(rideable_type)) 
raw_202107 <-  mutate(raw_202107, ride_id = as.character(ride_id)
                      ,rideable_type = as.character(rideable_type)) 
raw_202108 <-  mutate(raw_202108, ride_id = as.character(ride_id)
                      ,rideable_type = as.character(rideable_type)) 
raw_202109 <-  mutate(raw_202109, ride_id = as.character(ride_id)
                      ,rideable_type = as.character(rideable_type)) 
raw_202110 <-  mutate(raw_202110, ride_id = as.character(ride_id)
                      ,rideable_type = as.character(rideable_type)) 
raw_202111 <-  mutate(raw_202111, ride_id = as.character(ride_id)
                      ,rideable_type = as.character(rideable_type)) 
raw_202112 <-  mutate(raw_202112, ride_id = as.character(ride_id)
                      ,rideable_type = as.character(rideable_type)) 

# Stack individual quarter's data frames into one big data frame
all_trips <- bind_rows(raw_202101, raw_202102, raw_202103, raw_202104,
                       raw_202105, raw_202106, raw_202107, raw_202108,
                       raw_202109, raw_202110, raw_202111, raw_202112)

# Remove lat, long
all_trips <- all_trips %>%  
  select(-c(start_lat, start_lng, end_lat, end_lng))

#======================================================
# STEP 3: CLEAN UP AND ADD DATA TO PREPARE FOR ANALYSIS
#======================================================
# Inspect the new table that has been created
colnames(all_trips)  #List of column names
nrow(all_trips)  #How many rows are in dataframe?
dim(all_trips)  #Dimensions of the dataframe?
head(all_trips)  #See the first 6 rows of dataframe.
tail(all_trips) # See the last 6 rows of dataframe.
str(all_trips)  #See list of columns and data types (numeric, character, etc)
summary(all_trips)  #Statistical summary of data.

# Checking the two labels of data "casual" and "member"
table(all_trips$member_casual)

# Spliting date-time at started_at column to separate columns
# There are total 5 columns:
  # date: The beginning date format yyyy-mm-dd
all_trips$date <- as.Date(all_trips$started_at)
  # Separate date to day - month - year respectively
all_trips$month <- format(as.Date(all_trips$date), "%m")
all_trips$day <- format(as.Date(all_trips$date), "%d")
all_trips$year <- format(as.Date(all_trips$date), "%Y")
  # Finding the day of week
all_trips$day_of_week <- format(as.Date(all_trips$date), "%A")


# Just like we practiced on spreadsheets earlier,
  # now, we need to calculate the ride length for the data.
all_trips$ride_length <- difftime(all_trips$ended_at,all_trips$started_at)

# Viewing the structure of new data
str(all_trips)

# Make sure that ride_length in numeric type
is.factor(all_trips$ride_length)
all_trips$ride_length <- as.numeric(as.character(all_trips$ride_length))
is.numeric(all_trips$ride_length)

# Some of the ride_length value is negative
  # which can be consider as "bad" data.
# To handle this situation, all we have to do is just delete these data.
# Notice: When doing a process that affect the data like removing attributes,
  # data points, etc.; it could be better to create new dataframe to store it.
all_trips_v2 <- all_trips[!(all_trips$start_station_name == "HQ QR" | all_trips$ride_length<0),]

# View our final result:
head(all_trips_v2)

#=====================================
# STEP 4: CONDUCT DESCRIPTIVE ANALYSIS
#=====================================
# Descriptive analysis on ride_length (all figures in seconds)
mean(all_trips_v2$ride_length)
median(all_trips_v2$ride_length)
max(all_trips_v2$ride_length)
min(all_trips_v2$ride_length)

# Using summary() function
summary(all_trips_v2$ride_length)

# Now we will calculate the statistic based on the label of the data
  # There are two labels in this data: "member" and "causal"
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN=mean)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN=median)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN=max)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN=min)

# The average ride time by day of each label
all_trips_v2$day_of_week <- ordered(all_trips_v2$day_of_week, levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$day_of_week, FUN = mean)

# Analyze ridership data by type and weekday
all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>%  #creates weekday field using wday()
  group_by(member_casual, weekday) %>%  #groups by usertype and weekday
  summarise(number_of_rides = n()							#calculates the number of rides and average duration 
            ,average_duration = mean(ride_length)) %>% 		# calculates the average duration
  arrange(member_casual, weekday)		

# Let's visualize the number of rides by rider type
all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge")

# Let's create a visualization for average duration
all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = average_duration, fill = member_casual)) +
  geom_col(position = "dodge")

#=================================================
# STEP 5: EXPORT SUMMARY FILE FOR FURTHER ANALYSIS
#=================================================
# Create a csv file that we will visualize in Excel, Tableau, or my presentation software
counts <- aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$day_of_week, FUN = mean)
write.csv(counts, file = '/Users/USER/Desktop/MitCourses/GoogleDataAnalytics/GDA_CaseStudy/Document/avg_ride_length.csv')

