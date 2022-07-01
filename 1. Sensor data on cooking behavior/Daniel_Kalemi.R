rm(list = ls()) #clear the environment

# set the working directory
directory = '/Users/danielkalemi/Downloads/week6-7'
setwd(directory)

library(tidyverse)
library(ggplot2)
library(dplyr)

# PROBLEM 1: read in the class_data.csv into a dataframe
class_data = read.csv('class_data.csv')

# PROBLEM 2: convert the character string time into a POSIXct
class_data = class_data %>% mutate(time = as.POSIXct(time)) %>% mutate(temperature = as.numeric(temperature))

# PROBLEM 3: create a new column call temp_diff & pad the first sample with NA
class_data = class_data %>%
  group_by(person) %>%
  mutate(temp_diff = c(NA, diff(temperature)))

# PROBLEM 3.5: make a new column called cooking that is true if the
# temperature difference is > 0.2°C/minute or if temperature > 35°C
class_data = class_data %>% ungroup()
class_data = class_data %>%
  mutate(cooking = temp_diff > 0.2 | temperature > 35)

# PROBLEM 4: filter only to my data using ==
ggplot(class_data %>% filter(person == 'daniel_kalemi')) +
  geom_point(aes(x = time, y = temperature, color = cooking, shape = cooking))
# zoom in a bit
ggplot(class_data %>% filter(person == 'daniel_kalemi') %>% filter(time > '2022-02-13' & time < '2022-02-14')) +
  geom_point(aes(x = time, y = temperature, color = cooking, shape = cooking))

# DISCUSSION: In my case, I only cooked 3 times during this 2 week period, since
# I'm on a student meal plan. The logger recoded 4 times, because in 1 instance
# I placed it closer to my room heater and on the graph is shown as cooking data
# (false positive). During the cooking interval the algorithm fails to clearly distinguish
# cooking and non-cooking states in some occasions.

# PROBLEM 5: summarize the cooking data 
my_cooking_time = class_data %>% filter(person == 'daniel_kalemi') %>%
  summarize(
    cooking_minutes = sum(cooking, na.rm = TRUE),
  )
print(my_cooking_time)

# PROBLEM 6: Make a plot of 5 random students’ temperature data & facet by person

# METHOD 1: random student using 'sample(5)'
ggplot(class_data %>% filter(person %in% unlist(person %>% sample(5)))) +
  geom_point(aes(x=time, y=temperature, color=cooking)) +
  facet_grid(person ~ .)

# METHOD 2: Pick 5 students from the dataset
#classmate_data = class_data %>% filter(person == 'ingrid_xhafa' | person =='pierre_lucas'
#                                       | person == 'victoria_sogomo' | person == 'calvin_chen'
#                                       | person == 'danny_wilson')
#ggplot(classmate_data) +
#  geom_point(aes(x = time, y = temperature, color = cooking, shape = cooking)) +
#  facet_grid(person ~ .)

# Discussion: In this random sample I got data from Curtis, Greg, Malika, Patricia
# Zhongyun. Only Curtis and Patricia kept recording for the entire 2 weeks, 
# but had no "real" cooking times. Malika only recorded the 1st week and 
# captured 5 cooking sessions. Greg and Zhongyun started recording from week 2.

# PROBLEM 7: Create a stats_by_person data frame
stats_by_person = class_data %>%
  group_by(person) %>%
  summarize(
    max_temp = max(temperature),
    min_temp =min(temperature),
    mean_temp = mean(temperature),
    max_rh = max(humidity),
    min_rh = min(humidity),
    mean_rh = mean(humidity),
    sample_count = n()
  )
print(stats_by_person)

# PROBLEM 8: filters the stats data frame for the person with the highest temperature.
max_temp_person = stats_by_person %>% filter(max_temp == max(max_temp))
print(max_temp_person)

# PROBLEM 9: filters the stats data frame for the person with the highest sample count.
max_sample_count_person = stats_by_person %>% filter(sample_count == max(sample_count))
print(max_sample_count_person)

# PROBLEM 10: filters the stats data frame for the person with the lowest mean temp.
minimum_mean_temp_person = stats_by_person %>% filter(mean_temp == min(mean_temp))
print(minimum_mean_temp_person)

# PROBLEM 11: Find the person with the highest temp swing
max_temp_swing_person = stats_by_person %>% filter(max_temp-min_temp == max(max_temp-min_temp))
print(max_temp_swing_person)

# PROBLEM 12: Create a single-column data frame for the distinct trip_description
# ungroup the data 
class_data %>% ungroup()
unique_descriptions = class_data %>% distinct(trip_description)
print(unique_descriptions)

# PROBLEM 13: By person, who used which start_mode? 
# Make a two-column data frame called start_mode_by_person with columns person 
# and start_mode that shows the distinct start mode used by each person in the class. 
start_mode_by_person = class_data %>%
  group_by(person) %>%
  summarize(
    start_mode = unique(start_mode)
  )
print(start_mode_by_person)

# PROBLEM 14: Which start mode was the most popular class-wide? 
# Make a data frame called start_mode_counts that counts the number of times 
# each start mode was used.  
start_mode_counts = start_mode_by_person %>% 
  group_by(start_mode) %>% 
  tally()
#METHOD 2
#start_mode_counts = table(start_mode_by_person$start_mode)
#start_mode_counts = as.data.frame(start_mode_counts)
print(start_mode_counts)

# PROBLEM 15: total minutes the whole class cooked. There should be just one number.
# Does this number seem high, low, or about right? Why?
cooking_minutes_for_class = class_data %>% filter(cooking == TRUE) %>% 
  select(cooking) %>% sum(na.rm = TRUE)
print(cooking_minutes_for_class)

# During 2 weeks, the total for 38 individuals (37 + Danny), was 14098 minutes.
# This is around 26.5 minutes a days/student, which might seem reasonable, since
# students might be on meal plans, eat outside, order delivery etc.

# PROBLEM 16: How many minutes did each person spend cooking?
# Who cooked the most? Who cooked the least? Do these numbers
# seem plausible? What does graphing the data of the most-cooking and least-cooking people show you?
cooking_minutes_by_person = class_data %>% 
  group_by(person) %>%
  summarize(
    cooking_minutes = sum(cooking, na.rm = TRUE) 
  )
print(cooking_minutes_by_person)

# - The person who cooked the most in Barbara with 2304 min and the person who cooked
ggplot(class_data %>% filter(person =='barbara_mensah')) + 
  geom_point(aes(x=time, y=temperature, color=cooking))

# During the first week, Barbara isn't cooking till the last 4 days. That pattern
# repeats the following week. So we can observe that she cooks 4 outs of 7 days/week.
# However she still achieved the top cooking time, which could be explained on the longer
# cooking duration for each meal, which seems to be very long.

# - The person who cooked the least is Abigail Chin with 0 min. Since she doesn't
# have any recorded cooking time we would get an error on her graph because we don't
# have data points where cooking is TRUE. Instead, the next student in line with
# the least cooking time is Carly with just 1 min.

#ggplot(class_data %>% filter(person =='abigail_chin')) + 
#  geom_point(aes(x=time, y=temperature, color=cooking))

ggplot(class_data %>% filter(person =='carly_hancherick')) + 
  geom_point(aes(x=time, y=temperature, color=cooking))

#From Carly's data, her temperature fluctuates from 20.5 to 25.5 degrees, which
# aren't detected as cooking time by our algorithm, since the temp_diff per minute
# is lower than 0.2 and or the temperature never goes beyond the 35 degrees mark.
# Perhaps she left her logger in the sun since the fluctuations are almost identical
# on a daily basis.

# PROBLEM 17: Find the top-5 cooks in terms of their total cooking time. 
lotta_cooking_people = cooking_minutes_by_person %>% 
  slice_max (cooking_minutes, n = 5)
print(lotta_cooking_people)

# PROBLEM 18: Plot the temperature vs. time data for the 5 lotta_cooking_people.
ggplot(class_data %>% filter(person %in% unlist(lotta_cooking_people["person"]) )) +
  geom_point(aes(x=time, y=temperature, color=cooking)) +
  facet_grid(person ~ .)

# I learnt that Victor and Victoria placed their logger on the same kitchen, very
# close to each other, but still got different results 423 cooking minutes.
# However, on the graph the cooking pattern is almost identical (flaws in sensitivity)
# Subham arounf Feb 15 got a record high temperature...perhaps changed the location of
# the logger closer to the stove. Brian seems to be cooking more often than the other 4.
# Barbara is cooking fewer times than Brian, Victor and Victoria, but cooking time seems 
# to be longer per meal.


# PROBLEM 19: Find the lowest-5 cooks in terms of their total cooking time.
notta_lotta_cooking_people = cooking_minutes_by_person %>% 
  slice_min (cooking_minutes, n = 5)
print(notta_lotta_cooking_people)

# PROBLEM 20: Plot the temperature vs. time data for the 5 notta_lotta_cooking_people,
# color it by cooking, and row facet it by person. Describe the data, 
# the performance of the cooking algorithm, and any interesting patterns you see.
ggplot(class_data %>% filter(person %in% unlist(notta_lotta_cooking_people["person"]) )) +
  geom_point(aes(x=time, y=temperature, color=cooking)) +
  facet_grid(person ~ .)

# - Abigail's logger has only 1 data point recorded in the middle of this trial, 
# Feb 14-15, which makes me assume that she probably tried to set it up but perhaps,
# mistakenly stopped the recorder after the first minute and forgot about it until
# she collected the data.
# - Carly started recording a week late (like Abigail and Riley) and has only 1 week
# worth of data. Ingrid and Yixin recorded for almost 2 weeks, but I believe that
# none of those 5 students actually cooked and the algorithm should've allocated
# 0 mins of cooking time to all of them. The fluctuations in their data seems like
# room temperature or left in the sun. Yixin's pattern is a bit weird because,
# the first week has higher temp than the 2nd...perhaps changed pacement.

# EVALUATION: I have already included print statements on each question block,
# but you can also run these for evaluations.
print(my_cooking_time)
print(stats_by_person)
print(max_temp_person)
print(max_sample_count_person)
print(minimum_mean_temp_person)
print(max_temp_swing_person)
print(unique_descriptions)
print(start_mode_by_person)
print(start_mode_counts)
print(cooking_minutes_for_class)
print(cooking_minutes_by_person)
print(lotta_cooking_people)
print(notta_lotta_cooking_people)
