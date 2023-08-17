# Installing some packages I might need for this analysis

install.packages("tidyverse")
install.packages("lubridate")
install.packages("ggplot2")
install.packages("dplyr")
install.packages("janitor")
install.packages("skimr")
library(tidyverse)
library(lubridate)
library(ggplot2)
library(dplyr)
library(janitor)
library(skimr)

#Packages for plot themes

install.packages("Lahman")
library(Lahman)
install.packages("ggthemes")
library(ggthemes)

#Uploading the relevant datasets

daily_activity <- read_csv("dailyActivity_merged - dailyActivity_merged.csv")
daily_sleep <- read_csv("sleepDay_merged - sleepDay_merged.csv")


#Exploring daily activity dataset

head(daily_activity)
skim_without_charts(daily_activity)
glimpse(daily_activity)
str(daily_activity)

#Exploring sleep datatset

head(daily_sleep)
skim_without_charts(daily_sleep)
glimpse(daily_sleep)
str(daily_sleep)

#Cleaning the Sleep and Activity Datasets

daily_activity <- daily_activity %>% 
  mutate(Date = as.Date(Date, format = "%m/%d/%y"))

head(daily_activity)

daily_sleep <- daily_sleep %>% 
  mutate(Date = as.Date(Date, format = "%m/%d/%y"))
head(daily_sleep)


#Merging Sleep and Activity Datasets

joined_activity <- merge(daily_activity, daily_sleep, by = c('Id', 'Date'), all = TRUE)

head(joined_activity)
str(joined_activity)
summary(joined_activity)



#Visualizing data

# First, we'll examine Calories vs Total Steps
 
ggplot(data=joined_activity, aes(x=TotalSteps, y=Calories)) +
  geom_point() +
  geom_smooth() +
  labs(title= "Total Steps vs Calories", y = 'Calories',
       x = 'Total Steps',
       caption ='Data Source: FitBit Fitness Tracker Data')+
  theme_economist()

#Calculating Pearson coefficient

cor.test(joined_activity$TotalSteps, joined_activity$Calories, method = 'pearson', conf.level = 0.95)

# Pearson's product-moment correlation

# data:  joined_activity$TotalSteps and joined_activity$Calories
# t = 22.472, df = 938, p-value < 2.2e-16
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
#   0.5483688 0.6316184
# sample estimates:
 # cor 
# 0.5915681 

#As we would expect the Pearson correlation test indicates a statistically significant positive correlation between TotalSteps and Calories.
#The correlation coefficient is approximately 0.59, suggesting a moderate to strong positive linear relationship between the two variables. 
#This means that as the number of steps increases, the number of calories burned tends to increase as well. 
#The very low p-value suggests that the observed correlation is unlikely to have occurred by chance. 

#Let's look at the relationship between activity and sleep

#do more active users sleep more?

ggplot(data=joined_activity, aes(x=TotalSteps, y=TotalMinutesAsleep)) +
  geom_point() +
  geom_smooth() +
  labs(title= "Total Steps vs Minutes Asleep", y = 'Minutes Asleep',
       x = 'Total Steps',
       caption ='Data Source: FitBit Fitness Tracker Data')+
  theme_economist()

#Interestngly, it doesn't seem like more active users are sleeping more. Let's see if there is a correlation between the variables

cor.test(joined_activity$TotalSteps, joined_activity$TotalMinutesAsleep, method = 'pearson', conf.level = 0.95)

#data:  joined_activity$TotalSteps and joined_activity$TotalMinutesAsleep
#t = -3.9164, df = 408, p-value = 0.0001054
#alternative hypothesis: true correlation is not equal to 0
#95 percent confidence interval:
#  -0.28199288 -0.09525253
#sample estimates:
#  cor 
#-0.1903439 

#The data actually indicates a negative relationship between steps and sleep. 


#I'm going to classify users into how active they are based on number of steps. 
#This will give me a deeper understanding of activity levels and how it might relate to sleep. 
#Based on the correlation test, I would expect the more sedentary users to sleep better


joined_activity <- joined_activity %>%
  mutate(user_type = case_when(
    TotalSteps < 5000 ~ "sedentary",
    TotalSteps >= 5000 & TotalSteps < 7499 ~ "lightly active", 
    TotalSteps >= 7500 & TotalSteps < 9999 ~ "fairly active", 
    TotalSteps >= 10000 ~ "very active"
  ))


skim_without_charts(joined_activity)

#activity minutes vs sleep

ggplot(joined_activity, aes(x = user_type, y = TotalMinutesAsleep)) +
  geom_bar(stat = "summary", fun = "mean",  fill = "steelblue") +
  labs(title = "Minutes Asleep by User Type", x = "User Type", y = "Mean Minutes Asleep", caption ='Data Source: FitBit Fitness Tracker Data') +
  theme_economist_white()

ggplot(joined_activity, aes(x = Calories, y = TotalMinutesAsleep)) +
  geom_point() +
  geom_smooth()+
  facet_wrap(~user_type) +
  labs(title = "Calories vs Minutes Asleep by Activity Level", caption ='Data Source: FitBit Fitness Tracker Data') +
  theme_economist_white()

#Lets see how often each user is actually using their smart device. 

# Create a histogram of activity log counts per user

joined_activity %>%
  group_by(Id) %>%
  summarise(activity_count = n()) %>%
  ggplot(aes(x = activity_count)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black") +
  labs(title = "Activity Log Frequency by User",
       x = "Number of Activity Logs",
       y = "Number of Users")

  #Most people used the watch regularly and the watch tracked some sort of activity almost every day throughout 
# the month. 
#Let's see if people are intentially logging things versus just wearing the watch

# Create a histogram for LoggedActivitiesDistance by user

logged_histogram <- joined_activity %>%
  ggplot(aes(x = LoggedActivitiesDistance, fill = "Logged Activities Distance")) +
  geom_histogram(binwidth = 1, color = "black", position = "identity", alpha = 0.6) +
  labs(title = "Histogram of Logged Activities Distance by User",
       x = "Logged Activities Distance",
       y = "Frequency") +
  scale_fill_manual(values = c("Logged Activities Distance" = "blue"))

#Create histogram for tracked activity

tracked_histogram <- joined_activity %>%
  ggplot(aes(x = TrackerDistance, fill = "Tracker Distance")) +
  geom_histogram(binwidth = 1, color = "black", position = "identity", alpha = 0.6) +
  labs(title = "Histogram of Tracker Distance by User",
       x = "Tracker Distance",
       y = "Frequency") +
  scale_fill_manual(values = c("Tracker Distance" = "red"))

# Combine the two histograms using grid.arrange (gridExtra package)

install.packages("gridExtra")
library(gridExtra)
grid.arrange(logged_histogram, tracked_histogram, ncol = 2)

#Most people are not actively recording things in this dataset 
#they are just wearing the tracker and letting it do its thing

#Maybe people are more prone to record sleep than a workout?

# Create a histogram of sleep record counts summarized by user

joined_activity %>%
  group_by(Id) %>%
  summarise(total_sleep_records = sum(TotalSleepRecords)) %>%
  ggplot(aes(x = total_sleep_records)) +
  geom_histogram(binwidth = 1, fill = "purple", color = "black", alpha = 0.6) +
  labs(title = "Histogram of Total Sleep Records by User",
       x = "Total Sleep Records",
       y = "Frequency") +
  theme_minimal()

# Create a bar plot of user type counts by user

joined_activity %>%
  group_by(user_type) %>%
  summarise(user_type_count = n()) %>%
  ggplot(aes(x = user_type, y = user_type_count, fill = user_type)) +
  geom_bar(stat = "identity", alpha = 0.7) +
  labs(title = "Bar Plot of User Type Counts",
       x = "User Type",
       y = "User Type Count",
       fill = "User Type") +
  theme_minimal() +
  theme(legend.position = "top", legend.title = element_blank())