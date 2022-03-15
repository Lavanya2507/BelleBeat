#https://rstudio-pubs-static.s3.amazonaws.com/834490_81cc375d5cdd451da7c7428872d0d05f.html


library(tidyverse)
library(ggplot2)
library(janitor)
library(lubridate)
library(skimr)
library(dplyr)





##Import data from merged.csv file
setwd('L:/coursera/Fitbit_tracker/')
daily_activity <- read.csv("L:/coursera/Fitbit_tracker/dailyActivity_merged.csv")
head(daily_activity)
nrow(daily_Activity)
colnames(daily_activity)
glimpse(daily_activity)


##Import data from daily_sleep merged.csv file

daily_sleep <-read.csv("L:/coursera/Fitbit_tracker/sleepDay_merged.csv")
head(daily_sleep)
nrow(daily_sleep)
colnames(daily_sleep)
glimpse(daily_sleep)


##To calculate first 10 rows of daily_activity dataframe
head(daily_activity,10)

##To calculate first 10 rows of daily_sleep dataframe
head(daily_sleep, 10)

##To view internal structure of the daily_activity dataframe
str(daily_activity)

##To view internal structure of the daily_sleep dataframe
str(daily_sleep)


##To calculate distinct statistics of daily_activity dataframe
n_distinct(daily_activity$Id)
##To calculate distinct statistics of daily_sleep dataframe
n_distinct(daily_sleep$Id)
sum(is.na(daily_sleep))

##To check missing values
sum(is.na(daily_activity))
summary(daily_activity)
sum(is.na(daily_sleep))

##To remove duplicate files in daily_activity data frame
sum(duplicated(daily_activity))
head(daily_activity)

##To remove duplicate file in daily_sleep data frame
sum(duplicated(daily_sleep))
head(daily_sleep)

##Remove duplicates and NA from daily_activity data frame
daily_sleep <- daily_sleep %>%
  distinct() %>%
  drop_na()
sum(duplicated(daily_sleep))
head(daily_sleep)
sum(is.na(daily_activity))
sum(is.na(daily_sleep))
##To check data cleaning in daily_activity dataframe
clean_names(daily_activity)

sum(duplicated(daily_sleep))
head(daily_sleep)

##To clean daily_sleep dataset
sum(is.na(daily_sleep))
clean_names(daily_sleep)

##To convert of both dataframe date format to  yyyy/mm/dd and rename as Date

daily_activity <- daily_activity %>%
  rename(Date = ActivityDate) %>%
  mutate(Date = as_date(Date, format = "%m/%d/%Y"))


daily_sleep <- daily_sleep %>%
  rename(Date = SleepDay) %>%
  mutate(Date = as_date(Date,format ="%m/%d/%Y %I:%M:%S %p" , tz=Sys.timezone()))

head(daily_activity)
head(daily_sleep)


##To calculate number of observations in each of dataframe
nrow(daily_activity)
nrow(daily_sleep)


##To calculate summary statistics in each dataframe
daily_activity %>%
  select(TotalSteps, TotalDistance, SedentaryMinutes) %>%
  summary()

daily_sleep %>%
  select(TotalSleepRecords, TotalMinutesAsleep, TotalTimeInBed) %>%
  summary()
##For plot exploration between totalsteps and sedentaryminutes
ggplot(data = daily_activity , aes(x = TotalSteps ,y = SedentaryMinutes, color = Calories)) + geom_point()+ 
  labs(x = 'TotalSteps', y = 'Sedentaryinutes', title = 'Relation between TotalSteps and  SedentaryMinutes')
ggsave("chart1.png")


##To plot exploration relation between TotalTimeInBed and TotalMinutesAsleep
ggplot(data=daily_sleep, aes(x=TotalMinutesAsleep, y=TotalTimeInBed))+
  geom_point(aes(color=Date))+ 
  labs(x= 'TotalMinutesAsleep', y = 'TotalTimeInBed', title='Relation between TotalMinutesAsleep and TotalTimeInBed')
ggsave("chart2.png")

##To merge the two data frame by common column name

daily_data <- merge(daily_activity, daily_sleep, by=c ("Id", "Date"))
glimpse(daily_data)
n_distinct(daily_data$Id)

##To execute FULL OUTER JOIN with combined data frames with two column names all = "TRUE) 
combined_data <- merge(daily_activity, daily_sleep, by=c ("Id", "Date"), all = "TRUE")
head(combined_data)

n_distinct(combined_data$Id)

sum(is.na(combined_data))
##To create a new column day of week with format as like Monday=0,Tuesday=1
combined_data <- combined_data %>% 
  mutate_if(is.numeric, ~replace(., is.na(.), 0))
sum(is.na(combined_data))


format(as.Date(combined_data$Date),"%w")


combined_data$DayOfTheWeek = weekdays(as.Date(combined_data$Date,format = "%Y-%m-%d"))
combined_data$DayOfTheWeek = factor(combined_data$Day, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday","Sunday"))
combined_data$DayOfTheWeek = weekdays(as.Date(combined_data$Date,format = "%Y-%m-%d"))
combined_data$DayOfTheWeek = factor(combined_data$Day, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday","Sunday"))
combined_data$TotalHours <- round((combined_data$TotalMinutes/60), digits=2)
str(combined_data)

combined_data$DayOfTheWeek <- format(as.Date(combined_data$Date),"%w")
wday(combined_data$Date, label=TRUE)

combined_data$DayOfTheWeek = strftime(combined_data$Date,'%A')
combined_data$TotalHours <- round((combined_data$TotalMinutes/60), digits=2)

combined_data %>%  
  select(TotalSteps,
         SedentaryMinutes,
         Calories) %>%
  summary()


library(ggpubr)

p1 = ggplot(data = combined_data)+
  geom_point(mapping = aes(x = TotalSteps, y = Calories), color = 'red')+
  labs(title = "TotalSteps vs Calories")

p1

ggsave("TotalSteps Vs Calories.png")

p2 = ggplot(data = combined_data)+
  geom_point(mapping = aes(x = TotalDistance, y = Calories), color = 'brown')+
  labs(title = "TotalDistance vs Calories")

p3 = ggplot(data = combined_data)+
  geom_point(mapping = aes(x = SedentaryMinutes, y = Calories), color = 'purple')+
  labs(title = "TotalMinutes vs Calories")
## arrange

ggarrange(p1, p2, p3, ncol = 3, nrow = 1)

##To check correct weekday
combined_data <- combined_data          
combined_data$DayOfTheWeek <- factor(combined_data$DayOfTheWeek, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday","Sunday"))


ggplot(data=combined_data,aes(x=DayOfTheWeek,fill=DayOfTheWeek)) + geom_bar(stat = "count") +
  theme(plot.title = element_text(hjust = 0.5, lineheight = 0.8, face = "bold")) + 
  labs(x = 'Day of Week',
       y = 'Frequency',
       title = 'Nr. of times users logged in app across the week')    



##To check calories burned for every step

combined_data %>%
  select(TotalSteps, Calories) %>%
  summary()

##To draw ggplot for every step how many calories burned
ggplot(combined_data)+geom_point(mapping=aes(x=TotalSteps, y=Calories, color=TotalSteps))+
  scale_color_gradientn(colours = "rainbow"(6))+
geom_hline(yintercept = 2304, color = "red", size = 1) +
  geom_vline(xintercept = 7654, color= "blue", size = 1)+
geom_text(aes(x=10000, y=2100, label="Mean"), color="black", size=5) +
  theme(plot.title = element_text(hjust = 0.5, lineheight = 0.8, face = "bold")) +
  labs(
    x = 'Steps taken',
    y = 'Calories burned',
    title = 'Calories burned for every step taken')



##To check calories burned per hour logged
combined_data %>%
  select(TotalHours, SedentaryMinutes, 
         Calories) %>%

  
  ggplot(data = combined_data) + geom_point(mapping = aes(x=TotalHours, y=Calories, color=TotalSteps)) + 
  scale_color_gradientn(colours = "rainbow"(3)) + 
  theme(plot.title = element_text(hjust = 0.5, lineheight = 0.8, face = "bold")) + 
  geom_hline(aes(yintercept= 2304, linetype = "Average Hours"), colour= 'red', size=1) +
  geom_vline(aes(xintercept= 991/60, linetype = "Average Sedentary"), colour= 'blue', size=1) +
  geom_vline(aes(xintercept = 20.31, linetype = "Average Steps"), colour='brown', size=1) +
  scale_linetype_manual(name = "Statistics", values = c(2, 2, 2), 
                        guide = guide_legend(override.aes = list(color = c("blue", "red", "brown")))) +
  labs(
    x = 'Hours logged',
    y = 'Calories burned',
    title = 'Calories burned for every hour logged')
  
##To plot an activity in pie chart
  
Data_Ind <- combined_data %>%
  summarise(Sum_VAM = sum(VeryActiveMinutes/1148807*100), 
            Sum_FAM = sum(FairlyActiveMinutes/1148807*100), 
            Sum_LAM = sum(LightlyActiveMinutes/1148807*100), 
            Sum_SEM = sum(SedentaryMinutes/1148807*100),
            Sum_TOTAL=sum(VeryActiveMinutes+FairlyActiveMinutes+LightlyActiveMinutes+SedentaryMinutes)) %>% 
  round(digits = 2)
  
  
  slices <- c(Data_Ind$Sum_VAM, Data_Ind$Sum_FAM, Data_Ind$Sum_LAM, Data_Ind$Sum_SEM)
lbls <- c("Very Active Min", "Fairly Active Min", "Lightly Active Min", "Sedentary Min")
pie(slices,
    labels=paste(lbls, slices, sep=" ", "%"),
    col = rainbow(5),
    main="Pie Chart - % of Activity in Minutes")

                   