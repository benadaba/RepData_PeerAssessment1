wd <- getwd()
current_proj_dir  <- "/Reproducible Research/RepData_PeerAssessment1"
wd1 <- paste0(wd, current_proj_dir, "/activity/activity.csv")

activity <- read.csv(wd1)

head(activity)
str(activity)
names(activity)

library(lubridate)
activity$date <- ymd(activity$date)
str(activity)
head(activity)

#1. Calculate the total number of steps taken per day
ttl_walks_by_day <- aggregate(activity["steps"], by=list(Date=activity$date), FUN=sum)

#2. Make a histogram of the total number of steps taken each day
hist(ttl_walks_by_day$steps
        , breaks = 20
        , col = "pink"
        , main = "Total Steps By Day"
        ,xlab = "Steps"
        ,labels = TRUE)


#3a. Calculate and report the mean of the total number of steps taken per day
daily_mean <- aggregate(activity["steps"], by=list(Date=activity$date), FUN=mean)
daily_mean

#3b Calculate and report the median of the total number of steps taken per day
daily_median <- aggregate(activity["steps"], by=list(Date=activity$date), FUN=median)
daily_median


#1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) 
#and the average number of steps taken, averaged across all days (y-axis)
library(dplyr)
activity_new <- activity %>%
            group_by(interval) %>%
            summarise(AvgSteps = mean(steps, na.rm = TRUE))

plot(activity_new, 
    type ="l", 
    xlab= "5-minute interval", 
    ylab="averaged steps across all days",
    main ="Average Daily Steps Over Each 5 Minute Interval",
    col = "orange")

library(ggplot2)
ggplot(activity_new, aes(interval, AvgSteps,col = AvgSteps)) + geom_line() +
            xlab("5-minute interval") + ylab("averaged steps across all days")
    


#check if there are all NAs
sum(!is.na(activity_new$AvgSteps))


#2. which interval on average contains the maximum number of steps
activity_new$interval[which.max(activity_new$AvgSteps)]


# Imputing missing values
# 1 Calculate and report the total number of missing values in the dataset 
#(i.e. the total number of rows with NAs)

# #impute MISSING VALUES with the interval average steps


activity_imputed <- activity %>%
    group_by(interval) %>%
    mutate(AvgSteps = ifelse( is.na(steps), mean(steps, na.rm = TRUE), steps))
#View(activity_imputed)

#check if there are any NAs in the average
sum(is.na(activity_imputed$AvgSteps))


#4a. Calculate the total number of steps taken per day
# Make a histogram of the total number of steps taken each day 
activity_imputed_ttl_daily_steps <- aggregate(activity_imputed["AvgSteps"], by=list(Date=activity_imputed$date), FUN=sum)

#4bhistory gram
hist(activity_imputed_ttl_daily_steps$AvgSteps, 
        breaks = 20, 
        col="green", 
        main = "Total Steps By Day After inputing Missen values",
        xlab = "Steps",
        labels = T)

# 4c and Calculate and report the mean and median total number of steps taken per day. 
#4ci mean
daily_mean_after_imputation <- aggregate(activity_imputed["AvgSteps"], by=list(Date=activity_imputed$date), FUN=mean)
daily_mean_after_imputation

#4cii median
daily_median_imputation <- aggregate(activity_imputed["AvgSteps"], by=list(Date=activity_imputed$date), FUN=median)
daily_median_imputation



# Do these values differ from the estimates from the first part of the assignment? 
# ANS: yes, they do, the missen values are well filled out which impacts the distribution  of the steps

# What is the impact of imputing missing data on the estimates of the total daily 
# number of steps?
# ANS: it makes it more clear that on average more steps were taken which counted around
#between 10000 and 15000


# 5 Are there differences in activity patterns between weekdays and weekends?

activity_imputed_wkdays <- activity_imputed %>%
     mutate(wkday =  as.factor(ifelse( weekdays(date) %in% c("Saturday", "Sunday"), "weekend","weekday")))
    #mutate(wkday = as.factor(weekdays(date)))
    
    
str(activity_imputed_wkdays)
table(activity_imputed$wkday)

g <- ggplot(data = activity_imputed_wkdays, aes(x= interval, y=AvgSteps, col = wkday) ) + geom_line()
g + facet_grid(wkday ~ .)


# daily_mean_after_imputation_wkday <- daily_mean_after_imputation %>%
#     mutate(wkday =  as.factor(ifelse( weekdays(Date) %in% c("Saturday", "Sunday"), "weekend","weekday")))
# #mutate(wkday = as.factor(weekdays(date)))
# g2 <- ggplot(data = daily_mean_after_imputation_wkday, aes(x= interval, y=AvgSteps, col = wkday) ) + geom_line()
# g2 + facet_grid(wkday ~ .)