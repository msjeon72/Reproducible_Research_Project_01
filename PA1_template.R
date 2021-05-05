#### Load the input data
setwd("~/Desktop/Coursera/Reproducible Research/Project01")
activity <- read.csv("activity.csv")

#### Calculate the total number of steps taken per day
steps.all <- aggregate(steps~date, activity, sum, na.rm=TRUE)
head(steps.all)

#### Make a histogram of the total number of steps taken each day
hist(steps.all$steps, breaks = 10, xlab='Total number of steps taken each day')

#### Calculate and report the mean and median of the total number of steps taken per day
mean(steps.all$steps, na.rm = TRUE)
median(steps.all$steps, na.rm = TRUE)

#### Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
steps.interval<-aggregate(steps~interval, data=activity, mean,na.rm=TRUE)
plot(steps~interval,data=steps.interval,type="l")

#### Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps? 
MaxInt5min <- steps.interval[which.max(steps.interval$steps),]$interval

#### Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)
sum(is.na(activity$steps))

#### Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
activity$imputedSteps <- ifelse(!is.na(activity$steps), activity$steps, round(steps.interval$steps[match(activity$interval, steps.interval$interval)],0))

#### Create a new dataset that is equal to the original dataset but with the missing data filled in.
activeImputed <- activity[-c(1)]
activeImputed <- activeImputed[, c(3,1,2)]
names(activeImputed)[1] <- paste("steps")

#### Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. 
setps.date.activeImputed <-aggregate(steps~date, activeImputed ,sum)
hist(setps.date.activeImputed$steps, breaks = 10, xlab='Steps' )
mean(setps.date.activeImputed$steps)
median(setps.date.activeImputed$steps)

#### Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.
activeImputed$date <- as.Date(activeImputed$date, format="%Y-%m-%d")
activeImputed$weekday <- weekdays(activeImputed$date)
activeImputed$DayType <- ifelse(activeImputed$weekday=='Saturday' | activeImputed$weekday=='Sunday', 'weekend','weekday')

#### Make a panel plot containing a time series plot (i.e. \color{red}{\verb|type = "l"|}type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).
library(ggplot2)
steps.int.Daytype.activeImputed <- aggregate(steps~interval+DayType, activeImputed ,mean)
ggplot(steps.int.Daytype.activeImputed, aes(interval, steps)) + 
    geom_line() + 
    facet_grid(DayType ~ .) +
    xlab("5-minute interval") + 
    ylab("avarage number of steps")
