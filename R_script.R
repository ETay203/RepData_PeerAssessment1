###Setting Global Options
```{r setoptions, echo = TRUE}
library(knitr)
opts_chunk$set(echo=TRUE)
```

###libraries
#library(dplyr)
#library(tidyr)
#library(data.table)

activity<-unzip("activity.zip")
data<-read.csv("activity.csv", stringsAsFactors = FALSE)
data$date<-as.Date(data$date, "%Y-%m-%d")
data$steps<-as.numeric(data$steps)


## What is mean total number of steps taken per day?
#For this part of the assignment, you can ignore the missing values in the dataset.
#Calculate the total number of steps taken per day

steps_per_day<-tapply(data$steps, data$date, sum, na.rm = TRUE)
steps_per_day<-as.data.frame.table(steps_per_day, stringsAsFactors = FALSE)
names(steps_per_day)[1:2]<-c("Date", "TotSteps")
steps_per_day$Date<-as.Date(steps_per_day$Date, "%Y-%m-%d")


##Make a histogram of the total number of steps taken each day
library(ggplot2)
g<-ggplot(steps_per_day, aes(x=TotSteps))
p<-g+geom_histogram(binwidth = 500) +xlab("Total number of steps taken each day")
print(p)

##Calculate and report the mean and median of the total number of steps taken per day

mean(steps_per_day$TotSteps)
median(steps_per_day$TotSteps)


## What is the average daily activity pattern?

#Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) 
#and the average number of steps taken, averaged across all days (y-axis)

interval_steps<-tapply(data$steps, data$interval, mean, na.rm = TRUE)
interval_steps<-as.data.frame.table(interval_steps, stringsAsFactors = FALSE)
names(interval_steps)[1:2]<-c("Interval", "AvSteps")
interval_steps$Interval<-as.numeric(interval_steps$Interval)
g<-ggplot(interval_steps, aes(x=Interval, y=AvSteps))
p<-g+geom_line() + xlab("5-minute interval") + ylab("Average number of steps taken (across all days)")
print(p)

#Which 5-minute interval, on average across all the days in the dataset, 
#contains the maximum number of steps?
most_steps<-subset(interval_steps, interval_steps$AvSteps==max(interval_steps$AvSteps))
most_steps



## Imputing missing values
#Note that there are a number of days/intervals where there are missing values (coded as NA). 
#The presence of missing days may introduce bias into some calculations or summaries of the data.

#1. Calculate and report the total number of missing values in the dataset 
#(i.e. the total number of rows with NAs)
Tot_NA<-sum(is.na(data$steps))
Tot_NA

#2.Devise a strategy for filling in all of the missing values in the dataset. 
#The strategy does not need to be sophisticated. 
#For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

#Let's use the mean for that day.
#table of means for each day

day_means<-tapply(data$steps, data$date, mean, na.rm = TRUE)
day_means<-replace(day_means,is.nan(day_means),0) 
day_means<-as.data.frame.table(day_means, stringsAsFactors = FALSE)
names(day_means)[1:2]<-c("Date", "MeanSteps")
day_means$Date<-as.Date(day_means$Date, "%Y-%m-%d")

#3.Create a new dataset that is equal to the original dataset but with the missing data filled in.

library(data.table)
day_means<-as.data.table(day_means, key= "Date")
data2<-data.table(data, key = "date")

#merge data.tables based on keys
data_fill<-data2[day_means]
#replace na values with mean values for that day
data_fill<-data_fill[is.na(steps),steps:=MeanSteps]
#just keep the original columns needed
data_fill<-data_fill[,list(steps, date, interval)]


#4.Make a histogram of the total number of steps taken each day and 
#Calculate and report the mean and median total number of steps taken per day. 
steps_per_day2<-tapply(data_fill$steps, data_fill$date, sum, na.rm = TRUE)
steps_per_day2<-as.data.frame.table(steps_per_day2, stringsAsFactors = FALSE)
names(steps_per_day2)[1:2]<-c("Date", "TotSteps")
steps_per_day2$Date<-as.Date(steps_per_day2$Date, "%Y-%m-%d")


##Make a histogram of the total number of steps taken each day
library(ggplot2)
g<-ggplot(steps_per_day2, aes(x=TotSteps))
p<-g+geom_histogram() +xlab("Total number of steps taken each day")
print(p)

mean(steps_per_day2$TotSteps)
median(steps_per_day2$TotSteps)

#Do these values differ from the estimates from the first part of the assignment?
#What is the impact of imputing missing data on the estimates of the total daily number of steps?
#no difference, in this case there is no impact.  
#That is because we replaced NAs with the mean for that day.  Certain days had all their values missing,
#therefore we replace these with a mean of 0, similar to just ignoring the NAs.


## Are there differences in activity patterns between weekdays and weekends?
#For this part the weekdays() function may be of some help here. 
#Use the dataset with the filled-in missing values for this part.
#Create a new factor variable in the dataset with two levels - "weekday" and "weekend" 
#indicating whether a given date is a weekday or weekend day.
data_fill$days<-weekdays(data_fill$date, abbreviate = TRUE)
days2<-c("Mon","Tue", "Wed", "Thu", "Fri")
data_fill$levels<-factor(data_fill$days %in% days2, levels = c("TRUE", "FALSE"), labels = c("weekday", "weekend"))
data_fill<-data_fill[,list(steps,date,interval,levels)]


#Make a panel plot containing a time series plot (i.e. type = "l")
#of the 5-minute interval (x-axis) and 
#the average number of steps taken, averaged across all weekday days or weekend days (y-axis). 
#See the README file in the GitHub repository to see an example of what this plot should look 
#like using simulated data.

data_weekday<-data_fill[data_fill$levels=="weekday"]
interval_steps_weekday<-tapply(data_weekday$steps, data_weekday$interval, mean, na.rm = TRUE)
interval_steps_weekday<-as.data.frame.table(interval_steps_weekday, stringsAsFactors = FALSE)
names(interval_steps_weekday)[1:2]<-c("Interval", "AvStep")
interval_steps_weekday$Interval<-as.numeric(interval_steps_weekday$Interval)
interval_steps_weekday$day<-"weekday"

data_weekend<-data_fill[data_fill$levels=="weekend"]
interval_steps_weekend<-tapply(data_weekend$steps, data_weekend$interval, mean, na.rm = TRUE)
interval_steps_weekend<-as.data.frame.table(interval_steps_weekend, stringsAsFactors = FALSE)
names(interval_steps_weekend)[1:2]<-c("Interval", "AvStep")
interval_steps_weekend$Interval<-as.numeric(interval_steps_weekend$Interval)
interval_steps_weekend$day<-"weekend"

avsteps<-rbind(interval_steps_weekday, interval_steps_weekend)

avstep_plot<-ggplot(avsteps, aes(x=Interval, y=AvStep))
p<-avstep_plot+geom_line() + xlab("5-minute interval") + ylab("Average number of steps taken (across all days)")
g<-p+facet_grid(day ~ .)
print(g)


