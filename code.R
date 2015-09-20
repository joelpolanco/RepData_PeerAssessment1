library(plyr)
library(dplyr)
library(tidyr)
library(knitr)
library(lattice)


setwd("C:/Users/jgpolanc/Desktop/Coursera/c5p1/RepData_PeerAssessment1")

dat <- read.csv(paste(getwd(),"/data/activity.csv",sep=""))
dat$interval<- as.factor(dat$interval)

##Calculate the total number of steps taken per day
tot_steps_day<-aggregate(dat$steps, by=list(Date=dat$date), FUN=sum,na.rm=TRUE)

##Make a histogram of the total number of steps taken each day
hist(tot_steps_day$x)

##Calculate and report the mean and median of the total number of steps taken per day
mean_steps_day<-mean(tot_steps_day$x,na.rm = TRUE)
median_steps_day<-median(tot_steps_day$x,na.rm = TRUE)

mean_steps_day
median_steps_day

##Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and 
##the average number of steps taken, averaged across all days (y-axis)
avg_steps_interval<-aggregate(dat$steps, by=list(interval=dat$interval), FUN=mean,na.rm=TRUE)
plot(avg_steps_interval$interval, avg_steps_interval$x, type="l")

##Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
max_interval<-apply(avg_steps_interval, 2, max)
max_interval

##Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)
total_na<-sum(is.na(dat$steps))
total_na

##Devise a strategy for filling in all of the missing values in the dataset. 
##The strategy does not need to be sophisticated. 
##For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

total <- merge(dat,avg_steps_interval,by="interval")
total[is.na(total$steps),2]<- total[is.na(total$steps),4]

##Create a new dataset that is equal to the original dataset but with the missing data filled in.
total <- subset(total, select=-x)

##Make a histogram of the total number of steps taken each day and 
##Calculate and report the mean and median total number of steps taken per day. 
##Do these values differ from the estimates from the first part of the assignment? 
##What is the impact of imputing missing data on the estimates of the total daily number of steps?
tot_steps_day1<-aggregate(total$steps, by=list(Date=dat$date), FUN=sum,na.rm=TRUE)
hist(tot_steps_day1$x)
mean_steps_day1<-mean(tot_steps_day1$x,na.rm = TRUE)
median_steps_day1<-median(tot_steps_day1$x,na.rm = TRUE)
mean_steps_day1
median_steps_day1
median_shift <- median_steps_day1-median_steps_day
median_shift

##Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether 
##a given date is a weekday or weekend day.
total$date <- as.Date(total$date)
weekdays <- c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday')
total$daytype <- factor((weekdays(total$date) %in% weekdays),levels=c(FALSE, TRUE), labels=c('weekend', 'weekday')) 

##Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average 
##number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub 
##repository to see an example of what this plot should look like using simulated data.

meansteps<-aggregate(total$steps, by=list(interval=total$interval,daytype=total$daytype), FUN=mean,na.rm=TRUE)
xyplot(x~interval | factor(daytype), data=meansteps, pch=19,
       main="Weekday v Weekend Steppin'", xlab="interval",  ylab="Number of Steps",layout=c(1,2),type=c("l","l"))
       