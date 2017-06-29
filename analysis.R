file = "RepData_PeerAssessment1/activity.csv"
if (!file.exists(file)){
    unzip("RepData_PeerAssessment1/activity.zip")
}

library(ggplot2)
library(lattice)
library(Hmisc)
library(dplyr)

data <- read.csv("activity.csv",header=TRUE,na.strings = "NA")

#totalstepsperdate <- tapply(data$steps,data$date,FUN=sum,na.rm=TRUE)
#qplot(totalstepsperdate,xlab="Total steps per day")

totalstepsperdate <- aggregate(steps~date,data=data,sum)
hist(totalstepsperdate$steps,breaks = 15,main="Total steps on each day",xlab="number of steps",col="red")

meansteps = mean(totalstepsperdate$steps)
mediansteps = median(totalstepsperdate$steps)

totstepsbyinterval <- aggregate(steps~interval,data=data,mean)

with(totstepsbyinterval,plot(interval,steps,type='l',main="Average daily activity pattern"))

totstepsbyinterval[which.max(totstepsbyinterval$steps),]

sum(!is.na(data))

#replacing missing values

meandata <- data
meandata <- mutate(meandata, steps=impute(steps))
#steps <- meandata$steps
#steps[is.na(steps)] <- mean(steps,na.rm = TRUE)
#meandata$steps <- steps

stepsperdata <- aggregate(steps~date,data=meandata,sum)
hist(stepsperdata$steps,main="steps per day after mean imputation",xlab="Number of steps",col="green",breaks=15)

#calculating mean and median after imputation.
meansteps = mean(stepsperdata$steps)
mediansteps = median(stepsperdata$steps)

daycreater <- function(date) {
    day <- weekdays(date)
    if (day %in% c("Monday","Tuesday","Wednesday","Thursday","Friday")) {
        return ("weekday")
    } else if (day %in% c("Saturday","Sunday")){
        return ("weekend")
    } else {
        print (day)
        return ("Invalid date")
        
    }
}

meandata$date <- as.Date(meandata$date)
meandata$day <- sapply(meandata$date,FUN = daycreater)

stepsperinterval <- aggregate(steps~interval + day,meandata,mean)

averages <- aggregate(steps ~ interval + day, data=meandata, mean)
ggplot(averages, aes(interval, steps)) + geom_line() + facet_grid(day ~ .) +
    xlab("5-minute interval") + ylab("Number of steps")
