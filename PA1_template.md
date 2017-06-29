# Reproducible Research: Peer Assessment 1


```r
library(ggplot2)
```

```
## Warning: package 'ggplot2' was built under R version 3.2.5
```

```r
library(lattice)
library(Hmisc)
```

```
## Warning: package 'Hmisc' was built under R version 3.2.5
```

```
## Loading required package: survival
```

```
## Warning: package 'survival' was built under R version 3.2.5
```

```
## Loading required package: Formula
```

```
## Warning: package 'Formula' was built under R version 3.2.5
```

```
## 
## Attaching package: 'Hmisc'
```

```
## The following objects are masked from 'package:base':
## 
##     format.pval, round.POSIXt, trunc.POSIXt, units
```

```r
library(dplyr)
```

```
## Warning: package 'dplyr' was built under R version 3.2.5
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:Hmisc':
## 
##     combine, src, summarize
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

## Loading and preprocessing the data

```r
file = "RepData_PeerAssessment1/activity.csv"
if (!file.exists(file)){
    unzip("RepData_PeerAssessment1/activity.zip")
}
data <- read.csv("activity.csv",header=TRUE,na.strings = "NA")

totalstepsperdate <- aggregate(steps~date,data=data,sum)
hist(totalstepsperdate$steps,breaks = 5,main="Total steps on each day",xlab="number of steps",col="red")
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)<!-- -->


## What is mean total number of steps taken per day?

```r
meansteps = mean(totalstepsperdate$steps)
mediansteps = median(totalstepsperdate$steps)
print (meansteps)
```

```
## [1] 10766.19
```

```r
print (mediansteps)    
```

```
## [1] 10765
```


## What is the average daily activity pattern?

```r
totstepsbyinterval <- aggregate(steps~interval,data=data,mean)
with(totstepsbyinterval,plot(interval,steps,type='l',main="Average daily activity pattern"))
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

```r
totstepsbyinterval[which.max(totstepsbyinterval$steps),]
```

```
##     interval    steps
## 104      835 206.1698
```


## Imputing missing values

```r
meandata <- data
meandata <- mutate(meandata, steps=impute(steps))

stepsperdata <- aggregate(steps~date,data=meandata,sum)
hist(stepsperdata$steps,main="steps per day after mean imputation",xlab="Number of steps",col="green",breaks=15)
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

## Mean and median after imputation

```r
meansteps = mean(stepsperdata$steps)
mediansteps = median(stepsperdata$steps)
```

## Are there differences in activity patterns between weekdays and weekends?

```r
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
```

![](PA1_template_files/figure-html/unnamed-chunk-7-1.png)<!-- -->

