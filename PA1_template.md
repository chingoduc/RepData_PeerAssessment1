---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---

```r
library(data.table)
```

```
## data.table 1.9.4  For help type: ?data.table
## *** NB: by=.EACHI is now explicit. See README to restore previous behaviour.
## 
## Attaching package: 'data.table'
## 
## The following object is masked _by_ '.GlobalEnv':
## 
##     .N
```

```r
library(plyr)
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
## 
## The following objects are masked from 'package:plyr':
## 
##     arrange, count, desc, failwith, id, mutate, rename, summarise,
##     summarize
## 
## The following objects are masked from 'package:data.table':
## 
##     between, last
## 
## The following object is masked from 'package:stats':
## 
##     filter
## 
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
library(ggplot2)
```

## Loading and preprocessing the data

```r
activities <- read.csv("activity.csv", header=TRUE, colClasses=c("numeric", "Date", "numeric"))
activities <- data.table(activities)

act <- activities[!is.na(activities$step), ]
```
## What is mean total number of steps taken per day?


```r
dailyTotal <- act[, sum(steps), by=date]
dailyMean <- mean(dailyTotal$V1)
dailyMedian <- quantile(dailyTotal$V1, probs=0.5)
intervalMean <- act[, mean(steps), by=date]
meanofMean <- mean(intervalMean$V1)
plot(act$steps, type="l", ylab = "Nb of Steps",  xlab="Interval",
     main="NA removed")
abline(h=meanofMean, col="lightblue", lty=3)
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png) 

```r
intervalMedian <- act[, quantile(steps, probs=0.5), by=date]
paste("The Daily Means of steps is: ", round(dailyMean,2))
```

```
## [1] "The Daily Means of steps is:  10766.19"
```

```r
paste("The Daily Median of steps is: ",round(dailyMedian,2))
```

```
## [1] "The Daily Median of steps is:  10765"
```
## What is the average daily activity pattern?

```r
maxdate <- intervalMean[which(intervalMean$V1 >= max(intervalMean$V1)),]$date
maxdateOnAct <- act[which(act$date == maxdate),]
highestStep <- max(maxdateOnAct$step)         
maxinterv <- maxdateOnAct[which(maxdateOnAct$step >= highestStep),]$interval
paste("The highest number of steps is: ", highestStep , "happened on: " ,
       maxdate , "on the interval: ", maxinterv)
```

```
## [1] "The highest number of steps is:  760 happened on:  2012-11-23 on the interval:  1520"
## [2] "The highest number of steps is:  760 happened on:  2012-11-23 on the interval:  1530"
```
## Imputing missing values

```r
activ <- activities
intervalMean2 <- activ[, mean(steps, na.rm=TRUE), by=date]

##This is applied for the case in which all the steps of all intervals are NA
intervalMean2[which(is.na(intervalMean2$V1)),]$V1 <- 0

xyz <- left_join(activ, intervalMean2, by="date")
for (i in 1:nrow(activ)) {
  if (is.na(activ[i]$steps)) activ[i]$steps <- xyz[i]$V1
}
```

```r
dailyTotal3 <- activ[, sum(steps), by=date]
intervalMean3 <- activ[, mean(steps), by=date]
intervalMedian3 <- activ[, quantile(steps, probs=0.5), by=date]

dailyMean3 <- mean(dailyTotal3$V1)
dailyMedian3 <- quantile(dailyTotal3$V1, probs=0.5)
paste("Mean of daily steps after simply removing NAs :", round(dailyMean,2))
```

```
## [1] "Mean of daily steps after simply removing NAs : 10766.19"
```

```r
paste("Mean after subtitution of NAs by mean of interval: ", round(dailyMean3,2))
```

```
## [1] "Mean after subtitution of NAs by mean of interval:  9354.23"
```

```r
paste("Median of daily steps after simply removing NAs : ", round(dailyMedian,2))
```

```
## [1] "Median of daily steps after simply removing NAs :  10765"
```

```r
paste("Median after subtitution of NAs by mean of interval: ", round(dailyMedian3,2))
```

```
## [1] "Median after subtitution of NAs by mean of interval:  10395"
```

```r
meanofMean3 <- mean(intervalMean3$V1)

par(mfrow=c(1,2))
plot(act$steps, type="l", ylab = "Nb of Steps",  xlab="Interval",
     main="NA removed")
abline(h=meanofMean, col="lightblue", lty=3)

plot(activ$steps, type="l", ylab = "Nb of Steps",  xlab="Interval",
     main="NA replaced by mean of interval")
abline(h=meanofMean3, col="red", lty=3)
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-1.png) 

```r
par(mfrow=c(1,2))
plot(dailyTotal$V1, type="h", ylab="Total Steps per day", xlab = "Day",
     main="NA removed")

plot(dailyTotal3$V1, type="h" , ylab="Total daily Steps", xlab = "Day",
     main="NA replaced by mean of interval")
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-2.png) 

```r
par(mfrow=c(1,2))
plot(intervalMean$V1, type="p", ylab="Mean Steps per Interval", xlab = "Day",
     main="NA removed")

plot(intervalMean3$V1, type="p", ylab="Mean Steps per Interval", xlab = "Day",
     main="NA replaced by mean of interval")
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-3.png) 

## Are there differences in activity patterns between weekdays and weekends?



```r
whichday <- function (dt) {
  wd <- weekdays(dt)
  if (wd %in% c("Monday", "Tuesday","Wednesday", "Thursday", "Friday"))
    return("weekday") 
  else
    return("weekend")
}
wday <- vector()
for (i in 1: nrow(activ)) {
  wday[i] <- whichday(activ[i]$date)
}
wday <- factor(wday)
activwd <- activ %>% mutate( wday=wday) %>%
             group_by(wday, interval) %>% summarise (meanInt = mean (steps))
ggplot(activwd, aes(interval, meanInt)) + geom_line() +
  facet_wrap( ~ wday, nrow=2) + 
  labs (title= "Average Interval Steps: Weekend vs Weekday") +
  theme(plot.title = element_text(size = rel(1.5), colour = "blue"))
```

![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-7-1.png) 

```r
zzz <- activwd[wday=="weekend",]
meanWE <- sum(zzz$meanInt)/nrow(zzz)
uuu <- activwd[wday=="weekday",]
meanWD <- sum(uuu$meanInt)/nrow(uuu)
paste("Walking more on the weekend")
```

```
## [1] "Walking more on the weekend"
```

```r
paste("Mean step per interval during weekend: ", round(meanWE,2))
```

```
## [1] "Mean step per interval during weekend:  37.69"
```

```r
paste("Mean step per interval during weekday: ", round(meanWD,2))
```

```
## [1] "Mean step per interval during weekday:  30.63"
```




