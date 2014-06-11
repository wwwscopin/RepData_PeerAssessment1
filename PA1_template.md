---
title: "Activity"
author: "Baohua Wu"
date: "06/10/2014"
output: html_document
---

# Reproducible Research: Peer Assessment 1

## Loading and preprocessing the data


```r
act<-read.csv("activity.csv", header=TRUE)
```

```
## Warning: cannot open file 'activity.csv': No such file or directory
```

```
## Error: cannot open the connection
```

```r
str(act)
```

```
## Error: object 'act' not found
```

```r
act$Date<-as.Date(act$date, format = "%Y-%m-%d")
```

```
## Error: object 'act' not found
```

```r
head(act)
```

```
## Error: object 'act' not found
```

## What is mean total number of steps taken per day?

```r
tot_steps<-tapply(act$steps, act$Date, sum, na.rm=TRUE)
```

```
## Error: object 'act' not found
```

```r
hist(tot_steps, xlab="Total Number of Steps Taken per Day", main="Histogram for Total Number of Steps per Day")
```

```
## Error: object 'tot_steps' not found
```

```r
mean<-mean(tot_steps, na.rm=TRUE)
```

```
## Error: object 'tot_steps' not found
```

```r
mean
```

```
## function (x, ...) 
## UseMethod("mean")
## <bytecode: 0x4258ec8>
## <environment: namespace:base>
```

```r
median<-median(tot_steps, na.rm=TRUE)
```

```
## Error: object 'tot_steps' not found
```

```r
median
```

```
## function (x, na.rm = FALSE) 
## UseMethod("median")
## <bytecode: 0x3edae50>
## <environment: namespace:stats>
```

## What is the average daily activity pattern?

```r
avg<-tapply(act$steps, act$interval, mean, na.rm=TRUE)
```

```
## Error: object 'act' not found
```

```r
interval<-as.numeric(names(avg))
```

```
## Error: object 'avg' not found
```

```r
step<-as.data.frame(cbind(avg, interval))
```

```
## Error: object 'avg' not found
```

```r
require(ggplot2)
```

```
## Loading required package: ggplot2
```

```r
ggplot(step, aes(interval, avg)) + geom_line() +  xlab("Interval") + ylab("Avg Steps")
```

```
## Error: ggplot2 doesn't know how to deal with data of class function
```


## Imputing missing values

```r
sum(is.na(act$steps)) 
```

```
## Error: object 'act' not found
```

```r
wbh<-act[,-1]
```

```
## Error: object 'act' not found
```

```r
wbh$steps<-ifelse(is.na(act$steps),mean(act$steps, na.rm=TRUE), act$steps)
```

```
## Error: object 'act' not found
```

```r
#str(wbh)
#head(wbh)

tot_steps1<-tapply(wbh$steps, wbh$Date, sum, na.rm=TRUE)
```

```
## Error: object 'wbh' not found
```

```r
hist(tot_steps1, xlab="Total Number of Steps Taken per Day", main="Histogram for Total Number of Steps per Day")
```

```
## Error: object 'tot_steps1' not found
```

## What is the mean and median after imputing of missing?


```r
mean1<-mean(tot_steps1, na.rm=TRUE)
```

```
## Error: object 'tot_steps1' not found
```

```r
mean1
```

```
## Error: object 'mean1' not found
```

```r
median1<-median(tot_steps1, na.rm=TRUE)
```

```
## Error: object 'tot_steps1' not found
```

```r
median1
```

```
## Error: object 'median1' not found
```

## Are there differences in activity patterns between weekdays and weekends?


```r
wbh$day<-weekdays(wbh$Date)
```

```
## Error: object 'wbh' not found
```

```r
wbh$wkd<-ifelse(wbh$day %in% c("Saturday", "Sunday"), "Weekend", "Weekday")
```

```
## Error: object 'wbh' not found
```

```r
avg<-tapply(wbh[wbh$wkd=="Weekday",]$steps, wbh[wbh$wkd=="Weekday",]$interval, mean, na.rm=TRUE)
```

```
## Error: object 'wbh' not found
```

```r
interval<-names(avg)
```

```
## Error: object 'avg' not found
```

```r
wkd<-"Weekday"
week<-as.data.frame(cbind(avg,interval,wkd))
```

```
## Error: object 'avg' not found
```

```r
avg<-tapply(wbh[wbh$wkd=="Weekend",]$steps, wbh[wbh$wkd=="Weekend",]$interval, mean, na.rm=TRUE)
```

```
## Error: object 'wbh' not found
```

```r
interval<-names(avg)
```

```
## Error: object 'avg' not found
```

```r
wkd<-"Weekend"
week1<-as.data.frame(cbind(avg,interval,wkd))
```

```
## Error: object 'avg' not found
```

```r
wk<-rbind(week,week1)
```

```
## Error: object 'week' not found
```

```r
require(lattice)
```

```
## Loading required package: lattice
```

```r
xyplot(as.numeric(levels(wk$avg))[wk$avg]~as.numeric(levels(wk$interval))[wk$interval] |wk$wkd, wk,layout = c(1, 2), xlab="Interval", ylab="Number of Steps",
  panel=function(x, y){
  panel.xyplot(x, y, lty=1, type="l")
}, as.table=T)
```

```
## Error: object 'wk' not found
```

