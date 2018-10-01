---
title: "Reproducible Research Assignment"
output: 
  html_document: 
    keep_md: yes
---
##Loading the Data

```r
library(knitr)
```

Download the Activity Monitoring data file.

```r
if(!file.exists("activity.csv")) {
        tempfile <- tempfile()
        download.file("http://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip",destfile = tempfile)
        unzip(tempfile)
        unlink(tempfile)
}
```

Load the Activity Monitoring data file.

```r
activitydata <- read.csv("activity.csv")
```

Summary of raw Activity Monitoring data file.

```r
summary(activitydata)
```

```
##      steps                date          interval     
##  Min.   :  0.00   2012-10-01:  288   Min.   :   0.0  
##  1st Qu.:  0.00   2012-10-02:  288   1st Qu.: 588.8  
##  Median :  0.00   2012-10-03:  288   Median :1177.5  
##  Mean   : 37.38   2012-10-04:  288   Mean   :1177.5  
##  3rd Qu.: 12.00   2012-10-05:  288   3rd Qu.:1766.2  
##  Max.   :806.00   2012-10-06:  288   Max.   :2355.0  
##  NA's   :2304     (Other)   :15840
```

##Summary Statistics for Daily Steps
Number of total steps taken per day, excluding missing values:

```r
dailysteps <- aggregate(steps ~ date, data=activitydata, FUN=sum, na.rm=TRUE)
dailysteps
```

```
##          date steps
## 1  2012-10-02   126
## 2  2012-10-03 11352
## 3  2012-10-04 12116
## 4  2012-10-05 13294
## 5  2012-10-06 15420
## 6  2012-10-07 11015
## 7  2012-10-09 12811
## 8  2012-10-10  9900
## 9  2012-10-11 10304
## 10 2012-10-12 17382
## 11 2012-10-13 12426
## 12 2012-10-14 15098
## 13 2012-10-15 10139
## 14 2012-10-16 15084
## 15 2012-10-17 13452
## 16 2012-10-18 10056
## 17 2012-10-19 11829
## 18 2012-10-20 10395
## 19 2012-10-21  8821
## 20 2012-10-22 13460
## 21 2012-10-23  8918
## 22 2012-10-24  8355
## 23 2012-10-25  2492
## 24 2012-10-26  6778
## 25 2012-10-27 10119
## 26 2012-10-28 11458
## 27 2012-10-29  5018
## 28 2012-10-30  9819
## 29 2012-10-31 15414
## 30 2012-11-02 10600
## 31 2012-11-03 10571
## 32 2012-11-05 10439
## 33 2012-11-06  8334
## 34 2012-11-07 12883
## 35 2012-11-08  3219
## 36 2012-11-11 12608
## 37 2012-11-12 10765
## 38 2012-11-13  7336
## 39 2012-11-15    41
## 40 2012-11-16  5441
## 41 2012-11-17 14339
## 42 2012-11-18 15110
## 43 2012-11-19  8841
## 44 2012-11-20  4472
## 45 2012-11-21 12787
## 46 2012-11-22 20427
## 47 2012-11-23 21194
## 48 2012-11-24 14478
## 49 2012-11-25 11834
## 50 2012-11-26 11162
## 51 2012-11-27 13646
## 52 2012-11-28 10183
## 53 2012-11-29  7047
```

Histogram of the total number of steps taken each day.

```r
hist(dailysteps$steps, xlab="Number of Steps per Day", col="blue", main = "Total number of steps taken each day")
```

![](Reproducible_Research_Assignment_1_files/figure-html/unnamed-chunk-6-1.png)<!-- -->


```r
meansteps <- mean(dailysteps$steps)
meansteps <- format(meansteps, digits=1)
```
Mean number of steps taken per day: 10766



```r
mediansteps <- median(dailysteps$steps)
mediansteps <- format(mediansteps, digits=1)
```
Median number of steps taken per day: 10765


##Average Daily Activity Pattern

```r
meandailysteps <- aggregate(steps ~ interval, data=activitydata, FUN=mean, na.rm=TRUE)
plot(meandailysteps$interval, meandailysteps$steps, type="l", xlab="Time Intervals", ylab="Total Steps per 5-min Interval", main="Mean Number of Steps per 5-Minute Interval")
```

![](Reproducible_Research_Assignment_1_files/figure-html/unnamed-chunk-9-1.png)<!-- -->

Maximum number of steps within 5-minute interval:

```r
maxsteps <- max(meandailysteps$steps)
maxinterval <- meandailysteps$interval[which.max(meandailysteps$steps)]
```
Maximum number of steps within 5-minute interval: 206.1698113.
The 5-min interval containing the maximum number of steps is 835.

##Missing Value Imputation
Total number of missing 'steps' values in Activity Monitoring data:

```r
sum(is.na(activitydata))
```

```
## [1] 2304
```

Data exploration of missing values in Steps and Dates 

```r
naTRUEdata <- subset(activitydata, is.na(steps))
par(mfrow=c(2,1), mar=c(2,2,1,1))
hist(naTRUEdata$interval, main="Missing Values per Interval")
hist(as.numeric(naTRUEdata$date), main="Missing Values per Date", breaks=61)
```

![](Reproducible_Research_Assignment_1_files/figure-html/unnamed-chunk-12-1.png)<!-- -->

Imputing missing values using mean of intervals.

```r
avgstepsinterval <- tapply(activitydata$steps, activitydata$interval, mean, na.rm=TRUE)
activity_NA <- activitydata[is.na(activitydata$steps),]
activity_noNA <- activitydata[!is.na(activitydata$steps),]
activity_NA$steps <- as.factor(activity_NA$interval)
levels(activity_NA$steps) <- avgstepsinterval
levels(activity_NA$steps) <- round(as.numeric(levels(activity_NA$steps)))
activity_NA$steps <- as.integer(as.vector(activity_NA$steps))
imputed_activitydata <- rbind(activity_NA, activity_noNA)
```

Histogram of total number of steps taken each data using imputed mean step values.

```r
newdailysteps <- aggregate(steps ~ date, data=imputed_activitydata, FUN=sum, na.rm=TRUE)
hist(newdailysteps$steps, xlab="Number of Steps per Day", col="green", main = "Total number of steps taken each day (with imputed values)")
```

![](Reproducible_Research_Assignment_1_files/figure-html/unnamed-chunk-14-1.png)<!-- -->

Comparison of mean and median for original Activity data and imputed Activity data.

```r
newmeansteps <- mean(newdailysteps$steps)
newmediansteps <- median(newdailysteps$steps)
newmeansteps <- format(newmeansteps, digits=1)
newmediansteps <- format(newmediansteps, digits=1)
summarysteps <- data.frame(c(meansteps, mediansteps), c(newmeansteps, newmediansteps))
colnames(summarysteps) <- c("Original Data", "Imputed Data")
rownames(summarysteps) <- c("Mean", "Median")
kable(summarysteps, format="markdown", digits=1)
```



|       |Original Data |Imputed Data |
|:------|:-------------|:------------|
|Mean   |10766         |10766        |
|Median |10765         |10762        |

The mean steps in the original data is the same as the mean steps in the imputed data.
The median steps in the original data is larger than the median steps in the imputed data.
Imputing mean steps for missing values does not change the mean but decreases the median.


##Weekday and Weekend Patterns
Indicator variable for weekdays and weekends.

```r
imputed_activityday <- ifelse(weekdays(as.Date(imputed_activitydata$date)) == "Saturday" | weekdays(as.Date(imputed_activitydata$date)) == "Sunday", "weekend", "weekday")
```

Time series plot of 5-minute intervals and average number of steps, by weekday or weekend.

```r
weekendsteps <- tapply(imputed_activitydata[imputed_activityday == "weekend" ,]$steps, imputed_activitydata[imputed_activityday == "weekend" ,]$interval, mean, na.rm=TRUE)
weekdaysteps <- tapply(imputed_activitydata[imputed_activityday == "weekday" ,]$steps, imputed_activitydata[imputed_activityday == "weekday" ,]$interval, mean, na.rm=TRUE)
par(mfrow=c(1,2), cex.main=1)
plot(as.numeric(names(weekdaysteps)), weekdaysteps, xlab = "5-min Interval", ylab = "Steps", main="Weekday Average Number of Steps", type="l")
plot(as.numeric(names(weekendsteps)), weekendsteps, xlab = "5-min Interval", ylab = "Steps", main="Weekend Average Number of Steps", type="l")
```

![](Reproducible_Research_Assignment_1_files/figure-html/unnamed-chunk-16-1.png)<!-- -->
