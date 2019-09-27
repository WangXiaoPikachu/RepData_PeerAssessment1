---
title: "Reproducible Research Project 1"
author: "Xiao Wang"
date: "2019/9/25"
output:
  md_document:



## Introduction
It is now possible to collect a large amount of data about personal movement using activity monitoring devices such as a Fitbit, Nike Fuelband, or Jawbone Up. These type of devices are part of the “quantified self” movement - a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. But these data remain under-utilized both because the raw data are hard to obtain and there is a lack of statistical methods and software for processing and interpreting the data.

This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

## Data For the Analysis

The data for this assignment can be downloaded from the course web site:  

 **Dataset**: [Activity monitoring](https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip)
 
 *The variables included in this dataset are:*
 
 **steps**: Number of steps taking in a 5-minute interval (missing values are coded as NA)

 **date**: The date on which the measurement was taken in YYYY-MM-DD format

 **interval**: Identifier for the 5-minute interval in which measurement was taken

The dataset is stored in a comma-separated-value (CSV) file and there are a total of 17,568 observations in this dataset.  

## Loading and preprocessing the data
setting the working directory

```r
if(!file.exists("./data")){dir.create("./data")}
```
setting the URL & download the dataset

```r
URL<-"https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
download.file(URL,destfile="./data/RRpj1.zip",method="auto") 
```
unzip the dataset& read into R

```r
unzip(zipfile="./data/RRpj1.zip",exdir="./data")
data=read.csv("./data/activity.csv",heade=TRUE)
```

## data preparation

### loading packages

```r
library(ggplot2)
```


```r
library(dplyr)
```
### setting system into English version

```r
Sys.setlocale(category = "LC_ALL", locale = "english")
```

```
## [1] "LC_COLLATE=English_United States.1252;LC_CTYPE=English_United States.1252;LC_MONETARY=English_United States.1252;LC_NUMERIC=C;LC_TIME=English_United States.1252"
```
### convert the **date** into **DateTime** with POSIXct format

```r
data$DateTime<- as.POSIXct(data$date, format="%Y-%m-%d")
```
### creat new variable **day** to store *weekdays*

```r
data$day <- weekdays(as.Date(data$date))
```

## Q1 Histogram of the total number of steps taken each day
First, getting the total steps per date

```r
data=tbl_df(data)
Q1=data%>% group_by(date)%>%summarise(sum(steps))
colnames(Q1)<- c("Date", "Steps")
```
Second,Make a histogram of the total number of steps taken each day

```r
ggplot(data=Q1,aes(x=Steps))+geom_histogram(bins=50)+xlab("Steps")+labs(title="Total Steps per Day")
```

```
## Warning: Removed 8 rows containing non-finite values (stat_bin).
```

![plot of chunk unnamed-chunk-10](figure/unnamed-chunk-10-1.png)

## Q2 Mean and median number of steps taken each day
First,calculate and report the mean of the total number of steps taken per day

```r
Q2=Q1
mean(Q2$Steps,na.rm=T)
```

```
## [1] 10766.19
```
Second,calculate and report the median of the total number of steps taken per day

```r
median(Q2$Steps,na.rm=T)
```

```
## [1] 10765
```

**The average number of steps taken each day was 10766 steps.**

**The median number of steps taken each day was 10765 steps.**

## Q3 Time series plot of the average number of steps taken
First,remove missing values

```r
Q3<-data
Q3 <- Q3[!is.na(Q3$steps),]
```
Second,create average number of steps for each interval

```r
Q3<-data
Q3 <- Q3[!is.na(Q3$steps),]
Q3=Q3%>% group_by(interval)%>%summarise(mean(steps))
colnames(Q3)<- c("interval", "Avg.steps")
```
Create line plot of average number of steps per interval

```r
p3 <- ggplot(Q3, aes(x=interval, y=Avg.steps))
p3 + geom_line()+xlab("Interval")+ylab("Average Number of Steps")+ggtitle("Average Number of Steps per Interval")
```

![plot of chunk unnamed-chunk-15](figure/unnamed-chunk-15-1.png)

## Q4 The 5-minute interval that, on average, contains the maximum number of steps  
First, find the maximum number of steps for a 5-minute interval.

```r
Q4=Q3
maxSteps <- max(Q4$Avg.steps)
maxSteps
```

```
## [1] 206.1698
```
**The maximum number of steps for a 5-minute interval was 206 steps. **

Second,find the 5-minute interval which had the maximum number of steps.

```r
Q4[Q4$Avg.steps==maxSteps,1]
```

```
## # A tibble: 1 x 1
##   interval
##      <int>
## 1      835
```
**The 5-minute interval which had the maximum number of steps was the 835 interval.**

## Q5 Code to describe and show a strategy for imputing missing data
First, count how many rows which contains missing data and the weight of missing data.

```r
Q5=data
nrow(Q5[is.na(Q5$steps),])
```

```
## [1] 2304
```

```r
nrow(Q5[is.na(Q5$steps),])/nrow(Q5)
```

```
## [1] 0.1311475
```
We can find that the dataset has 2304 missing values which are 13% of the total data set.

Second,since the missing datas are only share 13% percentage of the total data set, I decide to use mean for that 5-minute interval to fill the missing values.

```r
Avg.steps=Q5%>% group_by(interval)%>%summarise(mean(steps,na.rm = TRUE))
colnames(Avg.steps)<- c("interval", "Avg.steps")
NADATA=Q5%>%filter(is.na(steps))
mergedDATA=full_join(NADATA, Avg.steps, by = "interval")
mergedDATA$steps=mergedDATA$Avg.steps
mergedDATA=mergedDATA[,1:5]
cleaned=na.omit(rbind(Q5,mergedDATA))
```

## Q6 Histogram of the total number of steps taken each day after missing values are imputed  

First, getting the total steps per date

```r
Q6=cleaned
Q6=Q6%>% group_by(date)%>%summarise(sum(steps))
colnames(Q6)<- c("Date", "Steps")
```
Second,Make a histogram of the total number of steps taken each day

```r
ggplot(data=Q6,aes(x=Steps))+geom_histogram(bins=50)+xlab("Steps")+labs(title="Total Steps per Day after cleaned dataset")
```

![plot of chunk unnamed-chunk-21](figure/unnamed-chunk-21-1.png)

## Q7 Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends
First,create a new factor variable in the dataset with two levels - “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.

```r
Q7=cleaned
Q7$DayCategory <- ifelse(Q7$day %in% c("Saturday", "Sunday"), "Weekend", "Weekday")
```

Second,summarize data by interval and type of day

```r
Q7=Q7%>% group_by(interval,DayCategory)%>%summarise(mean(steps))
colnames(Q7)<- c("interval", "DayCategory","Avg.steps")
```

Third,Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends  

```r
ggplot(data=Q7,aes(x=interval,y=Avg.steps))+geom_line()+facet_grid(.~DayCategory) 
```

![plot of chunk unnamed-chunk-24](figure/unnamed-chunk-24-1.png)
  
  
**The step activity trends are different based on whether the day occurs on a weekend or not. This may be due to people having an increased opportunity for activity beyond normal work hours for those who work during the week.**
