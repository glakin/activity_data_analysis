#This file was used to write the code that ended up in PA1_template.Rmd

library(dplyr)
library(ggplot2)

temp <- tempfile()
download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip",temp)
df <- read.csv(unz(temp, "activity.csv"))
unlink(temp)

# steps taken per day
dfDate = df %>% group_by(date) %>%
    summarize(
    sumSteps = sum(steps, na.rm = TRUE)
)
head(dfDate)

# histogram of steps per day
ggplot(dfDate, aes(x=sumSteps)) + 
    geom_histogram() +
    ggtitle("Daily Steps Histogram") +
    xlab("Daily Steps")

# average number of steps per day
mean(dfDate$sumSteps, na.rm = TRUE)

# median number of steps per day
median(dfDate$sumSteps, na.rm = TRUE)

# daily activity pattern - average steps per interval
dfInt = df %>% group_by(interval) %>%
    summarize(
        avgSteps = mean(steps, na.rm = TRUE)
    )
head(dfInt)

# plot avg daily activity
ggplot(dfInt, aes(x = interval, y = avgSteps)) +
    geom_line() +
    ggtitle("Average Steps per 5 Min Interval") +
    xlab("Interval") +
    ylab("Average Steps")

# which interval contains max number of steps
dfInt[dfInt$avgSteps == max(dfInt$avgSteps), 1]

# how many NAs in the dataset?
sum(!complete.cases(df))

# fill in missing data with the average from corresponding interval
dfMerge <- merge(df[!complete.cases(df), ], dfInt, by = "interval", sort = FALSE)
dfMerge <- dfMerge[order(dfMerge$date, dfMerge$interval),]

# Create new dataset equal to the original but with missing data imputed
dfImputed <- data.frame(df)
dfImputed[!complete.cases(dfImputed), ]$steps <- dfMerge$avgSteps

# find steps per day again
dfImputedDate = dfImputed %>% group_by(date) %>%
    summarize(
        sumSteps = sum(steps, na.rm = TRUE)
    )

# histogram of steps per day
ggplot(dfImputedDate, aes(x=sumSteps)) + 
    geom_histogram() +
    ggtitle("Daily Steps  - Missing Values Imputed") +
    xlab("Daily Steps")

# average number of steps per day
mean(dfImputedDate$sumSteps, na.rm = TRUE)

# median number of steps per day
median(dfImputedDate$sumSteps, na.rm = TRUE)

# create new variable indicating weekends and weekdays 
dfImputed$date <- as.Date(dfImputed$date, format = "%Y-%m-%d")
dfImputed$dayType <- factor(NA, levels = c("weekday","weekend"))

weekdayList = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")
weekendList = c("Saturday", "Sunday")
dfImputed[weekdays(dfImputed$date) %in% weekdayList, "dayType"]<- "weekday"
dfImputed[weekdays(dfImputed$date) %in% weekendList, "dayType"]<- "weekend"

# daily activity pattern - average steps per interval
dfImputedInt = dfImputed %>% group_by(interval, dayType) %>%
    summarize(
        avgSteps = mean(steps, na.rm = TRUE)
    )

# plot avg daily activity
ggplot(dfImputedInt, aes(x = interval, y = avgSteps)) +
    geom_line() +
    ggtitle("Average Steps per 5 Min Interval") +
    xlab("Interval") +
    ylab("Average Steps") +
    facet_wrap(dayType ~ .) 


