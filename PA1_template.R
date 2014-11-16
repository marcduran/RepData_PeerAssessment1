# Loading and preprocessing the data
dir <- getwd()
file <- paste0(dir, "/activity.csv")
zipfile <- "repdata_data_activity.zip"
if (!exists(file)) unzip(paste0(dir, "/data/", zipfile), exdir = dir)
activity <- read.csv(file)
par(mfcol = c(1,1))

# What is mean total number of steps taken per day?
day_steps <- aggregate(activity$steps, by = list(activity$date), FUN = sum, na.rm = TRUE)
names(day_steps) <- c("date", "steps")
with(day_steps, hist(steps, xlab = "steps per day", ylab = "number of days"))
mean_steps <- mean(day_steps$steps)
median_steps <- median(day_steps$steps)
print(paste0("The mean total number of steps during a day is ", mean_steps))
print(paste0("The median total number of steps during a day is ", median_steps))

# What is the average daily activity pattern?
interval_steps <- aggregate(activity$steps, by = list(activity$interval), FUN = mean, na.rm = TRUE)
names(interval_steps) <- c("interval", "steps")
with(interval_steps, plot(steps ~ interval, type = "l", main = "Steps in 5-minutes intervals"))
max_steps <- max(interval_steps$steps)
max_interval <- interval_steps$interval[which(interval_steps$steps == max_steps)]
print(paste0("The interval with the maximum number of steps is ", max_interval))
print(paste0("The interval with the maximum number of steps has ", max_steps, " steps"))

# Imputing missing values
na_count <- sum(is.na(activity$steps))
print(paste0("The dataset has ", na_count, " rows with missing values (NA)"))
activity$steps[is.na(activity$steps)] <- as.integer(interval_steps$steps)
day_steps2 <- aggregate(activity$steps, by = list(activity$date), FUN = sum, na.rm = TRUE)
names(day_steps2) <- c("date", "steps")
with(day_steps2, hist(steps, xlab = "steps per day (full data)", ylab = "number of days"))
mean_steps2 <- mean(day_steps2$steps)
median_steps2 <- median(day_steps2$steps)
print(paste0("The mean total number of steps during a day is now ", mean_steps2))
print(paste0("The median total number of steps during a day is now ", median_steps2))

# Are there differences in activity patterns between weekdays and weekends?
Sys.setlocale("LC_TIME","en_US.utf8")   # make sure days are in English
activity$day <- factor(ifelse(weekdays(as.Date(activity$date)) %in% c("Saturday", "Sunday"), "weekend", "weekday"))
interval_steps_day <- aggregate(activity$steps, by = list(activity$interval, activity$day), FUN = mean, na.rm = TRUE)
names(interval_steps_day) <- c("interval", "day", "steps")
week <- interval_steps_day$day == "weekday"
par(mfcol = c(2,1), mar = c(4, 4, 1, 1), ps = 10)
with(interval_steps_day, {
    plot(steps[week] ~ interval[week],  col = "Black", type = "l", main = "Weekdays", xlab = "", ylab = "steps")
    plot(steps[!week] ~ interval[!week], col = "Red", type = "l", main = "Weekends", xlab = "5-minute interval", ylab = "steps")
})