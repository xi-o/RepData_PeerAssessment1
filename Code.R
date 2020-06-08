library(ggplot2)
library(dplyr)
### Loading and preprocessing the data

# 1. Load the data (i.e. `read.csv()`)

if (!file.exists('activity.csv')){
        unzip('activity.zip')
        }

data <- read.csv('activity.csv')

# 2. Process/transform the data (if necessary) into a format suitable for your analysis


### What is mean total number of steps taken per day?

# For this part of the assignment, you can ignore the missing values in
# the dataset.


# 1. Make a histogram of the total number of steps taken each day

plot1 <- ggplot(data, aes(date, steps))

plot1+geom_col()+ theme(axis.text.x = element_text(angle = 90)) + labs(title = "Number of steps taken each day")

# 2. Calculate and report the **mean** and **median** total number of steps taken per day

summary <- data %>% group_by(date) %>%
        summarise(total=sum(steps, na.rm = TRUE), mean= mean(steps, na.rm = TRUE), median = median(steps, na.rm = TRUE))

overall <- summary %>% summarise(total_mean=mean(total), total_median=median(total))


### What is the average daily activity pattern?

#1. Make a time series plot (i.e. `type = "l"`) of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

intervals <- data %>% mutate(intervalfactor = as.factor(interval)) %>% 
                        group_by(intervalfactor) %>%
                        summarise(meansteps = mean(steps, na.rm = TRUE)) %>%
                        mutate(interval = as.numeric(as.character(intervalfactor)))

plot(intervals$interval, intervals$meansteps, type='l', lwd=4,
     xlab="interval [min]", ylab="mean steps",
     main="Average number of steps taken per Interval")

plot2 <- ggplot(intervals, aes(x=interval, y=meansteps))

plot2 + geom_line() + labs(title = "Mean activity level", y="steps")



# 2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

print(intervals$interval[which.max(intervals$meansteps)])

### Imputing missing values
        #         Note that there are a number of days/intervals where there are missing
# values (coded as `NA`). The presence of missing days may introduce
# bias into some calculations or summaries of the data.

# 1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with `NA`s)

allnas <- sum(is.na(data))


# 2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
# 3. Create a new dataset that is equal to the original dataset but with the missing data filled in.

narm_data <- data

#create interval key

interval_key <- data$interval[(is.na(data$steps))]

indexfun <- function(x) {which(intervals$interval==x)}

mean_key <- sapply(interval_key, indexfun)

#exchange NAs for mean during interval

narm_data$steps[(is.na(data$steps))] <- intervals$meansteps[mean_key]

#check if na left

sum(is.na(narm_data))

# 4. Make a histogram of the total number of steps taken each day and Calculate and report the **mean** and **median** total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

plot3 <- ggplot(narm_data, aes(date, steps))

plot3+geom_col()

summary_rmna <- narm_data %>% group_by(date) %>%
        summarise(total=sum(steps, na.rm = TRUE), mean= mean(steps, na.rm = TRUE), median = median(steps, na.rm = TRUE))

overall_rmna <- summary_rmna %>% summarise(mean=mean(total), median=median(total))


### Are there differences in activity patterns between weekdays and weekends?
        
# For this part the `weekdays()` function may be of some help here. Use
# the dataset with the filled-in missing values for this part.

# 1. Create a new factor variable in the dataset with two levels -- "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.

narm_data <- mutate(narm_data, weekdays = weekdays(as.Date(date)))

weekday <- narm_data %>% filter(weekdays == "Monday"|weekdays == "Tuesday"|weekdays == "Wednesday"|weekdays == "Thursday"| weekdays == "Friday") %>%
                        group_by(factor = as.factor(interval)) %>%
                        summarise(mean = mean(steps)) %>%
                        mutate(weekfactor = as.factor("weekday"), interval = as.numeric(as.character(factor)))
                        


weekend <- narm_data %>% filter(weekdays == "Saturday" | weekdays == "Sunday") %>%
                        group_by(factor = as.factor(interval)) %>%
                        summarise(mean = mean(steps)) %>%
                        mutate(weekfactor = as.factor("weekend"), interval = as.numeric(as.character(factor)))


narm_week <- rbind(weekday, weekend)


# 1. Make a panel plot containing a time series plot (i.e. `type = "l"`) of the 
# 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday 
# days or weekend days (y-axis). The plot should look something like the following, which was created using **simulated data**:

plot4 <- ggplot(narm_week, aes(x=interval, y=mean))

plot4 + geom_line() + facet_grid(weekfactor ~ .) + labs(title = "Mean activity level: Weekday vs Weekend", y= "steps")


