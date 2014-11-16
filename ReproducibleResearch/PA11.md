#Loading and preprocessing the data
connection <- unz("activity.zip","activity.csv")
activity <- read.table(file = connection,header = TRUE,sep = ",")
activity$datetime <- as.POSIXct(with(activity,paste(date,paste(interval %/% 100, interval %% 100, sep=":"))),format="%Y-%m-%d %H:%M",tz="")
library(ggplot2)
library(scales)
library(grid)
library(gridExtra)
#What is mean total number of steps taken per day?
# calculating data for histogram
stepsPerDay <- setNames(
      aggregate(
          steps~as.Date(date),
          activity,
          sum,
          na.rm = TRUE),
      c("date","steps")
    )

# plotting histogram
hist1 <- ggplot(stepsPerDay,aes(x=date,y=steps)) + 
  geom_bar(stat="identity") + 
  ggtitle("Total number of steps per day (source data)")

print(hist1)
  summary(stepsPerDay$steps)
  mm_1 <- c(mean = mean(stepsPerDay$steps),median = median(stepsPerDay$steps))
  print(mm_1)
  #What is the average daily activity pattern?
  avgPattern <- aggregate(steps~interval,activity,mean,na.rm = TRUE)
avgPattern$time <- as.POSIXct(with(avgPattern,paste(interval %/% 100, interval %% 100, sep=":")),format="%H:%M")
plot <- ggplot(avgPattern,aes(x=time,y=steps)) + 
          geom_line() + 
          scale_x_datetime(breaks = date_breaks("2 hour"),labels = date_format("%H:%M"))
print(plot)
with(avgPattern,avgPattern[steps == max(steps),])
#Imputing missing values
mis <- aggregate(cnt~date,cbind(activity[is.na(activity$steps),],cnt=c(1)),sum,na.rm = FALSE)
mis$dow <- weekdays(as.Date(mis$date),abbreviate=TRUE)
print(mis[,c(1,3,2)])
unique(mis$dow)
ref_ds <- aggregate(steps~interval+weekdays(datetime,abbreviate=TRUE),activity,FUN=mean,na.rm=TRUE)
colnames(ref_ds) <- c("interval","dow","avg_steps")
ref_ds$dow <- factor(ref_ds$dow,levels = c("Mon","Tue","Wed","Thu","Fri","Sat","Sun"))
ggplot(ref_ds,aes(x=interval,y=avg_steps)) + geom_line() + facet_grid("dow ~ .")
activity$dow <- weekdays(activity$datetime,abbreviate=TRUE)
af <- merge(activity,ref_ds,by=c("dow","interval"),all.x = TRUE)
af <- af[with(af,order(date,interval)),]
af$fixed_steps <- ifelse(is.na(af$steps),af$avg_steps,af$steps)
# calculating data set for histogram
  stepsPerDay2 <- setNames(
      aggregate(
          fixed_steps~as.Date(date),
          af,
          sum,
          na.rm = TRUE),
      c("date","steps")
    )
# plotting histogram
hist2 <- ggplot(stepsPerDay2,aes(x=date,y=steps)) + 
  geom_bar(stat="identity") + 
  ggtitle("Total number of steps per day (fixed data)")
# combining with previous
grid.arrange(hist1, hist2, nrow=2)
 mm_2 <- c(mean = mean(stepsPerDay2$steps),median = median(stepsPerDay2$steps))
  comparison <- rbind(source = mm_1, fixed = mm_2, delta = mm_2-mm_1)
  print(comparison)
  #Are there differences in activity patterns between weekdays and weekends?
  week_diff <- aggregate(
  steps~dow+interval,  # group steps by weekend/weekday and interval to find average steps 
  with(
    activity,
    data.frame(
      dow = factor(
        ifelse(
          weekdays(as.Date(date)) %in% c("Sunday","Saturday"),
          "weekend",  # if sunday or saturday
          "weekday"   # else
        )
      ),
      interval,
      steps
    )
  ),
  FUN = mean,
  rm.na = TRUE
)
ggplot(week_diff,aes(x=interval,y=steps)) + geom_line() + facet_grid("dow ~ .")
