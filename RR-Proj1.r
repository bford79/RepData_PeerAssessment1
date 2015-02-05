if("knitr" %in% rownames(installed.packages()) == FALSE) 
{install.packages("knitr")}
library(knitr)
if("dplyr" %in% rownames(installed.packages()) == FALSE) 
{install.packages("dplyr")}
library(dplyr)
if("lattice" %in% rownames(installed.packages()) == FALSE) 
{install.packages("lattice")}
library(lattice)

act <- read.csv("activity.csv")
act <- tbl_df(act)
meansteps <- act %>% group_by(date) %>% select(date, steps) %>% summarise_each(funs(sum))
mean <- as.integer(mean(meansteps$steps, na.rm = T))
med <- as.integer(median(meansteps$steps, na.rm = T))

png(file = "plot1.png", width = 480, height = 480)
hist(meansteps$steps, breaks = 20, main = "Mean Steps/Day Histogram", 
     xlab = "Mean Steps/Day", ylab = "Frequency")
abline(v = mean, col = "blue", lwd = 2)
abline(v = med, col = "red", lwd = 2, lty = 2)
legend("topright", legend = paste(c("Mean", "Median"), c(mean, med), 
                                  sep = " = "), col=c("blue", "red"), lwd=2)
dev.off()

intmeans <- act %>% group_by(interval) %>% select(interval, steps) %>% 
     summarise_each(funs(mean(., na.rm = T)))
png(file = "plot2.png", width = 480, height = 480)
plot(intmeans$interval, intmeans$steps, type = "l", main = "Mean Steps/Interval", 
     xlab = "Interval", ylab = "Steps")
abline(v = intmeans[which(intmeans$steps == max(intmeans$steps, na.rm = T)),1],
       col = "red", lwd = 1)
legend("topright", col = "red", lwd = 1, legend = paste("Max Steps Interval", 
          intmeans[which(intmeans$steps == max(intmeans$steps, na.rm = T)),1], sep = " = "))
dev.off()

actfilled <- as.data.frame(act)
for(i in 1:nrow(actfilled)) {
     if(is.na(actfilled[i,1])) {
          actfilled[i,1] <- intmeans[which(intmeans$interval == actfilled[i,3]),2]
     }
}

actfilled <- tbl_df(actfilled)
meanstepsfilled <- actfilled %>% group_by(date) %>% select(date, steps) %>% summarise_each(funs(sum))
mean <- as.integer(mean(meanstepsfilled$steps, na.rm = T))
med <- as.integer(median(meanstepsfilled$steps, na.rm = T))
png(file = "plot3.png", width = 480, height = 480)
hist(meanstepsfilled$steps, breaks = 20, main = "Mean Steps/Day Histogram 2", 
     xlab = "Mean Steps/Day", ylab = "Frequency")
abline(v = mean, col = "blue", lwd = 2)
abline(v = med, col = "red", lwd = 2, lty = 2)
legend("topright", legend = paste(c("Mean", "Median"), c(mean, med), 
                                  sep = " = "), col=c("blue", "red"), lwd=2)
dev.off()

diff <- sum(meanstepsfilled$steps, na.rm = T) - sum(meansteps$steps, na.rm = T)

actdays <- as.data.frame(act)
actdays$date <- as.POSIXlt(actdays$date)
actdays$day <- weekdays(actdays$date)
actdays[,"type"] <- NA
for(i in 1:nrow(actdays)){
     if(actdays[i,4] == "Saturday"| actdays[i,4] == "Sunday"){
        actdays[i,5] <- "Weekend"
     }else{
          actdays[i,5] <- "Weekday"
     }
     }
actdays$date <- as.character(actdays$date)
actdays <- tbl_df(actdays)
wkend <- actdays %>% group_by(interval, type) %>% select(interval,type,steps) %>% 
     summarise_each(funs(mean(.,na.rm = T)))
png(file = "plot4.png", width = 480, height = 480)
densityplot(~interval|type, data = wkend)
xyplot(steps~interval|type,
 ylab="Steps", xlab="Interval", 
 main="Steps/Interval by Day Type", 
 layout=(c(1,2)), type = "l", data = wkend)
dev.off()