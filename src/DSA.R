rm(list = ls()) #clean environment
cat("\014")     #clear console
dev.off()       #clear previous plots
setwd("C:/Users/oboke/Desktop/R Assignment")

#1 Read in the 3 discharge data files.
Q1<-read.table("Q1_2.txt",header = T)
Q2<-read.table("Q2_2.txt",header = T)
Q3<-read.table("Q3_2.txt",header = T)

#2 Choose the longest common period for which discharge is observed at all gauges and
#combine the data in a single matrix, which contains year, month, day and discharge of
#the individual stations.

head(Q1)
#Year
Q_1_year<-Q1[,1]
Q_2_year<-Q2[,1]
Q_3_year<-Q3[,1]

#Month
Q_1_month<-Q1[,2]
Q_2_month<-Q2[,2]
Q_3_month<-Q3[,2]

#Day
Q_1_day<-Q1[,3]
Q_2_day<-Q2[,3]
Q_3_day<-Q3[,3]

#Discharge
Qa_1<-Q1[,4]
Qa_2<-Q2[,4]
Qa_3<-Q3[,4]

startyear=Q_1_year[1]
endyear=Q_1_year[length(Q_1_year)]
startmonth=Q_1_month[1]
endmonth=Q_1_month[length(Q_1_month)]
startday=Q_1_day[1]
endday=Q_1_day[length(Q_1_day)]
startdischarge=Qa_1[1]
enddischarge=Q_1_day[length(Qa_1)]

start = which(Q_2_year == startyear & Q_2_month == startmonth &
                Q_2_day == startday)
start = which(Q_3_year == startyear & Q_3_month == startmonth &
                Q_3_day == startday)

end = which(Q_2_year == endyear & Q_2_month == endmonth & Q_2_day == endday)
end = which(Q_3_year == endyear & Q_3_month == endmonth & Q_3_day == endday)

Q3 = Q3[start:end,]

#Combine the data in a single matrix
data <- cbind(Q_1_year, Q_1_month, Q_1_day,Q1[,4],Q2[,4],Q3[,4])
colnames(data) = c("Year", "Month", "Day", "Q1", "Q2", "Q3")

#3 Transform the discharge from m?/s to l/(s*km?) (specific discharge).
Areas<-read.table("areas_2.txt",header = T)
Area_Q1 <- Areas[,1]
Area_Q2 <- Areas[,2]
Area_Q3 <- Areas[,3]

data[,"Q1"] = data[,"Q1"]*1000/Area_Q1
data[,"Q2"] = data[,"Q2"]*1000/Area_Q2
data[,"Q3"] = data[,"Q3"]*1000/Area_Q3

#4 Draw the boxplots for the specific discharge at the individual stations in a single plot
boxplot(data[,c("Q1","Q2","Q3")],
        col = c("red","blue","green"),
        names = c(1,2,3),
        main = "Boxplot",
        xlab = "Gauge Number",
        ylab = "Q[l/(s*km^2)]",
        outline = FALSE)

#5 What is the specific discharge that is exceeded by 90% of the data at each gauge?
##gauge_1##

Q1_quantile = quantile(data[,4], c(0.1))
print(Q1_quantile)

##gauge_2##
Q2_quantile = quantile(data[,5], c(0.1))
print(Q2_quantile)

##gauge_3##
Q3_quantile = quantile(data[,6], c(0.1))
print(Q3_quantile)

#6 DSA with both the other two discharge records
Q1<-Q1[,4]
Q2
Q2<-Q2[,4]
Q1sum <- cumsum(Q1)
Q2sum <- cumsum(Q2)

#Q1 plot
plot(Q1sum, type = "l",main="Double sum analysis for Q1",
     ylab="Reference series",xlab="Test series")

#Q2 plot
plot(Q2sum, type = "l",main="Double sum analysis for Q2",
     ylab="Reference series",xlab="Test series")

# Plotting Q1 & Q2 together
par(mfrow=c(1,2))
plot(Q1sum, type = "l",main="Double sum analysis for Q1",
     ylab="Reference series",xlab="Test series")
plot(Q2sum, type = "l",main="Double sum analysis for Q2",
     ylab="Reference series",xlab="Test series")

#7 Plot a histogram for each discharge gauge. The y-axis should show relative frequencies
head(data)

Q1<-data[,4]
nclasses<-round(5*log10(length(Q1)),0)
classwidth <- (max(Q1)-min(Q1))/nclasses
breaksQ1 <- seq(min(Q1), max(Q1), classwidth)
hist(Q1, breaks = breaksQ1, freq=FALSE,
     xlab = "Q[1/(s*km^2)]", ylab="Density")

#Histogram discharge gauge 2
Q2<-data[,5]
nclasses<-round(5*log10(length(Q2)),0)
classwidth <- (max(Q2)-min(Q2))/nclasses
breaksQ2 <- seq(min(Q2), max(Q2), classwidth)
hist(Q2, breaks = breaksQ2, freq=FALSE,
     xlab = "Q[1/(s*km^2)]", ylab="Density")

#Histogram discharge gauge 3
head(data)
Q3<-data[,6]
nclasses<-round(5*log10(length(Q3)),0)
classwidth <- (max(Q3)-min(Q3))/nclasses
breaksQ3 <- seq(min(Q3), max(Q3), classwidth)
hist(Q3, breaks = breaksQ3, freq=FALSE,
     xlab = "Q[1/(s*km^2)]", ylab="Density")

#7B Skewness

install.packages("moments")
library(moments)
skewness(Q1<-data[,4])
skewness(Q2<-data[,5])
skewness(Q3<-data[,6])

#8 8.	Calculate the average daily discharge in the watershed (mean over all three gauges)
Q = data.frame(Q1,Q2,Q3)
rowMeans(Q)
###############Or
Qa <- cbind(Q1,Q2,Q3)
Qa_Mean <- rowMeans(Qa)
Year = Q_1_year
Month = Q_1_month
Day = Q_1_day
DailyQa <- data.frame(Year,Month,Day,Qa_Mean)
write.table(DailyQa,file = "dailyQ.txt", row.names = FALSE)

#9 Plot the average daily flow for the first year: 
daily_flow_1 <- Qa_Mean[1:366]
plot(daily_flow_1, main = "Average Daily Flow (Year 1; 1940)",
     xlab = "Time[d]", ylab = "Q[1/s*km2]",type = "l")

# 10 Calculate the long-term average monthly discharge in the watershed for each gauge.
##Plot the averages in a barplot using unstacked bars:
head(data)
Qmonthly <-aggregate(data[,c("Q1","Q2","Q3")], by = list(data[,"Month"]), FUN = mean)
Qmonthly=Qmonthly[,-1]
rownames(Qmonthly)<-month.abb
barplot(t(Qmonthly), beside = T, col = c("red","green","yellow"),
        main = "Average Monthly Q", xlab = "Month",
        ylab = "Average Q [1/(s*km2)]")
