d<-read.csv('/Users/jiwanhwang/Desktop/flight-delays/flights.csv')

## Relavant descriptive Statistics & histograms
data<-d[,c(12, 17, 18, 23, 25, 26)]

total<-which(data$CANCELLED==0 & data$ARRIVAL_DELAY != 'NA')
totaldf<-data[total,]
summary(totaldf[1:4])
(t<-nrow(totaldf))

ontime<-which(totaldf$ARRIVAL_DELAY<=0)
ontimedf<-totaldf[ontime,]
summary(ontimedf[1:4])
(o<-nrow(ontimedf))

# ontime(and early) arrival rate
print(o/t)

# histogram
hist(totaldf$ARRIVAL_DELAY, xlim=c(-50,2300))
hist(totaldf$AIR_TIME, xlim=c(0,800))
hist(totaldf$DISTANCE)
hist(totaldf$DEPARTURE_DELAY)

## Q3 Relationship between distance and airtime.
# Scatter plots
plot(data$DISTANCE, data$AIR_TIME, xlab="Distance", ylab="Air time", main="Scatter Plot", pch=1)
abline(lm(data$DISTANCE ~ data$AIR_TIME))

nonna<-which(data$AIR_TIME != "NA" & data$CANCELLED == 0)
nonnadf<-data[nonna,]
summary(nonnadf)
cor(nonnadf$DISTANCE, nonnadf$AIR_TIME)
plot(nonnadf$DISTANCE, nonnadf$AIR_TIME, xlab="Distance", ylab="Air time", main="Scatter Plot", pch=1)
abline(lm(nonnadf$AIR_TIME ~ nonnadf$DISTANCE))
linreg<-lm(nonnadf$AIR_TIME ~ nonnadf$DISTANCE, data=nonnadf)
print(linreg)
summary(linreg)

## Q4 Relationship between Departure delay(>0) and arrival delay time (negative value included). 
# data when having departure delay(+ value) with no cancellation (to get rid of NA value b/c cancellation)
deptdelay<-which(data$DEPARTURE_DELAY>0 & data$CANCELLED == 0 & data$ARRIVAL_DELAY != 'NA')
deptdelaydf<-data[deptdelay,]
summary(deptdelaydf)
summary(deptdelaydf$DEPARTURE_DELAY)
sd(deptdelaydf$DEPARTURE_DELAY)
(a<-nrow(deptdelaydf)) #total number of departure delays
#Scatter plots
plot(deptdelaydf$DEPARTURE_DELAY, deptdelaydf$ARRIVAL_DELAY, xlab="Departure Delay time", ylab="Arrival Delay Time", main="Scatter Plot", pch=1)
abline(lm(deptdelaydf$DEPARTURE_DELAY ~ deptdelaydf$ARRIVAL_DELAY))
cor(deptdelaydf$DEPARTURE_DELAY, deptdelaydf$ARRIVAL_DELAY)
linreg2<-lm(deptdelaydf$DEPARTURE_DELAY ~ deptdelaydf$ARRIVAL_DELAY, data=deptdelaydf)
print(linreg2)
summary(linreg2)

# Q5 Number and Percentage of arrival delays when having departure delays
deptdelay2<-which(deptdelaydf$ARRIVAL_DELAY <= 0)
deptdelay2df<-deptdelaydf[deptdelay2,]
summary(deptdelay2df)
nrow(deptdelay2df)
(b<-nrow(deptdelay2df)) #Number of not having arrival delays inspite of departure delays
(c<-a-b) #Number of arrival delays when having departure delays
(d<-c/a) #Pecentage of arrival delays when having departure delays
(b/a) #Percentage of no arrivial delays when having departure delays

# Q6 Number and Percentage of no depature delays when having arrival delays(pure arrival delays)
# data when having arrival delay(+value) with no cancellation
arrvdelay<-which(data$ARRIVAL_DELAY>0 & data$CANCELLED == 0)
arrvdelaydf<-data[arrvdelay,]
summary(arrvdelaydf)
sd(arrvdelaydf$ARRIVAL_DELAY)
(aa<-nrow(arrvdelaydf))

arrvdelay2<-which(arrvdelaydf$DEPATURE_DELAY <= 0)
arrvdelay2df<-arrvdelaydf[arrvdelay2,]
summary(deptdelay2df)
(bb<-nrow(deptdelay2df)) # number of Not having departure delays among having arrival delays
(cc=bb/aa)
(1-cc)

## Other realationships
# Relationship between distance and arrival delays
plot(totaldf$DISTANCE, totaldf$ARRIVAL_DELAY, xlab="Distance", ylab="Arrival Delay Time", main="Scatter Plot", pch=1)
abline(lm(totaldf$DISTANCE ~ totaldf$ARRIVAL_DELAY))
cor(totaldf$DISTANCE, totaldf$ARRIVAL_DELAY)
linreg3<-lm(totaldf$DISTANCE ~ totaldf$ARRIVAL_DELAY, data=totaldf)
print(linreg3)
summary(linreg3)

# Relationship between Airtime and arrival delays
plot(totaldf$AIR_TIME, totaldf$ARRIVAL_DELAY, xlab="Air Time", ylab="Arrival Delay Time", main="Scatter Plot", pch=1)
abline(lm(totaldf$AIR_TIME ~ totaldf$ARRIVAL_DELAY))
cor(totaldf$AIR_TIME, totaldf$ARRIVAL_DELAY)
linreg4<-lm(totaldf$AIR_TIME ~ totaldf$ARRIVAL_DELAY, data=totaldf)
print(linreg4)
summary(linreg4)

# Cancellation Reason
countscancel<-table(data$CANCELLATION_REASON)
countscancel[2:5]
barplot(countscancel[2:5], ylim=c(0, 50000))
