# Read all the data #

data1 <- read.csv("C:/Users/shant/Downloads/Calls_for_Service_2011.csv")
data2 <- read.csv("C:/Users/shant/Downloads/Calls_for_Service_2012.csv")
data3 <- read.csv("C:/Users/shant/Downloads/Calls_for_Service_2013.csv")
data4 <- read.csv("C:/Users/shant/Downloads/Calls_for_Service_2014.csv")
data5 <- read.csv("C:/Users/shant/Downloads/Calls_for_Service_2015.csv")
bigData <- rbind(data1, data2, data3, data4, data5)

options(digits = 11)

ct <- data.frame(table(bigData$Type_))

# Answer to Question 1 #
common = max(ct[,2])/sum(ct[,2])

bigData$TimeCreate = as.POSIXlt(bigData$TimeCreate, format="%m/%d/%Y %H:%M:%S %p")
bigData$TimeDispatch = as.POSIXlt(bigData$TimeDispatch, format="%m/%d/%Y %H:%M:%S %p")
bigData$TimeArrive = as.POSIXlt(bigData$TimeArrive, format="%m/%d/%Y %H:%M:%S %p")
bigData$TimeClosed = as.POSIXlt(bigData$TimeClosed, format="%m/%d/%Y %H:%M:%S %p")

bigData$response_time = bigData$TimeArrive - bigData$TimeDispatch

# Answer to Question 2 #
median_res_time = median(bigData$response_time, na.rm = T)

mrt_district = data.frame(PoliceDistrict = levels(as.factor(bigData$PoliceDistrict)), mrt = tapply(bigData$response_time, bigData$PoliceDistrict, mean, na.rm = T))

# Answer to Question 3 #
diff = max(mrt_district$mrt) - min(mrt_district$mrt)

new <- split(bigData, as.factor(bigData$PoliceDistrict))

pd0 <- as.data.frame(new[['0']])
pd1 <- as.data.frame(new[['1']])
pd2 <- as.data.frame(new[['2']])
pd3 <- as.data.frame(new[['3']])
pd4 <- as.data.frame(new[['4']])
pd5 <- as.data.frame(new[['5']])
pd6 <- as.data.frame(new[['6']])
pd7 <- as.data.frame(new[['7']])
pd8 <- as.data.frame(new[['8']])

t0 <- data.frame(pd = 0, table(pd0$Type_))
t1 <- data.frame(pd = 1, table(pd1$Type_))
t2 <- data.frame(pd = 2, table(pd2$Type_))
t3 <- data.frame(pd = 3, table(pd3$Type_))
t4 <- data.frame(pd = 4, table(pd4$Type_))
t5 <- data.frame(pd = 5, table(pd5$Type_))
t6 <- data.frame(pd = 6, table(pd6$Type_))
t7 <- data.frame(pd = 7, table(pd7$Type_))
t8 <- data.frame(pd = 8, table(pd8$Type_))


t0$cond <- t0$Freq/sum(t0$Freq)
t1$cond <- t1$Freq/sum(t1$Freq)
t2$cond <- t2$Freq/sum(t2$Freq)
t3$cond <- t3$Freq/sum(t3$Freq)
t4$cond <- t4$Freq/sum(t4$Freq)
t5$cond <- t5$Freq/sum(t5$Freq)
t6$cond <- t6$Freq/sum(t6$Freq)
t7$cond <- t7$Freq/sum(t7$Freq)
t8$cond <- t8$Freq/sum(t8$Freq)

un <- data.frame(table(bigData$Type_))

t0$uncond <- un$Freq/sum(un$Freq)
t1$uncond <- un$Freq/sum(un$Freq)
t2$uncond <- un$Freq/sum(un$Freq)
t3$uncond <- un$Freq/sum(un$Freq)
t4$uncond <- un$Freq/sum(un$Freq)
t5$uncond <- un$Freq/sum(un$Freq)
t6$uncond <- un$Freq/sum(un$Freq)
t7$uncond <- un$Freq/sum(un$Freq)
t8$uncond <- un$Freq/sum(un$Freq)


t_all = rbind(t1, t2, t3, t4, t5, t6, t7, t8)
t_all$req <- t_all$cond/t_all$uncond
t_all_req <- subset(t_all, Freq > 100)

# Answer to Question 4 #
max(t_all_req$req)

library(lubridate)

new <- aggregate(bigData[,18], by = list(as.factor(year(bigData$TimeCreate)), as.factor(bigData$Type_)), sum)
names(new) <- c('Year', 'Type', 'Freq');
new2 <- split(new, as.factor(new$Year))
y2011 <- as.data.frame(new2[['2011']])
y2015 <- as.data.frame(new2[['2015']])
new3 <- merge(y2011, y2015, by="Type",all=TRUE)
new3$Percent.Decrease = ((new3$Freq.x - new3$Freq.y)/new3$Freq.x)*100
i = which(new3$Percent.Decrease == max(new3$Percent.Decrease))

# Answer to Question 5 #
fraction = (new3$Freq.x[i] - new3$Freq.y[i])/sum(new3$Freq.x)

names(t_all) = c('PoliceDistrict', 'Type_', 'Freq.')

library(plyr)
rmv <- data.frame(gsub("\\(", " ", bigData$Location))
rmv <- data.frame(gsub("\\)", " ", rmv[,1]))
rmv <- data.frame(gsub("\\,", " ", rmv[,1]))
long = as.numeric(as.factor(substring(as.character(rmv[,1]),2,19)))
lat = as.numeric(as.factor(substring(as.character(rmv[,1]),22,399)))
newData = data.frame(PoliceDistrict = bigData$PoliceDistrict, Longitude = long, Latitude = lat)



