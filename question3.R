
data <- read.csv("C:/Users/shant/Downloads/NYPD_Motor_Vehicle_Collisions.csv", na.strings=c(""," ","NA"))
data$CASUALTIES <- rowSums(data[,11:18])
p <- ggplot(data, aes(VEHICLE.TYPE.CODE.1, log(CASUALTIES + 1))) + geom_boxplot() + theme(axis.text.x  = element_text(angle=90, vjust=0.5)) + ggtitle("Casualties vs. Vehicle Type") + labs(x="Vehicle Type", y="Casualties (on log scale)")
p2 <- ggplot(data, aes(VEHICLE.TYPE.CODE.2, log(CASUALTIES + 1))) + geom_boxplot()
data3 <- aggregate(data2$VEHICLE.TYPE.CODE.1, by = data2$CASUALTIES, FUN = sum)
data3 <- data.frame(weekdays(as.POSIXlt(data$DATE, format="%m/%d/%Y")), data$CASUALTIES)
p3 <- ggplot(data3, aes(Weekday, Casualties)) + geom_boxplot()
data$deaths <- rowSums(data[,c(12,14,16,18)])
p <- ggplot(data, aes(CONTRIBUTING.FACTOR.VEHICLE.1, log(CASUALTIES + 1))) + geom_boxplot() + theme(axis.text.x  = element_text(angle=90, vjust=0.5)) + ggtitle("Casualties vs. Factor") + labs(x="Factor Type", y="Casualties (on log scale)")

zip <- data[,c(4,30)]
zip2 <- aggregate(CASUALTIES~ZIP.CODE, zip, sum)
zip3 <- data.frame(table(as.factor(data$ZIP.CODE)))
zip2$Freq <- zip3[,2]
zip2$Ratio <- zip2[,2]/zip2[,3]
zip4 <- zip2[ order(-zip2[,4]),]



factor <- data[,c(4,19)]
library(reshape2)
factor2 <- dcast(factor, zip~factor, length)
factor3 <- log(factor2[,2:49] + 1)
names(factor3) <- strtrim(make.names(names(factor3), unique = T),23)
DM <- dist(t(factor3))
hc <- hclust(DM, method = "average")
plot(hc)


names(datan2) <- strtrim(make.names(names(datan2), unique = T),23)
datan <- log(datan2[,2:49] + 1)
DM <- dist(t(datan))
hc <- hclust(DM, method = "average")
plot(hc)

newD <- data.frame(zip = data[,4], vehicle = data[,25], casualties = data[,30])
newD2 <- dcast(newD, zip~vehicle, value.var = "casualties", sum)
newD3 <- log(newD2[,2:19] + 1)
DM <- dist(t(newD3))
hc <- hclust(DM)
plot(hc)

nload <- load[with(load, order(Comp.1)), ]


map <- data.frame(factor = data[,19], injuries = rowSums(data[,c(11,13,15,17)]), deaths = rowSums(data[,c(12,14,16,18)]))
map$No.of.accidents <- 1
map2 <- aggregate(map[,2:4], by = list(map$factor), sum)
map2$Casualties.per.accident <- (map2[,2] + map2[,3])/map2[,4]
p3 <- ggplot(map, aes(factor, log(injuries + 1))) + geom_boxplot() + theme(axis.text.x  = element_text(angle=90, vjust=0.5))
mpa$cluster <- 1


cluster1 <- map2$factor %in% c('Accelerator Defective',
'Animals Action',
'Cell Phone (hand-held)',
'Cell Phone (hands-free)',
'Drugs (Illegal)',
'Fell Asleep',
'Following Too Closely',
'Headlights Defective',
'Lane Marking Improper/Inadequate',
'Other Lighting Defects',
'Passing or Lane Usage Improper',
'Pavement Defective',
'Pedestrian/Bicyclist/Other Pedestrian Error/Confusion',
'Shoulders Defective/Improper',
'Steering Failure',
'Tire Failure/Inadequate',
'Tow Hitch Defective',
'Traffic Control Device Improper/Non-Working',
'Unsafe Lane Changing',
'Unsafe Speed',
'Windshield Inadequate');

map2[cluster1,]$cluster <- 'Cluster 1';

cluster2 <- map2$factor %in% c('Aggressive Driving/Road Rage',
'Brakes Defective',
'Failure to Keep Right',
'Glare',
'Illness',
'Other Electronic Device',
'Oversized Vehicle',
'Passenger Distraction',
'Reaction to Other Uninvolved Vehicle',
'View Obstructed/Limited');

map2[cluster2,]$cluster <- 'Cluster 2';

cluster3 <- map2$factor %in% c('Alcohol Involvement',
'Backing Unsafely',
'Driver Inattention/Distraction',
'Driver Inexperience',
'Failure to Yield Right-of-Way',
'Fatigued/Drowsy',
'Lost Consciousness',
'Obstruction/Debris',
'Other Vehicular',
'Outside Car Distraction',
'Pavement Slippery',
'Physical Disability',
'Prescription Medication',
'Traffic Control Disregarded',
'Turning Improperly',
'Unspecified')

map2[cluster3,]$cluster <- 'Cluster 3';

map2$cause <- 1
map2[cluster3,]$cause <- 'Drivers Fault';
map2[cluster2,]$cause <- 'Not Drivers Fault';
map2[cluster1,]$cause <- 'Not Drivers Fault';
p3 <- ggplot(map2, aes(factor, log(injuries + 1))) + geom_boxplot(aes(fill = cause)) + theme(axis.text.x  = element_text(angle=90, vjust=0.5))

map4$cluster <- 1
map4[map4$Group.1 %in% c('Alcohol Involvement',
'Backing Unsafely',
'Driver Inattention/Distraction',
'Driver Inexperience',
'Failure to Yield Right-of-Way',
'Fatigued/Drowsy',
'Lost Consciousness',
'Obstruction/Debris',
'Other Vehicular',
'Outside Car Distraction',
'Pavement Slippery',
'Physical Disability',
'Prescription Medication',
'Traffic Control Disregarded',
'Turning Improperly',
'Unspecified'),]$cluster = "Cluster 3"

map4[map4$Group.1 %in% c('Aggressive Driving/Road Rage',
'Brakes Defective',
'Failure to Keep Right',
'Glare',
'Illness',
'Other Electronic Device',
'Oversized Vehicle',
'Passenger Distraction',
'Reaction to Other Uninvolved Vehicle',
'View Obstructed/Limited'),]$cluster = "Cluster 2"

map4[map4$Group.1 %in% c('Accelerator Defective',
'Animals Action',
'Cell Phone (hand-held)',
'Cell Phone (hands-free)',
'Drugs (Illegal)',
'Fell Asleep',
'Following Too Closely',
'Headlights Defective',
'Lane Marking Improper/Inadequate',
'Other Lighting Defects',
'Passing or Lane Usage Improper',
'Pavement Defective',
'Pedestrian/Bicyclist/Other Pedestrian Error/Confusion',
'Shoulders Defective/Improper',
'Steering Failure',
'Tire Failure/Inadequate',
'Tow Hitch Defective',
'Traffic Control Device Improper/Non-Working',
'Unsafe Lane Changing',
'Unsafe Speed',
'Windshield Inadequate'),]$cluster = "Cluster 1"

