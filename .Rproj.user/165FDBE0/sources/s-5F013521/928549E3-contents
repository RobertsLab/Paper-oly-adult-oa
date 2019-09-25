
#--------- Temperature during hatchery phase of experiment  

HOBO.data.temp <- read.csv("Data/temp-treatment-data.csv", header=TRUE, stringsAsFactors = FALSE)[,c(1:3)]
HOBO.data.temp$Time.Point <- as.POSIXct(HOBO.data.temp$Time.Point,
         format = "%m/%d/%Y %H:%M")

# When did the hobos go into the water?  Olys were in treatment Dec. 6th -> Feb. 5th. First temp. treatment reading @ Dec. 6 @ 13:30.
plot(x=HOBO.data.temp$Time.Point, y=HOBO.data.temp$Cold.temp)
plot(x=HOBO.data.temp$Time.Point, y=HOBO.data.temp$Ambient.temp)
temp.treat <- subset(HOBO.data.temp, Time.Point > "2016-12-21 13:00:00" & Time.Point < "2017-02-04 00:00:00")

# replot data
plot(x=temp.treat$Time.Point, y=temp.treat$Cold.temp) #ambient temp over time 
plot(x=temp.treat$Time.Point, y=temp.treat$Ambient.temp) #chilled temp over time 

# pull mean and sd 
summary(temp.treat$Cold.temp) #average temp in chilled = 6.101
sd(na.omit(temp.treat$Cold.temp)) #sd temp in chilled =0.2318497 
summary(temp.treat$Ambient.temp) #average temp in ambient = 10.242
sd(na.omit(temp.treat$Ambient.temp)) #sd temp in ambient =0.5179633

# compare temperatures using t-test 
t.test(x=temp.treat$Cold.temp, y=temp.treat$Ambient.temp, alternative = "two.sided")
#p=0

#-------- Temperature during pH treatment 

HOBO.data.pH <- read.csv("Data/pH-treatment-temp-data.csv", header=TRUE, stringsAsFactors = FALSE)[,c(1:7)]
HOBO.data.pH$Time.Point <- as.POSIXct(HOBO.data.pH$Time.Point,
                                        format = "%m/%d/%Y %H:%M")
# pH treatment was from Feb. 15 - April 08
pH.treat <- subset(HOBO.data.pH, Time.Point > "2017-02-15 00:00:00" & Time.Point < "2017-04-09 00:00:00")

# When did the hobos go into the water?  Olys were in treatment Dec. 6th -> Feb. 5th. First temp. treatment reading @ Dec. 6 @ 13:30.
par(mfrow=c(2,3))
for( i in 2:7) {
  plot(x=pH.treat$Time.Point, y=pH.treat[,i], main=names(pH.treat)[i], xlab="Date", ylab="Temperature during pH Treat")
}

# tank-specific mean and sd 
apply(pH.treat[-1], 2, mean) #column means = average temp during pH exp. 
apply(pH.treat[-1], 2, sd) #column sd = sd during pH exp. 

# mean and sd temperature for each treatment (across 3 tanks each )
mean(as.vector(as.matrix(pH.treat[,c("T1.low", "T2.low", "T3.low")]))) #mean across low pH tanks 
sd(as.vector(as.matrix(pH.treat[,c("T1.low", "T2.low", "T3.low")]))) #sd across low pH tanks 
mean(as.vector(as.matrix(pH.treat[,c("T4.amb", "T5.amb", "T6.amb")]))) #mean across ambient pH tanks 
sd(as.vector(as.matrix(pH.treat[,c("T4.amb", "T5.amb", "T6.amb")]))) #sd across ambient pH tanks 

# plot temp data during pH treatments 
pH.treat.melted <- melt(data=pH.treat, id.vars = "Time.Point")
pH.treat.melted$ph <- as.factor(substr(pH.treat.melted$variable, 4,6))
plot(pH.treat.melted$value ~ pH.treat.melted$variable)

# Import temperatures from spawning buckets, calculate mean and sd for each and then average across all 

files <- list.files("Data/temps-spawning-buckets", pattern = "\\.csv") 
spawning.temps <- vector("list", length(files))
names(spawning.temps) <- files
for (i in 1:length(files)) {
  spawning.temps[[i]] <- read.csv(paste("Data/temps-spawning-buckets/", files[i], sep=""), stringsAsFactors = FALSE, header=FALSE)[-1:-2,2:3]
}

for (i in 1:length(files)) {
  names(spawning.temps[[i]]) <- c("Time.Point", "temperature")
  spawning.temps[[i]]$Time.Point <- as.POSIXct(spawning.temps[[i]]$Time.Point,format = "%m/%d/%y %H:%M:%S")
  spawning.temps[[i]]$temperature <- as.numeric(spawning.temps[[i]]$temperature)
  
}

# Mean temperature in each bucket 
for (i in 1:length(files)) {
  print(paste(files[i], round(mean(na.omit(spawning.temps[[i]][,2])), 2), sep=" = "))  
}

# SD temperature in each bucket 
for (i in 1:length(files)) {
  print(paste(files[i], round(sd(na.omit(spawning.temps[[i]][,2])), 2), sep=" = "))  
}

# create a vector of average temperatures for each bucket 
mean.temp <- vector()
for (i in 1:length(files)) {
  mean.temp[i] <- mean(na.omit(spawning.temps[[i]][,2]))
}

mean(mean.temp) #mean of the mean temps 
sd(mean.temp) #sd of the mean temps
