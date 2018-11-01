##import both in-situ and satellite database 

sat <-read.csv('data/sat_data.csv')
muka <-read.csv('data/muka_head_data.csv')
rain <- read.csv('data/precipitation_data.csv')

##Change the date format
# For the muka head data
date <- as.POSIXct(muka$daytime, format = "%d/%m/%Y %H:%M", tz = "Asia/Kuala_Lumpur")
muka <- cbind(date,muka)
muka <- muka[,-2]

# For the satellite data
date <- as.POSIXct(sat$DATE, format = "%Y-%m-%dT%H:%M:%S", tz = "GMT")
date <- as.POSIXct(sat$DATE, format = "%Y-%m-%dT%H:%M:%S", tz = "Asia/Kuala_Lumpur")
sat <- cbind(date,sat)
sat <- sat[,-2]
# Replace sat's nill value with NA
sat[sat==-32767]<-NA

# For the precipitation data
date <- as.POSIXct(rain$Time, format = "%Y-%m-%d %H:%M:%S")
date <- as.POSIXct(rain$Time, format = "%Y-%m-%d %H:%M:%S", tz = "Asia/Kuala_Lumpur")
rain <- cbind(date,rain)


# Filter and quality check the data
# Remove the column TA_0_0_1 and rain
muka <- muka[,-c(2,4,5)]
rain <- rain[,-c(2,3,4,5,6,7,8,9,10,11,13,14,16,17,18)]


# Remove problematic TS_0_0_1 values
muka$TS_0_0_1[19050:19200] <- NA
muka$TS_0_0_1[19050:19200] <- NA
muka$TS_0_0_1[muka$TS_0_0_1 < 22] <- NA
muka$TS_0_0_1[28415:29150] <- NA
muka$TS_0_0_1[27250:28500] <- NA
plot(muka$TS_0_0_1)

## Standardise the number of column of both data with half an hour per interval 
library(openair)
## Standardise the sat data
names(sat)[1] <- "date"
temp <- timeAverage(sat, avg.time = "30 min", start.date = "2015-11-02 02:30:00", interval = "30 min")

## Standardise the rain data
temp1 <- timeAverage(rain, avg.time = "30 min", start.date = "2015-11-12 00:30:00", interval = "30 min")


##Merge the both sets of data 
merged_df <- merge(muka, temp, by = c("date","date"))
merged_df2 <- merge(merged_df,temp1,by=c("date","date"))

#based on wind direction and qc, remove the unwanted co2_flux value
merged_df2$co2_flux[merged_df2$wind_dir>90]<- NA
merged_df2$co2_flux[merged_df2$qc_co2_flux == 2]<- NA
merged_df2$co2_flux[merged_df2$HourlyPrecipMM>0]<- NA
merged_df2$co2_flux[30500:31000] <- NA
merged_df2$co2_flux[merged_df2$co2_flux>0.5]<- NA
merged_df2$co2_flux[merged_df2$co2_flux< -0.5]<- NA
plot(merged_df2$date,merged_df2$co2_flux)
plot(merged_df2$co2_flux)

##Export quality data as csv
write.csv(merged_df2, file="completed_data.csv")

## Read the combined data for data analysis
combined_data <-read.csv('completed_data.csv')
combined_data <- combined_data[,-1]
date <- as.POSIXct(combined_data$date, format= "%Y-%m-%d %H:%M:%S")
combined_data <- cbind(date,combined_data)
combined_data <- combined_data[,-2]
summary(combined_data)


##Pearson correlation test
cor.test(combined_data$TS_0_0_1,combined_data$co2_flux)
cor.test(combined_data$TS_0_0_1,combined_data$SST)
cor.test(combined_data$TS_0_0_1,combined_data$POC)
cor.test(combined_data$TS_0_0_1,combined_data$PIC)
cor.test(combined_data$TS_0_0_1,combined_data$CHL)
cor.test(combined_data$TS_0_0_1,combined_data$PAR)
cor.test(combined_data$SST,combined_data$POC)
cor.test(combined_data$SST,combined_data$PIC)
cor.test(combined_data$SST,combined_data$PAR)
cor.test(combined_data$PAR,combined_data$co2_flux)
cor.test(combined_data$SST,combined_data$CHL)
cor.test(combined_data$SST,combined_data$TS_0_0_1)
cor.test(combined_data$CHL,combined_data$POC)
cor.test(combined_data$CHL,combined_data$PIC)
cor.test(combined_data$CHL,combined_data$PAR)
cor.test(combined_data$CHL,combined_data$co2_flux)
cor.test(combined_data$CHL,combined_data$SST)
cor.test(combined_data$CHL,combined_data$TS_0_0_1)
cor.test(combined_data$POC,combined_data$CHL)
cor.test(combined_data$POC,combined_data$PIC)
cor.test(combined_data$POC,combined_data$PAR)
cor.test(combined_data$POC,combined_data$co2_flux)
cor.test(combined_data$POC,combined_data$SST)
cor.test(combined_data$PAR,combined_data$SST)
cor.test(combined_data$co2_flux,combined_data$CHL)

## Plotting the time series trends
library("reshape2")
library("ggplot2")
ggplot(combined_data, aes(date))+geom_smooth(aes(y=SST,color="SST")) + geom_smooth(aes(y=TS_0_0_1, color="TS_0_0_1"))
ggplot(combined_data, aes(date)) + geom_smooth(aes(y=TS_0_0_1)) 
ggplot(combined_data, aes(date)) + geom_smooth(aes(y=CHL))
ggplot(combined_data, aes(date)) + geom_smooth(aes(y=POC))
ggplot(combined_data, aes(date)) + geom_smooth(aes(y=PIC))
ggplot(combined_data, aes(date)) + geom_smooth(aes(y=PAR))                                                                                          
ggplot(combined_data, aes(date)) + geom_smooth(aes(y=SST))                                                                                           
ggplot(combined_data, aes(date)) + geom_smooth(aes(y=co2_flux))  
ggplot(combined_data, aes(date))+geom_line(aes(y=SST,color="SST")) + geom_line(aes(y=TS_0_0_1, color="TS_0_0_1"))
ggplot(combined_data, aes(date)) + geom_line(aes(y=TS_0_0_1)) 
ggplot(combined_data, aes(date)) + geom_line(aes(y=CHL))
ggplot(combined_data, aes(date)) + geom_line(aes(y=POC))
ggplot(combined_data, aes(date)) + geom_line(aes(y=PIC))
ggplot(combined_data, aes(date)) + geom_line(aes(y=PAR))                                                                                          
ggplot(combined_data, aes(date)) + geom_line(aes(y=SST))                                                                                           
ggplot(merged_df2, aes(date)) + geom_smooth(aes(y=co2_flux)) 
