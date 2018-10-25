##import both in-situ and satellite database 
library(readxl)
s<-read.csv(file.choose())
m<-read.csv(file.choose())

##Change the date format
date <- as.POSIXct(m$daytime, format = "%d/%m/%Y %H:%M", tz = "Asia/Kuala_Lumpur")
m <- cbind(date,m)
m <- m[,-2]

date <- as.POSIXct(s$DATE, format = "%Y-%m-%dT%H:%M:%S", tz = "GMT")
date <- as.POSIXct(s$DATE, format = "%Y-%m-%dT%H:%M:%S", tz = "Asia/Kuala_Lumpur")
s <- cbind(date,s)
s <- s[,-2]

## Standardise the number of column of both data with half an hour per interval 
library(openair)
names(s)[1] <- "date"
temp <- timeAverage(s, avg.time = "30 min", start.date = "2015-11-02 02:30:00", interval = "30 min")

##Merge the both sets of data 
merged_df <- merge(m, temp, by = c("date","date"))
View(merged_df)

## Change the nil data to NA
merged_df[merged_df==-32767]<-NA

##Boxplot for checking the on-site outliers
boxplot.stats(merged_df2$co2_flux)
boxplot.stats(merged_df2$TS_0_0_1)

#based on wind direction and qc, remove the unwanted
merged_df2 <- merged_df
merged_df2$co2_flux[merged_df2$wind_dir > 90 & merged_df2$qc_co2_flux == 2] <- NA
merged_df2$co2_flux[merged_df2$co2_flux < -0.00336243] <- NA
merged_df2$co2_flux[merged_df2$co2_flux > 0.55644058] <- NA
merged_df2$TS_0_0_1[merged_df2$TS_0_0_1 < 23] <- NA
merged_df2$TS_0_0_1[merged_df2$TS_0_0_1 > 32] <- NA


## Write the data into .csv format
write.csv(merged_df2, file="combined_data.csv")
combined_data <- file.choose()
m <- cbind(date,m)

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

## sensitivity test
library(tidyverse)
library(lubridate)
combined_data2<-combined_data
combined_data2$month<-as.Date(combined_data2$date,format="%m")
combined_data2$year<-as.Date(combined_data2$date,format="%Y")
y <-combined_data2 %>% group_by(month = floor_date(date, "7 days")) %>% summarise(amount = mean(combined_data2$SST))
combined_data2 %>% group_by(month = floor_date(date, "month")) %>% summarise(amount = sum(combined_data2$SST))
combined_data2 %>% group_by(month = floor_date(date, "year")) %>% summarise(amount = mean(combined_data2$SST))
