#open MH and Wave data
library(readxl)
m<-read.csv(file.choose())
w<-read.csv(file.choose())

#change the time to be same
#Muka Head Data (m)
date <- as.POSIXct(m$date, format = "%d/%m/%Y %H:%M", tz = "Asia/Kuala_Lumpur")
m <- cbind(date,m)
m <- m[,-2]

#Wave Data(w)
date <- as.POSIXct(w$time1, format = "%Y-%m-%d %H:%M:%S", tz = "GMT")
date <- as.POSIXct(w$time1, format = "%Y-%m-%d %H:%M:%S", tz = "Asia/Kuala_Lumpur")
w <- cbind(date,w)
w <- w[,-6]

#Merge 2 data
library(openair)
names(w)[1] <- "date"
temp <- timeAverage(w, avg.time = "30 min", start.date = "2015-11-03 11:09:00", interval = "30 min")
merged_df <- merge(m, temp, by = c("date","date"))

#Write in csv
write.csv(merged_df,file="merged_data_wt.csv")


#Data analysis
#Time Series
#Boxplot

plot(merged_df$swh1,merged_df$LE, pch=19, col = "green", ylim=c(-20,20))
plot(merged_df$swh1,merged_df$co2_flux, pch=19, col = "blue", xlim=c(0,2), ylim=c(-1,1))

#Filter data
#Create temp data frame
merged_df2 <- merged_df
#based on wind direction
merged_df2$co2_flux[merged_df2$dir..0.90. > 90 & merged_df2$qc_co2_flux == 2] <- NA
merged_df2$LE[merged_df2$dir..0.90. > 90 & merged_df2$qc_LE == 2] <- NA



# co2_flux <- merged_df2$co2_flux[merged_df2$dir..0.90. <= 90 & 
#                                   merged_df2$qc_co2_flux != 2]
# swh <-merged_df2$swh1[merged_df2$dir..0.90. <= 90 & 
#                         merged_df2$qc_co2_flux != 2]
# LE <- 

filter_data <-
