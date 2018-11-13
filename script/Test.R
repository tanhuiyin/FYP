#### 1. Preliminaries ####

library(openair)
library(Hmisc)
library(dplyr)

# This step is needed if the dataframes are named other than 'df'
# Just need to change the name of df here and the rest of the script
# would be the same.
df <- read.csv('FYP/data/completed_data.csv')
df <- df[,-1]
time_stamp <- as.POSIXct(df$date, format= "%Y-%m-%d %H:%M:%S")
df <- cbind(time_stamp,df)
df <- df[,-2]
summary(df)

### Month and Year Classification
Month_class <- NA 
for (i in 1:nrow(df)) {
  if (df$time_stamp[i] < as.POSIXct('2015-11-30 23:30:00',
                                    '%Y-%m-%d %H:%M:%S',
                                    tz = 'Asia/Kuala_Lumpur')){
    Month_class[i] <- 'Nov2015'
  }
  if (df$time_stamp[i] > as.POSIXct('2015-11-30 23:30:00',
                                    '%Y-%m-%d %H:%M:%S',
                                    tz = 'Asia/Kuala_Lumpur') &
      df$time_stamp[i] < as.POSIXct('2015-12-31 23:30:00',
                                    '%Y-%m-%d %H:%M:%S',
                                    tz = 'Asia/Kuala_Lumpur')){
    Month_class[i] <- 'Dec2015'
  }
  if (df$time_stamp[i] > as.POSIXct('2015-12-31 23:30:00',
                                    '%Y-%m-%d %H:%M:%S',
                                    tz = 'Asia/Kuala_Lumpur') &
      df$time_stamp[i] < as.POSIXct('2016-01-31 23:30:00',
                                    '%Y-%m-%d %H:%M:%S',
                                    tz = 'Asia/Kuala_Lumpur')){
    Month_class[i] <- 'Jan2016'
  }
  if (df$time_stamp[i] > as.POSIXct('2016-01-31 23:30:00',
                                    '%Y-%m-%d %H:%M:%S',
                                    tz = 'Asia/Kuala_Lumpur') &
      df$time_stamp[i] < as.POSIXct('2016-02-29 23:30:00',
                                    '%Y-%m-%d %H:%M:%S',
                                    tz = 'Asia/Kuala_Lumpur')){
    Month_class[i] <- 'Feb2016'
  }
  if (df$time_stamp[i] > as.POSIXct('2016-02-29 23:30:00',
                                    '%Y-%m-%d %H:%M:%S',
                                    tz = 'Asia/Kuala_Lumpur') &
      df$time_stamp[i] < as.POSIXct('2016-03-31 23:30:00',
                                    '%Y-%m-%d %H:%M:%S',
                                    tz = 'Asia/Kuala_Lumpur')){
    Month_class[i] <- 'Mar2016'
  }
  if (df$time_stamp[i] > as.POSIXct('2016-03-31 23:30:00',
                                    '%Y-%m-%d %H:%M:%S',
                                    tz = 'Asia/Kuala_Lumpur') &
      df$time_stamp[i] < as.POSIXct('2016-04-30 23:30:00',
                                    '%Y-%m-%d %H:%M:%S',
                                    tz = 'Asia/Kuala_Lumpur')){
    Month_class[i] <- 'Apr2016'
  }
  if (df$time_stamp[i] > as.POSIXct('2016-04-30 23:30:00',
                                    '%Y-%m-%d %H:%M:%S',
                                    tz = 'Asia/Kuala_Lumpur') &
      df$time_stamp[i] < as.POSIXct('2016-05-31 23:30:00',
                                    '%Y-%m-%d %H:%M:%S',
                                    tz = 'Asia/Kuala_Lumpur')){
    Month_class[i] <- 'May2016'
  }
  if (df$time_stamp[i] > as.POSIXct('2016-05-31 23:30:00',
                                    '%Y-%m-%d %H:%M:%S',
                                    tz = 'Asia/Kuala_Lumpur') &
      df$time_stamp[i] < as.POSIXct('2016-06-30 23:30:00',
                                    '%Y-%m-%d %H:%M:%S',
                                    tz = 'Asia/Kuala_Lumpur')){
    Month_class[i] <- 'Jun2016'
  }
  if (df$time_stamp[i] > as.POSIXct('2016-06-30 23:30:00',
                                    '%Y-%m-%d %H:%M:%S',
                                    tz = 'Asia/Kuala_Lumpur') &
      df$time_stamp[i] < as.POSIXct('2016-07-31 23:30:00',
                                    '%Y-%m-%d %H:%M:%S',
                                    tz = 'Asia/Kuala_Lumpur')){
    Month_class[i] <- 'Jul2016'
  }
  if (df$time_stamp[i] > as.POSIXct('2016-07-31 23:30:00',
                                    '%Y-%m-%d %H:%M:%S',
                                    tz = 'Asia/Kuala_Lumpur') &
      df$time_stamp[i] < as.POSIXct('2016-08-31 23:30:00',
                                    '%Y-%m-%d %H:%M:%S',
                                    tz = 'Asia/Kuala_Lumpur')){
    Month_class[i] <- 'Aug2016'
  }
  if (df$time_stamp[i] > as.POSIXct('2016-08-31 23:30:00',
                                    '%Y-%m-%d %H:%M:%S',
                                    tz = 'Asia/Kuala_Lumpur') &
      df$time_stamp[i] < as.POSIXct('2016-09-30 23:30:00',
                                    '%Y-%m-%d %H:%M:%S',
                                    tz = 'Asia/Kuala_Lumpur')){
    Month_class[i] <- 'Sep2016'
  }
  if (df$time_stamp[i] > as.POSIXct('2016-09-30 23:30:00',
                                    '%Y-%m-%d %H:%M:%S',
                                    tz = 'Asia/Kuala_Lumpur') &
      df$time_stamp[i] < as.POSIXct('2016-10-31 23:30:00',
                                    '%Y-%m-%d %H:%M:%S',
                                    tz = 'Asia/Kuala_Lumpur')){
    Month_class[i] <- 'Oct2016'
  }
  if (df$time_stamp[i] > as.POSIXct('2016-10-31 23:30:00',
                                    '%Y-%m-%d %H:%M:%S',
                                    tz = 'Asia/Kuala_Lumpur') &
      df$time_stamp[i] < as.POSIXct('2016-11-30 23:30:00',
                                    '%Y-%m-%d %H:%M:%S',
                                    tz = 'Asia/Kuala_Lumpur')){
    Month_class[i] <- 'Nov2016'
  }
  if (df$time_stamp[i] > as.POSIXct('2016-11-30 23:30:00',
                                    '%Y-%m-%d %H:%M:%S',
                                    tz = 'Asia/Kuala_Lumpur') &
      df$time_stamp[i] < as.POSIXct('2016-12-31 23:30:00',
                                    '%Y-%m-%d %H:%M:%S',
                                    tz = 'Asia/Kuala_Lumpur')){
    Month_class[i] <- 'Dec2016'
  }
  if (df$time_stamp[i] > as.POSIXct('2016-12-31 23:30:00',
                                    '%Y-%m-%d %H:%M:%S',
                                    tz = 'Asia/Kuala_Lumpur') &
      df$time_stamp[i] < as.POSIXct('2017-01-31 23:30:00',
                                    '%Y-%m-%d %H:%M:%S',
                                    tz = 'Asia/Kuala_Lumpur')){
    Month_class[i] <- 'Jan2017'
  }
  if (df$time_stamp[i] > as.POSIXct('2017-01-31 23:30:00',
                                    '%Y-%m-%d %H:%M:%S',
                                    tz = 'Asia/Kuala_Lumpur') &
      df$time_stamp[i] < as.POSIXct('2017-02-28 23:30:00',
                                    '%Y-%m-%d %H:%M:%S',
                                    tz = 'Asia/Kuala_Lumpur')){
    Month_class[i] <- 'Feb2017'
  }
  if (df$time_stamp[i] > as.POSIXct('2017-2-28 23:30:00',
                                    '%Y-%m-%d %H:%M:%S',
                                    tz = 'Asia/Kuala_Lumpur') &
      df$time_stamp[i] < as.POSIXct('2017-03-31 23:30:00',
                                    '%Y-%m-%d %H:%M:%S',
                                    tz = 'Asia/Kuala_Lumpur')){
    Month_class[i] <- 'Mac2017'
  }
  if (df$time_stamp[i] > as.POSIXct('2017-3-31 23:30:00',
                                    '%Y-%m-%d %H:%M:%S',
                                    tz = 'Asia/Kuala_Lumpur') &
      df$time_stamp[i] < as.POSIXct('2017-04-30 23:30:00',
                                    '%Y-%m-%d %H:%M:%S',
                                    tz = 'Asia/Kuala_Lumpur')){
    Month_class[i] <- 'Apr2017'
  }
  if (df$time_stamp[i] > as.POSIXct('2017-04-30 23:30:00',
                                    '%Y-%m-%d %H:%M:%S',
                                    tz = 'Asia/Kuala_Lumpur') &
      df$time_stamp[i] < as.POSIXct('2017-05-31 23:30:00',
                                    '%Y-%m-%d %H:%M:%S',
                                    tz = 'Asia/Kuala_Lumpur')){
    Month_class[i] <- 'May2017'
  }
  if (df$time_stamp[i] > as.POSIXct('2017-05-31 23:30:00',
                                    '%Y-%m-%d %H:%M:%S',
                                    tz = 'Asia/Kuala_Lumpur') &
      df$time_stamp[i] < as.POSIXct('2017-06-30 23:30:00',
                                    '%Y-%m-%d %H:%M:%S',
                                    tz = 'Asia/Kuala_Lumpur')){
    Month_class[i] <- 'Jun2017'
  }
  if (df$time_stamp[i] > as.POSIXct('2017-06-30 23:30:00',
                                    '%Y-%m-%d %H:%M:%S',
                                    tz = 'Asia/Kuala_Lumpur') &
      df$time_stamp[i] < as.POSIXct('2017-07-31 23:30:00',
                                    '%Y-%m-%d %H:%M:%S',
                                    tz = 'Asia/Kuala_Lumpur')){
    Month_class[i] <- 'Jul2017'
  }
  if (df$time_stamp[i] > as.POSIXct('2017-07-31 23:30:00',
                                    '%Y-%m-%d %H:%M:%S',
                                    tz = 'Asia/Kuala_Lumpur') &
      df$time_stamp[i] < as.POSIXct('2017-08-31 23:30:00',
                                    '%Y-%m-%d %H:%M:%S',
                                    tz = 'Asia/Kuala_Lumpur')){
    Month_class[i] <- 'Aug2017'
  }
  if (df$time_stamp[i] > as.POSIXct('2017-08-31 23:30:00',
                                    '%Y-%m-%d %H:%M:%S',
                                    tz = 'Asia/Kuala_Lumpur') &
      df$time_stamp[i] < as.POSIXct('2017-09-30 23:30:00',
                                    '%Y-%m-%d %H:%M:%S',
                                    tz = 'Asia/Kuala_Lumpur')){
    Month_class[i] <- 'Sep2017'
  }
  if (df$time_stamp[i] > as.POSIXct('2017-09-3 23:30:00',
                                    '%Y-%m-%d %H:%M:%S',
                                    tz = 'Asia/Kuala_Lumpur') &
      df$time_stamp[i] < as.POSIXct('2017-10-31 23:30:00',
                                    '%Y-%m-%d %H:%M:%S',
                                    tz = 'Asia/Kuala_Lumpur')){
    Month_class[i] <- 'Oct2017'
  }
}
Month_class <- as.factor(Month_class)

df <- cbind(df,Month_class)
df$Month_class <- factor(df$Month_class, levels = c('Nov2015', 'Dec2015',
                                                    'Jan2016', 'Feb2016',
                                                    'Mar2016', 'Apr2016',
                                                    'May2016', 'Jun2016',
                                                    'Jul2016', 'Aug2016',
                                                    'Sep2016', 'Oct2016',
                                                    'Nov2016', 'Dec2016',
                                                    'Jan2017', 'Feb2017',
                                                    'Mar2017', 'Apr2017',
                                                    'May2017', 'Jun2017',
                                                    'Jul2017', 'Aug2017',
                                                    'Sep2017', 'Oct2017'))

## Export into csv
write.csv(df, file = 'df.csv')

#### Correlational analysis and plots between CO2 and environmental drivers ####
# Sat_SST and Sea surface temperature
path_fig <- file.path('/Users/tanhuiyin/FYP/figs/SSTandCO2.jpg')
jpeg(file=path_fig,width=16,height=8,res=400, units = 'cm')
par(family='Times', mfrow = c(2,2))
par(mar = c(3,3,0.5,0))
plot(df$TS_0_0_1,
     df$co2_flux, pch = 16,
     xlab = '', ylab = '',
     xlim= c(22,33), ylim = c(-3,3), col = alpha('red',0.2), yaxt = 'n')
points(df$TS_0_0_1,
       df$co2_flux, pch = 16,
       col = alpha('blue',0.2))
axis(side = 2, at = c(-1,-0.5,0,0.5,1), 
     labels = c(paste('\u2212',1,sep=''),paste('\u2212',0.5,sep=''),0,0.5,1))
minor.tick(4)
mtext(expression(paste('CO'['2'],' flux')),side = 2,line=2)
mtext(expression('SST'['Sat']),side = 1,line=2)
lmT <- lm(df$co2_flux ~ 
            df$TS_0_0_1)
lmTenso <- lm(df$co2_flux ~ 
                df$TS_0_0_1)
lmTxenso <- lm(df$co2_flux ~ 
                 df$TS_0_0_1)
abline(lmTenso, col = 'red', lwd = 3, lty = 2)
abline(lmTxenso, col = 'blue', lwd = 3, lty = 2)
abline(lmT, col = 'black', lwd = 3, lty = 2)
summary(lmT)
summary(lmTenso)
summary(lmTxenso)


# CHL and CO2_flux
par(mar = c(3,2.5,0.5,0.5))
plot(df$CHL,
     df$co2_flux, pch = 16,
     xlab = '', ylab = '',
     xlim= c(0,25.4.5), ylim = c(-3,3), col = alpha('red',0.2),yaxt = 'n')

points(df$CHL,
       df$co2_flux, pch = 16,
       col = alpha('blue',0.2))
axis(side = 2, at = c(-1,-0.5,0,0.5,1), 
     labels = c(paste('\u2212',1,sep=''),paste('\u2212',0.5,sep=''),0,0.5,1))

mtext('U',side = 1,line=2)
minor.tick()
lmU <- lm(df$co2_flux ~ 
            df$CHL)
lmUenso <- lm(df$co2_flux ~ 
                df$CHL)
lmUxenso <- lm(df$co2_flux ~ 
                 df$CHL)
abline(lmUenso, col = 'red', lwd = 3, lty = 2)
abline(lmUxenso, col = 'blue', lwd = 3, lty = 2)
abline(lmU, col = 'black', lwd = 3, lty = 2)
summary(lmU)
summary(lmUenso)
summary(lmUxenso)

# CHL and CO2_flux
par(mar = c(3,2.5,0.5,0.5))
plot(df$CHL,
     df$co2_flux, pch = 16,
     xlab = '', ylab = '',
     xlim= c(0,25.4.5), ylim = c(-3,3), col = alpha('red',0.2),yaxt = 'n')

points(df$CHL,
       df$co2_flux, pch = 16,
       col = alpha('blue',0.2))
axis(side = 2, at = c(-1,-0.5,0,0.5,1), 
     labels = c(paste('\u2212',1,sep=''),paste('\u2212',0.5,sep=''),0,0.5,1))

mtext('U',side = 1,line=2)
minor.tick()
lmU <- lm(df$co2_flux ~ 
            df$CHL)
lmUenso <- lm(df$co2_flux ~ 
                df$CHL)
lmUxenso <- lm(df$co2_flux ~ 
                 df$CHL)
abline(lmUenso, col = 'red', lwd = 3, lty = 2)
abline(lmUxenso, col = 'blue', lwd = 3, lty = 2)
abline(lmU, col = 'black', lwd = 3, lty = 2)
summary(lmU)
summary(lmUenso)
summary(lmUxenso)
dev.off()

# Net radiation
path_fig <- file.path('/Users/Yusri/Documents/Work/Data_analysis/muka_head/figs/RNcor.jpg')
jpeg(file=path_fig,width=8,height=8,res=400, units = 'cm')
par(family='Times')
par(mar = c(4.1,4.1,0.5, 0.5))
plot(df$RN_1_1_1[which(indexCO2_5 == TRUE)],
     df$co2_flux[which(indexCO2_5 == TRUE)], pch = 19,
     xlab = 'RN', ylab = expression(paste('CO'['2'],' flux')),
     xlim= c(-100,300), ylim = c(-1,1), col = 'grey60')
axis(side = 2, at = c(-0.2,0), labels = c(-0.2,0))
minor.tick()
lmRN <- lm(df$co2_flux[which(indexCO2_5 == TRUE)] ~ 
             df$RN_1_1_1[which(indexCO2_5 == TRUE)])
lmRNenso <- lm(df$co2_flux[which(indexCO2_5_enso == TRUE)] ~ 
                 df$RN_1_1_1[which(indexCO2_5_enso == TRUE)])
lmRNxenso <- lm(df$co2_flux[which(indexCO2_5_xenso == TRUE)] ~ 
                  df$RN_1_1_1[which(indexCO2_5_xenso == TRUE)])
abline(lmRN, col = 'green', lwd = 3, lty = 2)
abline(lmRNenso, col = 'red', lwd = 3, lty = 2)
abline(lmRNxenso, col = 'blue', lwd = 3, lty = 2)
summary(lmRN)
summary(lmRNenso)
summary(lmRNxenso)
dev.off()


