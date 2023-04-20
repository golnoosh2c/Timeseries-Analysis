library(dplyr)
library(imputeTS)
library(forecast)

setwd("C:/Lamar_University/Spring_2023/Machine_Learning/Project_5")
j <- read.csv("j17.csv")
j$Date_Parsed <- as.Date(j$Date, format = "%m/%d/%Y")  #convert to date
bgn <- j$Date_Parsed[length(j$Date_Parsed)]
end <- j$Date_Parsed[1]
SEQ <- seq.Date(bgn, end, "day")

length(SEQ) - length(j$Date_Parsed)
sum(is.na(j$WaterLevelElevation))

# Summary statistics before filling missing values
summary(j$WaterLevelElevation)

j <- j %>% 
  mutate(WaterLevelElevation_filled = na_kalman(WaterLevelElevation))

j$WaterLevelElevation_filled <- round(j$WaterLevelElevation_filled, digits = 1)

TS <- ts(j$WaterLevelElevation_filled, frequency = 365)

# Summary statistics
summary(TS)

# Decompose time series
decomp_ts <- stl(TS, s.window="periodic")

# Plot decomposed time series
plot(decomp_ts)

# Plot ACF
acf(TS)

# Plot PACF
pacf(TS)


j$D30 <- ma(TS, order=30) #30 Day Moving Avg
j$D90 <- ma(TS, order=90) #90 Day Moving Avg
j$D365 <- ma(TS, order=365) #365 Day Moving Avg
j$D1000 <- ma(TS, order=1000) #1000 Day Moving Avg
j$D5000 <- ma(TS, order=5000) #5000 Day Moving Avg

# Summary statistics of moving averages
summary(j[c("D30", "D90", "D365", "D1000", "D5000")])

ggplot(data = j, aes(x = Date_Parsed)) + 
  geom_line(aes(y = D30), color = "darkred") + 
  geom_line(aes(y = D365), color = "steelblue", linetype = "twodash")
