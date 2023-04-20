library(dplyr)
library(imputeTS)
library(forecast)

setwd("C:/Lamar_University/Spring_2023/Machine_Learning/Project_5")
SAN <- read.csv("San_Marcos.csv")
SAN$Date_Parsed <- as.Date(SAN$Date, format = "%m/%d/%Y")  #convert to date
end <- SAN$Date_Parsed[length(SAN$Date_Parsed)]
bgn <- SAN$Date_Parsed[1]
SEQ <- seq.Date(bgn, end, "day")
length(SEQ) - length(SAN$Date_Parsed)
sum(is.na(SAN$Discharge))

# Summary statistics before filling missing values
summary(SAN$Discharge)

# Fill missing values with Kalman method
SAN <- SAN %>%
  mutate(Discharge_filled = na_kalman(Discharge))

# Round the filled values to one decimal place
SAN$Discharge_filled <- round(SAN$Discharge_filled, digits = 1)

# Convert the data to a time series object
TS <- ts(SAN$Discharge_filled, frequency = 365)

# Summary statistics before moving average
summary(TS)

# Decompose time series
decomp_ts <- stl(TS, s.window="periodic")

# Plot decomposed time series
plot(decomp_ts)

# Plot ACF
acf(TS)

# Plot PACF
pacf(TS)

# Add moving averages to the SAN data frame
SAN$D30 <- ma(TS, order = 30) #30 Day Moving Avg
SAN$D90 <- ma(TS, order = 90) #90 Day Moving Avg
SAN$D365 <- ma(TS, order = 365) #365 Day Moving Avg
SAN$D1000 <- ma(TS, order = 1000) #1000 Day Moving Avg
SAN$D5000 <- ma(TS, order = 5000) #5000 Day Moving Avg

# Plot moving averages
ggplot(data = SAN, aes(x = Date_Parsed)) + 
  geom_line(aes(y = D30), color = "darkred") + 
  geom_line(aes(y = D365), color = "steelblue", linetype = "twodash")

# Summary statistics of moving averages
summary(SAN[c("D30", "D90", "D365", "D1000", "D5000")])



# Aggregate data from daily to monthly
SAN_monthly <- SAN %>%
  group_by(Year = year(Date_Parsed), Month = month(Date_Parsed)) %>%
  summarise(mean_Discharge_filled = mean(Discharge_filled, na.rm = TRUE))



