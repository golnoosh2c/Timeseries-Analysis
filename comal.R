library(dplyr)
library(imputeTS)
library(forecast)

setwd("C:/Lamar_University/Spring_2023/Machine_Learning/Project_5")
COMAL <- read.csv("Comal.csv")
COMAL$Date_Parsed <- as.Date(COMAL$Date, format = "%m/%d/%Y")  #convert to date
end <- COMAL$Date_Parsed[length(COMAL$Date_Parsed)]
bgn <- COMAL$Date_Parsed[1]
SEQ <- seq.Date(bgn, end, "day")
length(SEQ) - length(COMAL$Date_Parsed)
sum(is.na(COMAL$Discharge))

# Fill missing values with Kalman method
COMAL <- COMAL %>%
  mutate(Discharge_filled = na_kalman(Discharge))

# Round the filled values to one decimal place
COMAL$Discharge_filled <- round(COMAL$Discharge_filled, digits = 1)

# Convert the data to a time series object
TS <- ts(COMAL$Discharge_filled, frequency = 365)

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

# Add moving averages to the COMAL data frame
COMAL$D30 <- ma(TS, order = 30) #30 Day Moving Avg
COMAL$D90 <- ma(TS, order = 90) #90 Day Moving Avg
COMAL$D365 <- ma(TS, order = 365) #365 Day Moving Avg
COMAL$D1000 <- ma(TS, order = 1000) #1000 Day Moving Avg
COMAL$D5000 <- ma(TS, order = 5000) #5000 Day Moving Avg

# Plot moving averages
ggplot(data = COMAL, aes(x = Date_Parsed)) + 
  geom_line(aes(y = D30), color = "darkred") + 
  geom_line(aes(y = D365), color = "steelblue", linetype = "twodash")

# Summary statistics of moving averages
summary(COMAL[c("D30", "D90", "D365", "D1000", "D5000")])
