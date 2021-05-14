library(readxl) ; library(ggplot2) ; library(dplyr) ; library(lubridate)

setwd("E:/CCMS Hackathon/CCMS/Dummy data/Instant Raw Report")
raw = read_xlsx("Instant Raw Report (11).xlsx")

raw$Timestamp2 <- as.POSIXct(raw$Timestamp, format = "%Y-%m-%d %H:%M:%S")
raw$`Active Power kW` <- as.numeric(raw$`Active Power kW`)
raw$`Apparent Power kVA` <- as.numeric(raw$`Apparent Power kVA`)
raw$`Current R (A)` <- as.numeric(raw$`Current R (A)`)
raw$`Current Y (A)` <- as.numeric(raw$`Current Y (A)`)
raw$`Current B (A)` <- as.numeric(raw$`Current B (A)`)
raw$`Voltage R (V)` <- as.numeric(raw$`Voltage R (V)`)
raw$`Voltage Y (V)` <- as.numeric(raw$`Voltage Y (V)`)
raw$`Voltage B (V)` <- as.numeric(raw$`Voltage B (V)`)
raw$`Frequency (Hz)` <- as.numeric(raw$`Frequency (Hz)`)
raw$`Active Power kW` <- as.numeric(raw$`Active Power kW`)
raw$`Active Power kW` <- as.numeric(raw$`Active Power kW`)
raw$kWh <- as.numeric(raw$kWh)

ggplot(raw, aes(Timestamp2, `Active Power kW`))+geom_point()+
  ylim(c(0,6))

plot(raw$Timestamp2, raw$kWh, col = "blue", main = "Time Vs Energy Consumption", xlab = "Time", ylab = "kWh", ylim = c(0,3))
abline(h = 1.35, col = "red")
abline(h = 1.42, col = "green", lty =2)
abline(h = 1.28, col = "green", lty =2)
library(tidyr)

Timestamp3 <- separate(raw, Timestamp, into = c("Date","Time"), sep = " ")
Timestamp3$Time <- as.Date(Timestamp3$Time,"%H:%M:%S")

df = raw %>% group_by(Timestamp3) %>% mutate(newdate = as.POSIXct(date(min(Timestamp3)))) %>% filter(kWh > 0)
