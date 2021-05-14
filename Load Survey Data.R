setwd("E:/CCMS Hackathon/CCMS/Dummy data/Load Survey Data")

#load <- readxl::read_xlsx("Load Survey Data (6).xlsx")
load <- read.csv("Load Survey Data.csv")
str(load)

load$`CCMS ID` <- factor(load$`CCMS ID`)
load$Timestamp <- as.POSIXct(load$Timestamp, "%Y-%m-%d %H:%M:%S")
load$KWh <- as.numeric(load$KWh)
load$KVAh <- as.numeric(load$KVAh)
load$`Vr (Volt)` <- as.numeric(load$`Vr (Volt)`)
load$`Vy (Volt)` <- as.numeric(load$`Vy (Volt)`)
load$`Vb (Volt)` <- as.numeric(load$`Vb (Volt)`)
load$`Ir (Amp)` <- as.numeric(load$`Ir (Amp)`)
load$`Iy (Amp)` <- as.numeric(load$`Iy (Amp)`)
load$`Ib (Amp)` <- as.numeric(load$`Ib (Amp)`)
load$`Freq (Hz)` <- as.numeric(load$`Freq (Hz)`)
load$`PF Total` <- factor(load$`PF Total`)
load$`PF Total`[load$`PF Total`=="--"] <- NA
load$`Total Load` <- as.numeric(load$`Total Load`)

load$kWh <- as.numeric(load$kWh)

a = load[!(load$KWh==0),] #Mean : 4.17 , Median : 4.48

load %>% ggplot(aes(Time.Slot2,KWh)) + geom_boxplot(col = rainbow(12))+
  labs(title = "Time Slot Vs kWh", x = "Time Slot", y = "kWh", subtitle = "LOAD SAMPLE SURVEY DATA - BOX PLOT") + theme_classic() +
  theme(plot.title = element_text(hjust = 0.5)) + theme(plot.subtitle = element_text(hjust = 0.5)) +
  scale_x_discrete(limits = c("16 - 18","18 - 20","20 - 22","22 - 24","0 - 2","2 - 4","4 - 6","6 - 8","8 - 10","10 - 12","12 - 14","14 - 16"))#+
  scale_y_continuous()

plot(load$`Time Slot`, load$kWh, col = "blue", main = "Time Vs kWh", xlab = "Time Slot", ylab = "kWh")
abline(h = 5.1, col = "red")

#load$Timestamp[is.na(load$Timestamp)] <- c("2019-04-25 10:00:00","2019-04-25 09:00:00")
load$Timestamp <- as.POSIXct(load$Timestamp, format = "%Y-%m-%d %H:%M:%S")
str(load)

load <- load[,c(2,3,4,5,6,7,8,9,10,11,13)]
backup <- load
sum(is.na(load))
x <- load[145:168,]

library(mlr)
sapply(load, function(x) sum(is.na(x))) #8 Missing Values

#Imputing Median
z <- impute(y, classes = list(numeric = imputeMedian()))$data
sum(is.na(z))  #No Missing Values

y <- load %>% filter(KWh==0, KVAh==0) %>% select(everything())
median(y$Vr..Volt.,na.rm = T)
median(y$Vy..Volt.,na.rm = T)
median(y$Vb..Volt.,na.rm = T)

#Visualization
ggplot(load, aes(Timestamp, KVAh))+geom_point()+theme_classic()+
  labs(title = "Time Vs KVAh", x = "Time", y = "KVAh")+
  theme(plot.title = element_text(hjust = 0.5))

load[load$`Freq (Hz)`==0,]

y$Timestamp2 <- as.character(y$Timestamp)
y$Timestamp2 <- tidyr::separate(y,Timestamp2,into = c("Date","Time"), sep = " ")

#MEAN NUMBER OF HOURS WHEN kWh & kVAh == 0
z <- c(10,10,10,10,10,10,10,10,10,10,10,11,11,11,11,11,12,11,11,11,11,11,11,11,11,11,10,11,11,11,11)
mean(z)

expected <- ts(load$KWh, start = c(2019,91), frequency = 365)
library(sarima); library(forecast); library(tseries); library(prophet)

clean <- tsclean(expected)
fit <- auto.arima(clean)
fcast <- forecast(fit, h =31)
plot(fcast)

ds <- seq(as.Date("2019-04-01"), as.Date("2019-04-30"), by = "day")
y <- expected
df <- data.frame(ds, y)

m <- prophet(df, weekly.seasonality = T)
future <- make_future_dataframe(m, periods = 31, freq = "day")

pred <- predict(m, future)

plot(m, pred, title = "Prophet Forecasting", xlab = "Duration", ylab = "Expected kWh")+theme_classic()+
  labs(title = "Prophet Forecasting", subtitle = "Forecasting for May 2019")+
  theme(plot.title =  element_text(hjust = 0.5))+
  theme(plot.subtitle =  element_text(hjust = 0.5))
