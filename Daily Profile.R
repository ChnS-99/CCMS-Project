daily_profile <- readxl::read_xlsx("DailyProfileData (7).xlsx")
str(daily_profile)

#Encoding
daily_profile$`CCMS ID` <- factor(daily_profile$`CCMS ID`)
daily_profile$`Cum kwh` <- as.numeric(daily_profile$`Cum kwh`)
daily_profile$`Cum kVAh` <- as.numeric(daily_profile$`Cum kVAh`)

daily_profile <- separate(daily_profile, Timestamp, into = c("Date","Time"), sep = " ")
daily_profile$Time <- NULL
daily_profile$Date <- as.Date(daily_profile$Date, format = "%Y-%m-%d")

y <- tidyr::separate(daily_profile, col = `Meter ON Duration`, into = c("Hours","Minutes","Seconds"), sep = ":")
y$Hours <- as.numeric(y$Hours)
y$Minutes <- as.numeric(y$Minutes)
y$Seconds <- as.numeric(y$Seconds)
y$Meter_Hours <- NULL
for (i in 1:nrow(y)){
  y$Meter_Hours[i]=((y$Hours[i])+(round((y$Minutes[i]/60),4))+(round((y$Seconds[i]/3600),4)))
}

daily_profile <- read.csv("Daily Profile.csv")
daily_profile$Date <- as.Date(as.character(daily_profile$Date), format = "%d-%m-%Y")

#Visualizations
num <- select_if(daily_profile, is.numeric)
corrplot(cor(num), method = "circle")

ggplot(daily_profile,aes(Date, Relay.ON.Hours))+
  geom_point(col = rainbow(30)) + theme_classic() +
  labs(title = "Date Vs Relay ON Hours", y = "Relay ON Hours", subtitle = "Monthly Average ON Duration : 10.8 Hours",caption = "Line Chart Visualization")+
  geom_line(col = "black")+
  theme(plot.title =  element_text(hjust = 0.5))+
  theme(plot.subtitle =  element_text(hjust = 0.5))

ggplot(daily_profile, aes(Date))+
  geom_line(aes(y = CCMS.kWh, color = "CCMS kWh"))+geom_point(aes(y = CCMS.kWh),col = rainbow(30))+
  geom_line(aes(y = Expected.kWh, color = "Expected kWh"))+geom_point(aes(y = Expected.kWh),col = rainbow(30))+
  theme_classic()+labs(title = "Date Vs CCMS kWh Vs Expected kWh", subtitle = "Line Chart Visualization", y = "kWh")+
  scale_color_manual("",values = c("CCMS kWh"="blue","Expected kWh"="red"))+
  theme(plot.title =  element_text(hjust = 0.5))+
  theme(plot.subtitle =  element_text(hjust = 0.5))

daily_profile %>% group_by(Day) %>% summarise(kWh = mean(Relay.ON.Hours)) %>% 
  ggplot(aes(Day, kWh, fill = rainbow(length(kWh)))) + geom_bar(stat = "identity")+theme_classic()+
  labs(title = "Days Vs Relay ON Hours", y = "Hours", subtitle = "Bar Plot Visualization")+
  geom_label(aes(Day, kWh, label = kWh))+theme(legend.position = "none")+
  scale_x_discrete(limits = c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"))+
  theme(plot.title =  element_text(hjust = 0.5))+
  theme(plot.subtitle =  element_text(hjust = 0.5))

daily_profile %>% ggplot(aes(Day, Expected.kWh))+geom_boxplot(aes(fill = factor(Day)))+theme_classic()+labs(title = "Days Vs Expected kWh", subtitle = "Box Plot Visualization", y = "Expected kWh")+
  scale_x_discrete(limits = c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"))+
  theme(plot.title =  element_text(hjust = 0.5))+
  theme(plot.subtitle =  element_text(hjust = 0.5))+
  theme(legend.position = "none")

expected <- ts(daily_profile$Expected.kWh, start = c(2019,91), frequency = 365)
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

