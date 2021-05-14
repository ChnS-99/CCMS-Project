daily_cons <- readxl::read_xlsx("DailyConsumption (4).xlsx")
str(daily_cons)

library(tidyr) ; library(tidyverse) ; library(corrplot)
daily_cons <- separate(daily_cons, Timestamp, into = c("Date","Time"), sep = " ")
daily_cons$Time <- NULL
daily_cons$`CCMS ID` <- factor(daily_cons$`CCMS ID`)
daily_cons$Date <- as.Date(daily_cons$Date, format = "%Y-%m-%d")
daily_cons$Day <- factor(weekdays(daily_cons$Date))
daily_cons$`CCMS kWh` <- as.numeric(daily_cons$`CCMS kWh`)
daily_cons$`Grid kVAh` <- as.numeric(daily_cons$`Grid kVAh`)
daily_cons$`Expected kWh` <- as.numeric(daily_cons$`Expected kWh`)

num <- select_if(daily_cons, is.numeric)
corrplot(cor(num), method = "circle")
corrgram(num)

daily_cons$x <- as.character(daily_cons$`Meter ON Duration`)
y <- tidyr::separate(daily_cons, daily_cons$x,into=c("Hours","Minutes","Seconds"), sep = ":")


#2 Additional Variables for Subtraction :
daily_cons$`Meter ON Duration2` <- strptime(daily_cons$`Meter ON Duration`, format = "%H:%M:%S")
daily_cons$`ON Load Duration2` <- strptime(daily_cons$`ON Load Duration`, format = "%H:%M:%S")

daily_cons$`Meter ON Duration` <- format(as.POSIXct(strptime(daily_cons$`Meter ON Duration2`,"%Y-%m-%d %H:%M:%S", tz="")) ,format = "%H:%M:%S")
daily_cons$`ON Load Duration` <- format(as.POSIXct(strptime(daily_cons$`ON Load Duration2`,"%Y-%m-%d %H:%M:%S", tz="")) ,format = "%H:%M:%S")

#EDA
#### Univariate Analysis
#CCMS ID - Constant
summary(daily_cons$`CCMS kWh`)
ggplot(daily_cons,aes(`CCMS kWh`))+geom_histogram(color = "black",fill = rainbow(length(daily_cons$`CCMS kWh`)),bins = 30)+theme_classic()

summary(daily_cons$`Grid kVAh`)
ggplot(daily_cons,aes(`Grid kVAh`))+geom_histogram()

summary(daily_cons$`Expected kWh`)
ggplot(daily_cons,aes(`Expected kWh`))+geom_histogram()

daily_cons %>% group_by(Day) %>% summarise(kWh = sum(`Grid kVAh`)) %>% arrange(desc(kWh))

ggplot(daily_cons, aes(Date, `CCMS kWh`))+geom_point(col = rainbow(length(daily_cons$`CCMS kWh`)))+theme_classic()+
  labs(title = "Date Vs CCMS kWh", y = "CCMS kWh", subtitle = "Line Chart Visualization")+
  geom_line(col = "black")+
  theme(plot.title =  element_text(hjust = 0.5))+
  theme(plot.subtitle =  element_text(hjust = 0.5))#+geom_label(aes(Date,`CCMS kWh`, label = `CCMS kWh`))

daily_cons %>% group_by(Day) %>% summarise(kWh = sum(`CCMS kWh`)) %>% 
  ggplot(aes(Day, kWh, fill = rainbow(length(kWh)))) + geom_bar(stat = "identity")+theme_classic()+
  labs(title = "Days Vs CCMS kWh", y = "CCMS kWh", subtitle = "Bar Plot Visualization")+
  geom_label(aes(Day, kWh, label = kWh))+theme(legend.position = "none")+
  scale_x_discrete(limits = c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"))+
  theme(plot.title =  element_text(hjust = 0.5))+
  theme(plot.subtitle =  element_text(hjust = 0.5))

ggplot(daily_cons, aes(Date, `Grid kVAh`))+geom_point(col = rainbow(length(daily_cons$`Grid kVAh`)))+theme_classic()+
  labs(title = "Date Vs Grid kVAh", y = "Grid kVAh", subtitle = "Line Chart Visualization")+
  geom_line(col = "black")+
  theme(plot.title =  element_text(hjust = 0.5))+
  theme(plot.subtitle =  element_text(hjust = 0.5))

daily_cons %>% group_by(Day) %>% summarise(kWh = sum(`Grid kVAh`)) %>% 
  ggplot(aes(Day, kWh, fill = rainbow(length(kWh)))) + geom_bar(stat = "identity")+theme_classic()+
  labs(title = "Days Vs Grid kVAh", y = "Grid kVAh", subtitle = "Bar Plot Visualization")+
  geom_label(aes(Day, kWh, label = kWh))+theme(legend.position = "none")+
  scale_x_discrete(limits = c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"))+
  theme(plot.title =  element_text(hjust = 0.5))+
  theme(plot.subtitle =  element_text(hjust = 0.5))

ggplot(daily_cons, aes(Date, `Expected kWh`))+geom_point(col = rainbow(length(daily_cons$`Expected kWh`)))+theme_classic()+
  labs(title = "Date Vs Expected kWh", y = "Expected kWh", subtitle = "Line Chart Visualization")+
  geom_line(col = "black")+
  theme(plot.title =  element_text(hjust = 0.5))+
  theme(plot.subtitle =  element_text(hjust = 0.5))

daily_cons %>% group_by(Day) %>% summarise(kWh = sum(`Expected kWh`)) %>% 
  ggplot(aes(Day, kWh, fill = rainbow(length(kWh)))) + geom_bar(stat = "identity")+theme_classic()+
  labs(title = "Days Vs Expected kWh", y = "Expected kWh", subtitle = "Bar Plot Visualization")+
  geom_label(aes(Day, kWh, label = kWh))+theme(legend.position = "none")+
  scale_x_discrete(limits = c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"))+
  theme(plot.title =  element_text(hjust = 0.5))+
  theme(plot.subtitle =  element_text(hjust = 0.5))

daily_cons %>% ggplot(aes(Day, `Expected kWh`))+geom_violin(fill = "red")+theme_classic()+labs(title = "Days Vs Expected kWh", subtitle = "Box Plot Visualization", y = "Expected kWh")+
  scale_x_discrete(limits = c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"))+
  theme(plot.title =  element_text(hjust = 0.5))+
  theme(plot.subtitle =  element_text(hjust = 0.5))
