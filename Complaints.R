setwd("E:/CCMS Hackathon/CCMS/Dummy data/Complaints Data")

complaints <- readxl::read_xlsx("Street light Complaint Details last 6 Month.xlsx", range = "A1:B157165")
#ERROR : Cell references aren't uniformly A1 or R1C1 format

complaints <- read.csv("Complaints.csv")

extracted <- read.csv("Closed Call Cases.csv")

sum(is.na(complaints$Language.Preference)) #0 NA Values
length(levels(factor(complaints$Language.Preference)))

complaints$Date.of.Call.Closer <- as.character(complaints$Date.of.Call.Closer)
complaints$Date.of.Call.Closer <- as.Date(complaints$Date.of.Call.Closer, format = "%d-%m-%Y")

x = complaints %>% filter(State == "DELHI (DL)")
levels(factor(x$District))

##### Visualizations #####
#Call Status
s <- a %>% group_by(State) %>% summarise(Count = n()) %>% arrange(desc(Count))
s$percentage <- round(100*s$Count/sum(s$Count), 1)
s %>% ggplot(aes(State, percentage)) + geom_bar(stat = "identity", fill = rainbow(length(s$Count)))+
  theme(legend.position = "none")+labs(title = "State Wise Complaints Distribution", x = "State", y = "Percentage")+
  geom_label(aes(State, percentage, label = percentage)) + theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))+theme(axis.text.x = element_text(hjust = 1, angle = 60))

s <- x %>% group_by(District) %>% summarise(Count = n()) %>% arrange(desc(Count))
s$percentage <- round(100*s$Count/sum(s$Count), 3)
s %>% ggplot(aes(Agent, percentage)) + geom_bar(stat = "identity", fill = rainbow(length(s$Count)))+
  theme(legend.position = "none")+labs(title = "Agent Distribution", subtitle =  "TOTAL AGENTS : 86",x = "Agent", y = "Percentage")+
  geom_label(aes(Agent, percentage, label = percentage)) + theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(plot.subtitle = element_text(hjust = 0.5))+
  theme(axis.text.x = element_text(hjust = 1, angle = 30))

#4 Major Agents : WEBCONSUMER, NIDAN, YESUL AND NITIN (Approx. 25% of total)
s <- complaints %>% group_by(Agent) %>% summarise(Count = n())
s$percentage <- round(100*s$Count/sum(s$Count), 4)
x <- s %>% arrange(desc(s$Count))

s <- complaints %>% group_by(Street.Ligh.Complaint.From) %>% summarise(Count = n())
s$percentage <- round(100*s$Count/sum(s$Count), 2)
s %>% ggplot(aes(Street.Ligh.Complaint.From, percentage)) + geom_bar(stat = "identity", aes(fill = rainbow(length(s$Count))))+
  labs(title = "Street Light Complaint From", x = "Complaint From", y = "Percentage")+
  geom_label(aes(Street.Ligh.Complaint.From, percentage, label = percentage)) + theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position = "none")+
  theme(axis.text.x = element_text(hjust = 1, angle = 30))

z <- complaints %>% filter(Street.Ligh.Complaint.From=="")
z = extracted
z$Date.of.Call <- as.Date(as.character(z$Date.of.Call),"%d-%m-%Y")
z$Date.of.Escalation <- as.Date(as.character(z$Date.of.Escalation),"%d-%m-%Y")
z$Date.of.Call.Closer <- as.Date(as.character(z$Date.of.Call.Closer),"%d-%m-%Y")

z <- complaints[!(is.na(complaints$Date.of.Call)) & !(complaints$Date.of.Call.Closer==""),]

z$Date.of.Call <- as.Date(as.character(z$Date.of.Call), format = "%d-%m-%Y")
z$Date.of.Call.Closer <- as.Date(as.character(z$Date.of.Call.Closer), format = "%d-%m-%Y")

z$Duration2 <- NULL
for (i in 1:nrow(z)){
  z$Duration2[i] <- as.numeric(z$Date.of.Call.Closer[i]-z$Date.of.Call[i])
}

z1 = z %>% filter(!(is.na(Date.of.Escalation)))
z1$Duration2 <- NULL
for (i in 1:nrow(complaints)){
  complaints$Duration[i] <- as.numeric(complaints$Date.of.Call.Closer[i]-complaints$Date.of.Call[i])
}

complaints$Category = ifelse(complaints$Duration<=2,"Excellent",
                           ifelse(complaints$Duration<=7,"Good",
                                  ifelse(complaints$Duration<=14,"Average",
                                         ifelse(complaints$Duration<=30,"Poor","Terrible"))))
summary(z1$Duration2)

as.numeric(sort(prop.table(table(z$Language.Preference)),decreasing = T))
z <- read.csv("Extracted Data.csv")
z1 <- z %>% group_by(Unique.Query.Number) %>% summarise(Count = n())

table(complaints$Unique.Query.Number)
mean(z$Duration)

s <- complaints %>% group_by(Category) %>% summarise(Count = n()) %>% arrange(desc(Count))
s$percentage <- round(100*s$Count/sum(s$Count), 2)
s %>% ggplot(aes(Category, percentage)) + geom_bar(stat = "identity", aes(fill = rainbow(length(s$Count))))+
  labs(title = "Service Level Frequency Plot", x = "Service Level Category", y = "Percentage", subtitle = "April 2019 Closed Cases Analysis")+
  geom_label(aes(Category, percentage, label = percentage)) + theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(plot.subtitle = element_text(hjust = 0.5))+
  theme(legend.position = "none")

z$Category <- NA
z$Category[z$Duration<=2] <- "Excellent"
z$Category[z$Duration>2 & z$Duration<=7] <- "Good"
z$Category[z$Duration>7 & z$Duration<=14] <- "Average"
z$Category[z$Duration>14 & z$Duration<=30] <- "Poor"
z$Category[z$Duration>30] <- "Terrible"

z1 = data.frame(Address = a$Address)
z = select(data, contains("Kapasheda"))
z1 = t(z1) %>% as.data.frame()
data = read.csv("Transpose.csv", header = F)
