library(ggplot2)
library(lubridate)
library(dplyr)
library(stringi)
library(ggpubr)
library(stringr)

#Import data into R environment
ctcd <- read.csv(file.choose())
View(ctcd)
head(ctcd)
sum(is.na(ctcd))
summary(ctcd)


#1.Provide the trend chart for the number of complaints at monthly and daily granularity levels
class(ctcd$Date)

#Convert from character to date
ctcd$Date <- dmy(ctcd$Date)
class(ctcd$Date)

#Monthly Complaints
monthly <- summarise(group_by(ctcd,Month =as.integer(month(Date))),Count = n())
View(monthly)

monthly_plot <- ggplot(monthly,aes(Month,Count)) + geom_line(aes(group=1)) + geom_point() + scale_y_continuous(name = "Number of Complaints",limits = c(0,1170)) + scale_x_discrete(name="",limits=c("Jan","Feb","Mar","Apr","May","Jun",
                                                 "Jul","Aug","Sep","Oct","Nov","Dec"))
monthly_plot
ggsave("monthly_plot.png")

#daily Complaints
daily <- summarise(group_by(ctcd,day=weekdays(as.Date(Date))),Count = n())
View(daily)

daily_plot <- ggplot(daily, aes(day, Count))  + geom_line(aes(group=1)) + geom_point() + scale_y_continuous(name = "Number of Complaints",limits = c(0,500)) + scale_x_discrete(name="",limits=c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"))

daily_plot
ggsave("daily_plot.png")

#2.Provide a table with the frequency of complaint types
network <- contains(ctcd$Customer, match = "network", ignore.case = TRUE)
internet <- contains(ctcd$Customer, match = "internet", ignore.case = TRUE)
bills <- contains(ctcd$Customer, match = "billing", ignore.case = TRUE)
charges <- contains(ctcd$Customer, match = "charge", ignore.case = TRUE)
email <- contains(ctcd$Customer, match = "email", ignore.case = TRUE)
data_capacity <- contains(ctcd$Customer, match = "data cap", ignore.case = TRUE)

ctcd$Complaint.Type[network] <- "network"
ctcd$Complaint.Type[internet] <- "internet"
ctcd$Complaint.Type[bills] <- "billing"
ctcd$Complaint.Type[charges] <- "charges"
ctcd$Complaint.Type[email] <- "email"
ctcd$Complaint.Type[data_capacity] <- "data capacity"
ctcd$Complaint.Type[-c(network,internet,bills,charges,email,data_capacity)] <- "others"

table(ctcd$Complaint.Type)

#3.Create a new categorical variable with value as Open and Closed
open <- (ctcd$Status=="Open"|ctcd$Status=="Pending")
closed <- (ctcd$Status=="Closed"|ctcd$Status=="Solved")

ctcd$Status.Category[open] <- "Open"
ctcd$Status.Category[closed] <- "Closed"

#4.Provide state wise status of complaints in a stacked bar chart.Use the categorized variable from Q3
ctcd <- group_by(ctcd,State,Status.Category)
chart <- summarise(ctcd,Count = n())
ggplot(as.data.frame(chart) ,mapping = aes(State,Count))+
  geom_col(aes(fill = Status.Category),width = 0.90)+
  theme(axis.text.x = element_text(family="serif",angle = 90),
        axis.title.y = element_text(family="serif",size = 15),
        axis.title.x = element_text(family="serif",size = 15),
        title = element_text(family="serif",size = 16,colour = "black"))+
  labs(x = "State", y = "Number of Tickets", fill= "")
ggsave("soc_stacked_chart.png")

#Which state has the highest percentage of unresolved complaints
chart%>% filter(Status.Category=="Open")-> Open_complaints
Open_complaints[Open_complaints$Count == max(Open_complaints$Count),c(1,3)]

#5.Provide the percentage of complaints resolved till date,
#which were received through the Internet and customer care calls.
total_complaints<- summarise(group_by(ctcd,Status.Category) ,percentage =(n()/nrow(resolved_data)*100))
total_complaints
resolved <- group_by(ctcd,Received.Via,Status.Category)
summarise(resolved ,percentage =(n()/nrow(resolved)*100)) 


