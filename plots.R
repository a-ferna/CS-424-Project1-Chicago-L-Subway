

# to load "UIC-Halsted" data
# uicHalsted<-read.table("UIC-Halsted.csv", sep = "\t", header = TRUE, quote = "\"")

# convert string to Date type
# uicHalsted$date<- as.Date(uicHalsted$date, format="%Y-%m-%d")



# load "O'Hare" data
# oHare<-read.table("oHare.csv", sep = "\t", header = TRUE, quote = "\"")

# convert string to Date type
# oHare$date<- as.Date(oHare$date, format="%Y-%m-%d")


#
### Initial plot UIC Halsted ridership by year
# aggregate by year
# uicHalstedYearly<-aggregate(uicHalsted$rides, list(uicHalsted$year), FUN=sum) 

# assign col names to total rider per year
# colnames(uicHalstedYearly)<- c("year", "totalRides")

# bar plot of YEARLY ridership
ggplot(uicHalstedYearly, aes(x=year, y=totalRides))+
  geom_bar(stat="identity", width=0.8, fill="indianred2")+
  labs(x="Year", y="Riders", title="Yearly Ridership at UIC-Halsted station")



#
###this will then be an user input
yearToPlot<-2021

# data for specific year
uicHalstedYear<-uicHalsted[uicHalsted$year == yearToPlot,]



#
### Plot daily ridership
ggplot(uicHalstedYear, aes(x=date, y=rides))+
  geom_bar(stat="identity", fill="indianred2")+
  scale_x_date(date_labels = "%b")+
  labs(x='Date', y='Riders', title='Daily Riders of UIC-Halsted Station')

# ? how to elongate so that more months show



#
### plot monthly ridership
uicHalstedMonth<-aggregate(uicHalstedYear$rides, list(uicHalstedYear$month), FUN=sum)

# change cols name
colnames(uicHalstedMonth)<- c("month", "totalRides")

ggplot(uicHalstedMonth, aes(x=month, y=totalRides))+
  geom_bar(stat="identity", fill="indianred2")+
  scale_x_discrete(limits=c("Jan", "Feb", "Mar", "Apr", "May", "June", "July", "Aug", "Sep", "Oct", "Nov", "Dec"))+
  labs(x="Month", y="Riders", title="Monthly Ridership at UIC-Halsted station")



#
### plot by day of the week
uicHalstedDay<-aggregate(uicHalstedYear$rides, list(uicHalstedYear$day), FUN=sum)

colnames(uicHalstedDay) <- c("day", "totalRiders")
uicHalstedDay<-uicHalstedDay[order(uicHalstedDay$day),]

ggplot(uicHalstedDay, aes(x=day, y=totalRiders))+
  geom_bar(stat="identity", fill="indianred2")+
  scale_x_discrete(limits = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"))+
  labs(x="Day of the Week", y="Total Riders", title="Total ridership by day of the week")

# ? how does the same function do different things? (scale_x_discrete)