library(lubridate)


# read in file to a ridership table
ridership<-read.table("CTA_-_Ridership_-__L__Station_Entries_-_Daily_Totals.tsv", sep="\t", header = TRUE, quote="\"")


colnames(ridership)[1] <- "station_id"



#
# isolate data records specific to station "UIC-Halsted"
uicHalsted<-ridership[ridership$stationname == "UIC-Halsted",]

# convert date to Date type
uicHalsted$date<-mdy(uicHalsted$date)

# order by date
uicHalsted<-uicHalsted[order(uicHalsted$date),]

# add column for just year
uicHalsted$year <- year(uicHalsted$date)

# add column for just month
uicHalsted$month <- month(uicHalsted$date)

# add day of the week column
uicHalsted$day <- wday(uicHalsted$date, label=TRUE)

# write "UIC-Halsted" ridership data to a file
write.table(uicHalsted, "UIC-Halsted.csv", sep="\t", row.names=FALSE)



#
# isolate data records specific to station "O'Hare Airport"
oHare<-ridership[ridership$stationname == "O'Hare Airport",]

# convert date to Date type
oHare$date<-mdy(oHare$date)

# order in by date
oHare<-oHare[order(oHare$date),]

# add column for just year
oHare$year <- year(oHare$date)

# add column for just month
oHare$month <- month(oHare$date)

# add day of the week column
oHare$day <- wday(oHare$date, label=TRUE)

# write "O'Hare" ridership data to a file
write.table(oHare, "oHare.csv", sep="\t", row.names=FALSE)


#
# isolate data records specific to station Racine
rac<-ridership[ridership$stationname == "Racine",]

# convert date to Date type
rac$date<-mdy(rac$date)

# order in by date
rac<-oHare[order(rac$date),]

# add column for just year
rac$year <- year(rac$date)

# add column for just month
rac$month <- month(rac$date)

# add day of the week column
rac$day <- wday(rac$date, label=TRUE)

# write Racine ridership data to a file
write.table(rac, "racine.csv", sep="\t", row.names=FALSE)
