# = = = = = = = = = = = = = = =
#
# Bitcoin: The longest running & well known cryptocurrency
#
# Data Source: bitstampUSD_1-min_data_2012-01-01_to_2017-05-31.csv - 13% of all BTC Volume (past 30 days from last update of this data set)
#
# = = = = = = = = = = = = = = = 
#
# bitstamp <- read.csv("... data from Kaggle.. i.e. Data Source", header = TRUE)

summary(bitstamp)
str(bitstamp)

# = = = Add columns to convert Timestamp UNIX to readable date format = = =

bitstamp$Time.read <- as.POSIXct(bitstamp$Timestamp, origin = "1970-01-01", tz = "GMT")
bitstamp$Year <- format(as.Date(bitstamp$Time.read, format="%d/%m/%Y"),"%Y")
bitstamp$Month <- format(as.Date(bitstamp$Time.read, format="%d/%m/%Y"),"%m")
bitstamp$YearMonth <- format(as.Date(bitstamp$Time.read, format="%d/%m/%Y"),"%Y-%m")
bitstamp$Day <- format(as.Date(bitstamp$Time.read, format="%d/%m/%Y/%H/%M/%S"),"%d")
bitstamp$Time <- format(as.POSIXct(bitstamp$Time.read), format = "%H:%M:%S")
bitstamp$Intervals <- format(as.POSIXct(bitstamp$Time.read), format = "%H")
bitstamp$Min.Sec <- format(as.POSIXct(bitstamp$Time.read), format = "%M:%S")

bitstamp$Transact <- ifelse(bitstamp$Volume_.BTC. >=0, 1, 0)
bitstamp$Transact[is.na(bitstamp$Transact)] <- 0

# = = = Missing Values as subest of BITSTAMP = = =

B.Mis.Val <- subset(bitstamp, is.na(Open)| is.na(High)| is.na(Low)| is.na(Close)| is.na(Volume_.BTC.)| is.na(Volume_.Currency.)| is.na(Weighted_Price))

summary(B.Mis.Val)
table(B.Mis.Val)
Perc.B.Mis.Val <- nrow(B.Mis.Val)*100/nrow(bitstamp)

# = = = Convert Year and Month into Factors = = = 

bitstamp$Year <- as.factor(bitstamp$Year)
bitstamp$Month <- as.factor(bitstamp$Month)
bitstamp$YearMonth <- as.factor(bitstamp$YearMonth)
bitstamp$Day <- as.factor(bitstamp$Day)

# = = = Missing Values and their inferences = = =

sort(tapply(bitstamp$Transact == 1, bitstamp$Year, sum))
sort(tapply(bitstamp$Transact == 0, bitstamp$Year, sum))
sort(tapply(bitstamp$Transact == 1, months(bitstamp$Time.read), sum))
sort(tapply(bitstamp$Transact == 0, months(bitstamp$Time.read), sum))
sort(tapply(bitstamp$Transact == 1, weekdays(bitstamp$Time.read), sum))

plot((tapply(bitstamp$Transact == 0, bitstamp$Intervals, sum)*60)/(tapply(bitstamp$Transact == 0, bitstamp$Intervals, sum) + tapply(bitstamp$Transact == 1, bitstamp$Intervals, sum)), col = "red", xlab = "Hour of the Day", ylab = "Minutes" ,main = "# Minutes per Hour with ZERO Transactions", xlim = c(0,24) ,ylim = c(20, 35), type = "h")

plot((tapply(bitstamp$Transact == 1, bitstamp$Intervals, sum)*60)/(tapply(bitstamp$Transact == 0, bitstamp$Intervals, sum) + tapply(bitstamp$Transact == 1, bitstamp$Intervals, sum)), col = "navyblue", xlab = "Hour of the Day", ylab = "Minutes" ,main = "# Minutes per Hour with Volumes Transacted", xlim = c(0,24) ,ylim = c(25, 40), type = "h")

# = = = One hour time intervals pattern - across 7 years of Bitcoin trade = = =

sort(tapply(bitstamp$Volume_.Currency., bitstamp$Intervals, sum, na.rm = TRUE))

# = = = Sneekpeak into 13:00 hrs to 15:59 hrs - across 7 years of Bitcoin trade = = = 

bitstamp.13.hrs <- subset(bitstamp, Intervals == 13)
str(bitstamp.13.hrs)
summary(bitstamp.13.hrs)

sort(tapply(bitstamp.13.hrs$Volume_.Currency., bitstamp.13.hrs$YearMonth, sum, na.rm = TRUE))
sum(bitstamp.13.hrs$Volume_.Currency., na.rm = TRUE)

bitstamp.14.hrs <- subset(bitstamp, Intervals == 14)
str(bitstamp.14.hrs)
summary(bitstamp.14.hrs)

sort(tapply(bitstamp.14.hrs$Volume_.Currency., bitstamp.14.hrs$YearMonth, sum, na.rm = TRUE))
sum(bitstamp.14.hrs$Volume_.Currency., na.rm = TRUE)

bitstamp.15.hrs <- subset(bitstamp, Intervals == 15)
str(bitstamp.15.hrs)
summary(bitstamp.15.hrs)

sort(tapply(bitstamp.15.hrs$Volume_.Currency., bitstamp.15.hrs$YearMonth, sum, na.rm = TRUE))
sum(bitstamp.15.hrs$Volume_.Currency., na.rm = TRUE)

# = = = Most successful years in Bitcoin trade = = = 

sort(tapply(bitstamp$Volume_.BTC., bitstamp$Year, sum, na.rm = TRUE))
Vol.BTC.2015 <- 100*(5.525311e+06)/sum(bitstamp$Volume_.BTC., na.rm = TRUE)
Vol.BTC.2015

sort(tapply(bitstamp$Volume_.Currency., bitstamp$Year, sum, na.rm = TRUE))
Vol.Curr.2014 <- 100*(2.615293e+09)/sum(bitstamp$Volume_.Currency., na.rm = TRUE)
Vol.Curr.2014

# = = = Sneekpeak into 2014 & 2015 = = = 

bitstamp2014 <- subset(bitstamp, Year == 2014)
bitstamp2014$Year <- factor(bitstamp2014$Year)

sort(tapply(bitstamp2014$Volume_.Currency., bitstamp2014$Month, sum, na.rm = TRUE))
sort(tapply(bitstamp2014$Transact == 1, weekdays(bitstamp2014$Time.read), sum))
sort(tapply(bitstamp2014$Transact == 1, bitstamp2014$Intervals, sum))

bitstamp2015 <- subset(bitstamp, Year == 2015)
bitstamp2015$Year <- factor(bitstamp2015$Year)

sort(tapply(bitstamp2015$Volume_.Currency., bitstamp2015$Month, sum, na.rm = TRUE))
sort(tapply(bitstamp2015$Transact == 1, weekdays(bitstamp2015$Time.read), sum))
sort(tapply(bitstamp2015$Transact == 1, bitstamp2015$Intervals, sum))

# = = = to be continued = = = 