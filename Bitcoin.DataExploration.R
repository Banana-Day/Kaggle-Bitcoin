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

# = = = Convert Year and Month into Factors = = = 

bitstamp$Year <- as.factor(bitstamp$Year)
bitstamp$Month <- as.factor(bitstamp$Month)
bitstamp$YearMonth <- as.factor(bitstamp$YearMonth)

# = = = 1st set of graphs to visualize the complete data on LOG scale = = = 
boxplot(log(bitstamp$Volume_.BTC.) ~ bitstamp$YearMonth, range = 0, las = 2, ylab = "Log(Volume BTC)", main = "Log(Volume BTC)/(Year + Month) & whishers at ZERO", col = rainbow(9), ylim = c(-18, 8))

boxplot(log(bitstamp$Volume_.Currency.) ~ bitstamp$YearMonth, las = 2, range = 0, ylab = "Log(Vol Currency)", main = "Log(Vol Currency)/(Year + Month) & whiskers at ZERO", col = "yellow")

boxplot(log(bitstamp$Volume_.BTC.) ~ bitstamp$Month, range = 0, ylab = "Log(Volume BTC)", main = "Log(Volume BTC)/Month cumulated across all years  *Bitstamp USD*", names = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"), at = c(1,2,3, 5,6,7, 9,10,11, 13,14,15), col = "orange")

boxplot(log(bitstamp$Volume_.Currency.) ~ bitstamp$Month, range = 0, ylab = "Log(Volume Currency)", main = "Log(Volume Currency)/Month cumulated across all years  *Bitstamp USD*", names = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"), at = c(1,2,3, 5,6,7, 9,10,11, 13,14,15), col = "yellow")

library(ggplot2)

ggplot(bitstamp, aes(x = Month, fill = Volume_.Currency.))+
  geom_bar(col = "white")+
  facet_wrap(~ Year)+
  ggtitle("Volume Currency each Year/Month")+
  xlab("Month")+
  ylab("Volume Currency")

ggplot(bitstamp, aes(x = Year, fill = Volume_.Currency.))+
  stat_count(aes(fill = Volume_.Currency.))+
  facet_wrap(~ Month)+
  ggtitle("Volume Currency each Month/Year")+
  xlab("Years")+
  ylab("Volume Currency")

# = = = 1st attempt to know the data >>> to be continued = = = 