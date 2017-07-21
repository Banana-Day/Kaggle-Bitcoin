# = = = = = = = = = = = = = = =

# Bitcoin: The longest running & well known cryptocurrency
# Data Source: bitstampUSD_1-min_data_2012-01-01_to_2017-05-31.csv - 13% of all BTC Volume (past 30 days from last update of this data set)

# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =

#bitstamp <- read.csv( link to Data Source on Kaggle, header = TRUE)
str(bitstamp)

# = = = Add columns to convert Timestamp UNIX to readable date format followed by replacing NaNs with ZEROs. = = =

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

bitstamp$Open[is.nan(bitstamp$Open)] <- 0
bitstamp$High[is.nan(bitstamp$High)] <- 0
bitstamp$Low[is.nan(bitstamp$Low)] <- 0
bitstamp$Close[is.nan(bitstamp$Close)] <- 0
bitstamp$Volume_.BTC.[is.nan(bitstamp$Volume_.BTC.)] <- 0
bitstamp$Volume_.Currency.[is.nan(bitstamp$Volume_.Currency.)] <- 0
bitstamp$Weighted_Price[is.nan(bitstamp$Weighted_Price)] <- 0

library(ggplot2)
library(dplyr)
library(gridExtra)

# = = = When data is rearranged to look into the number of transactions per variable = = =

Trns.bit <- (aggregate(cbind(Transact, Volume_.BTC., Volume_.Currency.) ~ Intervals + Day + Month + YearMonth + Year, data = bitstamp, sum))
tTrnsbit <- subset(Trns.bit, Year == 2013 | Year == 2014 | Year == 2015)
tTrnsbit$Year <- as.factor(tTrnsbit$Year)
Trns.bit$Year <- as.factor(Trns.bit$Year)
Trns.bit$YearMonth <- as.factor(Trns.bit$YearMonth)
Trns.bit$Month <- as.factor(Trns.bit$Month)
Trns.bit$Day <- as.factor(Trns.bit$Day)
Trns.bit$Intervals <- as.factor(Trns.bit$Intervals)
str(Trns.bit)

# = = = The 1st global view as per Transactions = = = 

TrnsBTC <- ggplot(Trns.bit, aes(x = Transact, y = YearMonth))+
  geom_tile(aes(fill = Volume_.BTC.))+
  scale_fill_gradient(low = "lavender", high = "navyblue")+
  xlab("Transactions/Day/Hour")+
  ylab("Year & Month")+
  ggtitle("Volumes BTC", subtitle = "Years: 20-11/12/13/14/15/16/17")
TrnsCUR <- ggplot(Trns.bit, aes(x = Transact, y = YearMonth))+
  geom_tile(aes(fill = Volume_.Currency.))+
  scale_fill_gradient(low = "wheat", high = "red3")+
  xlab("Transactions/Day/Hour")+
  ylab("Year & Month")+
  ggtitle("Volumes Currency", subtitle = "Years: 20-11/12/13/14/15/16/17")

Trnsplot <- grid.arrange(TrnsBTC, TrnsCUR, ncol = 2)
Trnsplot

# = = = The next levels as per Transactions = = =

summary(tTrnsbit$Transact)
Trns.bitQ1 <- subset(tTrnsbit, Transact <= 33)
Trns.bitQ2 <- subset(tTrnsbit, Transact > 33 & Transact <= 45)
Trns.bitQ3 <- subset(tTrnsbit, Transact > 45 & Transact <= 54)
Trns.bitQ4 <- subset(tTrnsbit, Transact > 54)

# = = = Transactions' Quadrants based graphs Volumes in BTC = = = = 

pBTCQ1 <- ggplot(Trns.bitQ1, aes(x=Transact, y=Month)) +
  geom_point(aes(size = Volume_.BTC.), col = "red", alpha = 0.27)+
  facet_grid(.~Year, scales = "free")+
  xlab("1st Quar - Transactions/Day/Hour")+
  ylab("Months")+
  theme(legend.position = "top", legend.text = element_text(size = 10))
pBTCQ2 <- ggplot(Trns.bitQ2, aes(x=Transact, y=Month)) +
  geom_point(aes(size = Volume_.BTC.), col = "darkgreen", alpha = 0.27)+
  facet_grid(.~Year, scales = "free")+
  xlab("2nd Quar - Transactions/Day/Hour")+
  ylab("Months")+
  theme(legend.position = "top", legend.text = element_text(size = 10))
pBTCQ3 <- ggplot(Trns.bitQ3, aes(x=Transact, y=Month)) +
  geom_point(aes(size = Volume_.BTC.), col = "maroon", alpha = 0.27)+
  facet_grid(.~Year, scales = "free")+
  xlab("3rd Quar - Transactions/Day/Hour")+
  ylab("Months")+
  theme(legend.position = "top", legend.text = element_text(size = 10))
pBTCQ4 <- ggplot(Trns.bitQ4, aes(x=Transact, y=Month)) +
  geom_point(aes(size = Volume_.BTC.), col = "royalblue", alpha = 0.27)+
  facet_grid(.~Year, scales = "free")+
  xlab("4th Quar - Transactions/Day/Hour")+
  ylab("Months")+
  theme(legend.position = "top", legend.text = element_text(size = 10))

Trns.Qua.BTC <- grid.arrange(pBTCQ1, pBTCQ3, pBTCQ2, pBTCQ4)
Trns.Qua.BTC

# = = = Transactions' Quadrants based graphs Volumes in Currency = = = = 

pCURQ1 <- ggplot(Trns.bitQ1, aes(x=Intervals, y=Day)) +
  geom_point(aes(size = Volume_.Currency.), col = "royalblue", alpha = 0.27)+
  facet_grid(.~Year, scales = "free")+
  xlab("Trans' 1st Quar & Hour of the Day")+
  ylab("Day of Month")+
  theme(legend.position = "top")
pCURQ2 <- ggplot(Trns.bitQ2, aes(x=Intervals, y=Day)) +
  geom_point(aes(size = Volume_.Currency.), col = "maroon", alpha = 0.27)+
  facet_grid(.~Year, scales = "free")+
  xlab("Trans' 2nd Quar & Hour of the Day")+
  ylab("Day of Month")+
  theme(legend.position = "top")
pCURQ3 <- ggplot(Trns.bitQ3, aes(x=Intervals, y=Day)) +
  geom_point(aes(size = Volume_.Currency.), col = "red", alpha = 0.27)+
  facet_grid(.~Year, scales = "free")+
  xlab("Trans' 3rd Quar & Hour of the Day")+
  ylab("Day of Month")+
  theme(legend.position = "top")
pCURQ4 <- ggplot(Trns.bitQ4, aes(x=Intervals, y=Day)) +
  geom_point(aes(size = Volume_.Currency.), col = "darkgreen", alpha = 0.27)+
  facet_grid(.~Year, scales = "free")+
  xlab("Trans' 4th Quar & Hour of the Day")+
  ylab("Day of Month")+
  theme(legend.position = "top")
  
Trns.Qua.CUR <- grid.arrange(pCURQ1, pCURQ3, pCURQ2, pCURQ4)
Trns.Qua.CUR

# = = = = = = = = to be continued = = = = = = = = = = = = = = 