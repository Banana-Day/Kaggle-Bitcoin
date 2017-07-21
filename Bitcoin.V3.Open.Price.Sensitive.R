# = = = = = = = = = = = = = = =
# Bitcoin: The longest running & well known cryptocurrency
# Data Source: bitstampUSD 1-min data from 2012-01-01 to 2017-05-31
#
# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
#
# bitstamp <- read.csv("... data from Kaggle.. i.e. Data Source", header = TRUE)
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

colnames(bitstamp)[6] <- "BTC"
colnames(bitstamp)[7] <- "CUR"
colnames(bitstamp)[8] <- "Price"

library(ggplot2)
library(gridExtra)
library(caTools)
library(caret)
library(rpart)
library(rpart.plot)

# = = = K-means clustering process post normalisation = = =

bitstampmod <- bitstamp
bitstampmod$CUR <- NULL

preProc <- preProcess(bitstampmod[2:7])
bitstampnorm <- predict(preProc, bitstampmod)

set.seed(1)
bit.KMC <- kmeans(bitstampnorm[2:7], centers = 9, iter.max = 1000)

# = = = Creation of data subsets from Un-normalised data = = = 

bitstamp.unNor1 <- subset(bitstamp, bit.KMC$cluster == 1)
bitstamp.unNor2 <- subset(bitstamp, bit.KMC$cluster == 2)
bitstamp.unNor3 <- subset(bitstamp, bit.KMC$cluster == 3)
bitstamp.unNor4 <- subset(bitstamp, bit.KMC$cluster == 4)
bitstamp.unNor5 <- subset(bitstamp, bit.KMC$cluster == 5)
bitstamp.unNor6 <- subset(bitstamp, bit.KMC$cluster == 6)
bitstamp.unNor7 <- subset(bitstamp, bit.KMC$cluster == 7)
bitstamp.unNor8 <- subset(bitstamp, bit.KMC$cluster == 8)
bitstamp.unNor9 <- subset(bitstamp, bit.KMC$cluster == 9)

# = = = Two of the three trade's soft spot ranges = = =

tapply(bitstamp.unNor5$Open, bitstamp.unNor5$YearMonth, mean)
mean(bitstamp.unNor5$Open)
mean(bitstamp.unNor5$BTC)

tapply(bitstamp.unNor7$Open, bitstamp.unNor7$YearMonth, mean)
mean(bitstamp.unNor7$Open)
mean(bitstamp.unNor7$BTC)

# = = = = = = BTC vs Open graph in 9 clusters = = = = =  

BTC.Clus1 <- ggplot(bitstamp.unNor1, aes(y=BTC, x=Open)) +
  geom_density2d( col = "red", alpha = 0.7)+
  facet_grid(.~Year, scales = "free")+
  xlab("K-means' 1st Cluster - Open price")+
  ylab("Volume BTC")+
  theme(legend.position = "top")
BTC.Clus2 <- ggplot(bitstamp.unNor2, aes(y=BTC, x=Open)) +
  geom_density2d(col = "darkgreen", alpha = 0.7)+
  facet_grid(.~Year, scales = "free")+
  xlab("K-means' 2nd Cluster - Open price")+
  ylab("Volume BTC")+
  theme(legend.position = "top")
BTC.Clus3 <- ggplot(bitstamp.unNor3, aes(y=BTC, x=Open)) +
  geom_density2d(col = "navyblue", alpha = 0.7)+
  facet_grid(.~Year, scales = "free")+
  xlab("K-means' 3rd Cluster - Open price")+
  ylab("Volume BTC")+
  theme(legend.position = "top")
BTC.Clus4 <- ggplot(bitstamp.unNor4, aes(y=BTC, x=Open)) +
  geom_density2d( col = "red", alpha = 0.7)+
  facet_grid(.~Year, scales = "free")+
  xlab("K-means' 4th Cluster - Open price")+
  ylab("Volume BTC")+
  theme(legend.position = "top")
BTC.Clus5 <- ggplot(bitstamp.unNor5, aes(y=BTC, x=Open)) +
  geom_density2d( col = "darkgreen", alpha = 0.7)+
  facet_grid(.~Year, scales = "free")+
  xlab("K-means' 5th Cluster - Open price")+
  ylab("Volume BTC")+
  theme(legend.position = "top")
BTC.Clus6 <- ggplot(bitstamp.unNor6, aes(y=BTC, x=Open)) +
  geom_density2d(col = "navyblue", alpha = 0.7)+
  facet_grid(.~Year, scales = "free")+
  xlab("K-means' 6th Cluster - Open price")+
  ylab("Volume BTC")+
  theme(legend.position = "top")
BTC.Clus7 <- ggplot(bitstamp.unNor7, aes(y=BTC, x=Open)) +
  geom_density2d( col = "red", alpha = 0.7)+
  facet_grid(.~Year, scales = "free")+
  xlab("K-means' 7th Cluster - Open price")+
  ylab("Volume BTC")+
  theme(legend.position = "top")
BTC.Clus8 <- ggplot(bitstamp.unNor8, aes(y=BTC, x=Open)) +
  geom_density2d( col = "darkgreen", alpha = 0.7)+
  facet_grid(.~Year, scales = "free")+
  xlab("K-means' 8th Cluster - Open price")+
  ylab("Volume BTC")+
  theme(legend.position = "top")
BTC.Clus9 <- ggplot(bitstamp.unNor9, aes(y=BTC, x=Open)) +
  geom_density2d( col = "navyblue", alpha = 0.7)+
  facet_grid(.~Year, scales = "free")+
  xlab("K-means' 9th Cluster - Open price")+
  ylab("Volume BTC")+
  theme(legend.position = "top")
Clusters.BTC <- grid.arrange(BTC.Clus1, BTC.Clus2, BTC.Clus3, BTC.Clus4, BTC.Clus5, BTC.Clus6, BTC.Clus7, BTC.Clus8, BTC.Clus9)
Clusters.BTC

# = = = = = = To be continued = = = = =  