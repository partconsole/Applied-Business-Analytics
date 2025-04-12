#Ethan & Minh

#Activity 7
library(mosaic)
library(ggplot2)
library(cluster)
library(readr)
HC <- read_csv("Documents/EM 150/HC.csv")
HC$Nationality<-as.factor(HC$Nationality)
HC$MarketSegment<-as.factor(HC$MarketSegment)

#1A
dim(HC)

#1B amount of different nationalities
length(unique(HC$Nationality))

#1C Distribution of Market Segment
ggplot(HC, aes(x=MarketSegment))+
  geom_bar()+
  ggtitle("Barchart of Market Segment")+
  xlab("Market Segment")
market_table<-table(HC$MarketSegment,useNA="always")
market_table
round(prop.table(market_table)*100,2)

#1D Distribution of Avg. Lead Time
ggplot(HC, aes(x=AverageLeadTime))+ 
  geom_histogram()+
  ggtitle("Histogram of Average Lead Time")+
  xlab("Average Lead Time (in days)")
favstats(HC$AverageLeadTime)

#2a Gower's distance
distance<-daisy(HC, metric = "gower")

#2b cluster analysis
HC_results<-agnes(distance,method="ward")


#2c
plot(HC_results,which.plots=2,main="Dendrogram")

#2d
#4 clusters
HC$clusters4<-cutree(HC_results,k=4)
summary(HC[HC$clusters4==1,])
summary(HC[HC$clusters4==2,])
summary(HC[HC$clusters4==3,])
summary(HC[HC$clusters4==4,])

