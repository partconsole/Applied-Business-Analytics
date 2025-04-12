#library
library(ggplot2)
library(mosaic)

install.packages("GGally")
library(GGally)

install.packages("ggfortify")
library(ggfortify)

#2 Check variable type
str(duke_forest)

#3 Population regression model/line
duke_reg<-lm(price ~ bed + bath + area + year_built + cooling + lot, data = duke_forest)
summary(duke_reg)

#4 Scatterplot of sale price against area of the home
ggplot(data = duke_forest, aes(x=area,y=price))+
  geom_point()+
  ggtitle("Scatterplot of Price against area of the home")+
  xlab("Area of the home (ft2)")+
  ylab("Price ($)")+
  geom_smooth(method="lm",se=FALSE)

#5 Boxplot of sale price against type of cooling system of the home
ggplot(data=duke_forest,aes(x=cooling,y=price))+
  geom_boxplot()+
  ggtitle("Scatterplot of Price against type of cooling system of the home")+
  xlab("Type of cooling system")+
  ylab("Price ($)")

#6 Scatterplot matrix
ggpairs(duke_forest)

#7 Summary table for parameter estimates
summary(duke_reg)

#9 95% CI for variable cooling
confint(duke_reg)

#10 predicting Silvivo's house price
silvio_houseprice<-data.frame(bed=4, bath=3, area=2624, year_built=1992, cooling="central", lot=0.35)
predict(duke_reg,newdata=silvio_houseprice)

#11 diagnostic plot
autoplot(duke_reg)
