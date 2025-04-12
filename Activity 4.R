#library
library(ggplot2)
library(mosaic)
library(GGally)


#1 check variable type
str(workers)

#change variables to their right categories
workers$Stay<-as.factor(workers$Stay)
workers$Assembly<-as.factor(workers$Assembly)
workers$Female<-as.factor(workers$Female)

#2 check response variable type
class(workers$Stay)

#3a Obtain the median age and IQR of age for workers that stay for at least a year and those that do not
favstats(Age~Stay, data=workers)

#3b Boxplot of age against stay status
ggplot(data = workers, aes(x=Stay,y=Age))+
  geom_boxplot()+
  ggtitle("Boxplot of age against stay status")+
  xlab("Stay Status")+
  ylab("Age")

#4a proportionality inputs for table that examines whether the proportion of workers that stay for at least a year differs by previous assembly line experience
assembly_exp<-table(workers$Stay, workers$Assembly, dnn=c("Assembly", "Stay"), useNA = "always")
assembly_exp

round(prop.table(assembly_exp, margin = 2)*100,2)

#4b barchart that examines whether the proportion of workers that stay for at least one year differs by assembly line experience.
ggplot(data = workers, aes(x=Assembly,fill=Stay))+
  geom_bar(position="fill")+
  ggtitle("Barchart of Assembly by Stay")+
  ylab("Count")+
  xlab("Assembly experience (1=Yes,2=No)")
  

#5 odds of staying for at least one year
prop.table(table(workers$Stay))

#6 logistic regression model for predicting whether an employee stays for at least one year based on Age, Female, and Assembly

workers_logreg<-glm(Stay~Age+Female+Assembly, family=binomial (link="logit"), data = workers)
summary(workers_logreg)

#8 obtain 95% significance level
confint(workers_logreg)

#9 obtain sofia's probability
sofia_obs<-data.frame(Age=40, Female="1", Assembly="0")
predict(workers_logreg, newdata=sofia_obs, type = "response")

