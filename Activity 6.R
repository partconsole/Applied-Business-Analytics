#Ethan, Minh

#Activity 6

library(caret)
library(rpart)
library(rpart.plot)
library(forecast)
library(readr)
thera <- read_csv("Documents/EM 150/thera.csv")
str(thera)
thera$Education<-as.factor(thera$Education)
thera$Securities<-as.factor(thera$Securities)
thera$CD<-as.factor(thera$CD)
thera$Online<-as.factor(thera$Online)
thera$CreditCard<-as.factor(thera$CreditCard)
thera$PersonalLoan<-as.factor(thera$PersonalLoan)

#1 Proportion of converted customers
freq_tab<-table(thera$PersonalLoan,useNA="always")
round(prop.table(freq_tab)*100,2)

#2 Partitioning data
set.seed(150)
Partition<-createDataPartition(thera$PersonalLoan,p=0.7,list=FALSE)
trainingSetP<-thera[Partition,]
validationSetP<-thera[-Partition,]
#3 default tree 
default_treeT<-rpart(PersonalLoan~.,data=trainingSetP,method="class")
prp(default_treeT,type=1,extra=1,under=TRUE)
#4 grow full tree
grow<-rpart(PersonalLoan~.,data=trainingSetP,method="class",
                 cp=0,minsplit=2,minbucket=1)
#best pruned tree
printcp(grow)


#5 best tree grown
best<-prune(grow,cp=0.0016534)
prp(best)
varImp(best)

#6 Confusion matrix
Predicted_class<-predict(best,validationSetP, type="class")
confusionMatrix(Predicted_class,validationSetP$PersonalLoan,positive="1")

#7 loading data set and converting variables as needed
thera_score <- read_csv("Documents/EM 150/thera_score.csv")
str(thera_score)
thera_score$Education<-as.factor(thera_score$Education)
thera_score$Securities<-as.factor(thera_score$Securities)
thera_score$CD<-as.factor(thera_score$CD)
thera_score$Online<-as.factor(thera_score$Online)
thera_score$CreditCard<-as.factor(thera_score$CreditCard)
#predicting which customers will convert
predict(best, thera_score, type="class")

