#libraries
library(ggplot2)
library(mosaic)
library(ggfortify)
library(caret)
library(rpart)
library(rpart.plot)
library(forecast)

#1 find number of obervations
dim(hr)

#2 variables types
str(hr)

##change types
hr$FromDiversityJobFairID<-as.factor(hr$FromDiversityJobFairID)
hr$Termd<-as.factor(hr$Termd)

#3 identify categories for performance ratings
unique(hr$PerformanceScore)

hr_tab<-table(hr$PerformanceScore, useNA="always")
hr_tab

#4 boxplot
ggplot(data=hr, aes(x=EngagementSurvey))+
  geom_boxplot()+
  ggtitle("Histogram")+
  xlab("xlab")+
  ylab("ylab")

favstats(hr$EngagementSurvey)

#5 side by side boxplots.
ggplot(data=hr, aes(x=EngagementSurvey, y=PerformanceScore))+
  geom_boxplot()+
  ggtitle("title")+
  xlab("Engagement Score")+
  ylab("Performance Score")


###QUESTION 1
#1 
unique(hr$RaceDesc)

race_tab<-table(hr$RaceDesc, useNA="always")
race_tab

#2a 
unique(hr$RecruitmentSource)

rec_tab<-table(hr$RecruitmentSource, useNA="always")
rec_tab

#2b
rec_prop<-table(hr$RecruitmentSource)
prop.table(rec_prop)

#3a
ggplot(data=hr, aes(x=RaceDesc, fill=RecruitmentSource))+
  geom_bar(position="fill")+
  ggtitle("Race by Recruitment Score")+
  ylab("Count")+
  xlab("Race")

#4
ggplot(data=hr, aes(x=Age))+
  geom_histogram()+
  ggtitle("Distribution of Age")+
  xlab("Race")+
  ylab("Frequency")

fav_stats(hr$Age)

#5
ggplot(data=hr, aes(x=Age, y=RecruitmentSource))+
  geom_boxplot()+
  ggtitle("Age vs RecruitmentSource")+
  xlab("Age")+
  ylab("RecruitmentSource")

favstats(hr$Age~hr$RecruitmentSource)


###Question 2:
#1
unique(hr$Absences)

favstats(hr$Absences)

#2a scatterplot
ggplot(data=hr, aes(x=Absences,y=EngagementSurvey))+
  geom_point()+
  ggtitle("Distribution of Age")+
  xlab("Absences")+
  ylab("Engagement Score")+
  geom_smooth(method="lm", se=FALSE)

#3a
hr_reg<-lm(Absences~EngagementSurvey+PerformanceScore+Age+Sex, data=hr)
summary(hr_reg)

#3c
cor(hr$Absences, hr$EngagementSurvey)

#3d
autoplot(hr_reg)

#4b
cor(hr$Absences, hr$EmpSatisfaction)

###Question 3:
#1
termd_prop<-table(hr$Termd)
prop.table(termd_prop)

#2
unique(hr$TermReason)

#3 scatterplot
ggplot(data=hr, aes(x=PerformanceScore, fill=Termd))+
  geom_bar(position="fill")+
  ggtitle("Relationship between Performance and Termination")+
  xlab("Performance")+
  ylab("Termination")

#5
new_hr<-hr[,-c(10,11)]

#7
#1 seed
set.seed(150)
Index<-createDataPartition(new_hr$Termd,p=0.7,list=FALSE)
trainSet<-new_hr[Index,]
validationSet<-new_hr[-Index,]

#2: default tree
default_tree<-rpart(Termd~.,data=trainSet,method="class")
prp(default_tree,type=1,extra=1,under=TRUE)

#3 full tree
full_tree<-rpart(Termd~.,data=trainSet,method="class",
                 cp=0,minsplit=2,minbucket=1)

#4 best pruned tree
printcp(full_tree)

#prune
pruned_tree<-prune(full_tree,cp=0.0616438)

#view tree
prp(pruned_tree)

#5 model performance
predicted_class<-predict(pruned_tree,validationSet,
                         type="class")
confusionMatrix(predicted_class,validationSet$Termd,
                positive="1")









