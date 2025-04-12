Activity 2: Homeownership and loan

#Minh

library(readr)
library(ggplot2)
library(mosaic)
loans <- read_csv("loans.csv")

#1 categories of variable homeownership
#a.
unique(loans$homeownership)

#Bii
#Frequency of homeownership
home_cat<-table(loans$homeownership, useNA = "always")
print(home_cat)
#Biii
#relative frequency of homeownership
rel_home<-round(prop.table(home_cat)*100,2)
print(rel_home)

#C bar chart for homeownership variable
ggplot(data=loans,aes(x=homeownership))+
  geom_bar()+
  ggtitle("Bar Chart of Homeownership")+
  xlab("Homeownership")+
  ylab("Frequency")

#2 B.) Bivariate relationship between Homeownership and loan grade
ggplot(data=loans,aes(x=grade, y=..prop..,group=1))+
  geom_bar()+
  ggtitle("Bar Charts for Loan Grade Between Types of Homeownership")+
  xlab("Loan Grade by Homeownership")+
  ylab("Relative Frequency")+
  facet_wrap(~homeownership)

#3 looking at variable Loan Amount
#a. Basic statistics on loan amount variable
favstats(loans$loan_amount)

#b. Histogram for loan amount variable
ggplot(data=loans,aes(x=loan_amount))+
  geom_histogram(bin=25 )+
  ggtitle("Histogram for Loan Amount")+
  xlab("Loan Amount (USD$)")+
  ylab("Frequency")


#4 bivariate relationship between loan amount and homeownership
#B.) 
ggplot(data=loans,aes(x=loan_amount))+
  geom_histogram(bins=30)+
  ggtitle("Histogram for Loan Amount by Homeownership")+
  xlab("Loan Amount (USD$)")+
  ylab("Frequency")+
  facet_wrap(~homeownership)