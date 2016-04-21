path <- "C:/Users/akhoufi/Documents/Analytics Vidhya Workshop"
setwd(path)
train <- read.csv("train.csv")
test <- read.csv("test.csv")

#Univariate Analysis
str(train)

train_cont <- subset(train, select=c(ID,Age, Hours.Per.Week))
train_cat <- subset(train, select=-c(ID,Age, Hours.Per.Week))
summary(train_cont)

library("pastecs")
options(scipen=100)
options(digits=2)
stat.desc(train_cont)

apply(train_cat,2, function(x){length(unique(x))})

table(train_cat$Race)
as.matrix(prop.table(table(train_cat$Race)))

head(sort(table(train_cat$Native.Country),decreasing = TRUE),20)
head(round(sort(prop.table(table(train_cat$Native.Country)),decreasing = TRUE),6),20)
  
IQR(train_cont$Age) 

#Multivariate Analysis
library(gmodels)
CrossTable(train$Sex, train$Income.Group)
library(ggplot2)
ggplot(train,aes(Sex,fill=Income.Group))+ geom_bar()+labs(title="Stacked Bar Chart",x="Sex",y="Count")+theme_bw()

ggplot(train, aes(Sex,Hours.Per.Week))+geom_boxplot()+labs(title="Boxplot")

#Missing Values Treatment
train[train==""]<-NA
table(is.na(train))
colSums(is.na(train))
test[test==""]<-NA
colSums(is.na(train))
library(mlr)
imputed_data<-impute(train,classes=list(factor=imputeMode()))
train<-imputed_data$data
colSums(is.na((train)))

imputed_test_data<-impute(test, classes=list(factor=imputeMode()))
test<-imputed_test_data$data
colSums(is.na(test))

#Outlier Treatment
library(ggplot2)
ggplot(train,aes(ID,Age))+geom_jitter()
ggplot(train,aes(ID,Hours.Per.Week))+geom_jitter()

#Variable Transformation
sapply(train,class)
as.matrix(prop.table(table(train$Workclass)))
library(car)
train$Workclass <- recode(train$Workclass, "c('State-gov','Self-emp-inc','Federal-gov','Without-pay','Never-worked')='Others'")
test$Workclass <- recode(test$Workclass, "c('State-gov','Self-emp-inc','Federal-gov','Without-pay','Never-worked')='Others'")
as.matrix(prop.table(table(train$Workclass)))
