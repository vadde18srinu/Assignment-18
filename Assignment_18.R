1. Use the below given data set
DataSet
2. Perform the below given activities:
a. Create classification model using different decision trees.
b. Verify model goodness of fit.
c. Apply all the model validation techniques.
d. Make conclusions

setwd("F:/AcadGild/workings")

library(readr)
library(Hmisc)
library(dplyr)
library(MASS)
library(ggplot2)
library(lattice)
library(car)
library(caret)
library(rpart)
library(randomForest)
library(rpart)
library(rpart.plot)
library(RColorBrewer)
library(rattle)

#import test data set
Edata<-read.csv("F:/AcadGild/workings/Example_WearableComputing_weight_lifting_exercises_biceps_curl_variations_18.csv", header = TRUE)
summary(Edata)
str(Edata)

# list the levels for the class
sapply(Edata, class)

# Delete all columns missing values
sapply(Edata, function(x) sum(is.na(x))) # missing values  

Data <-Edata[,colSums(is.na(Edata)) == 0]
str(Data)
dim(Data)
sapply(Data,class)

# Delete variables irrelevant to the current project
Training <- Data[,-c(1:6)]

# data partition is split in to 70%Training 30%Testing. 
set.seed(1234)
ind=sample(1:nrow(Training),0.7*nrow(Training),replace = FALSE)
Train<-Training[ind,]
Test<-Training[-ind,]

# exploratory analysis
dim(Train)
str(Train)
describe(Train)
head(Train)

dim(Test)
str(Test)
describe(Test)
head(Test)

# data compressed due to heavy (to see output results)
set.seed(1234)
ten<-sample(1:nrow(Train), 0.2*nrow(Train),replace = FALSE)
tenth<-Train[ten,]
dim(tenth)

testten<-sample(1:nrow(Test), 0.2*nrow(Test),replace=FALSE)
testtenth<-Test[testten,]
dim(testtenth)

# Prediction First model - Decesion Tree
Model1 <- rpart(classe ~ ., data=tenth, method="class")

#View the Decision Tree using fancy
fancyRpartPlot(Model1)

#Predicting
FirstPrediction <- predict(Model1, testtenth, type = "class")

# Using confusion Matrix to test results:
confusionMatrix(FirstPrediction, testtenth$classe)

#Second Prediction Model - Random Forests (redused variables 53 due to model not accepting above 53 variable)
Ttenth<-tenth[,-c(5:8,31:36,55:63,40:41)]
dim(Ttenth)

SecondModel <- randomForest(classe ~. , data=Ttenth, method="class")

#Predicting:
SecondPrediction <- predict(SecondModel, testtenth, type = "class")

#Test results on TestingTraining data set:
confusionMatrix(SecondPrediction, testtenth$classe)

#Testing the better model on original Testing Set
FinalPrediction <- predict(SecondModel, Ttenth, type="class")
FinalPrediction

confusionMatrix(FinalPrediction, Ttenth$classe)

describe(FinalPrediction)

df<-write.csv(FinalPrediction,"Assignment18pred.csv")
df


# goones of fit - chi square test. 

final.table<-table(FinalPrediction)
final.table

barplot(final.table, xlab="class", ylim = c(0,200))

final.proportion.tab<-(final.table/sum(final.table))
final.proportion.tab

barplot((final.proportion.tab)*100, main = "percentage of class", ylim = c(0,40))

chisq.test(final.table)

chisq.test(final.proportion.tab)




