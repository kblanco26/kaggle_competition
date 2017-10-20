
setwd("~/Documents/GitHub/kaggle_competition/titanic/data")
installed.packages("tidyverse")
library(tidyverse)

gender_submission <- read.csv(file="gender_submission.csv",stringsAsFactors = FALSE)
test <- read.csv(file="test.csv", stringsAsFactors = FALSE)
train <- read.csv(file="train.csv",stringsAsFactors = FALSE)

train$Survived <- factor(train$Survived)

train$Sex <- factor(train$Sex)
train$Embarked <- factor(train$Embarked,levels=c("C","Q","S"),
                        labels=c("Cherbourg","Queenstown","Southampton"))
train$Pclass <- factor(train$Pclass)

unique(train$Embarked)
testing <- train[train$Embarked == "",]

 ggplot(data=train, aes(x=Survived, fill=Embarked)) +
    geom_bar()
 
 ggplot(data=train, aes(fill=Pclass, x=Embarked)) +
   geom_histogram(position="dodge")
 

 