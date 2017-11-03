###################################################
# Lorenzo_Titanic_code.R                          #
#                                                 #
# Description: Data Cleaning and Machine          #
#              Learning in R on                   #
#              Titainic survival data             #
#                                                 #
# Author: Lorenzo Tapia                           #
###################################################

setwd("~/Documents/GitHub/kaggle_competition/titanic/data")

###################
#### Packages #####
###################
library(tidyverse)
library(rmarkdown)
library(mice)
library(randomForest)
library(modelr)
library(broom)
library(VIM)
library(dplyr)
library(purrr)

##########################
#### Loading Raw Data ####
##########################
gender_submission <- read.csv(file="gender_submission.csv",stringsAsFactors = FALSE)
test <- read.csv(file="test.csv", stringsAsFactors = FALSE)
train <- read.csv(file="train.csv",stringsAsFactors = FALSE)

full_data <- bind_rows(train, test)

#########################
#### Data Inspection ####
#########################
names(full_data)
head(full_data)
str(full_data)
summary(full_data)

# Change string variables to factors
full_data$Survived <- factor(full_data$Survived)
full_data$Sex      <- factor(full_data$Sex)

# Relabel less intuative data elements
full_data$Embarked <- factor(full_data$Embarked,levels=c("C","Q","S"),
                        labels=c("Cherbourg","Queenstown","Southampton"))
full_data$Pclass <- factor(full_data$Pclass,levels=c(1,2,3),
                         labels=c("Upper","Middle","Lower"))
full_data$Survived <- factor(full_data$Survived,levels=c("0","1"),
                           labels=c("No","Yes"))

# Create variable family size based on the amount of 
# parents, children, spoused and themselves.
full_data$Familysize <- full_data$SibSp + full_data$Parch + 1

#################################
#### Imputing Missing Values ####
#################################
mice_mod <- mice(full_data[, !names(full_data) %in%
              c('PassengerId','Name','Ticket','Cabin','Family','Surname','Survived')], method='rf') 
mice_output <- complete(mice_mod)

#Replacing missing ages with imputed ages
full_data$Age <- mice_output$Age
full_data$Embarked <- mice_output$Embarked

#Split the data back into test and train
new_test <- full_data[is.na(full_data$Survived),]
new_train <- full_data[!is.na(full_data$Survived),]

#Check if all missing values have been filled
which(!complete.cases(new_train))
new_train[which(!complete.cases(new_train)),]
#############################
#### Logistic Regression #### 
#############################
set.seed(12345)
log_train <- new_train[1:799,]
log_test <- new_train[800:891,]
log_test2 <- new_train[800:891,]
log_test2$Survived <- NULL
titanic_lr <- glm(Survived ~ Age+Pclass+Sex+Familysize+Embarked+Fare,
                    family=binomial(link='logit'),data=log_train)
summary(titanic_lr)

fitted.percent <- predict(titanic_model, log_test2,type='response')
fitted.results <- ifelse(fitted.percent > 0.5,"Yes","No")
fitted.results != log_test$Survived
misClasificError <- mean(fitted.results != log_test$Survived)
1-misClasificError
# Save the solution to a dataframe with two columns: PassengerId and Survived (prediction)
solution <- data.frame(PassengerID = log_test$PassengerId, Survived_Predict = fitted.results,
                      Survived=log_test$Survived)

#####################################
#### K-Folds Logistic Regression #### 
#####################################
# k-fold cross validation
folds <- crossv_kfold(log_train, k = 10) # split data into 10 folds
folds <- folds %>% mutate(model =
            map(train, ~ titanic_lr))

# Logistic regression model built on first split of training and test set
folds$model[[1]] %>% summary()

# Test k models on test sets
predicted <- folds %>% unnest(map2(model, test, ~ augment(.x, newdata = .y, type.predict = "response")))
predicted

# Translate logisitic regression probabilities to indicator variables for survival and death
predicted$.fitted <- ifelse(test = round(predicted$.fitted) == 0, "No", "Yes")
predicted <- predicted %>% 
  mutate(residual = .fitted == survived)
# Error rate
predicted %>% group_by(residual) %>% summarise (n = n()) %>%
  mutate(freq = n / sum(n))

