#################################################
# titanic.R                                     #
#                                               #
# Description: Machine learning for Titanic     #
# data set on Kaggle                            #
#                                               #
#                                               #
#                                               #
# Authors: The Angry Zombies                    #
#################################################

setwd("~/Documents/GitHub/kaggle_competition/titanic/code")

##################
#### Packages ####
##################
library(tidyverse)
library(randomForest)
library(Amelia)

##################
#### Get Data ####
##################
training_set <- read.csv(file = "~/Documents/GitHub/kaggle_competition/titanic/data/train.csv", stringsAsFactors = FALSE)

# Format data
colnames(training_set) <- tolower(colnames(training_set))
training_set$survived <- factor(training_set$survived, levels = c(0,1), labels = c("No", "Yes"))
training_set$pclass <- factor(training_set$pclass, levels = c(1,2,3), labels = c("Upper", "Middle", "Lower"))
training_set$sex <- factor(training_set$sex)
training_set$cabin <- factor(training_set$cabin)
training_set$embarked <- factor(training_set$embarked, levels = c("C", "Q", "S"), labels = c("Cherbourg", "Queenstown", "Southampton"))

summary(training_set)

#############################
#### Impute Missing Data ####
#############################


#######################
#### Visualization ####
#######################
ggplot(data = training_set, aes(x = pclass, fill = survived)) + 
  geom_bar(position="dodge") +
  scale_fill_brewer(palette = "Set1")

ggplot(data = training_set, aes(x = sex, fill = survived)) + 
  geom_bar(position="dodge") +
  scale_fill_brewer(palette = "Set1")

ggplot(data = training_set, aes(x = embarked, fill = survived)) + 
  geom_bar(position="dodge") +
  scale_fill_brewer(palette = "Set1")

#######################
#### Decision Tree ####
#######################
titanic_rf <- randomForest(survived ~ pclass + sex + age + fare + embarked, data = training_set)
