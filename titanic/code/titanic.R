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
library(gridExtra)
library(randomForest)
library(mice)
library(VIM)

##################
#### Get Data ####
##################
training_set <- read.csv(file = "~/Documents/GitHub/kaggle_competition/titanic/data/train.csv", stringsAsFactors = FALSE)
test_set <- read.csv(file = "~/Documents/GitHub/kaggle_competition/titanic/data/test.csv", stringsAsFactors = FALSE)

# Combine training and test set
full_data <- bind_rows(training_set,test_set)

# Format data
colnames(full_data) <- tolower(colnames(full_data))
full_data$survived <- factor(full_data$survived, levels = c(0,1), labels = c("No", "Yes"))
full_data$pclass <- factor(full_data$pclass, levels = c(1,2,3), labels = c("Upper", "Middle", "Lower"))
full_data$sex <- factor(full_data$sex)
full_data$embarked <- factor(full_data$embarked, levels = c("C", "Q", "S"), labels = c("Cherbourg", "Queenstown", "Southampton"))
full_data$cabin <- substring(text = full_data$cabin, first = 1, last = 1) # first letter of cabin indicates boat level
full_data$cabin <- factor(full_data$cabin) 
full_data[full_data$cabin == '',]$cabin <- NA # set empty strings to NA

#############################
#### Impute Missing Data ####
#############################
summary(full_data)
sum(is.na(full_data$age)) # num. missing age
sum(is.na(full_data$embarked)) # num. missing embarked
sum(is.na(full_data$fare)) # num. missing fares

# Distribution of age before missing age is imputed
p1 <- 
  ggplot(data = full_data, mapping = aes(x = age)) + 
  geom_histogram(color = "black", fill = "gray21") +
  xlab("Age") +
  ylab("Count") +
  ggtitle("Distribution of Age: Original Data")

# Visualize missing patterns
md.pattern(full_data)
aggr(full_data, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(data), cex.axis=.7, gap=0.5, ylab=c("Histogram of missing data","Pattern"))

# Perform mice imputation using relevant variables
mice_mod <- mice(full_data[, !names(full_data) %in% c('passengerid','name','ticket','cabin','survived')], method='pmm') 
mice_output <- complete(mice_mod)

# Distribution of age after missing age is imputed
p2 <-
  ggplot(data = mice_output, mapping = aes(x = age)) + 
  geom_histogram(color = "black", fill = "gray21") +
  xlab("Age") +
  ylab("Count") +
  ggtitle("Distribution of Age: Imputed Data")
grid.arrange(p1,p2)

# Replace original age data (w/ missing variables) with complete, imputed data
full_data$age <- mice_output$age
full_data$embarked <- mice_output$embarked
sum(is.na(full_data$age))
sum(is.na(full_data$embarked))

# # Compare to Titanic tutorial
# mice_output[c(62, 830),]$embarked
# mice_output[1044,]
# median(full_data[full_data$pclass == 'Lower' & full_data$embarked == 'Southampton', ]$fare, na.rm = TRUE)

#######################
#### Visualization ####
#######################
# Plot survival outcome based on multiple variables
ggplot(data = full_data[!is.na(full_data$survived),], aes(x = pclass, fill = survived)) + 
  geom_bar(position="dodge") +
  scale_fill_brewer(palette = "Set1")

ggplot(data = full_data[!is.na(full_data$survived),], aes(x = sex, fill = survived)) + 
  geom_bar(position="dodge") +
  scale_fill_brewer(palette = "Set1")

ggplot(data = full_data[!is.na(full_data$survived),], aes(x = embarked, fill = survived)) + 
  geom_bar(position="dodge") +
  scale_fill_brewer(palette = "Set1")

#######################
#### Decision Tree ####
#######################
titanic_rf <- randomForest(survived ~ pclass + sex + age + fare + embarked, data = full_data)
