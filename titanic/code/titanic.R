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
rm(list = ls())
setwd("~/Documents/GitHub/kaggle_competition/titanic/code")

##################
#### Packages ####
##################
library(tidyverse)
library(gridExtra)
library(modelr)
library(broom)
library(VIM)
library(randomForest) # Random Forest
library(mice) # Imputation
library(e1071) # Support Vector Machine

###################
#### Functions ####
###################

# Root mean square error
rmse <- function(error)
{
  sqrt(mean(error^2))
}

##################
#### Get Data ####
##################
training_set <- read.csv(file = "~/Documents/GitHub/kaggle_competition/titanic/data/train.csv", stringsAsFactors = FALSE)
test_set <- read.csv(file = "~/Documents/GitHub/kaggle_competition/titanic/data/test.csv", stringsAsFactors = FALSE)

# Combine training and test set
full_data <- bind_rows(training_set,test_set)

# Format data
colnames(full_data) <- tolower(colnames(full_data)) # all lower case column names
full_data$survived <- factor(full_data$survived, levels = c(0,1), labels = c("No", "Yes")) # assign survival labels
full_data$pclass <- factor(full_data$pclass, levels = c(1,2,3), labels = c("Upper", "Middle", "Lower")) # assign class labels
full_data$sex <- factor(full_data$sex, levels = c("female", "male"), labels = c("Female","Male")) 
full_data$embarked <- factor(full_data$embarked, levels = c("C", "Q", "S"), labels = c("Cherbourg", "Queenstown", "Southampton")) # assign embarkation dock labels
full_data$cabin <- substring(text = full_data$cabin, first = 1, last = 1) # first letter of cabin indicates boat level
full_data$cabin <- factor(full_data$cabin) 
full_data[full_data$cabin == '',]$cabin <- NA # set empty strings to NA
full_data$family_size <- full_data$sibsp + full_data$parch + 1 # calculate family size from num. of siblings/spouses & num. of parents/children

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
mice_mod <- mice(full_data[, !names(full_data) %in% c('passengerid','name','ticket','cabin','survived')], method='rf') 
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
full_data$fare <- mice_output$fare
sum(is.na(full_data$age))
sum(is.na(full_data$embarked))
sum(is.na(full_data$fare)) # num. missing fares

summary(full_data)

# Partition data into training and testing set
training_set <- full_data[!is.na(full_data$survived),]
test_set <- full_data[is.na(full_data$survived),]
rm(full_data, mice_mod, mice_output)

#######################
#### Visualization ####
#######################
# Plot survival outcome based on multiple variables
ggplot(data = training_set, aes(x = pclass, fill = survived)) + 
  geom_bar(position="dodge") +
  xlab("Class") + 
  ylab("Count") + 
  scale_fill_brewer(palette = "Set1", name = "Survived") 

ggplot(data = training_set, aes(x = family_size, fill = survived)) + 
  geom_bar(position="dodge") +
  xlab("Family Size") + 
  ylab("Count") + 
  scale_fill_brewer(palette = "Set1", name = "Survived") 

ggplot(data = training_set, aes(x = sex, fill = survived)) + 
  geom_bar(position="dodge") +
  xlab("Sex") + 
  ylab("Count") + 
  scale_fill_brewer(palette = "Set1", name = "Survived") 

ggplot(data = training_set, aes(x = embarked, fill = survived)) + 
  geom_bar(position="dodge") +
  xlab("Port of Embarkation") + 
  ylab("Count") + 
  scale_fill_brewer(palette = "Set1", name = "Survived") 

ggplot(data = training_set, aes(x = fare, fill = survived)) + 
  geom_histogram(bins = 5, position = "dodge") +
  xlab("Fare Paid") + 
  ylab("Count") + 
  scale_fill_brewer(palette = "Set1", name = "Survived") 

#######################
#### Random Forest ####
#######################
# Set random seed
set.seed(189378)

# Random forest model
titanic_rf <- randomForest(formula = survived ~ pclass + sex + age + fare + embarked + family_size, 
                           ntree = 1000,
                           classwt = c(0.5,0.5),
                           importance = TRUE,
                         data = training_set)
print(titanic_rf)

# Plot MSE
plot(titanic_rf)
legend('topright', colnames(titanic_rf$err.rate), col=1:3, fill=1:3)
varImpPlot(titanic_rf)

# Variable importance table
variable_importance <- data.frame(importance(titanic_rf,type=2))
variable_importance$variables <- row.names(variable_importance)
colnames(variable_importance)[1] <- "mean_decrease_gini"
variable_importance[order(variable_importance$mean_decrease_gini,decreasing = TRUE),]

#############################
#### Logistic Regression ####
#############################
titanic_lr <- glm(formula = survived ~ pclass + sex + age + fare + embarked + family_size, family = binomial(link = "logit"), 
                  data = training_set)
summary(titanic_lr)

# k-fold cross validation
folds <- crossv_kfold(training_set, k = 10) # split data into 10 folds
folds <- folds %>% mutate(model = map(train, ~ glm(formula = survived ~ pclass + sex + age + fare + embarked + family_size, family = binomial(link = "logit"), data = .)))

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
