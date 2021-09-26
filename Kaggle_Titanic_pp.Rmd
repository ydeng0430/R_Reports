---
title: "Titanic - Machine Learning from Disaster (Kaggle Competition)"
author: "Yixi Deng"
output:
  html_document:
    df_print: paged
    toc: true
    toc_float: true
---

# Score: 0.78947

# Loading the data
```{r}
setwd("C:/Users/User/Desktop/JobSeeking/Project/Kaggle/titanic/120721")
train <- read.csv(file="train.csv", stringsAsFactors = FALSE, header = TRUE)
test <- read.csv(file = "test.csv", stringsAsFactors = FALSE, header = TRUE)
```

## Overview
As we have read in the dataset as a `data.table` object, we can use `head(test)` to look the first 6 rows. Next, we can use `summary()` to check summary statistics such as mean, min, and max values for each variable to see if there are any obvious outliers in the data and if there are any nulls in any of the columns (`NA's: number of nulls` will appear in the output if there are any nulls).
```{r}
# Examine passengers data in train dataset
head(train)
```

```{r}
# Examine passengers data in test dataset
head(test)
```

```{r}
# Summarise the data to check for nulls and possible outliers for the train dataset
summary(train)
```

```{r}
# Summarise the data to check for nulls and possible outliers for the test dataset
summary(test)
```

There are 177 nulls in `Age` column in train dataset and 86 nulls in `Age` column and 1 null in `Fare` column in test dataset; morever, we found there are some blank cells in the train dataset. So next, we need to see how many nulls and blank cells are in each variable.

# Identifying missing value


## Missing values in train dataset
```{r}
# Count both NA and blank cels for train dataset
sapply(train, function(x) sum(is.na(x)|x == "")) 
```

```{r}
# Show them in percentage for train dataset
sapply(train, function(x) sum(is.na(x)|x == "")/length(x)*100) 
```

There are 177 nulls in `Age` column, 687 blank cells in `Cabin` column, and 2 blank cells in `Embarked` column, which indicate that 19.87% of data in Age column, 77.10% of data in `Cabin` column, and 0.22% of data in `Embarked` column are missing in the train dataset. We have to leave `Cabin` column with those blank cells for it has too many missing data; however, we could try to fill the missing values in both `Age` and `Embarked` based on the existing data.

## Missing values in test dataset
```{r}
# Count both NA and blank cells for test dataset
sapply(test, function(x) sum(is.na(x)|x == "")) 
```


```{r}
# Show them in percentage for test dataset
sapply(test, function(x) sum(is.na(x)|x == "")/length(x)*100)
```

There are 86 nulls in `Age` column, 327 blank cells in `Cabin` column, and 1 blank cell in `Fare` column, which indicate that 20.57% of data in `Age` column, 78.23% of data in `Cabin` column, and 0.24% of data in `Embarked` column are missing in the test dataset. We also have to leave `Cabin` column with those blank cells for it has too many missing data; on the other hand, we could try to fill the missing values in both `Age` and `Fare` based on the existing data.

# Filling missing values
## Fill the missing values in Age variable
```{r}
# Fill the missing values of Age variable in train dataset with median
age.median.train <- median(train$Age,na.rm = TRUE)
train[is.na(train$Age), "Age"] <- age.median.train
sapply(train, function(x) sum(is.na(x)|x == "")) 
```

```{r}
# Fill the missing values of Age variable in test dataset with median
age.median.test <- median(test$Age,na.rm = TRUE)
test[is.na(test$Age), "Age"] <- age.median.test
sapply(test, function(x) sum(is.na(x)|x == "")) 
```

We use the median of `Age` to fill the missing values in the `Age` column for both train and test datasets. Now no missing value exists in the `Age` column any more.

## Fill the missing values in Fare variable
```{r}
# Fill the missing values of Fare variable in test dataset with median
fare.median.test <- median(test$Fare,na.rm = TRUE)
test[is.na(test$Fare), "Fare"] <- fare.median.test
sapply(test, function(x) sum(is.na(x)|x == "")) 
```

We use the median of `Fare` to fill the missing values in the `Fare` column for test dataset. There is no missing `Fare` value now.

## Fill the missing values in Embarked variable
```{r}
# Fill the missing values of Embarked variable in test dataset with "S"
table(train$Embarked)
train[train$Embarked =='',"Embarked"] <- 'S'
table(train$Embarked) 
sapply(train, function(x) sum(is.na(x)|x == "")) 
```

We use the 'S' to fill the missing `Embarked` cell, for it is the value with the highest frequency, which would affect the result least if we use the wrong value to fill the blank cell.

# Fitting the model
Before we fit the model, we need to use `str()` to look at the format of each column to see whether we need change the format of some variables into appropriate ones.

```{r}
str(train)
```

We found `Survived`, `Pclass`, `Sex`, and `Embarked` variables need to be changed to factor format.

```{r}
train$Survived <- as.factor(train$Survived)
train$Pclass <- as.factor(train$Pclass)
train$Sex <- as.factor(train$Sex)
train$Embarked <- as.factor(train$Embarked)
```

```{r}
str(train)
```

Now, we can begin to fit our model.

First, we fit the model with all variables except `PassengerId`, `Name`, `Ticket` and `Cabin`. The reason to remove `Cabin` is obvious, for it has too many missing values. We also removed `PassengerId`, `Name`, and `Ticket`, for they are all variables with unique values, which we may take into account to use them to make new variables if we have more evidences to show they are related to the survive rate.

```{r}
model_0 <- glm(train, formula = Survived ~ Pclass + Sex + Age + SibSp + Parch  + Fare +  Embarked,
    family = "binomial")
summary(model_0)
```

In model_0, we found that the P-values of `Parch` and `Fare` are greater than 0.05, so we decided to remove `Parch` to see whether the new model could have all variables with p-values which are less than 0.05 and have a lower AIC value. 

```{r}
# Remove Parch variable
model_01 <- glm(train, formula = Survived ~ Pclass + Sex + Age + SibSp +  Fare +  Embarked,
    family = "binomial")
summary(model_01)
```

In model_01, we found the AIC value decreased which means it is a better model than model_0; however, the p-value of `Fare` is still larger than 0.05. So we remove `Fare` and fit a new model. 

```{r}
# Remove Fare variable
model_02 <- glm(train, formula = Survived ~ Pclass + Sex + Age + SibSp + Embarked,
    family = "binomial")
summary(model_02)
```


Now, the model_02 has all variables with p-values which are less than 0.05 and has the smallest AIC value, so it is our final fitted model.

# Prediction
Before we use the fitted model to predict `Survived` values in test data, we need to use `str()` to look at the format of each column to see whether the formats of all variables are the same as those in train dataset. 
```{r}
str(test)
```

`Survived`, `Pclass`, `Sex`, and `Embarked` variables need to be changed to factor format.

```{r}
test$Survived <- NA
test$Survived <- as.factor(test$Survived)
test$Pclass <- as.factor(test$Pclass)
test$Sex <- as.factor(test$Sex)
test$Embarked <- as.factor(test$Embarked)
```

```{r}
str(test)
```

Now, we are ready to predict with random forest algorithm.

```{r}
library(randomForest)
```


```{r}
model <- randomForest(formula = Survived ~ Pclass + Sex + Age + SibSp + Embarked, 
                      data = train, ntree = 500, ntry = 3, nodesize = 0.01 * nrow(test))

Survived <- predict(model, newdata=test)

PassengerId <- test$PassengerId
output.df <- as.data.frame(PassengerId)
output.df$Survived <- Survived

write.csv(output.df, file="kaggle_submission_13.csv", row.names = FALSE)
```

Finally, we can use our output to see how accurate our prediction is.