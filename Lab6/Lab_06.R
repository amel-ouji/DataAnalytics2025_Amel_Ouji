################################
# Evaluating Regression Models #
################################

library(readr)
library(ggplot2)
library(e1071)
library(caret)
library(cv)

## read data
NY_House_Dataset <- read_csv("C:/Users/oujia/Downloads/NY-House-Dataset (1).csv")

dataset <- NY_House_Dataset

## column names
names(dataset)

## Plot dataset
ggplot(dataset, aes(x = log10(PROPERTYSQFT), y = log10(PRICE))) +
  geom_point()

ggplot(dataset, aes(x = PROPERTYSQFT, y = PRICE)) +
  geom_point()

## Linear Regression Model
lin.mod0 <- lm(log10(PRICE) ~ log10(PROPERTYSQFT), dataset)

summary(lin.mod0)

ggplot(dataset, aes(x = log10(PROPERTYSQFT), y = log10(PRICE))) +
  geom_point() +
  stat_smooth(method = "lm", col="blue")



## Cleaning
dataset.sub0 <- dataset[-which(dataset$PROPERTYSQFT==2184.207862),]

lin.mod1 <- lm(log10(PRICE) ~ log10(PROPERTYSQFT), dataset.sub0)
summary(lin.mod1)
ggplot(dataset.sub0, aes(x = log10(PROPERTYSQFT), y = log10(PRICE))) +
  geom_point() +
  stat_smooth(method = "lm", col="blue")


## SVM - linear
svm.mod0 <- svm(log10(PRICE) ~ log10(PROPERTYSQFT), dataset.sub0, kernel="linear")

summary(svm.mod0)

svm.pred <- predict(svm.mod0, dataset.sub0)

ggplot(dataset.sub0, aes(x = log10(PROPERTYSQFT), y = log10(PRICE))) +
  geom_point() +
  geom_line(aes(x=log10(PROPERTYSQFT), y=svm.pred), col="green")

## SVM - radial
svm.mod1 <- svm(log10(PRICE) ~ log10(PROPERTYSQFT), dataset.sub0, kernel="radial")

summary(svm.mod1)

svm.pred <- predict(svm.mod1, dataset.sub0)

ggplot(dataset.sub0, aes(x = log10(PROPERTYSQFT), y = log10(PRICE))) +
  geom_point() +
  geom_line(aes(x=log10(PROPERTYSQFT), y=svm.pred), col="red")

## SVM - radial - optimized
tuned.svm <- tune.svm(log10(PRICE) ~ log10(PROPERTYSQFT), data=dataset.sub0, kernel="radial", gamma = 10^seq(-3,2,1), cost = 10^seq(-3,2,1), tune.control=tune.control(cross = 5))
tuned.svm

svm.mod1 <- svm(log10(PRICE) ~ log10(PROPERTYSQFT), dataset.sub0, kernel="radial", gamma=0.1, cost=100)

summary(svm.mod1)

svm.pred <- predict(svm.mod1, dataset.sub0)

ggplot(dataset.sub0, aes(x = log10(PROPERTYSQFT), y = log10(PRICE))) +
  geom_point() +
  geom_line(aes(x=log10(PROPERTYSQFT), y=svm.pred), col="red")


## Estimating Model Errors

## split train/test
train.indexes <- sample(nrow(dataset.sub0),0.75*nrow(dataset.sub0))

train <- dataset.sub0[train.indexes,]
test <- dataset.sub0[-train.indexes,]

## LM
lin.mod <- lm(log10(PRICE) ~ log10(PROPERTYSQFT), train)

summary(cv(lin.mod))

lm.pred <- predict(lin.mod, test)

## err = predicted - real
err <- lm.pred-log10(test$PRICE)

## MAE
abs.err <- abs(err)
mean.abs.err <- mean(abs.err)

## MSE
sq.err <- err^2
mean.sq.err <- mean(sq.err)

## RMSE
sq.err <- err^2
mean.sq.err <- mean(sq.err)
root.mean.sq.err <- sqrt(mean.sq.err)

### Cross Validation ###

### Monte Carlo CV

## Lin Model
k = 10000
mae <- c()
mse <- c()
rmse <- c()

for (i in 1:k) {
  train.indexes <- sample(nrow(dataset.sub0),0.75*nrow(dataset.sub0))
  
  train <- dataset.sub0[train.indexes,]
  test <- dataset.sub0[-train.indexes,]
  
  lin.mod <- lm(log10(PRICE) ~ log10(PROPERTYSQFT), train)
  
  lm.pred <- predict(lin.mod, test)  
  
  err <- lm.pred-log10(test$PRICE)
  
  abs.err <- abs(err)
  mean.abs.err <- mean(abs.err)
  
  sq.err <- err^2
  mean.sq.err <- mean(sq.err)
  
  root.mean.sq.err <- sqrt(mean.sq.err)  
  
  mae <- c(mae,mean.abs.err)
  mse <- c(mse,mean.sq.err)
  rmse <- c(rmse,root.mean.sq.err)
}

mean(mae)
mean(mse)
mean(rmse)

## Linear SVM Model
k = 100
mae <- c()
mse <- c()
rmse <- c()

for (i in 1:k) {
  train.indexes <- sample(nrow(dataset.sub0),0.75*nrow(dataset.sub0))
  
  train <- dataset.sub0[train.indexes,]
  test <- dataset.sub0[-train.indexes,]
  
  svm.mod <- svm(log10(PRICE) ~ log10(PROPERTYSQFT), dataset.sub0, kernel="linear")
  
  svm.pred <- predict(svm.mod, test)  
  
  err <- svm.pred-log10(test$PRICE)
  
  abs.err <- abs(err)
  mean.abs.err <- mean(abs.err)
  
  sq.err <- err^2
  mean.sq.err <- mean(sq.err)
  
  root.mean.sq.err <- sqrt(mean.sq.err)  
  
  mae <- c(mae,mean.abs.err)
  mse <- c(mse,mean.sq.err)
  rmse <- c(rmse,root.mean.sq.err)
}

mean(mae)
mean(mse)
mean(rmse)

## Radial SVM Model
k = 100
mae <- c()
mse <- c()
rmse <- c()

for (i in 1:k) {
  train.indexes <- sample(nrow(dataset.sub0),0.75*nrow(dataset.sub0))
  
  train <- dataset.sub0[train.indexes,]
  test <- dataset.sub0[-train.indexes,]
  
  svm.mod <- svm(log10(PRICE) ~ log10(PROPERTYSQFT), dataset.sub0, kernel="radial", gamma=0.1, cost=100)
  
  svm.pred <- predict(svm.mod, test)  
  
  err <- svm.pred-log10(test$PRICE)
  
  abs.err <- abs(err)
  mean.abs.err <- mean(abs.err)
  
  sq.err <- err^2
  mean.sq.err <- mean(sq.err)
  root.mean.sq.err <- sqrt(mean.sq.err)  
  
  mae <- c(mae,mean.abs.err)
  mse <- c(mse,mean.sq.err)
  rmse <- c(rmse,root.mean.sq.err)
}

mean(mae)
mean(mse)
mean(rmse)


#### THE END ####


##Train and evaluate 3 regression models predicting price from square footage using the MAE, MSE and RMSE metrics##

library(rpart)
library(randomForest)

dataset_new <- subset(dataset, !is.na(PRICE) & !is.na(PROPERTYSQFT) & PRICE > 0 & PROPERTYSQFT > 0, select = c(PRICE, PROPERTYSQFT))


# Visualize data
ggplot(dataset_new, aes(x = PROPERTYSQFT, y = PRICE)) +
  geom_point(alpha = 0.5) +
  xlim(0, 10000) +
  ylim(0, 1e8) +
  labs(title = 'Square Footage vs. Price', x = 'Square Footage', y = 'Price')


set.seed(42)
trainIndex <- createDataPartition(dataset_new$PRICE, p = 0.8, list = FALSE)
trainData <- dataset_new[trainIndex, ]
testData <- dataset_new[-trainIndex, ]

#Linear Regression model
lin_model <- lm(PRICE ~ PROPERTYSQFT, data = trainData)
lin_preds <- predict(lin_model, testData)
summary(lin_model)

#Decision Tree model
dt_model <- rpart(PRICE ~ PROPERTYSQFT, data = trainData)
dt_preds <- predict(dt_model, testData)
summary(dt_model)

#Random Forest model
rf_model <- randomForest(PRICE ~ PROPERTYSQFT, data = trainData)
rf_preds <- predict(rf_model, testData)

# Evaluate models
evaluate_model <- function(true, pred) {
  mae <- mean(abs(true - pred))
  mse <- mean((true - pred)^2)
  rmse <- sqrt(mse)
  return(c(MAE = mae, MSE = mse, RMSE = rmse))
}

lm_metrics <- evaluate_model(testData$PRICE, lm_preds)
dt_metrics <- evaluate_model(testData$PRICE, dt_preds)
rf_metrics <- evaluate_model(testData$PRICE, rf_preds)

# Print results
cat('Linear Regression Metrics:', lm_metrics, '\n')
cat('Decision Tree Metrics:', dt_metrics, '\n')
cat('Random Forest Metrics:', rf_metrics, '\n')

