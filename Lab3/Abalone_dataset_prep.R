###################
##### Abalone #####
###################

# read dataset
abalone <- read.csv("C:/Users/oujia/Downloads/abalone_dataset.csv")

dataset <- abalone

## add new column age.group with 3 values based on the number of rings 
dataset$age.group <- cut(dataset$rings, br=c(0,8,11,35), labels = c("young", 'adult', 'old'))


## alternative way of setting age.group
dataset$age.group[dataset$rings<=8] <- "young"
dataset$age.group[dataset$rings>8 & dataset$rings<=11] <- "adult"
dataset$age.group[dataset$rings>11 & dataset$rings<=35] <- "old"

library(caret)
library(class)
#Subsets: Size vs Weight

feature1 <- dataset[, c("length", "diameter", "height")]
feature2 <- dataset[, c("whole_weight", "shucked_wieght", "viscera_wieght", "shell_weight")]

#Target label
label <- dataset$age.group


set.seed(255)

trainIndex <- createDataPartition(label, p = 0.8, list = FALSE)
train1 <- feature1[trainIndex, ]
test1 <- feature1[-trainIndex, ]

train2 <- feature2[trainIndex, ]
test2 <- feature2[-trainIndex, ]

trainLabels <- labels[trainIndex]
testLabels <- labels[-trainIndex]

#KNN models

knn.predict1 <- knn(train = train1, test = test1, cl = train1$height, k = 5)
knn.predict2 <- knn(train = train2, test = test2, cl = train2$whole_weight, k = 5)


contingency.table <- table(knn.predict1, train1$height, dnn=list('predicted','actual'))

