library(tidyverse)
library(class)
library(caret)
library(ggfortify)

url <- "https://archive.ics.uci.edu/ml/machine-learning-databases/wine/wine.data"
wine_data <- read.csv(url, header = TRUE)

Y <- as.factor(wine_data[,1])


colnames(wine_data) <- c("Class", "Alcohol", "Malic_Acid", "Ash", "Alcalinity_of_Ash",
                        "Magnesium", "Total_Phenols", "Flavanoids", "Nonflavanoid_Phenols",
                        "Proanthocyanins", "Color_Intensity", "Hue", 
                        "OD280/OD315_of_Diluted_Wines", "Proline")

head(wine_data)

#Compute the PCs and plot the dataset 
#Identify the variables that contribute the most to the 1st PC.

p_components <- prcomp(wine_data, center = TRUE, scale. = TRUE)
summary(p_components)

pc_data <- data.frame(PC1 = p_components$x[, 1], PC2 = p_components$x[, 2], Class = Y)


ggplot(pc_data, aes(x = PC1, y = PC2, color = Class)) +
  geom_point() +
  theme_minimal() +
  labs(title = "PC1 vs PC2")


#Drop the variables least contributing to the 1st PC and rerun PCA.

pc1_contribution <- abs(p_components$rotation[, 1])
important_vars <- sort(pc1_contribution, decreasing = TRUE)
print(important_vars)

dropped <- wine_data[, !(names(wine_data) %in% c("Ash", "Color_Intensity"))]
pca_rerun <- prcomp(dropped[, -1], center = TRUE, scale. = TRUE)

pc_data_reduced <- data.frame(PC1 = pca_rerun$x[, 1], 
                              PC2 = pca_rerun$x[, 2], 
                              Class = as.factor(wine_data$Class))

ggplot(pc_data_reduced, aes(x = PC1, y = PC2, color = Class)) +
  geom_point() +
  theme_minimal() +
  labs(title = "PCA after Removing Least Contributing Variables")

#Train a classifier model (e.g. kNN) to predict wine type using the original dataset.
#Train a classifier model to predict wine type using the data projected into the first 3 PCs (scores)
#Compare the 2 classification models using contingency tables and prevision/recall/f1 metric


train_index <- createDataPartition(wine_data$Class, p = 0.8, list = FALSE)
train_data <- wine_data[train_index, ]
test_data <- wine_data[-train_index, ]

knn_model <- knn(train = train_data[, -1], test = test_data[, -1], 
                 cl = train_data$Class, k = 5)

conf_matrix_original <- table(Predicted = knn_model, Actual = test_data$Class)
print(conf_matrix_original)
