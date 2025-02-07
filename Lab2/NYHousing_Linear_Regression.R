library("ggplot2")
library("readr")

## read dataset
NY_House_Dataset <- read_csv("C:/Users/oujia/Downloads/NY-House-Dataset.csv")

dataset <- NY_House_Dataset

ggplot(dataset, aes(x = log10(PROPERTYSQFT), y = log10(PRICE))) +
  geom_point()

## filter data
dataset <- dataset[dataset$PRICE<195000000,]

dataset <- dataset[dataset$PROPERTYSQFT!=2184.207862,]

dataset$PROPERTYSQFT[dataset$BROKERTITLE=="Brokered by Douglas Elliman - 575 Madison Ave"][85]

## column names
names(dataset)

## fit linear model
lmod <- lm(PRICE~PROPERTYSQFT, data = dataset)

lmod <- lm(log10(PRICE)~log10(PROPERTYSQFT), data = dataset)

#Linear Models
model1 <- lm(log10(PRICE) ~ PROPERTYSQFT, data = dataset)
model2 <- lm(log10(PRICE) ~ PROPERTYSQFT + BATH, data = dataset)
model3 <- lm(log10(PRICE) ~ PROPERTYSQFT + BEDS + BATH, data = dataset)


## print model output
summary(lmod)
summary(model1)
summary(model2)
summary(model3)


ggplot(dataset, aes(x = log10(PROPERTYSQFT), y = PRICE)) +
  geom_point() + stat_smooth(method = "lm", col = "pink") 


## scatter plot of 2 variables
plot(PRICE~PROPERTYSQFT, data = dataset)
abline(lmod)

plot(log10(PRICE)~log10(PROPERTYSQFT), data = dataset)
abline(lmod)

## scatter plot of 2 variables
ggplot(dataset, aes(x = PROPERTYSQFT, y = PRICE)) +
  geom_point()

ggplot(dataset, aes(x = PROPERTYSQFT, y = PRICE)) +
  geom_point() +
  stat_smooth(method = "lm", col="red")

ggplot(dataset, aes(x = log10(PROPERTYSQFT), y = log10(PRICE))) +
  geom_point() +
  stat_smooth(method = "lm", col="red")




