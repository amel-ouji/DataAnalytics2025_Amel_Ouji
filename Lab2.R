


housing_data <- read.csv("C:/Users/oujia/Downloads/NY-House-Dataset.csv")

model1 <- lm(PRICE ~ PROPERTYSQFT + BEDS, data = housing_data)
model2 <- lm(PRICE ~ PROPERTYSQFT + BATH, data = housing_data)
model3 <- lm(PRICE ~ PROPERTYSQFT + BEDS + BATH, data = housing_data)

summary(model1)
summary(model2)
summary(model3)