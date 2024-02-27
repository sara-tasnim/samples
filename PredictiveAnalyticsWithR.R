# loading related packages
library(moments)
library(Amelia)
library(dplyr)
library(psych)
library(purrr)
library(tidyr)
library(ggplot2)
library(caret)
library(GGally)
library(corrplot)
library(plotly)
library(rpart)
library(rpart.plot)

#loading the dataset into R
housing <- read.csv("HousingValuationTest-V2.csv")
View(housing)
summary(housing)


#calculating Mean
mean(housing$FullBath, na.rm = TRUE)
mean(housing$HalfBath, na.rm = TRUE)
mean(housing$BedroomAbvGr, na.rm = TRUE)
mean(housing$KitchenAbvGr, na.rm = TRUE)
mean(housing$TotalRmsAbvGrd, na.rm = TRUE)
mean(housing$Fireplaces, na.rm = TRUE)
mean(housing$GarageCars, na.rm = TRUE)
mean(housing$PoolArea, na.rm = TRUE)
mean(housing$SalePrice, na.rm = TRUE)
mean(housing$LotArea, na.rm = TRUE)
mean(housing$LivingArea, na.rm = TRUE)
mean(housing$OpenPorchSF, na.rm = TRUE)
mean(housing$TotalBSF, na.rm = TRUE)
mean(housing$LowQualFinSF, na.rm = TRUE)

#calculating Median
median(housing$FullBath, na.rm = TRUE)
median(housing$HalfBath, na.rm = TRUE)
median(housing$BedroomAbvGr, na.rm = TRUE)
median(housing$KitchenAbvGr, na.rm = TRUE)
median(housing$TotalRmsAbvGrd, na.rm = TRUE)
median(housing$Fireplaces, na.rm = TRUE)
median(housing$GarageCars, na.rm = TRUE)
median(housing$PoolArea, na.rm = TRUE)
median(housing$SalePrice, na.rm = TRUE)
median(housing$LotArea, na.rm = TRUE)
median(housing$LivingArea, na.rm = TRUE)
median(housing$OpenPorchSF, na.rm = TRUE)
median(housing$TotalBSF, na.rm = TRUE)
median(housing$LowQualFinSF, na.rm = TRUE)

#calculating Max
max(housing$FullBath, na.rm = TRUE)
max(housing$HalfBath, na.rm = TRUE)
max(housing$BedroomAbvGr, na.rm = TRUE)
max(housing$KitchenAbvGr, na.rm = TRUE)
max(housing$TotalRmsAbvGrd, na.rm = TRUE)
max(housing$Fireplaces, na.rm = TRUE)
max(housing$GarageCars, na.rm = TRUE)
max(housing$PoolArea, na.rm = TRUE)
max(housing$SalePrice, na.rm = TRUE)
max(housing$LotArea, na.rm = TRUE)
max(housing$LivingArea, na.rm = TRUE)
max(housing$OpenPorchSF, na.rm = TRUE)
max(housing$TotalBSF, na.rm = TRUE)
max(housing$LowQualFinSF, na.rm = TRUE)

#calculating Min
min(housing$FullBath, na.rm = TRUE)
min(housing$HalfBath, na.rm = TRUE)
min(housing$BedroomAbvGr, na.rm = TRUE)
min(housing$KitchenAbvGr, na.rm = TRUE)
min(housing$TotalRmsAbvGrd, na.rm = TRUE)
min(housing$Fireplaces, na.rm = TRUE)
min(housing$GarageCars, na.rm = TRUE)
min(housing$PoolArea, na.rm = TRUE)
min(housing$SalePrice, na.rm = TRUE)
min(housing$LotArea, na.rm = TRUE)
min(housing$LivingArea, na.rm = TRUE)
min(housing$OpenPorchSF, na.rm = TRUE)
min(housing$TotalBSF, na.rm = TRUE)
min(housing$LowQualFinSF, na.rm = TRUE)

#calculating Standard Deviation
sd(housing$FullBath, na.rm = TRUE)
sd(housing$HalfBath, na.rm = TRUE)
sd(housing$BedroomAbvGr, na.rm = TRUE)
sd(housing$KitchenAbvGr, na.rm = TRUE)
sd(housing$TotalRmsAbvGrd, na.rm = TRUE)
sd(housing$Fireplaces, na.rm = TRUE)
sd(housing$GarageCars, na.rm = TRUE)
sd(housing$PoolArea, na.rm = TRUE)
sd(housing$SalePrice, na.rm = TRUE)
sd(housing$LotArea, na.rm = TRUE)
sd(housing$LivingArea, na.rm = TRUE)
sd(housing$OpenPorchSF, na.rm = TRUE)
sd(housing$TotalBSF, na.rm = TRUE)
sd(housing$LowQualFinSF, na.rm = TRUE)

#calculating Skewness
skewness(housing$FullBath, na.rm = TRUE)
skewness(housing$HalfBath, na.rm = TRUE)
skewness(housing$BedroomAbvGr, na.rm = TRUE)
skewness(housing$KitchenAbvGr, na.rm = TRUE)
skewness(housing$TotalRmsAbvGrd, na.rm = TRUE)
skewness(housing$Fireplaces, na.rm = TRUE)
skewness(housing$GarageCars, na.rm = TRUE)
skewness(housing$PoolArea, na.rm = TRUE)
skewness(housing$SalePrice, na.rm = TRUE)
skewness(housing$LotArea, na.rm = TRUE)
skewness(housing$LivingArea, na.rm = TRUE)
skewness(housing$OpenPorchSF, na.rm = TRUE)
skewness(housing$TotalBSF, na.rm = TRUE)
skewness(housing$LowQualFinSF, na.rm = TRUE)

#calculating frequency of each category in categorical variables
table(housing$Id)
table(housing$Utilities)
table(housing$LandContour)
table(housing$LotConfig)
table(housing$DwellClass)
table(housing$CentralAir)
table(housing$GarageType)
table(housing$PavedDrive)
table(housing$LotShape)
table(housing$Slope)
table(housing$OverallCondition)
table(housing$OverallQuality)
table(housing$ExteriorCondition)
table(housing$BasementCondition)
table(housing$KitchenQuality)
table(housing$MoSold)
table(housing$YrSold)
table(housing$YearBuilt)

#creating Histograms
hist(housing$LotArea, col="orange")
hist(housing$LivingArea, col="orange")
hist(housing$OpenPorchSF, col="orange")
hist(housing$TotalBSF, col="orange")
hist(housing$LowQualFinSF, col="orange")
hist(housing$FullBath, col="orange")
hist(housing$HalfBath, col="orange")
hist(housing$BedroomAbvGr, col="orange")
hist(housing$KitchenAbvGr, col="orange")
hist(housing$TotalRmsAbvGrd, col="orange")
hist(housing$Fireplaces, col="orange")
hist(housing$GarageCars, col="orange")
hist(housing$PoolArea, col="orange")
hist(housing$SalePrice, col="orange")

#creating Boxplots
boxplot(housing$LotArea)
boxplot(housing$LivingArea)
boxplot(housing$OpenPorchSF)
boxplot(housing$TotalBSF)
boxplot(housing$LowQualFinSF)
boxplot(housing$FullBath)
boxplot(housing$HalfBath)
boxplot(housing$BedroomAbvGr)
boxplot(housing$KitchenAbvGr)
boxplot(housing$TotalRmsAbvGrd)
boxplot(housing$Fireplaces)
boxplot(housing$GarageCars)
boxplot(housing$PoolArea)
boxplot(housing$SalePrice)

# changing GarageType's NA's
housing <- housing %>%
  mutate(GarageType = ifelse(is.na(GarageType), "No", GarageType))

#checking for missing values
missmap(housing, col = c('yellow', 'blue'), y.at = 1, y.labels = '', legend = TRUE)


#Replacing missing values
# method 1: Replacing NA's with 0
replacedWithZero <- housing
is.na(replacedWithZero)
replacedWithZero[is.na(replacedWithZero)] <- 0
summary(replacedWithZero)

pairs.panels(all.zeros, col="red")

# Method 2: Deleting the NA's
deletingValues <- housing[complete.cases(housing),]
View(deletingValues)
summary(deletingValues)

pairs.panels(all.deleted, col="red")

#Replacing missing values with Mean values
replacedWithMean <- housing
summary(replacedWithMean$LivingArea)
mean(replacedWithMean$LivingArea, na.rm = TRUE) 
is.na(replacedWithMean$LivingArea)
replacedWithMean$LivingArea[is.na(replacedWithMean$LivingArea)]
replacedWithMean$LivingArea[is.na(replacedWithMean$LivingArea)] <- mean(replacedWithMean$LivingArea, na.rm = TRUE)
summary(replacedWithMean$LivingArea)

summary(replacedWithMean$TotalBSF)
mean(replacedWithMean$TotalBSF, na.rm = TRUE) 
is.na(replacedWithMean$TotalBSF)
replacedWithMean$TotalBSF[is.na(replacedWithMean$TotalBSF)]
replacedWithMean$TotalBSF[is.na(replacedWithMean$TotalBSF)] <- mean(replacedWithMean$TotalBSF, na.rm = TRUE)
summary(replacedWithMean$TotalBSF)

pairs.panels(replacedWithMean, col ="red")

# Output summary statistics: Method 1
summary(replacedWithZero)

#calculating Standard Deviation
sd(replacedWithZero$FullBath, na.rm = TRUE)
sd(replacedWithZero$HalfBath, na.rm = TRUE)
sd(replacedWithZero$BedroomAbvGr, na.rm = TRUE)
sd(replacedWithZero$KitchenAbvGr, na.rm = TRUE)
sd(replacedWithZero$TotalRmsAbvGrd, na.rm = TRUE)
sd(replacedWithZero$Fireplaces, na.rm = TRUE)
sd(replacedWithZero$GarageCars, na.rm = TRUE)
sd(replacedWithZero$PoolArea, na.rm = TRUE)
sd(replacedWithZero$SalePrice, na.rm = TRUE)
sd(replacedWithZero$LotArea, na.rm = TRUE)
sd(replacedWithZero$LivingArea, na.rm = TRUE)
sd(replacedWithZero$OpenPorchSF, na.rm = TRUE)
sd(replacedWithZero$TotalBSF, na.rm = TRUE)
sd(replacedWithZero$LowQualFinSF, na.rm = TRUE)

#calculating Skewness
skewness(replacedWithZero$FullBath, na.rm = TRUE)
skewness(replacedWithZero$HalfBath, na.rm = TRUE)
skewness(replacedWithZero$BedroomAbvGr, na.rm = TRUE)
skewness(replacedWithZero$KitchenAbvGr, na.rm = TRUE)
skewness(replacedWithZero$TotalRmsAbvGrd, na.rm = TRUE)
skewness(replacedWithZero$Fireplaces, na.rm = TRUE)
skewness(replacedWithZero$GarageCars, na.rm = TRUE)
skewness(replacedWithZero$PoolArea, na.rm = TRUE)
skewness(replacedWithZero$SalePrice, na.rm = TRUE)
skewness(replacedWithZero$LotArea, na.rm = TRUE)
skewness(replacedWithZero$LivingArea, na.rm = TRUE)
skewness(replacedWithZero$OpenPorchSF, na.rm = TRUE)
skewness(replacedWithZero$TotalBSF, na.rm = TRUE)
skewness(replacedWithZero$LowQualFinSF, na.rm = TRUE)

# Output summary statistics: Method 2
summary(deletingValues)

#calculating Standard Deviation
sd(deletingValues$FullBath, na.rm = TRUE)
sd(deletingValues$HalfBath, na.rm = TRUE)
sd(deletingValues$BedroomAbvGr, na.rm = TRUE)
sd(deletingValues$KitchenAbvGr, na.rm = TRUE)
sd(deletingValues$TotalRmsAbvGrd, na.rm = TRUE)
sd(deletingValues$Fireplaces, na.rm = TRUE)
sd(deletingValues$GarageCars, na.rm = TRUE)
sd(deletingValues$PoolArea, na.rm = TRUE)
sd(deletingValues$SalePrice, na.rm = TRUE)
sd(deletingValues$LotArea, na.rm = TRUE)
sd(deletingValues$LivingArea, na.rm = TRUE)
sd(deletingValues$OpenPorchSF, na.rm = TRUE)
sd(deletingValues$TotalBSF, na.rm = TRUE)
sd(deletingValues$LowQualFinSF, na.rm = TRUE)

#calculating Skewness
skewness(deletingValues$FullBath, na.rm = TRUE)
skewness(deletingValues$HalfBath, na.rm = TRUE)
skewness(deletingValues$BedroomAbvGr, na.rm = TRUE)
skewness(deletingValues$KitchenAbvGr, na.rm = TRUE)
skewness(deletingValues$TotalRmsAbvGrd, na.rm = TRUE)
skewness(deletingValues$Fireplaces, na.rm = TRUE)
skewness(deletingValues$GarageCars, na.rm = TRUE)
skewness(deletingValues$PoolArea, na.rm = TRUE)
skewness(deletingValues$SalePrice, na.rm = TRUE)
skewness(deletingValues$LotArea, na.rm = TRUE)
skewness(deletingValues$LivingArea, na.rm = TRUE)
skewness(deletingValues$OpenPorchSF, na.rm = TRUE)
skewness(deletingValues$TotalBSF, na.rm = TRUE)
skewness(deletingValues$LowQualFinSF, na.rm = TRUE)

# Output summary statistics: Method 3
summary(replacedWithMean)

#calculating Standard Deviation
sd(replacedWithMean$FullBath, na.rm = TRUE)
sd(replacedWithMean$HalfBath, na.rm = TRUE)
sd(replacedWithMean$BedroomAbvGr, na.rm = TRUE)
sd(replacedWithMean$KitchenAbvGr, na.rm = TRUE)
sd(replacedWithMean$TotalRmsAbvGrd, na.rm = TRUE)
sd(replacedWithMean$Fireplaces, na.rm = TRUE)
sd(replacedWithMean$GarageCars, na.rm = TRUE)
sd(replacedWithMean$PoolArea, na.rm = TRUE)
sd(replacedWithMean$SalePrice, na.rm = TRUE)
sd(replacedWithMean$LotArea, na.rm = TRUE)
sd(replacedWithMean$LivingArea, na.rm = TRUE)
sd(replacedWithMean$OpenPorchSF, na.rm = TRUE)
sd(replacedWithMean$TotalBSF, na.rm = TRUE)
sd(replacedWithMean$LowQualFinSF, na.rm = TRUE)

#calculating Skewness
skewness(replacedWithMean$FullBath, na.rm = TRUE)
skewness(replacedWithMean$HalfBath, na.rm = TRUE)
skewness(replacedWithMean$BedroomAbvGr, na.rm = TRUE)
skewness(replacedWithMean$KitchenAbvGr, na.rm = TRUE)
skewness(replacedWithMean$TotalRmsAbvGrd, na.rm = TRUE)
skewness(replacedWithMean$Fireplaces, na.rm = TRUE)
skewness(replacedWithMean$GarageCars, na.rm = TRUE)
skewness(replacedWithMean$PoolArea, na.rm = TRUE)
skewness(replacedWithMean$SalePrice, na.rm = TRUE)
skewness(replacedWithMean$LotArea, na.rm = TRUE)
skewness(replacedWithMean$LivingArea, na.rm = TRUE)
skewness(replacedWithMean$OpenPorchSF, na.rm = TRUE)
skewness(replacedWithMean$TotalBSF, na.rm = TRUE)
skewness(replacedWithMean$LowQualFinSF, na.rm = TRUE)

# transforming skewed OpenPorchSF
trans_openPorch <- log(replacedWithMean$OpenPorchSF)
par(mfrow=c(1,2))
hist(replacedWithMean$OpenPorchSF, col = "orange", main = "Original")
hist(trans_openPorch, col = "orange", main = "Transformed")
boxplot(replacedWithMean$OpenPorchSF, main = "Original")
boxplot(trans_openPorch, main = "Transformed")

# transforming skewed SalePrice
trans_salePrice <- log(replacedWithMean$SalePrice)
par(mfrow=c(1,2))
hist(replacedWithMean$SalePrice, col = "orange", main = "Original")
hist(trans_salePrice, col = "orange", main = "Transformed")
boxplot(replacedWithMean$SalePrice, main = "Original")
boxplot(trans_salePrice, main = "Transformed")

par(mfrow=c(1,1))

# transforming data
#creating a new dataset for further analysis
housing_data <- replacedWithMean

housing_data$Id <- as.numeric(housing_data$Id)
class(housing_data$Id)

housing_data$LandContour <- factor(housing_data$LandContour,
                                   levels = c("Lvl", "Bnk", "HLS", "Low"),
                                   labels = c(1,2,3,4))
housing_data$LandContour <- as.numeric(housing_data$LandContour)
class(housing_data$LandContour)

housing_data$Utilities <- factor(housing_data$Utilities,
                                 levels = c("AllPub", "NoSewr", "NoSeWa", "ELO"),
                                 labels = c(1,2,3,4))
housing_data$Utilities <- as.numeric(housing_data$Utilities)
class(housing_data$Utilities)

housing_data$LotConfig <- factor(housing_data$LotConfig,
                                 levels = c("Inside", "Corner", "CulDSac", "FR2", "FR3"),
                                 labels = c(1,2,3,4,5))
housing_data$LotConfig <- as.numeric(housing_data$LotConfig)
class(housing_data$LotConfig)

housing_data$DwellClass <- as.numeric(factor(housing_data$DwellClass))

housing_data$CentralAir <- factor(housing_data$CentralAir,
                                  levels = c("N", "Y"),
                                  labels = c(0,1))
housing_data$CentralAir <- as.numeric(housing_data$CentralAir)
class(housing_data$CentralAir)

housing_data$GarageType <- factor(housing_data$GarageType,
                                 levels = c("2Types", "Attchd", "Basment", "BuiltIn", "CarPort", "Detchd", "No"),
                                 labels = c(1,2,3,4,5,6,7))
housing_data$GarageType <- as.numeric(housing_data$GarageType)
class(housing_data$GarageType)

housing_data$PavedDrive <- factor(housing_data$PavedDrive,
                                  levels = c("Y", "P", "N"),
                                  labels = c(1,2,3))
housing_data$PavedDrive <- as.numeric(housing_data$PavedDrive)
class(housing_data$PavedDrive)

housing_data$LotShape <- as.numeric(housing_data$LotShape)
class(housing_data$LotShape)

housing_data$Slope <- factor(housing_data$Slope,
                                 levels = c("Gtl", "Mod", "Sev"),
                                 labels = c(1,2,3))
housing_data$Slope <- as.numeric(housing_data$Slope)
class(housing_data$Slope)

housing_data$OverallQuality <- as.numeric(housing_data$OverallQuality)
class(housing_data$OverallQuality)

housing_data$OverallCondition <- as.numeric(housing_data$OverallCondition)
class(housing_data$OverallCondition)

housing_data$ExteriorCondition <- factor(housing_data$ExteriorCondition,
                                             levels = c("Po", "Fa", "TA", "Gd", "Ex"),
                                             labels = c(1,2,3,4,5))
housing_data$ExteriorCondition <- as.numeric(housing_data$ExteriorCondition)
class(housing_data$ExteriorCondition)

housing_data$BasementCondition <- factor(housing_data$BasementCondition,
                                             levels = c("NB", "Po", "Fa", "TA", "Gd", "Ex"),
                                             labels = c(0,1,2,3,4,5))
housing_data$BasementCondition <- as.numeric(housing_data$BasementCondition)
class(housing_data$BasementCondition)

housing_data$KitchenQuality <- factor(housing_data$KitchenQuality,
                                          levels = c("Po", "Fa", "TA", "Gd", "Ex"),
                                          labels = c(1,2,3,4,5))
housing_data$KitchenQuality <- as.numeric(housing_data$KitchenQuality)
class(housing_data$KitchenQuality)

housing_data$MoSold <- as.numeric(housing_data$MoSold)
class(housing_data$MoSold)

housing_data$YearBuilt <- as.numeric(housing_data$YearBuilt)
class(housing_data$YearBuilt)

housing_data$YrSold <- as.numeric(housing_data$YrSold)
class(housing_data$YrSold)

View(housing_data)

#checking for any missing value
missmap(housing_data, col = c('yellow', 'blue'), y.at = 1, y.labels = '', legend = TRUE)

#correlation analysis
cor.plot(housing_data, numbers = TRUE, method.args = list(text = list(srt = 45, adj = 1)))
cor.housing <- cor(housing_data)
round(cor.housing, digits = 3)
corrplot(cor.housing, method = "circle", tl.cex = 0.7)

ggcorr(housing_data, label = TRUE)


# observing target variable distribution
selected_attrhousing <- housing_data$SalePrice
par(mfrow = c(1,2))
hist(selected_attrhousing, col = "orange", main = "Histogram")
plot(density(selected_attrhousing, na.rm =TRUE), main = "Density")

par(mfrow=c(1,1))

# dimensionality reduction
target <- housing_data$SalePrice
housingdata2 <- subset(housing_data, select = -c(SalePrice))

ggcorr(housingdata2, label = TRUE)

housingM <- data.matrix(housingdata2)
corrhousingM <- cor(housingM)

highlycprrhousingM <- findCorrelation(corrhousingM, cutoff = 0.5)
names(housingdata2)[highlycprrhousingM]

housing_selected = subset(housingdata2, select = -c(OverallQuality, YearBuilt, FullBath))
View(housing_selected)

ggcorr(housing_selected, label = TRUE)
housing_selected$SalePrice <- target

par(mfrow=c(1,1))

# exploring distribution of the selected variables against SalePrice
ggcorr(housing_selected, label = TRUE)



# PART C
# 1.a
# building linear model

samplesize <- floor(2/3 * nrow(housing_selected))
set.seed(3)
housing_selected <- housing_selected[sample(nrow(housing_selected)),]
housing.train <- housing_selected[1:samplesize,]
housing.test <- housing_selected[(samplesize+1):nrow(housing_selected),]

formula = SalePrice~.

#housing.train and housing.test is showing X and X.1 columns with missing values
housing.train = subset(housing.train, select = -c(X, X.1))
housing.test = subset(housing.test, select = -c(X, X.1))

linearmodel <- lm(formula = formula, data = housing.train)
summary(linearmodel)$coefficients

as.formula(
  paste0("y ~ ", round(coefficients(linearmodel)[1],2), " + ",
         paste(sprintf("%.2f * %s", coefficients(linearmodel)[-1],
                       names(coefficients(linearmodel)[-1])),
               collapse = " + ")
  )
)

# evaluating linear model
# making predictions for test and train datasets
housing.train$predicted.saleprice <- predict(linearmodel, housing.train)
housing.test$predicted.saleprice <- predict(linearmodel, housing.test)
print("Actual Values")
head(housing.test$SalePrice[1:5])
print("Predicted Values")
head(housing.test$predicted.saleprice[1:5])

# plotting
plot_test <- housing.test %>%
  ggplot(aes(SalePrice, predicted.saleprice)) +
  geom_point(alpha = 0.5) +
  stat_smooth(aes(colour = 'red')) +
  xlab('Actual value of SalePrice') +
  ylab('Predicted value of SalePrice') +
  theme_bw()
ggplotly(plot_test)

# R-squared value

r_squared <- summary(linearmodel)$r.squared
print(paste("R Squared: ", r_squared))

# RMSE
housing.error <- housing.test$SalePrice - housing.test$predicted.saleprice
rmse <- sqrt(mean(housing.error^2))
print(paste("Root Mean Square Error: ", rmse))

# 1.b
# building better linear models
# model 2
housing_selected2 = subset(housing_selected, select = -c(TotalRmsAbvGrd, LivingArea))

samplesize2 <- floor(2/3 * nrow(housing_selected))
set.seed(3)
housing_selected2 <- housing_selected2[sample(nrow(housing_selected)),]
housing.train2 <- housing_selected2[1:samplesize,]
housing.test2 <- housing_selected2[(samplesize+1):nrow(housing_selected2),]

#housing.train and housing.test is showing X and X.1 columns with missing values
housing.train2 = subset(housing.train2, select = -c(X, X.1))
housing.test2 = subset(housing.test2, select = -c(X, X.1))

formula = SalePrice~.

linearmodel2 <- lm(formula = formula, data = housing.train2)
summary(linearmodel2)$coefficients

as.formula(
  paste0("y ~ ", round(coefficients(linearmodel2)[1],2), " + ",
         paste(sprintf("%.2f * %s", coefficients(linearmodel2)[-1],
                       names(coefficients(linearmodel2)[-1])),
               collapse = " + ")
  )
)

# evaluating model 2
housing.train2$predicted.saleprice <- predict(linearmodel2, housing.train2)
housing.test2$predicted.saleprice <- predict(linearmodel2, housing.test2)
print("Actual Values")
head(housing.test2$SalePrice[1:5])
print("Predicted Values")
head(housing.test2$predicted.saleprice[1:5])

# plotting for lm 2
plot_test2 <- housing.test2 %>%
  ggplot(aes(SalePrice, predicted.saleprice)) +
  geom_point(alpha = 0.5) +
  stat_smooth(aes(colour = 'red')) +
  xlab('Actual value of SalePrice') +
  ylab('Predicted value of SalePrice') +
  theme_bw()
ggplotly(plot_test2)

# R-squared value for lm 2

r_squared2 <- summary(linearmodel2)$r.squared
print(paste("R Squared: ", r_squared2))

# RMSE for lm 2
housing.error2 <- housing.test2$SalePrice-housing.test2$predicted.saleprice
rmse2 <- sqrt(mean(housing.error2^2))
print(paste("Root Mean Square Error: ", rmse2))

# model 3
ggcorr(housing_selected2, label = TRUE)
housing_selected3 = subset(housing_selected2, select = -c(PavedDrive))

samplesize <- floor(2/3 * nrow(housing_selected3))
set.seed(3)
housing_selected3 <- housing_selected3[sample(nrow(housing_selected3)),]
housing.train3 <- housing_selected3[1:samplesize,]
housing.test3 <- housing_selected3[(samplesize+1):nrow(housing_selected3),]

#housing.train and housing.test is showing X and X.1 columns with missing values
housing.train3 = subset(housing.train3, select = -c(X, X.1))
housing.test3 = subset(housing.test3, select = -c(X, X.1))

formula = SalePrice~.

linearmodel3 <- lm(formula = formula, data = housing.train3)
summary(linearmodel)$coefficients

as.formula(
  paste0("y ~ ", round(coefficients(linearmodel)[1],2), " + ",
         paste(sprintf("%.2f * %s", coefficients(linearmodel)[-1],
                       names(coefficients(linearmodel)[-1])),
               collapse = " + ")
  )
)

# evaluating model 3
housing.train3$predicted.saleprice <- predict(linearmodel3, housing.train3)
housing.test3$predicted.saleprice <- predict(linearmodel3, housing.test3)
print("Actual Values")
head(housing.test3$SalePrice[1:5])
print("Predicted Values")
head(housing.test3$predicted.saleprice[1:5])

# plotting model 3
plot_test3 <- housing.test3 %>%
  ggplot(aes(SalePrice, predicted.saleprice)) +
  geom_point(alpha = 0.5) +
  stat_smooth(aes(colour = 'red')) +
  xlab('Actual value of SalePrice') +
  ylab('Predicted value of SalePrice') +
  theme_bw()
ggplotly(plot_test3)

# R-squared value for lm 3

r_squared3 <- summary(linearmodel3)$r.squared
print(paste("R Squared: ", r_squared3))

# RMSE for lm 3
housing.error3 <- housing.test3$SalePrice-housing.test3$predicted.saleprice
rmse3 <- sqrt(mean(housing.error3^2))
print(paste("Root Mean Square Error: ", rmse3))



# 2.a
# decision tree
ggcorr(housing_selected, label = TRUE)

# sampling and divind dataset into training and testing
samplesize <- floor(2/3 * nrow(housing_selected))
set.seed(3)
housing_sdtree <- housing_selected[sample(nrow(housing_selected)),]
housing.traindt <- housing_selected[1:samplesize,]
housing.testdt <- housing_selected[(samplesize+1):nrow(housing_selected),]

formula = SalePrice~.

#housing.train and housing.test is showing X and X.1 columns with missing values
housing.traindt = subset(housing.traindt, select = -c(X, X.1))
housing.testdt = subset(housing.testdt, select = -c(X, X.1))

par(mfrow=c(1,1))

dtree.housing <- rpart(formula, data = housing.traindt, method = "anova")
dtree.housing$variable.importance
rpart.plot(dtree.housing, type = 4, fallen.leaves = FALSE)
print(dtree.housing)


# 2.b
# evaluating the model
predicted.saleprice.dtree <- predict(dtree.housing, housing.testdt)
print("Actual Values")
head(housing.testdt$SalePrice[1:5])
print("Predicted Values")
head(predicted.saleprice.dtree[1:5])

error.dtree <- housing.testdt$SalePrice - predicted.saleprice.dtree
rmse.dtree <- sqrt(mean(error.dtree^2))
print(paste("Root Mean Square Error: ", rmse.dtree))

# prunning to build better model
printcp(dtree.housing)
dtree.housing$cptable[which.min(dtree.housing$cptable[,"xerror"]),"CP"]

# 2nd dtree
pruned_dtree2 <- prune(dtree.housing, cp = 0.012344)
rpart.plot(pruned_dtree2, type = 4, fallen.leaves = FALSE)

predicted_prunedsaleprice <- predict(pruned_dtree2, housing.testdt)
error.dtree2 <- housing.testdt$SalePrice - predicted_prunedsaleprice
rmse.dtree2 <- sqrt(mean(error.dtree2^2))
print(paste("Root Mean Square Error for dtree2: ", rmse.dtree2))

# building 3rd tree
pruned_dtree3 <- prune(dtree.housing, cp = 0.010000)
rpart.plot(pruned_dtree3, type = 4, fallen.leaves = FALSE)
predicted_prunedsaleprice2 <- predict(pruned_dtree3, housing.testdt)
error.dtree3 <- housing.testdt$SalePrice - predicted_prunedsaleprice2
rmse.dtree3 <- sqrt(mean(error.dtree3^2))
print(paste("Root Mean Square Error for dtree3: ", rmse.dtree3))

