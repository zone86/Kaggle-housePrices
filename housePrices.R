setwd('house prices')
##################################################### load libraries #####################################################
loadLibs = c('dplyr', 'ggplot2', 'readr', 'tidyr', 'reshape', 'purrr', 'glmnet', 'caret')
installLibs = loadLibs[!loadLibs %in% installed.packages()]

for(libs in installLibs) 
  install.packages(libs, dependences = TRUE)
  sapply(loadLibs, require, character = TRUE)

##################################################### data exploration #####################################################
### read train and test files ###
train = read_csv('train.csv')
test = read_csv('test.csv')
entireFrame = bind_rows(train, test)

### find missing values ###
isNA = sapply(entireFrame, function(x){ sum(is.na(x)) })

### 1460 data points (rows)
### > 1000 missing values: V7 [alley](1369 missing), V73 [poolQC](1453 missing), V74 [fence](1179 missing), V75 [misc feature](1406 missing) <<<<<< remove
### < 700 missing values: V4 [lotFrontage](259 missing), V58[fireplaceQu](690 missing) 
### < 100 missing values: V59 , V60, V61, V64, V65, V31 , V32, V33, V37, V36, V26, V27, V43 

# create new variable total house size
totalSF = as.integer(entireFrame$`1stFlrSF` + entireFrame$TotalBsmtSF + entireFrame$`2ndFlrSF`)
entireFrame = cbind(entireFrame, totalSF)

# recode continuous variables as character (turn into factor later)
Fireplaces = as.factor(entireFrame$Fireplaces)

##################################################### plots #####################################################
### find all variables which are categorical and continuous ###  
intVars = names(entireFrame)[which(sapply(entireFrame, is.integer))]
trainCont = entireFrame[intVars][-1]

### make correlation plot to see which continuous vars correlate with salePrice
correlations = cor(na.omit(trainCont)) # use all continuous variables other than Id 
rowInd = apply(correlations, 1, function(x) sum(x > 0.3 | x < -0.3) > 1)
correlations = correlations[rowInd, rowInd]
corrPlot = corrplot(correlations, method = 'circle', type = 'lower',  sig.level = 0.01, insig = 'blank')
### the following continuous variables have high correlation with SalePrice (from highest to lowest):
### OverallQual, GrLivArea, garageCars, garageArea, TotalBsmtSF, 1stFlrSF, <<<<<<<< highest correlations
### FullBath, TotRmsAbvGrd, YearBuilt, YearRemodAdd, GarageYrBlt, MassVnrArea, BsmtFinSF1 <<<<<<<< lower correlations
### '2ndFlrSF', LotFrontage, LotArea <<<<<<<< lower correlations
### although garageCars and GarageArea are highly correlated with each other, as are GrLivArea and TotRmsAbvGrd

############################## create data frame with numerical variables that have the highest correlation variables with salePrice #############################
numFrame = entireFrame %>% keep(is.integer)

## it appears there are missing variables in GarageYrBuilt and masVnrArea which means they do not have garages or masVnr >>>>>>> set these to 0
numFrame[is.na(numFrame)] = 0
numFrame$LotFrontage = as.integer(numFrame$LotFrontage)
numFrame$GarageArea = as.integer(numFrame$GarageArea)
numFrame$SalePrice = as.integer(numFrame$SalePrice)
numFrame$YearBuilt = as.integer(numFrame$YearBuilt)
numFrame$OverallCond = as.integer(numFrame$OverallCond)
numFrame$OverallQual = as.integer(numFrame$OverallQual)

sapply(numFrame, function(x){ sum(is.na(x)) })

### tells us which variables to keep: OverallQual, LotArea,  
### YearBuilt, OverallCond 

### plot all numeric variables against saleprice
numFrame =  numFrame %>% 
  select(OverallCond, 
         OverallQual, 
         YearBuilt, 
         GarageArea, 
         LotArea, 
         totalSF) #%>%
  #melt(., id = 'SalePrice') %>%
  #ggplot(aes(value, y = SalePrice)) + 
  #geom_point() +
  #facet_wrap(~variable)

### remove outliers - lotArea (vals > 100000), grlivarea (vals > 4500)
rmLotArea = which(numFrame$LotArea > 100000, arr.ind = TRUE)
numFrame$LotArea[rmLotArea] = mean(numFrame$LotArea)
rmtotalSF = which(numFrame$totalSF > 7000, arr.ind = TRUE)
numFrame$totalSF[rmtotalSF] = mean(numFrame$totalSF)

############################# create data frame of categorical variables #############################
catFrame = entireFrame %>% keep(is.character)

## do linear regression on all categorical variables for how well they predict salePrice
r2s = apply(catFrame, 2, function(x) summary(lm(entireFrame$SalePrice ~ x))$r.squared)

## the following have R-squared > .30: Neighborhood, ExterQual, BsmtQual, KitchenQual, GarageFinish, PoolQC, 
## > .20: FireplaceQu, Foundation, GarageType, BsmtFinType1, Alley
## > .10: HeatingQC, MasVnrType, Exterior1st, Exterior2nd, BsmtExposure, SaleType, SaleCondition, MSZoning,
## 0.05 < x < .1: GarageQual, LotShape, HouseStyle, RoofStyle, BsmtCond, CentralAir, Electrical, GarageCond, PavedDrive, 
## linear model (see below, relvealed that foundation and fireplace were not good predictors of salePrice)

catFrame = catFrame %>% select(Neighborhood,
                               KitchenQual, 
                               GarageFinish, 
                               GarageType, 
                               BsmtFinType1,
                               Fireplaces)
                               #ExterQual, 
                               #BsmtQual,

catFrame[is.na(catFrame)] = 'None'

sapply(catFrame, function(x){ sum(is.na(x)) })

############################# create final data frame #############################
### combine cat and num frames and split training data 70:30 into train and test before predicting the training set 
# transform SalePrice target to log form
y = data.frame(train$SalePrice)
y = log1p(y)

trainFrame = cbind(catFrame[1:1460,], numFrame[1:1460,], y)

trainFrame$LotArea = as.integer(trainFrame$LotArea)
trainFrame$totalSF = as.integer(trainFrame$totalSF)

trainFrame$KitchenQual = ordered(trainFrame$KitchenQual, 
                                 levels = c('None','Po','Fa','TA','Gd','Ex'))
#trainFrame$ExterQual = ordered(trainFrame$ExterQual,
  #                             levels = c('Po','Fa','TA','Gd','Ex'))
#trainFrame$BsmtQual = ordered(trainFrame$BsmtQual,
 #                             levels = c('None','Po','Fa','TA','Gd','Ex'))

trainFrame$GarageType = as.factor(trainFrame$GarageType)
trainFrame$BsmtFinType1 = as.factor(trainFrame$BsmtFinType1)
trainFrame$GarageFinish = as.factor(trainFrame$GarageFinish)
trainFrame$Neighborhood = as.factor(trainFrame$Neighborhood)

split = createDataPartition(y = trainFrame$train.SalePrice, p = .7, list = FALSE)
trainTrain = trainFrame[split,]
trainTest = trainFrame[-split,]

##################################################### linear/lasso/ridge regression #####################################################
## create train and test model within the data training set
x = model.matrix(train.SalePrice ~ ., model.frame(train.SalePrice ~ ., trainTrain, na.action = function(x)x))
xx = model.matrix(train.SalePrice ~ ., model.frame(train.SalePrice ~ ., trainTest, na.action = function(x)x))
ncol(x)
ncol(xx)

Y = as.matrix(trainTrain$train.SalePrice)
YY = as.matrix(trainTest$train.SalePrice)

## models for training set
modelLm = lm(train.SalePrice ~., trainTrain)
summary(modelLm)
modelRidge = cv.glmnet(x, Y, alpha = 0, family = "gaussian")
modelLasso = cv.glmnet(x, Y, alpha = 1, family = "gaussian")
modelNet = cv.glmnet(x, Y, alpha = .001, family = "gaussian")

predLm = expm1(predict(modelLm, newdata = trainTest))

bestLamLass = modelLasso$lambda.min
bestLamRidge = modelRidge$lambda.min
bestLamNet = modelNet$lambda.min

predRidge = expm1(predict.cv.glmnet(modelRidge, s = bestLamRidge, newx = xx))
predLasso = expm1(predict.cv.glmnet(modelLasso, s = bestLamLass, newx = xx))
predNet = expm1(predict.cv.glmnet(modelNet, s = bestLamNet, newx = xx))

## sum of squares to calculate best model performance
mean((predLasso - YY)^2)
mean((predRidge - YY)^2)
mean((predNet - YY)^2)
mean((predLm - YY)^2)
## ridge appears to perform the best

#################################### test set ####################################
testFrame = entireFrame[1461:2919,]

testCat = testFrame %>% keep(is.character)

testCat = testCat %>% select(Neighborhood, 
                             KitchenQual, 
                             GarageFinish, 
                             GarageType, 
                             BsmtFinType1)

testCat = cbind(testCat, FireplacesTest)
fireInd = which(FireplacesTest == 4, arr.ind = T)                        
testCat$FireplacesTest[fireInd] = 3
testCat$KitchenQual = ordered(testCat$KitchenQual, levels = c('None','Po','Fa','TA','Gd','Ex'))

testCat$KitchenQual[is.na(testCat$KitchenQual)] = 'None'
testCat$GarageFinish[is.na(testCat$GarageFinish)] = 'None'
testCat$GarageType[is.na(testCat$GarageType)] = 'None'
testCat$BsmtFinType1[is.na(testCat$BsmtFinType1)] = 'None'

sapply(testCat, function(x){ sum(is.na(x)) })

testNum = testFrame %>% keep(is.integer)
selectTestNum = testNum %>% select(OverallQual, 
                                   GarageArea, 
                                   YearBuilt, 
                                   OverallCond, 
                                   LotArea, 
                                   totalSF)

selectTestNum[is.na(selectTestNum)] = 0

sapply(selectTestNum, function(x){ sum(is.na(x)) })

testFrameNew = cbind(testCat, selectTestNum)
newX = model.matrix(~., data = testFrameNew)
ncol(newX)

predictions = expm1(predict.cv.glmnet(modelLasso, s = bestLamLass, newX))

salePrice = cbind.data.frame(testFrame$Id, predictions)
names(salePrice)[1] = "ID"
names(salePrice)[2] = "SalePrice"
write.csv(salePrice, "salePrice.csv", row.names = F)
