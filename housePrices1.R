#setwd('C:/Users/nija/Documents/house prices')
##################################################### load libraries #####################################################
loadLibs = c('dplyr', 'ggplot2', 'readr', 'reshape', 'purrr', 'glmnet', 'caret', 'xgboost')
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

################################### create new factors, and convert some factors ###################################
## convert some integer factors into character 
entireFrame$MSSubClass = as.character(entireFrame$MSSubClass)

# create new variable total house size
totalHouseSF = as.integer(entireFrame$`1stFlrSF` + entireFrame$`2ndFlrSF`+ entireFrame$TotalBsmtSF)
entireFrame = cbind(entireFrame, totalHouseSF)

# create variable for property size
noLotFront = which(is.na(entireFrame$LotFrontage), arr.ind = TRUE)
entireFrame$LotFrontage[noLotFront] = 0
totalPropSF = as.integer(entireFrame$LotArea + entireFrame$LotFrontage)
entireFrame = cbind(entireFrame, totalPropSF)

# create variable for overallCond * overallQual
overallCondxQual = as.integer(entireFrame$OverallQual * entireFrame$OverallCond)
entireFrame = cbind(entireFrame, overallCondxQual)

# create new variable for neighborhood (upper, upper-middle, middle, & lower class neighborhoods)
## upper: NoRidge, NridgHt, StoneBr 
## upper-middle: Somerst, Timber, Veenker
## middle: Blmngtn, ClearCr, CollgCr, Crawfor, Gilbert, NWAmes, SawyerW
## lower: MeadowV, BrDale, IDOTRR, BrkSide, Edwards, OldTown, Sawyer, BlueSte, SWISU, NPkVill, NAmes, Mitchel
entireFrame = cbind(entireFrame, rep(0))
names(entireFrame)[85] = 'Neighborhood1'
NoRidge = which(entireFrame$Neighborhood == 'NoRidge', arr.ind = T)
entireFrame$Neighborhood1[NoRidge] = 4
NridgHt = which(entireFrame$Neighborhood == 'NridgHt', arr.ind = T)
entireFrame$Neighborhood1[NridgHt] = 4
StoneBr = which(entireFrame$Neighborhood == 'StoneBr', arr.ind = T)
entireFrame$Neighborhood1[StoneBr] = 4

Somerst = which(entireFrame$Neighborhood == 'Somerst', arr.ind = T)
entireFrame$Neighborhood1[Somerst] = 3
Timber = which(entireFrame$Neighborhood == 'Timber', arr.ind = T)
entireFrame$Neighborhood1[Timber] = 3
Veenker = which(entireFrame$Neighborhood == 'Veenker', arr.ind = T)
entireFrame$Neighborhood1[Veenker] = 3

Blmngtn = which(entireFrame$Neighborhood == 'Blmngtn', arr.ind = T)
entireFrame$Neighborhood1[Blmngtn] = 2
ClearCr = which(entireFrame$Neighborhood == 'ClearCr', arr.ind = T)
entireFrame$Neighborhood1[ClearCr] = 2
CollgCr = which(entireFrame$Neighborhood == 'CollgCr', arr.ind = T)
entireFrame$Neighborhood1[CollgCr] = 2
Crawfor = which(entireFrame$Neighborhood == 'Crawfor', arr.ind = T)
entireFrame$Neighborhood1[Crawfor] = 2
Gilbert = which(entireFrame$Neighborhood == 'Gilbert', arr.ind = T)
entireFrame$Neighborhood1[Gilbert] = 2
NWAmes = which(entireFrame$Neighborhood == 'NWAmes', arr.ind = T)
entireFrame$Neighborhood1[NWAmes] = 2
SawyerW = which(entireFrame$Neighborhood == 'SawyerW', arr.ind = T)
entireFrame$Neighborhood1[SawyerW] = 2

BrDale = which(entireFrame$Neighborhood == 'BrDale', arr.ind = T)
entireFrame$Neighborhood1[BrDale] = 1
IDOTRR = which(entireFrame$Neighborhood == 'IDOTRR', arr.ind = T)
entireFrame$Neighborhood1[IDOTRR] = 1
BrkSide = which(entireFrame$Neighborhood == 'BrkSide', arr.ind = T)
entireFrame$Neighborhood1[BrkSide] = 1
Edwards = which(entireFrame$Neighborhood == 'Edwards', arr.ind = T)
entireFrame$Neighborhood1[Edwards] = 1
OldTown = which(entireFrame$Neighborhood == 'OldTown', arr.ind = T)
entireFrame$Neighborhood1[OldTown] = 1
Sawyer = which(entireFrame$Neighborhood == 'Sawyer', arr.ind = T)
entireFrame$Neighborhood1[Sawyer] = 1
Blueste = which(entireFrame$Neighborhood == 'Blueste', arr.ind = T)
entireFrame$Neighborhood1[Blueste] = 1
SWISU = which(entireFrame$Neighborhood == 'SWISU', arr.ind = T)
entireFrame$Neighborhood1[SWISU] = 1
NPkVill = which(entireFrame$Neighborhood == 'NPkVill', arr.ind = T)
entireFrame$Neighborhood1[NPkVill] = 1
NAmes = which(entireFrame$Neighborhood == 'NAmes', arr.ind = T)
entireFrame$Neighborhood1[NAmes] = 1
Mitchel = which(entireFrame$Neighborhood == 'Mitchel', arr.ind = T)
entireFrame$Neighborhood1[Mitchel] = 1
MeadowV = which(entireFrame$Neighborhood == 'MeadowV', arr.ind = T)
entireFrame$Neighborhood1[MeadowV] = 1

entireFrame$Neighborhood1 = ordered(entireFrame$Neighborhood1)

## create 2 cats of garagefinish: fin > RFn > Unf/None
entireFrame = cbind(entireFrame, rep(0))
names(entireFrame)[86] = 'GarageFinish1'
GarageFin = which(entireFrame$GarageFinish == 'Fin', arr.ind = T)
entireFrame$GarageFinish1[GarageFin] = 3
GarageRFin = which(entireFrame$GarageFinish == 'RFn', arr.ind = T)
entireFrame$GarageFinish1[GarageRFin] = 2
GarageUnf = which(entireFrame$GarageFinish == 'Unf', arr.ind = T)
entireFrame$GarageFinish1[GarageUnf] = 1
GarageNone = which(is.na(entireFrame$GarageFinish), arr.ind = T)
entireFrame$GarageFinish1[GarageNone] = 1

entireFrame$GarageFinish1 = ordered(entireFrame$GarageFinish1)

## 2 cats of garagetype: Attchd/BuiltIn > 2Types/Basement > CarPort/Detchd/None
entireFrame = cbind(entireFrame, rep(0))
names(entireFrame)[87] = 'GarageType1'
GarageAttchd = which(entireFrame$GarageType == 'Attchd', arr.ind = T)
entireFrame$GarageType1[GarageAttchd] = 3
GarageBuiltIn = which(entireFrame$GarageType == 'BuiltIn', arr.ind = T)
entireFrame$GarageType1[GarageBuiltIn] = 3

Garage2Types = which(entireFrame$GarageType == '2Types', arr.ind = T)
entireFrame$GarageType1[Garage2Types] = 2
GarageBasment = which(entireFrame$GarageType == 'Basment', arr.ind = T)
entireFrame$GarageType1[GarageBasment] = 2
GarageDetchd = which(entireFrame$GarageType == 'Detchd', arr.ind = T)
entireFrame$GarageType1[GarageDetchd] = 2

GarageCarPort = which(entireFrame$GarageType == 'CarPort', arr.ind = T)
entireFrame$GarageType1[GarageCarPort] = 1

GarageNone = which(is.na(entireFrame$GarageType), arr.ind = T)
entireFrame$GarageType1[GarageNone] = 0

entireFrame$GarageType1 = ordered(entireFrame$GarageType1)

## 2 cats of fireplaceQu: Ex/Gd/TA > Fa/Po/None
entireFrame = cbind(entireFrame, rep(0))
names(entireFrame)[88] = 'FireplaceQu1'
FireplaceEx = which(entireFrame$FireplaceQu == 'Ex', arr.ind = T)
entireFrame$FireplaceQu1[FireplaceEx] = 2
FireplaceGd = which(entireFrame$FireplaceQu == 'Gd', arr.ind = T)
entireFrame$FireplaceQu1[FireplaceGd] = 2
FireplaceTA = which(entireFrame$FireplaceQu == 'TA', arr.ind = T)
entireFrame$FireplaceQu1[FireplaceTA] = 2

FireplaceFa = which(entireFrame$FireplaceQu == 'Fa', arr.ind = T)
entireFrame$FireplaceQu1[FireplaceFa] = 1
FireplacePo = which(entireFrame$FireplaceQu == 'Po', arr.ind = T)
entireFrame$FireplaceQu1[FireplacePo] = 1
FireplaceNone = which(is.na(entireFrame$FireplaceQu), arr.ind = T)
entireFrame$FireplaceQu1[FireplaceNone] = 1

entireFrame$FireplaceQu1 = ordered(entireFrame$FireplaceQu1)

## create 3 levels of BsmtFinType: GLQ > ALQ/BLQ/LwQ/Rec/Unf > None
entireFrame = cbind(entireFrame, rep(0))
names(entireFrame)[89] = 'BsmtFinType1a'

BsmtFinType1GLQ = which(entireFrame$BsmtFinType1 == 'GLQ', arr.ind = T)
entireFrame$BsmtFinType1a[BsmtFinType1GLQ] = 3

BsmtFinType1ALQ = which(entireFrame$BsmtFinType1 == 'ALQ', arr.ind = T)
entireFrame$BsmtFinType1a[BsmtFinType1ALQ] = 2
BsmtFinType1BLQ = which(entireFrame$BsmtFinType1 == 'BLQ', arr.ind = T)
entireFrame$BsmtFinType1a[BsmtFinType1BLQ] = 2
BsmtFinType1LwQ = which(entireFrame$BsmtFinType1 == 'LwQ', arr.ind = T)
entireFrame$BsmtFinType1a[BsmtFinType1LwQ] = 2
BsmtFinType1Rec = which(entireFrame$BsmtFinType1 == 'Rec', arr.ind = T)
entireFrame$BsmtFinType1a[BsmtFinType1Rec] = 2
BsmtFinType1Unf = which(entireFrame$BsmtFinType1 == 'Unf', arr.ind = T)
entireFrame$BsmtFinType1a[BsmtFinType1Unf] = 2

BsmtFinType1GLQ = which(is.na(entireFrame$BsmtFinType1), arr.ind = T)
entireFrame$BsmtFinType1a[BsmtFinType1GLQ] = 1

entireFrame$BsmtFinType1a = ordered(entireFrame$BsmtFinType1a)

## create 3 levels of HeatingQC
entireFrame = cbind(entireFrame, rep(0))
names(entireFrame)[90] = 'HeatingQC1'

HeatingQCEx = which(entireFrame$HeatingQC == 'Ex', arr.ind = T)
entireFrame$HeatingQC1[HeatingQCEx] = 3

HeatingQCGd = which(entireFrame$HeatingQC == 'Gd', arr.ind = T)
entireFrame$HeatingQC1[HeatingQCGd] = 2
HeatingQCTA = which(entireFrame$HeatingQC == 'TA', arr.ind = T)
entireFrame$HeatingQC1[HeatingQCTA] = 2

HeatingQCFa = which(entireFrame$HeatingQC == 'Fa', arr.ind = T)
entireFrame$HeatingQC1[HeatingQCFa] = 1
HeatingQCPo = which(entireFrame$HeatingQC == 'Po', arr.ind = T)
entireFrame$HeatingQC1[HeatingQCPo] = 1

entireFrame$HeatingQC1 = ordered(entireFrame$HeatingQC1)

## create 3 levels of Functional1
entireFrame = cbind(entireFrame, rep(0))
names(entireFrame)[91] = 'Functional1'

FunctionalTyp = which(entireFrame$Functional == 'Typ', arr.ind = T)
entireFrame$Functional1[FunctionalTyp] = 2
FunctionalMaj1 = which(entireFrame$Functional == 'Maj1', arr.ind = T)
entireFrame$Functional1[FunctionalMaj1] = 2
FunctionalMin1 = which(entireFrame$Functional == 'Min1', arr.ind = T)
entireFrame$Functional1[FunctionalMaj1] = 2
FunctionalMin2 = which(entireFrame$Functional == 'Min2', arr.ind = T)
entireFrame$Functional1[FunctionalMaj1] = 2
FunctionalMod = which(entireFrame$Functional == 'Mod', arr.ind = T)
entireFrame$Functional1[FunctionalMaj1] = 2
FunctionalSev = which(entireFrame$Functional == 'Sev', arr.ind = T)
entireFrame$Functional1[FunctionalMaj1] = 2
FunctionalSal = which(entireFrame$Functional == 'Sal', arr.ind = T)
entireFrame$Functional1[FunctionalSal] = 2

FunctionalMaj1 = which(entireFrame$Functional == 'Maj2', arr.ind = T)
entireFrame$Functional1[FunctionalMaj1] = 1

entireFrame$Functional1 = ordered(entireFrame$Functional1)

## create 3 levels of Condition1
entireFrame = cbind(entireFrame, rep(0))
names(entireFrame)[92] = 'Condition1a'

Condition1aPosA = which(entireFrame$Condition1 == 'PosA', arr.ind = T)
entireFrame$Condition1a[Condition1aPosA] = 3

Condition1aNorm = which(entireFrame$Condition1 == 'Norm', arr.ind = T)
entireFrame$Condition1a[Condition1aNorm] = 2
Condition1aPosN = which(entireFrame$Condition1 == 'PosN', arr.ind = T)
entireFrame$Condition1a[Condition1aPosN] = 2
Condition1aRRNn = which(entireFrame$Condition1 == 'RRNn', arr.ind = T)
entireFrame$Condition1a[Condition1aRRNn] = 2
Condition1aRRNe = which(entireFrame$Condition1 == 'RRNe', arr.ind = T)
entireFrame$Condition1a[Condition1aRRNe] = 2
Condition1aRRAn = which(entireFrame$Condition1 == 'RRAn', arr.ind = T)
entireFrame$Condition1a[Condition1aRRAn] = 2

Condition1aArtery = which(entireFrame$Condition1 == 'Artery', arr.ind = T)
entireFrame$Condition1a[Condition1aArtery] = 1
Condition1aFeedr = which(entireFrame$Condition1 == 'Feedr', arr.ind = T)
entireFrame$Condition1a[Condition1aFeedr] = 1
Condition1aRRAe = which(entireFrame$Condition1 == 'RRAe', arr.ind = T)
entireFrame$Condition1a[Condition1aRRAe] = 1

Condition1aPosA = which(entireFrame$Condition1 == 'PosA', arr.ind = T)
entireFrame$Condition1a[Condition1aPosA] = 3
Condition1aPosA = which(entireFrame$Condition1 == 'PosA', arr.ind = T)
entireFrame$Condition1a[Condition1aPosA] = 3

entireFrame$Condition1a = ordered(entireFrame$Condition1a)

## create 3 levels of PoolQC
#entireFrame = cbind(entireFrame, rep(0))
#names(entireFrame)[90] = 'PoolQC1'

#PoolQCEx = which(entireFrame$PoolQC == 'Ex', arr.ind = T)
#entireFrame$PoolQC1[PoolQCEx] = 3

#PoolQCGd = which(entireFrame$PoolQC == 'Gd', arr.ind = T)
#entireFrame$PoolQC1[PoolQCGd] = 2
#PoolQCFa = which(entireFrame$PoolQC == 'Fa', arr.ind = T)
#entireFrame$PoolQC1[PoolQCFa] = 2

#PoolQCNA = which(is.na(entireFrame$PoolQC), arr.ind = T)
#entireFrame$PoolQC1[PoolQCNA] = 1

#entireFrame$PoolQC1 = ordered(entireFrame$PoolQC1)

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
### OverallQual, GrLivArea, garageCars, garageArea, xTotalBsmtSF, x1stFlrSF, <<<<<<<< highest correlations
### xFullBath, TotRmsAbvGrd, YearBuilt, YearRemodAdd, GarageYrBlt, MasVnrArea, Fireplaces, BsmtFinSF1 <<<<<<<< lower correlations
### x'2ndFlrSF', LotFrontage, LotArea <<<<<<<< lower correlations
### although garageCars and GarageArea are highly correlated with each other, 
### as are GrLivArea and TotRmsAbvGrd

############################## create data frame with numerical variables that have the highest correlation variables with salePrice #############################
numFrame = entireFrame %>% keep(is.integer)
numFrame$SalePrice = as.numeric(numFrame$SalePrice)
rmSalePrice = which(numFrame$SalePrice > 600000, arr.ind = TRUE)
numFrame$SalePrice[rmSalePrice] = mean(numFrame$SalePrice)

### plot all numeric variables against saleprice
numFrame =  numFrame %>% select(OverallQual,
                                overallCondxQual,
                                GrLivArea,
                                GarageArea, 
                                YearBuilt,
                                YearRemodAdd,
                                Fireplaces, 
                                BsmtFinSF1,
                                LotArea, 
                                totalHouseSF,
                                GarageYrBlt,
                                MasVnrArea
                                #totalPropSF
                                ) #%>%
  #melt(., id = 'SalePrice') %>%
  ggplot(entireFrame, aes(MasVnrArea, y = SalePrice)) + 
  geom_point()
  #facet_wrap(~variable)
  
### remove outliers - lotArea (vals > 100000), grlivarea (vals > 4500)
rmGrLivArea = which(numFrame$GrLivArea > 4500, arr.ind = TRUE)
numFrame$GrLivArea[rmGrLivArea] = mean(numFrame$GrLivArea)
rmLotArea = which(numFrame$LotArea > 100000, arr.ind = TRUE)
numFrame$LotArea[rmLotArea] = mean(numFrame$LotArea)
rmTotalBsmtSF = which(numFrame$TotalBsmtSF > 3000, arr.ind = TRUE)
numFrame$totalHouseSF[rmtotalHouseSF] = mean(numFrame$totalHouseSF)
rmGarageArea = which(numFrame$GarageArea > 1200, arr.ind = TRUE)
numFrame$GarageArea[rmGarageArea] = mean(numFrame$GarageArea)
rmBsmtFinSF1 = which(numFrame$BsmtFinSF1 > 5000, arr.ind = TRUE)
numFrame$BsmtFinSF1[rmBsmtFinSF1] = mean(numFrame$BsmtFinSF1)
rmtotalPropSF = which(numFrame$totalPropSF > 100000, arr.ind = TRUE)
numFrame$totalPropSF[rmtotalPropSF] = mean(numFrame$totalPropSF)
rmMasVnrArea = which(numFrame$MasVnrArea > 1500, arr.ind = TRUE)
numFrame$MasVnrArea[rmMasVnrArea] = mean(numFrame$MasVnrArea)
numFrame$TotalBsmtSF[rmTotalBsmtSF] = mean(numFrame$TotalBsmtSF)
rmtotalHouseSF = which(numFrame$totalHouseSF > 7000, arr.ind = TRUE)
numFrame$totalHouseSF[rmtotalHouseSF] = mean(numFrame$totalHouseSF)

numFrame[is.na(numFrame)] = 0

sapply(numFrame, function(x){ sum(is.na(x)) })

## convert factors to numeric
numFrame = apply(numFrame, 2, function(x) {as.numeric(x)})
numFrame = as.data.frame(numFrame)

## regression on num factors
numFrameReg = numFrame[1:1460,]
numMod = apply(numFrameReg, 2, function(x) summary(lm(train$SalePrice ~ x))$r.squared)
sort(numMod)
summary(lm(train$SalePrice ~ ., numFrameReg))

############################# create data frame of categorical variables #############################
catFrame = entireFrame %>% keep(is.character)

catFrameA = catFrame[1:1460,]
catFrameA = apply(catFrameA, 2, function(x) { as.factor(x)})
SalePrice = as.numeric(train$SalePrice)
catFrameA = cbind(catFrameA, SalePrice)
catMod = apply(catFrameA, 2, function(x) summary(lm(SalePrice ~ x)))

## do linear regression on all categorical variables for how well they predict salePrice
## R-squared > .30: Neighborhood, ExterQual, BsmtQual, KitchenQual, GarageFinish, 
## PoolQC 
## > .20: FireplaceQu, Foundation, GarageType, BsmtFinType1, Alley
## > .10: HeatingQC, MasVnrType, Exterior1st, Exterior2nd, BsmtExposure, 
## SaleType, SaleCondition (xNormal, xPartial), MSZoning (***)
## 0.05 < x < .1: GarageQual, LotShape, HouseStyle, RoofStyle, BsmtCond, 
## CentralAir, Electrical, GarageCond, PavedDrive

catFrame = catFrame %>% select(KitchenQual, 
                               ExterQual, 
                               MSZoning,
                               LotConfig, 
                               Condition1,
                               BldgType,
                               CentralAir,
                               Functional,
                               #GarageQual,
                               #HouseStyle, 
                               CentralAir, 
                               GarageCond
                               )

catFrame = cbind(catFrame, entireFrame$MSSubClass)
names(catFrame)[10] = 'MSSubClass'
catFrame = cbind(catFrame, entireFrame$BsmtFinType1a)
names(catFrame)[11] = 'BsmtFinType1a'
catFrame = cbind(catFrame, entireFrame$Neighborhood1)
names(catFrame)[12] = 'Neighborhood1'
catFrame = cbind(catFrame, entireFrame$GarageFinish1)
names(catFrame)[13] = 'GarageFinish1'
catFrame = cbind(catFrame, entireFrame$FireplaceQu1)
names(catFrame)[14] = 'FireplaceQu1'

catFrame[is.na(catFrame)] = 'None'

catFrame = apply(catFrame, 2, function(x) {as.factor(x)})
catFrame = as.data.frame(catFrame)

#catFrame$OverallCond = ordered(catFrame$OverallCond)
catFrame$KitchenQual = ordered(catFrame$KitchenQual, levels = c('None','Po','Fa','TA','Gd','Ex'))
catFrame$ExterQual = ordered(catFrame$ExterQual, levels = c('None','Fa','TA','Gd','Ex'))
catFrame$CentralAir = ordered(catFrame$CentralAir)
catFrame$GarageCond = ordered(catFrame$GarageCond, levels = c('None','Po','Fa','TA','Gd','Ex'))
#catFrame$GarageQual = ordered(catFrame$GarageQual, levels = c('None','Po','Fa','TA','Gd','Ex'))

sapply(catFrame, function(x){ sum(is.na(x)) })

### plots of factors against saleprice
plot(catFrame$BsmtFinType1[1:1460],train$SalePrice[1:1460])

catFrameReg = catFrame[1:1460,]
catMod = apply(catFrameReg, 2, function(x) summary(lm(train$SalePrice ~ x))$r.squared)
sort(catMod)
summary(lm(train$SalePrice ~ ., catFrameReg))

############################# create final data frame #############################
### combine cat and num frames and split training data 70:30 into train and test before predicting the training set 
# transform SalePrice distr. to log distr.
y = as.numeric(train$SalePrice)
y = log1p(y)

### create test and train frame for lasso/ridge/elasticnet
trainFrame = cbind(catFrame[1:1460,], numFrame[1:1460,], y)

split = createDataPartition(y = y, p = .7, list = FALSE)
trainTrain = trainFrame[split,]
trainTest = trainFrame[-split,]

##################################################### linear/lasso/ridge regression #####################################################
## create train and test model within the data training set
x = model.matrix(y ~ ., trainTrain)
xx = model.matrix(y ~ ., trainTest)
ncol(x)
ncol(xx)

Y = as.matrix(trainTrain$y)
YY = as.matrix(trainTest$y)

## models for training set
modelLm = lm(y ~., trainTrain)
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
mean((predLasso - YY)^2)#/ncol(xx)
mean((predRidge - YY)^2)
mean((predNet - YY)^2)
sqrt(mean(( predLm - YY )^2 ) / size(trainFrame))
## elasticNet appears to perform the best

### xgboost model
## Preparing matrix 
train = xgb.DMatrix(data = as.matrix(trainTrain[-27]), label = as.matrix(y)) 
dtest <- xgb.DMatrix(data = as.matrix(),label=as.matrix(Validation$SalePrice))

## Parameters - To get started
params = list(booster = "gbtree",
              objective = "reg:linear",
              eta = 0.015,
              gamma= 0,
              max_depth=10,
              min_child_weight=0,
              subsample=.5,
              colsample_bytree=.5,
              eval_metric='rmse',
              nthread=4,
              verbose=TRUE
)

xgbcv = xgb.cv( params = params, 
                data = x,
                nrounds = 2000,
                stratified = T,
                print_every_n = 50,
                early_stop_rounds = 20,
                maximize = F
)

xgbFit = xgboost(data = x,
                 nfold = 5, 
                 label = Y, 
                 nrounds = 2200, 
                 verbose = FALSE, 
                 objective = "reg:linear",
                 eval_metric = "rmse",
                 nthread = 8, 
                 eta = 0.01, 
                 gamma = 0.0468, 
                 max_depth = 6, 
                 min_child_weight = 1.7817, 
                 subsample = 0.5213, colsample_bytree = 0.4603)

print(xgbFit)

preds2 <- expm1(predict(xgbFit, newdata = xx))
mean((preds2 - YY)^2)
#################################### test set ####################################
testFrame = entireFrame[1461:2919,]

testCat = testFrame %>% keep(is.character)

testCat = testCat %>% select(KitchenQual, 
                             ExterQual, 
                             MSZoning,
                             LotConfig, 
                             Condition1,
                             BldgType,
                             CentralAir,
                             Functional,
                             #GarageQual,
                             #HouseStyle, 
                             CentralAir, 
                             GarageCond
                             )

testCat = cbind(testCat, testFrame$MSSubClass)
names(testCat)[10] = 'MSSubClass'
testCat = cbind(testCat, testFrame$BsmtFinType1a)
names(testCat)[11] = 'BsmtFinType1a'
testCat = cbind(testCat, testFrame$Neighborhood1)
names(testCat)[12] = 'Neighborhood1'
testCat = cbind(testCat, testFrame$GarageFinish1)
names(testCat)[13] = 'GarageFinish1'
testCat = cbind(testCat, testFrame$FireplaceQu1)
names(testCat)[14] = 'FireplaceQu1'

testCat[is.na(testCat)] = 'None'

testCat = apply(testCat, 2, function(x) {as.factor(x)})
testCat = as.data.frame(testCat)

testCat$KitchenQual = ordered(testCat$KitchenQual, levels = c('None','Po','Fa','TA','Gd','Ex'))
testCat$ExterQual = ordered(testCat$ExterQual, levels = c('None','Fa','TA','Gd','Ex'))
testCat$CentralAir = ordered(testCat$CentralAir)
testCat$GarageCond = ordered(testCat$GarageCond, levels = c('None','Po','Fa','TA','Gd','Ex'))
#testCat$GarageQual = ordered(testCat$GarageQual, levels = c('None','Po','Fa','TA','Gd','Ex'))
#testCat$GarageType = as.factor(testCat$GarageType)
#testCat$BsmtFinType1 = as.factor(testCat$BsmtFinType1)
#testCat$Foundation = as.factor(testCat$Foundation)

sapply(testCat, function(x){ sum(is.na(x)) })

testNum = testFrame %>% keep(is.integer)
selectTestNum = testNum %>% select(OverallQual,
                                   overallCondxQual,
                                   GrLivArea,
                                   GarageArea, 
                                   YearBuilt,
                                   YearRemodAdd,
                                   Fireplaces, 
                                   BsmtFinSF1,
                                   LotArea, 
                                   totalHouseSF,
                                   GarageYrBlt,
                                   MasVnrArea
                                   #totalPropSF
                                   ) #%>%

selectTestNum[is.na(selectTestNum)] = 0

sapply(selectTestNum, function(x){ sum(is.na(x)) })

selectTestNum = apply(selectTestNum, 2, function(x) { as.numeric(x) })
selectTestNum = as.data.frame(selectTestNum)

testFrameNew = cbind(testCat, selectTestNum)
newX = model.matrix(~., data = testFrameNew)
ncol(newX)

predictions = expm1(predict.cv.glmnet(modelNet, s = bestLamNet, newX))

salePrice = cbind.data.frame(testFrame$Id, predictions)
names(salePrice)[1] = "ID"
names(salePrice)[2] = "SalePrice"
write.csv(salePrice, "salePrice.csv", row.names = F)
