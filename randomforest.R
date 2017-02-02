library(Hmisc)
library(corrplot)
library(Amelia)
library(mice)
library(ggplot2)
library(lattice)
library(missForest)
library(randomForest)

train <- read.csv('C:/Users/JayZheng/Documents/Data Science/kaggle/houseprice/train.csv',header = TRUE, sep = ",")
test <- read.csv('C:/Users/JayZheng/Documents/Data Science/kaggle/houseprice/test.csv',header = TRUE, sep = ",")
describe(train$SalePrice) ## no missing
hist(train$SalePrice,xlab="Sale Price",main = "Sale Price")
describe(log(train$SalePrice))#use log transformation to normalize the data distribution 
#check distribution 
hist(log(train$SalePrice),xlab="Log Sale Price",main = "Log transformed Sale Price")

train$SalePrice<-log(train$SalePrice)

test$SalePrice <- NA # make a dummy sale column in test data
train$isTrain <- 1
test$isTrain <- 0
combined <- rbind(train,test)
# N2 <- gsub(",","",combined)
# N2= data.frame(N2)
#remoe the below 4 columns
removed <- c('PoolQC', 'MiscFeature', 'Alley', 'Fence') # remove the 4 columns with more than 50% NA
included <- setdiff(names(combined), removed)
combined <- combined[included] #new data without the 4 columns above

#-----------------------------------------------------------------------------------------------------
  
#impute these variable with 0 or mean
combined$MasVnrArea[which(is.na(combined$MasVnrArea))] <- 0# mean(combined$MasVnrArea,na.rm=T)




# Changing NA in MasVnrType to None
combined$MasVnrType1 <- as.character(combined$MasVnrType)
combined$MasVnrType1[which(is.na(combined$MasVnrType))] <- "None"
combined$MasVnrType <- as.factor(combined$MasVnrType1)
combined <- subset(combined,select = -MasVnrType1)

  # Imputing missing Lot Frontage by 0 or median whatever is better
combined$LotFrontage[which(is.na(combined$LotFrontage))] <- 0 #median(combined$LotFrontage,na.rm = T)

#change NA in FirePlaceQu
combined$FireplaceQu1 <- as.character(combined$FireplaceQu)
combined$FireplaceQu1[which(is.na(combined$FireplaceQu))] <- "None"
combined$FireplaceQu <- as.factor(combined$FireplaceQu1)
combined <- subset(combined,select = -FireplaceQu1)

# Changing NA in MiscFeature to None
combined$MiscFeature1 <- as.character(combined$MiscFeature)
combined$MiscFeature1[which(is.na(combined$MiscFeature))] <- "None"
combined$MiscFeature <- as.factor(combined$MiscFeature1)
combined <- subset(combined,select = -MiscFeature1)

# Changing NA in GarageType to None
combined$GarageType1 <- as.character(combined$GarageType)
combined$GarageType1[which(is.na(combined$GarageType))] <- "None"
combined$GarageType <- as.factor(combined$GarageType1)
combined <- subset(combined,select = -GarageType1)

# Changing NA in GarageYrBlt to None
combined$GarageYrBlt[which(is.na(combined$GarageYrBlt))] <- 0 

# Changing NA in GarageFinish to None
combined$GarageFinish1 <- as.character(combined$GarageFinish)
combined$GarageFinish1[which(is.na(combined$GarageFinish))] <- "None"
combined$GarageFinish <- as.factor(combined$GarageFinish1)
combined <- subset(combined,select = -GarageFinish1)

# Changing NA in GarageQual to None
combined$GarageQual1 <- as.character(combined$GarageQual)
combined$GarageQual1[which(is.na(combined$GarageQual))] <- "None"
combined$GarageQual <- as.factor(combined$GarageQual1)
combined <- subset(combined,select = -GarageQual1)

# Changing NA in GarageCond to None
combined$GarageCond1 <- as.character(combined$GarageCond)
combined$GarageCond1[which(is.na(combined$GarageCond))] <- "None"
combined$GarageCond <- as.factor(combined$GarageCond1)
combined <- subset(combined,select = -GarageCond1)

# Changing NA in BsmtQual to None
combined$BsmtQual1 <- as.character(combined$BsmtQual)
combined$BsmtQual1[which(is.na(combined$BsmtQual))] <- "None"
combined$BsmtQual <- as.factor(combined$BsmtQual1)
combined <- subset(combined,select = -BsmtQual1)

# Changing NA in BsmtCond to None
combined$BsmtCond1 <- as.character(combined$BsmtCond)
combined$BsmtCond1[which(is.na(combined$BsmtCond))] <- "None"
combined$BsmtCond <- as.factor(combined$BsmtCond1)
combined <- subset(combined,select = -BsmtCond1)

# Changing NA in BsmtExposure to None
combined$BsmtExposure1 <- as.character(combined$BsmtExposure)
combined$BsmtExposure1[which(is.na(combined$BsmtExposure))] <- "None"
combined$BsmtExposure <- as.factor(combined$BsmtExposure1)
combined <- subset(combined,select = -BsmtExposure1)

# Changing NA in BsmtFinType1 to None
combined$BsmtFinType11 <- as.character(combined$BsmtFinType1)
combined$BsmtFinType11[which(is.na(combined$BsmtFinType1))] <- "None"
combined$BsmtFinType1 <- as.factor(combined$BsmtFinType11)
combined <- subset(combined,select = -BsmtFinType11)

# Changing NA in BsmtFinType2 to None
combined$BsmtFinType21 <- as.character(combined$BsmtFinType2)
combined$BsmtFinType21[which(is.na(combined$BsmtFinType2))] <- "None"
combined$BsmtFinType2 <- as.factor(combined$BsmtFinType21)
combined <- subset(combined,select = -BsmtFinType21)

# Changing NA in Electrical to None
combined$Electrical1 <- as.character(combined$Electrical)
combined$Electrical1[which(is.na(combined$Electrical))] <- "None"
combined$Electrical <- as.factor(combined$Electrical1)
combined <- subset(combined,select = -Electrical1)

# Changing NA in MSZoning to None
combined$MSZoning1 <- as.character(combined$MSZoning)
combined$MSZoning1[which(is.na(combined$MSZoning))] <- "None"
combined$MSZoning <- as.factor(combined$MSZoning1)
combined <- subset(combined,select = -MSZoning1)

# Changing NA in Ulities to None
combined$Utilities1 <- as.character(combined$Utilities)
combined$Utilities1[which(is.na(combined$Utilities))] <- "None"
combined$Utilities <- as.factor(combined$Utilities1)
combined <- subset(combined,select = -Utilities1)

# Changing NA in BsmtFullBath and BsmtHalfBath to 0

combined$BsmtFullBath[which(is.na(combined$BsmtFullBath))] <- 0
combined$BsmtHalfBath[which(is.na(combined$BsmtHalfBath))] <- 0

# Changing NA in functional to None
combined$Functional1 <- as.character(combined$Functional)
combined$Functional1[which(is.na(combined$Functional))] <- "None"
combined$Functional <- as.factor(combined$Functional1)
combined <- subset(combined,select = -Functional1)

# Changing NA in Exterior1st to None
combined$Exterior1st1 <- as.character(combined$Exterior1st)
combined$Exterior1st1[which(is.na(combined$Exterior1st))] <- "None"
combined$Exterior1st <- as.factor(combined$Exterior1st1)
combined <- subset(combined,select = -Exterior1st1)

# Changing NA in Exterior2nd to None
combined$Exterior2nd1 <- as.character(combined$Exterior2nd)
combined$Exterior2nd1[which(is.na(combined$Exterior2nd))] <- "None"
combined$Exterior2nd <- as.factor(combined$Exterior2nd1)
combined <- subset(combined,select = -Exterior2nd1)

# Changing NA in BsmtFinSF1 to None

combined$BsmtFinSF1[which(is.na(combined$BsmtFinSF1))] <- 0


# Changing NA in BsmtFinSF2 to 0

combined$BsmtFinSF2[which(is.na(combined$BsmtFinSF2))] <- 0


# Changing NA in BsmtUnfSF to 0

combined$BsmtUnfSF[which(is.na(combined$BsmtUnfSF))] <- 0


# Changing NA in TotalBsmtSF to 0

combined$TotalBsmtSF[which(is.na(combined$TotalBsmtSF))] <- 0


# Changing NA in KitchenQual to None
combined$KitchenQual1 <- as.character(combined$KitchenQual)
combined$KitchenQual1[which(is.na(combined$KitchenQual))] <- "None"
combined$KitchenQual <- as.factor(combined$KitchenQual1)
combined <- subset(combined,select = -KitchenQual1)

# Changing NA in GarageCars to None
combined$GarageCars1 <- as.character(combined$GarageCars)
combined$GarageCars1[which(is.na(combined$GarageCars))] <- "None"
combined$GarageCars <- as.factor(combined$GarageCars1)
combined <- subset(combined,select = -GarageCars1)

# Changing NA in GarageArea to 0

combined$GarageArea[which(is.na(combined$GarageArea))] <- 0


# Changing NA in SaleType to None
combined$SaleType1 <- as.character(combined$SaleType)
combined$SaleType1[which(is.na(combined$SaleType))] <- "None"
combined$SaleType <- as.factor(combined$SaleType1)
combined <- subset(combined,select = -SaleType1)


levels(combined$MSZoning) <- c(levels(combined$MSZoning),"None")
levels(combined$Utilities) <- c(levels(combined$Utilities),"None","NoSeWa")
levels(combined$Exterior1st) <- c(levels(combined$Exterior1st),"None","ImStucc","Stone")
levels(combined$Exterior2nd) <- c(levels(combined$Exterior2nd),"None","Other")
levels(combined$KitchenQual) <- c(levels(combined$KitchenQual),"None")
levels(combined$Functional) <- c(levels(combined$Functional),"None")
levels(combined$SaleType) <- c(levels(combined$SaleType),"None")


#use the below manually if data still not clean
# #change to factor or numeric values
# combined$SalePrice <- as.numeric(combined$SalePrice)
# combined$GarageArea <- as.numeric(combined$GarageArea)
# combined$MoSold <- as.factor(combined$MoSold)
# combined$YrSold <- as.factor(combined$YrSold)
# combined$FirstFlrSF <- as.numeric(combined$FirstFlrSF)
# combined$SecondFlrSF <- as.numeric(combined$SecondFlrSF)
# combined$GrLivArea <- as.numeric(combined$GrLivArea)
# combined$MSSubClass <- as.numeric(combined$MSSubClass)
# combined$BsmtFinSF1 <- as.numeric(combined$BsmtFinSF1)
# combined$BsmtFinSF2 <- as.numeric(combined$BsmtFinSF2)
# combined$MiscVal <- as.numeric(combined$MiscVal)
# combined$GarageCars <- gsub(",","",combined$GarageCars)
# combined$GarageCars <- as.numeric(combined$GarageCars)
# combined$BsmtUnfSF <- gsub(",","",combined$BsmtUnfSF)
# combined$BsmtUnfSF <- as.numeric(combined$BsmtUnfSF)
# combined$BsmtFullBath <- gsub(",","",combined$BsmtFullBath)
# combined$BsmtFullBath <- as.numeric(combined$BsmtFullBath)
# combined$BsmtHalfBath <- gsub(",","",combined$BsmtHalfBath)
# combined$BsmtHalfBath <- as.numeric(combined$BsmtHalfBath)
# 
# 
# #impute the remaining missing data manually
# combined$MSZoning[which(is.na(combined$MSZoning))] <- "None"
# combined$Utilities[which(is.na(combined$Utilities))] <- "None"
# combined$BsmtFullBath[which(is.na(combined$BsmtFullBath))] <- "None"
# combined$Functional[which(is.na(combined$Functional))] <- "None"
# combined$Exterior1st[which(is.na(combined$Exterior1st))] <- "None"
# combined$Exterior2nd[which(is.na(combined$Exterior2nd))] <- "None"
# combined$BsmtFinSF1[which(is.na(combined$BsmtFinSF1))] <- 0
# combined$BsmtFinSF2[which(is.na(combined$BsmtFinSF2))] <- 0
# combined$TotalBsmtSF[which(is.na(combined$TotalBsmtSF))] <- 0
# combined$KitchenQual[which(is.na(combined$KitchenQual))] <- "None"
# combined$GarageArea[which(is.na(combined$GarageArea))] <- 0
# combined$SaleType[which(is.na(combined$SaleType))] <- "None"
# combined$BsmtHalfBath[which(is.na(combined$BsmtHalfBath))] <- "None"
# combined$BsmtUnfSF[which(is.na(combined$BsmtUnfSF))] <- "None"
# combined$GarageCars[which(is.na(combined$GarageCars))] <- "None"

#check for missing data graphically
missmap(combined[-1], col=c('Red', 'Green'), y.cex=0.2, x.cex=0.8,main = "Before Imputation")
ms_dt =sort(sapply(combined, function(x) { sum(is.na(x)) }), decreasing=TRUE)


str(combined)#summary of data


#now is time to split the data for training validation and test

train <- combined[combined$isTrain==1,]
test <- combined[combined$isTrain==0,]
smp_size <- floor(0.75 * nrow(train))
set.seed(123)
train_ind <- sample(seq_len(nrow(train)), size = smp_size)
train_new <- train[train_ind, ]
validate <- train[-train_ind, ]
train_new <- subset(train_new,select=-c(Id,isTrain))
validate <- subset(validate,select=-c(Id,isTrain))
nrow(train_new)
nrow(validate)
str(train_new)

fit <- randomForest(SalePrice ~ ., train_new,ntree=475) # tried different number of trees 450 gives roughly the best result
summary(fit)

prediction <- predict(fit,validate)
rmse <- sqrt(mean((log(prediction)-log(validate$SalePrice))^2))
rmse    
#test labels

prediction = predict(fit,test)
prediction = exp(prediction)
prediction[which(is.na(prediction))] <- mean(prediction,na.rm=T)
submit <- data.frame(Id=test$Id,SalePrice=prediction)
write.csv(submit,file="house_prediction.csv",row.names=F)


# random forest parameter tuning 
mtry <- tuneRF(train_new[-1],train_new$SalePrice, ntreeTry=300,
               stepFactor=1.5,improve=0.01, trace=TRUE, plot=TRUE)

best.m <- mtry[mtry[, 2] == min(mtry[, 2]), 1]

print(mtry)
print(best.m)
