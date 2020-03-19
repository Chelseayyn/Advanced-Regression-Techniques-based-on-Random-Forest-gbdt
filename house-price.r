library(randomForest)
library(caret)

train <- read.csv("D:/2019Summer/SSMA/kaggle/train.csv")
test <- read.csv("D:/2019Summer/SSMA/kaggle/test.csv")

test$SalePrice <- NA
all <- rbind(train, test)

Drop <- names(all) %in% c("PoolQC","MiscFeature","Alley","Fence","FireplaceQu")
all <- all[!Drop]

Garage <- c("GarageType","GarageQual","GarageCond","GarageFinish")
Bsmt <- c("BsmtExposure","BsmtFinType2","BsmtQual","BsmtCond","BsmtFinType1")
for (x in c(Garage, Bsmt) )
{
all[[x]] <- factor( all[[x]], levels= c(levels(all[[x]]),c('None')))
all[[x]][is.na(all[[x]])] <- "None"
}

all$GarageYrBlt[is.na(all$GarageYrBlt)] <- all$YearBuilt[is.na(all$GarageYrBlt)]

all$LotFrontage[is.na(all$LotFrontage)] <- median(all$LotFrontage, na.rm = T)

all[["MasVnrType"]][is.na(all[["MasVnrType"]])] <- "None"

all[["MasVnrArea"]][is.na(all[["MasVnrArea"]])] <- 0

all$Utilities <- NULL

Param0 <- c("BsmtFullBath","BsmtHalfBath","BsmtFinSF1","BsmtFinSF2","BsmtUnfSF","TotalBsmtSF","GarageCars","GarageArea")
for (x in Param0 )    all[[x]][is.na(all[[x]])] <- 0

Req <- c("MSZoning","Functional","Exterior1st","Exterior2nd","KitchenQual","Electrical","SaleType")
for (x in Req )    all[[x]][is.na(all[[x]])] <- levels(all[[x]])[which.max(table(all[[x]]))]

train <- all[!is.na(all$SalePrice), ]
test <- all[is.na(all$SalePrice), ]

'
write.csv(train, file = "D:/2019Summer/SSMA/kaggle/train_c.csv")
write.csv(test, file = "D:/2019Summer/SSMA/kaggle/test_c.csv")
'

set.seed(1926)


ctrl <- trainControl(method = "cv", number = 10, repeats = 20, verboseIter = TRUE)

lm.rf <- train(log(SalePrice)~ .-Id, data = train,  method = "rf",  trControl = ctrl,  tuneLength = 3)


lm.pred <- predict(lm.rf, test)
res <- data.frame(Id = test$Id, SalePrice = exp(lm.pred))
write.csv(res, file = "D:/2019Summer/SSMA/kaggle/res_randomForest.csv", row.names = FALSE)