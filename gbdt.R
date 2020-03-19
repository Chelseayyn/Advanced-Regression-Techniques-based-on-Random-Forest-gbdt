#GBM Gradient Tree Boosting 
lm.gbm <- train(log(SalePrice)~ .-Id, data = train,  method = "gbm",  trControl = ctrl)
lm.pred <- predict(lm.rf, test)
res <- data.frame(Id = test$Id, SalePrice = exp(lm.pred))
write.csv(res, file = "D:/2019Summer/SSMA/kaggle/res_gbm.csv", row.names = FALSE)
