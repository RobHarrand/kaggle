#KAGGLE HOUSE PRICE PREDICTIONS


#Today...



# Load libraries and data -------------------------------------------------

library('caret')
library('Boruta')
library('zoo')
library('e1071')
library('rpart')
library("glmnet")

train = read.csv("train.csv", stringsAsFactors = T)
test = read.csv("test.csv", stringsAsFactors = T)



# Simple pre-processing ---------------------------------------------------

#log transform the target,
train$SalePrice = log(train$SalePrice)

test$SalePrice = 0

sp = c(train$SalePrice, test$SalePrice) #Keep the saleprice data

combine = rbind(train, test) #Combine
combine$Id = NULL

#x1 = combine$YearRemodAdd
#x2 = combine$YearBuilt
#x3 = combine$MoSold
#x4 = combine$YrSold

nums <- sapply(combine, is.numeric)

i=1

while (i<length(nums[nums=='TRUE'])) {
    
    s = skewness(combine[,nums][i][,1], na.rm = T)
    if (abs(s)>=0.75) {combine[,nums][i][,1] = log(combine[,nums][i][,1]+1)}
    
i=i+1
    
}

#combine[,nums] = log(combine[,nums]+1) #Log tranform the numeric variables

#combine$SalePrice = sp #Put the right saleprices back
#combine$YearRemodAdd = x1
#combine$YearBuilt = x2
#combine$MoSold = x3
#combine$YrSold = x4



# Tidy the data -----------------------------------------------------------------


#Majority is NAs. Get rid of them...
combine$Alley = NULL
combine$MiscFeature = NULL
combine$PoolQC = NULL
combine$Fence = NULL
combine$FireplaceQu = NULL

#Where are the NAs?
nas = data.frame(sapply(combine, function(x) sum(is.na(x))))
nas$Names = rownames(nas)
nas = data.frame(nas[nas$sapply.combine..function.x..sum.is.na.x... != 0,])
nas = nas[order(-nas$sapply.combine..function.x..sum.is.na.x...),] #Get them in order of scale

#Use rpart to predict all the missing values...

# i=1
# 
# while (i<=length(nas$Names)) {
# 
#     temp = nas$Names[i]
#     temp_data = combine[!is.na(combine[,temp]),]
#     
#     if (is.factor(combine[,nas$Names[i]])) {
#         
#         temp_fit = rpart(temp_data[,temp] ~ ., method = 'class', data = temp_data)
#         combine[,temp][is.na(combine[,temp])] <- predict(temp_fit, combine[is.na(combine[,temp]),], type = 'class')
#         
#     } else {
#     
#         temp_fit = rpart(temp_data[,temp] ~ ., method = 'anova', data = temp_data)
#         combine[,temp][is.na(combine[,temp])] <- predict(temp_fit, combine[is.na(combine[,temp]),])
#         
#     }
#     i=i+1
# }

#Just use means...

i=1

while (i<=length(nas$Names)) {
    
    temp = nas$Names[i]
    temp_data = combine[!is.na(combine[,temp]),]
    
    if (is.factor(combine[,nas$Names[i]])) {
        
        temp_fit = rpart(temp_data[,temp] ~ ., method = 'class', data = temp_data)
        combine[,temp][is.na(combine[,temp])] <- predict(temp_fit, combine[is.na(combine[,temp]),], type = 'class')
        
    } else {
        
        combine[,temp][is.na(combine[,temp])] = mean(combine[,temp], na.rm = T)
    
    }
    i=i+1
}

#sapply(combine, function(x) sum(is.na(x)))


#write.csv(combine, "combined_no_na.csv")
combine = read.csv("combined_no_na.csv")
combine$X = NULL



# Feature Engineering -----------------------------------------------------


#Correlations

# library('corrplot')
# 
# n = train_bor[sapply(train_bor, class) == 'numeric']
# n
# 
# correlations <- cor(n, use="everything")
# corrplot(correlations, method="circle", type="lower",  sig.level = 0.01, insig = "blank")
# 
# correlations[correlations > 0.5 & correlations < 1]


combine$New1 = combine$TotalBsmtSF * combine$BsmtFinSF1
combine$New2 = combine$GarageCars * combine$GarageArea
combine$New3 = combine$GarageCars * combine$GarageYrBlt
combine$Remod = 0
combine$Remod[combine$YearBuilt != combine$YearRemodAdd] = 1
combine$Remod = as.factor(combine$Remod)
combine$AgeSold = combine$YrSold - combine$YearBuilt

combine$DateSold = paste(as.character(combine$MoSold), as.character(combine$YrSold), sep = '-')
combine$DateSold = as.yearmon(combine$DateSold, "%m-%Y")

combine$Total_sqf = combine$X1stFlrSF + combine$X2ndFlrSF + combine$LowQualFinSF + combine$GrLivArea
combine$Total_baths = combine$BsmtFullBath + combine$BsmtHalfBath + combine$FullBath + combine$HalfBath
combine$Total_outside = combine$WoodDeckSF + combine$OpenPorchSF + combine$EnclosedPorch + combine$X3SsnPorch + combine$ScreenPorch

combine$X1stFlrSF = NULL
combine$X2ndFlrSF = NULL
combine$LowQualFinSF = NULL
combine$GrLivArea = NULL
combine$BsmtFullBath = NULL
combine$BsmtHalfBath = NULL
combine$FullBath = NULL
combine$HalfBath = NULL
combine$WoodDeckSF = NULL
combine$OpenPorchSF = NULL
combine$EnclosedPorch = NULL
combine$X3SsnPorch = NULL
combine$ScreenPorch = NULL
combine$BsmtFinSF1 = NULL
combine$BsmtFinSF2 = NULL
combine$BsmtUnfSF = NULL


#Scale numerical features,



combine_p <- preProcess(combine[,-75], method = c("center", "scale"))

combine_t <- predict(combine_p, newdata = combine[,-75])
combine_t = cbind(combine_t, combine$SalePrice)
combine_t$Id = seq(1,2919,1)
combine = combine_t

#colnames(combine)[86] = 'Id'
colnames(combine)[84] = 'SalePrice'





#write.csv(combine, "combined_preprocessed.csv")
combine = read.csv("combined_preprocessed.csv")
combine$X.1 = NULL

train = combine[1:1460,]
test = combine[1461:2919,]



# Feature importance ------------------------------------------------------

set.seed(1)
bor.results <- Boruta(train, train$SalePrice, maxRuns=20, doTrace=0)

#plot(bor.results)

att = getSelectedAttributes(bor.results, withTentative = T)
train_bor = train[att]
test_bor = test[att]


#write.csv(train_bor, "train_bor.csv")
#write.csv(test_bor, "test_bor.csv")
train_bor = read.csv("train_bor.csv")
test_bor = read.csv("test_bor.csv")

colnames(train_bor)[1] = 'Id'
colnames(test_bor)[1] = 'Id'

train_bor$Remod = as.factor(train_bor$Remod)
test_bor$Remod = as.factor(test_bor$Remod)

#write.csv(train_bor$DateSold, "train_date.csv")
#write.csv(test_bor$DateSold, "test_date.csv")
#train_date = read.csv("train_date.csv")
#test_date = read.csv("test_date.csv")

#train_bor$DateSold = train_date$x #Put this back in
#test_bor$DateSold = test_date$x #Put this back in



# Fit the model -----------------------------------------------------------

set.seed(1)
fitControl <- trainControl(method = "repeatedcv", number = 5, repeats = 5, verboseIter=FALSE,
                          classProbs=FALSE)

# set up the cross-validated hyper-parameter search
gbm_grid = expand.grid(interaction.depth = c(1, 5, 9), 
                       n.trees = (1:30)*50, 
                       shrinkage = 0.1,
                       n.minobsinnode = 10)

rf_grid = expand.grid(.mtry=c(1:15))

xgb_grid = expand.grid(nrounds = 1000,
                         eta = c(0.03, 0.003, 0.0003),
                         max_depth = c(2, 4, 6, 8, 10),
                         colsample_bytree = 0.4,
                         min_child_weight = 1,
                         gamma = 0.1)


glmnet = expand.grid(alpha=1,  lambda=c(1,0.1,0.05,0.01,seq(0.009,0.001,-0.001),
                                                   0.00075,0.0005,0.0001))


# model = train(SalePrice ~., 
#               data = train_bor,
#               method = 'glmnet',
#               trControl = fitControl, 
#               #preProcess=c("pca"),
#               metric = "RMSE")

# model = train(SalePrice ~., 
#               data = train_bor,
#               method = 'gbm',
#               trControl = fitControl, 
#               tuneGrid = gbm_grid,
#               #preProcess=c("pca"),
#               metric = "RMSE")
# 
# 
# model2 = train(SalePrice ~., 
#                data = train_bor,
#                method = 'glm',
#                trControl = fitControl, 
#                #tuneGrid = grid,
#                #preProcess=c("pca"),
#                metric = "RMSE")
# 
# 
# model3 = train(SalePrice ~., 
#                data = train_bor,
#                method = 'rf',
#                trControl = fitControl, 
#                #tuneGrid = grid,
#                #preProcess=c("pca"),
#                metric = "RMSE")
# 
# 
# model4 = train(SalePrice ~., 
#                data = train_bor,
#                method = 'xgbTree',
#                trControl = fitControl , 
#                tuneGrid = xgb_grid_1,
#                #preProcess=c("pca"),
#                metric = "RMSE")
# 
#getTrainPerf(model4)
# getTrainPerf(model2)
# getTrainPerf(model3)
# getTrainPerf(model4)
# 
# 
library('caretEnsemble')

#With expanded tuning...
# model_list <- caretList(SalePrice ~. -Id, 
#                         data=train_bor,
#                         trControl=fitControl,
#                         tuneList = list(
#                             gbm=caretModelSpec(method = 'gbm', tuneGrid=gbm_grid),
#                             rf=caretModelSpec(method = 'rf', tuneGrid=rf_grid),
#                             xgbTree=caretModelSpec(method = 'xgbTree', tuneGrid=xgb_grid)),
#                         metric = 'RMSE')

#Default tuning...
model_list <- caretList(SalePrice ~., 
                        data=train_bor,
                        trControl=fitControl,
                        tuneList = list(
                            glmnet=caretModelSpec(method = 'glmnet', tuneGrid=glmnet),
                            gbm=caretModelSpec(method = 'gbm', tuneGrid=gbm_grid),
                            rf=caretModelSpec(method = 'rf', tuneGrid=rf_grid),
                            xgbTree=caretModelSpec(method = 'xgbTree', tuneGrid=xgb_grid)),
                        metric = 'RMSE')

caret_list_data = save(model_list, file = 'caret_list_data.RData')


model_list

getTrainPerf(model_list$glmnet)
getTrainPerf(model_list$rf)
getTrainPerf(model_list$gbm)
getTrainPerf(model_list$xgb)


gbm_ensemble <- caretStack(
    model_list,
    method="gbm",
    metric="RMSE",
    trControl=trainControl(
        method="boot",
        number=10,
        savePredictions="final"
    )
)

summary(gbm_ensemble)

#greedy_ensemble <- caretEnsemble(model_list)
#summary(greedy_ensemble)

ens_preds <- predict(gbm_ensemble, newdata=test_bor)

ens_preds = exp(ens_preds)

#   
# save(model4, file = 'xgb.RData')
# load('xgb.RData')
# 
#colnames(test_bor)[44] = 'FireplaceQu'


#varImp(greedy_ensemble)
#varImp(model)

#p_gbm = predict(model_list$gbm, test)
#p_rf = predict(model_list$rf, test)



# Submission --------------------------------------------------------------

sample = read.csv('sample_submission.csv')
#K_sub = read.csv('K_test_sub1.csv')

#plot(ens_preds, sample$SalePrice, xlim = c(0,7e5), ylim = c(0,300000))
#points(p_gbm, sample$SalePrice, xlim = c(0,7e5), ylim = c(0,300000), col = 'red')
#points(p_rf, sample$SalePrice, xlim = c(0,7e5), ylim = c(0,300000), col = 'blue')

#p = mean(p_glm, p_gbm, p_rf)

#points(p, sample$SalePrice, xlim = c(0,7e5), ylim = c(0,300000), col = 'green')

#plot(p, K_sub$SalePrice, xlim = c(0,7e5), ylim = c(0,300000))

#test_bor = cbind(test_bor, test$Id)
#colnames(test_bor)[63] = 'Id'

sub = data.frame(ens_preds)
colnames(sub) = c("SalePrice")

write.csv(sub, "sub_041116d.csv")
