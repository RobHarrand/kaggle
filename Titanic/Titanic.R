# 22/01/16 Sens 90%, Spec 70%

library(caret) #Classification and regression training

train = read.csv("Data\\train.csv", header = TRUE)
test = read.csv("Data\\test.csv", header = TRUE)

test$Survived = 0

combi = rbind(train, test)

combi$Name = as.character(combi$Name)

#combi$Surname = sapply(combi$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][1]})

combi$Title = sapply(combi$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][2]})
combi$Title = sub(' ', '', combi$Title)

# combi$Surname = as.factor(combi$Surname)
# surnames = as.data.frame(table(combi$Surname))

combi$Title[combi$Title %in% c('Capt', 'Don', 'Major', 'Sir', 'Col', 'Jonkheer', 'Rev', 'Dr')] = 'Mr'
combi$Title[combi$Title %in% c('Ms', 'Mlle')] <- 'Miss'
combi$Title[combi$Title %in% c('Lady', 'the Countess', 'Mme', 'Dona')] <- 'Mrs'
combi$Title[grep("Leader", combi$Name)] = 'Mrs'
combi$Title <- factor(combi$Title)

combi$Survived = factor(combi$Survived)
combi$Pclass = factor(combi$Pclass)

combi$family_size = combi$SibSp + combi$Parch + 1

combi$HasFamily = ifelse(combi$family_size > 1, 1, 0)
combi$HasFamily = as.factor(combi$HasFamily)

predicted_age = train(Age ~ Pclass + Sex + SibSp + Parch + Fare + Title,
                       data=combi[!is.na(combi$Age),], method="gbm")

combi$Age[is.na(combi$Age)] = predict(predicted_age, combi[is.na(combi$Age),])

combi$Embarked[combi$Embarked == ""] = 'S'
combi$Embarked = droplevels(combi$Embarked)

combi$Survived = as.character(combi$Survived)
combi$Survived[combi$Survived == '0'] = 'Perished'
combi$Survived[combi$Survived == '1'] = 'Survived'
combi$Survived = as.factor(combi$Survived)

train_new = combi[1:891,]
test_new = combi[892:1309,]
test_new$Survived = NULL


# gbmGrid <-  expand.grid(interaction.depth = c(1, 5, 9),
#                         n.trees = (1:30)*50,
#                         shrinkage = 0.1,
#                         n.minobsinnode = 20)
# 
# set.seed(1)
# fitControl = trainControl(method = "repeatedcv", number = 10, repeats = 20, summaryFunction = twoClassSummary, 
#                            classProbs = TRUE)
# 
# model = train(Survived ~ Pclass + Sex + Age + SibSp + Parch + Embarked + Title + family_size + HasFamily, 
#                     method = "ctree", 
#                     data = train_new,
#                     #tuneGrid = gbmGrid,
#                     trControl = fitControl,
#                     metric = "ROC")
# 
# getTrainPerf(model) #Internal CV performance
# 
# install.packages('rattle')
# library('rattle')
# fancyRpartPlot(model$finalModel)
# 
# plot(model$finalModel)
# 
# pred = predict(model,test_new)
# 
# predictions = data.frame(test_new$PassengerId, pred)
# colnames(predictions) = c("PassengerId", "Survived")
# 
# predictions$Survived = as.character(predictions$Survived)
# predictions$Survived[predictions$Survived == 'Perished'] = '0'
# predictions$Survived[predictions$Survived == 'Survived'] = '1'
# predictions$Survived = as.factor(predictions$Survived)
# 
# write.table(predictions, file = "pred1.csv", row.name=FALSE, sep = ",")
# 
# #https://triangleinequality.wordpress.com/2013/09/08/basic-feature-engineering-with-the-titanic-data/
# 
# ---
#     
#         
# set.seed(100) #Set seed to ensure reproducible results
# my_control = trainControl(method = "repeatedcv", number = 10, repeats = 20, summaryFunction = twoClassSummary, classProbs = TRUE)
# 
# library('caretEnsemble')
# 
# model_list <- caretList(
#     Survived ~ Pclass + Sex + Age + SibSp + Parch + Embarked + Title + family_size,
#     data = train_new,
#     trControl=my_control,
#     metric = 'ROC',
#     weights = train_new$weight,
#     methodList=c('glm', 'gbm', "svmLinear", "rf")
# )
# 
# model_list
# 
# greedy_ensemble <- caretEnsemble(model_list)
# summary(greedy_ensemble)
# 
# pred_ens = predict(greedy_ensemble, newdata = test_new)
# pred_ens = ifelse(pred_ens >= 0.5, 1, 0)
# predictions_ens = data.frame(test_new$PassengerId, pred_ens)
# colnames(predictions_ens) = c("PassengerId", "Survived")
# 
# write.table(predictions_ens, file = "pred2.csv", row.name=FALSE, sep = ",")
# 
# 
# 




library('pROC')

fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 20,
                           summaryFunction=twoClassSummary, 
                           classProbs=T,
                           savePredictions = T)

model1 = train(Survived ~ Pclass + Sex + Age + SibSp + Parch + Embarked + Title + family_size + HasFamily, data = train_new, 
               method = 'rf',
               trControl = fitControl,
               metric = "ROC")



model2 = train(Survived ~ Pclass + Sex + Age + SibSp + Parch + Embarked + Title + family_size + HasFamily, data = train_new, 
               method = 'gbm',
               trControl = fitControl,
               metric = "ROC",
               verbose = F)

model3 = train(Survived ~ Pclass + Sex + Age + SibSp + Parch + Embarked + Title + family_size + HasFamily, data = train_new, 
               method = 'ctree',
               trControl = fitControl,
               metric = "ROC")

model4 = train(Survived ~ Pclass + Sex + Age + SibSp + Parch + Embarked + Title + family_size + HasFamily, data = train_new, 
               method = 'svmLinear',
               trControl = fitControl,
               metric = "ROC")

model5 = train(Survived ~ Pclass + Sex + Age + SibSp + Parch + Embarked + Title + family_size + HasFamily, data = train_new, 
               method = 'knn',
               trControl = fitControl,
               metric = "ROC")

length(model1$pred$obs)
length(model1$pred$Survived)

plot.roc(model1$pred$obs, model1$pred$Survived)

plot.roc(model2$pred$obs, model2$pred$Survived, add = T, col = 'red')

plot.roc(model3$pred$obs, model3$pred$Survived, add = T, col = 'blue')

plot.roc(model4$pred$obs, model4$pred$Survived, add = T, col = 'purple')

plot.roc(model5$pred$obs, model5$pred$Survived, add = T, col = 'green')

legend(x=0.4, y=0.4, lty=c(1,1,1,1,1), legend = c("Random Forest", "GBM", "CTREE", "SVM", "KNN"),
       col = c("black", "red", "blue", "purple", "green"))

auc1 = auc(roc(model1$pred$obs, model1$pred$Survived))
auc2 = auc(roc(model2$pred$obs, model2$pred$Survived))
auc3 = auc(roc(model3$pred$obs, model3$pred$Survived))
auc4 = auc(roc(model4$pred$obs, model4$pred$Survived))
auc5 = auc(roc(model5$pred$obs, model5$pred$Survived))

aucs = data.frame(auc1, auc2, auc3, auc4, auc5)

barplot(as.matrix(aucs), main="Area Under Curve (ROC)", names.arg=c("Random Forest", "GBM", "CTREE", "SVM", "KNN"), 
        beside=TRUE, col=terrain.colors(5), ylim = c(0,1))


#Generate lift curves,
evalResults1 <- data.frame(Survived = model1$pred$obs[1:891])
evalResults1$rf <- model1$pred$Survived[1:891]

evalResults2 <- data.frame(Survived = model2$pred$obs[1:891])
evalResults2$gbm <- model2$pred$Survived[1:891]

evalResults3 <- data.frame(Survived = model3$pred$obs[1:891])
evalResults3$ctree <- model3$pred$Survived[1:891]

evalResults4 <- data.frame(Survived = model4$pred$obs[1:891])
evalResults4$svm <- model4$pred$Survived[1:891]

evalResults5 <- data.frame(Survived = model5$pred$obs[1:891])
evalResults5$knn <- model5$pred$Survived[1:891]

trellis.par.set(caretTheme())

liftData1 <- lift(Survived ~ rf, data = evalResults1, class = "Survived")
liftData2 <- lift(Survived ~ gbm, data = evalResults2, class = "Survived")
liftData3 <- lift(Survived ~ ctree, data = evalResults3, class = "Survived")
liftData4 <- lift(Survived ~ svm, data = evalResults4, class = "Survived")
liftData5 <- lift(Survived ~ knn, data = evalResults5, class = "Survived")

#Plot lift curve,
plot(liftData1, values = 50, auto.key = list(columns = 3, lines = TRUE, points = FALSE),lwd = 3)
curve(liftData2, values = 50, auto.key = list(columns = 3, lines = TRUE, points = FALSE),lwd = 3)





 