install.packages("stats", dependencies = TRUE)
install.packages("class",dependencies = TRUE)
install.packages("adabag",dependencies = TRUE)
install.packages("randomForest",dependencies = TRUE)
install.packages("multinom",dependencies = TRUE)
install.packages("AdaBag",dependencies = TRUE)
install.packages("AdaBoost.M1",dependencies = TRUE)
install.packages("pROC",dependencies = TRUE)
library(stats)
library(class)
library(adabag)
library(randomForest)
library(pROC)


  trainIndex	<- sample(1:nrow(transformed),	0.8 *	nrow(transformed))
  train	<- transformed[trainIndex,	]
  test	<- transformed[-trainIndex,	]
  testAttr<- test[ ,!(names(test) %in% c("class"))]
  testlabel<-test[,c("class")]

#knn
cat("===============Knn=================")
train_control <- trainControl(method="repeatedcv", number=10, savePredictions = TRUE)
# train the model
knn_model <- train(as.factor(class)~., data=train, trControl=train_control, method="knn")
# summarize results
print(knn_model)
pred.knn<-predict(knn_model,newdata=transformed)
confusionMatrix(data=pred.knn,transformed$class)
ROC.knn<-roc(transformed[,34],as.numeric(pred.knn))
plot(ROC.knn,col="red")

#Random Forest
cat("===============Random Forest=================\n")
train_control <- trainControl(method="repeatedcv", number=10, savePredictions = TRUE)
# train the model
rf_model <- train(as.factor(class)~., data=transformed, trControl=train_control, method="rf")
# summarize results
print(rf_model)
pred.rf<-predict(rf_model,newdata=transformed)
confusionMatrix(data=pred.rf,transformed$class)
ROC.rf<-roc(transformed[,34],as.numeric(pred.rf))
plot(ROC.rf,col="red")
#Bagging
cat("===============Bagging=================\n")
train_control <- trainControl(method="repeatedcv", number=10,repeats =3,savePredictions = TRUE)
# train the model
bg_model <- train(as.factor(class)~., data=transformed, trControl=train_control, method="AdaBag")
# summarize results
print(bg_model)
pred.bg<-predict(bg_model,newdata=transformed)
confusionMatrix(data=pred.bg,transformed$class)
ROC.bg<-roc(transformed[,34],as.numeric(pred.bg))
plot(ROC.bg,col="red")

#Boosting
cat("===============Boosting=================\n")
train_control <- trainControl(method="CV", number=10, savePredictions = TRUE)
# train the model
boost_model <- train(as.factor(class)~., data=transformed, trControl=train_control, method="AdaBoost.M1")
# summarize results
print(boost_model)
pred.boost<-predict(boost_model,newdata=transformed)
confusionMatrix(data=pred.boost,test$class)
ROC.boost<-roc(test[,34],as.numeric(pred.glm))
plot(ROC.boost,col="red")

#Logistic Regression
cat("===============Logistic Regression=================\n")
train_control <- trainControl(method="repeatedcv", number=10, savePredictions = TRUE)
# train the model
glm_model <- train(as.factor(class)~., data=transformed, trControl=train_control, method="multinom")
# summarize results
print(glm_model)
pred.glm<-predict(glm_model,newdata=transformed)
confusionMatrix(data=pred.glm,transformed$class)
ROC.glm<-roc(transformed[,34],as.numeric(pred.glm))
plot(ROC.glm,col="red")
