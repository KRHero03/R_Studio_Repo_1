library(tidyverse)
library(dslabs)
library(caret)
library(rpart)
library(randomForest)
models <- c("glm", "lda",  "naive_bayes",  "svmLinear", 
            "gamboost",  "gamLoess", "qda", 
            "knn", "kknn", "loclda", "gam",
            "rf", "ranger",  "wsrf", "Rborist", 
            "avNNet", "mlp", "monmlp",
            "adaboost", "gbm",
            "svmRadial", "svmRadialCost", "svmRadialSigma")
set.seed(1,sample.kind="Rounding")

data("mnist_27")

fits <- lapply(models, function(model){ 
  train(y ~ ., method = model, data = mnist_27$train)
})
names(fits) <- models


accuracy_cross_validation <- sapply(fits,function(model){
  min(model$results$Accuracy)
})mean(accuracy_cross_validation)


accuracy<- sapply(fits,function(x){
  y_hat<-predict(x,mnist_27$test)
  confusionMatrix(y_hat,mnist_27$test$y)$overall['Accuracy']
})
mean(accuracy)


y_hat <- sapply(fits,function(x){
  predict(x,mnist_27$test)
})
y_hat_ensemble <- matrix(ncol=1,nrow=200)
for(i in seq(1,200)){
  majority <- majorityVote(y_hat[i,])$majority
  y_hat_ensemble[i,1]=majority
}
accuracy_ensemble<- confusionMatrix(as.factor(y_hat_ensemble[,1]),mnist_27$test$y)$overall['Accuracy']


sum(accuracy>=accuracy_ensemble)
