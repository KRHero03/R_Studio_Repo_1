library(caret)
library(dslabs)
library(dplyr)
library(tidyverse)
data(heights)
y <- heights$sex
x <- heights$height
set.seed(1)
test_index <- createDataPartition(y,times=1,p=0.5,list=FALSE)
test_set1 <- heights[test_index, ]
train_set <- heights[-test_index, ]
cutoff <- seq(61,70)
accuracy <- map_dbl(cutoff,function(x){
  y_hat <- ifelse(train_set$height>x,"Male","Female") %>% factor(levels=levels(test_set1$sex))
  mean(y_hat==test_set1$sex)
})
best_cutoff <- cutoff[which.max(accuracy)]
y_hat <- ifelse(train_set$height>best_cutoff,"Male","Female") %>% factor(levels=levels(test_set1$sex))
#confusionMatrix(y_hat,test_set1$sex)
F_1 <- map_dbl(cutoff,function(x){
  y_hat <- ifelse(train_set$height>x,"Male","Female") %>% factor(levels=levels(test_set1$sex))
  F_meas(y_hat,reference=factor(test_set1$sex))
})
best_cutoff <- cutoff[which.max(F_1)]
y_hat <- ifelse(train_set$height>best_cutoff,"Male","Female") %>% factor(levels=levels(test_set1$sex))
confusionMatrix(y_hat,test_set1$sex)

