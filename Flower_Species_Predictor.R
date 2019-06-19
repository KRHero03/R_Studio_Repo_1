library(caret)
library(dslabs)
library(dplyr)
library(tidyverse)
data(iris)
iris$Species
iris <- iris[-which(iris$Species=='setosa'),]
y <- iris$Species
set.seed(2, sample.kind = "Rounding")
test_index <- createDataPartition(y,times=1,p=0.5,list=FALSE)
test <- iris[test_index,]
train <- iris[-test_index,]
petal_width_cutoff <- seq(1.3,2.1,0.1)
petal_length_cutoff <- seq(4.3,5.5,0.1)
accuracy <- map_dbl(petal_width_cutoff,function(x){
  accuracy1<- map_dbl(petal_length_cutoff,function(z){
    y_hat <- ifelse(train$Petal.Length>z || train$Petal.Width>x,"virginica","versicolor") %>% factor(levels=levels(test$Species))
    mean(y_hat==test$Species)
  })
})
accuracy
#y_hat <-ifelse(train$Petal.Length>=5,"virginica","versicolor") %>%factor(levels=levels(test$Species))
#mean(y_hat==test$Species)

