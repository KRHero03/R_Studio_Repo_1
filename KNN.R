library(caret)
library(dslabs)
set.seed(1,sample.kind="Rounding")
ks<-c(1,3,5,7,9,11) 
my_x<-tissue_gene_expression$x
my_y<- tissue_gene_expression$y

test_index <- createDataPartition(my_y,times=1,p=0.5,list=FALSE)
test_set_x <- my_x[-test_index,] 
test_set_y <- my_y[-test_index] 
train_set_x <- my_x[test_index,] 
train_set_y <- my_y[test_index]
accuracy<-sapply(ks,function(p){
  fit<-knn3(train_set_x,train_set_y,k=p)
  y_hat<-predict(fit,test_set_x,type="class")
  confusionMatrix(data=y_hat,reference=test_set_y)$overall['Accuracy']
}) 
accuracy

