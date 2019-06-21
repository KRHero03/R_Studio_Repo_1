library(tidyverse)
library(dslabs)
library(caret)
library(rpart)
library(randomForest)
set.seed(1991,sample.kind = "Rounding")
n <- 1000
sigma <- 0.25
x <- tissue_gene_expression$x
y <- tissue_gene_expression$y
dat <- data.frame(x = x, y = y)
fit_rf <- train(x,y,method="rf",tuneGrid=data.frame(mtry=seq(50,200,25)),nodesize=1)
fit_rpart <- train(x,y,method="rpart",tuneGrid=data.frame(cp=0.01), control = rpart.control(minsplit=0))
imp <- varImp(fit_rpart)
imp
tree_terms <- as.character(unique(fit_rpart$finalModel$frame$var[!(fit_rpart$finalModel$frame$var == "<leaf>")]))
plot(fit_rpart$finalModel)
text(fit_rpart$finalModel)
confusionMatrix(fit)
dat %>%mutate(y_hat = predict(fit)) %>% ggplot() + geom_point(aes(x, y)) +geom_step(aes(x, y_hat), col=2)
