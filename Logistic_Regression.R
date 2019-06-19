library(tidyverse)
library(caret)
library(dslabs)

m_1 <- seq(0,3,len=25)
n<-seq(1:25)
accuracy<-vector(mode="numeric",length=25)


set.seed(2,sample.kind="Rounding")
make_data <- function(n = 1000, p = 0.5, 
                      mu_0 = 0, mu_1 = m_1,
                      sigma_0 = 1,  sigma_1 = 1){
  
  y <- rbinom(n, 1, p)
  f_0 <- rnorm(n, mu_0, sigma_0)
  f_1 <- rnorm(n, mu_1, sigma_1)
  x <- ifelse(y == 1, f_1, f_0)
  
  test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
  
  list(train = data.frame(x = x, y = as.factor(y)) %>% slice(-test_index),
       test = data.frame(x = x, y = as.factor(y)) %>% slice(test_index))
}

set.seed(1,sample.kind="Rounding")

for(i in n)
{
  set.seed(1)
  dat<-make_data(mu_1=m_1[i])  
  glm_fit <- dat$train %>% glm(y ~ x, data=.,family = "binomial")
  p_hat_logit<-predict(glm_fit, newdata = dat$test, type = "response")
  y_hat_logit<-ifelse(p_hat_logit > 0.5,1,0) %>% factor
  result<-confusionMatrix(y_hat_logit, reference = dat$test$y)
  accuracy[i]<-result$byClass['Balanced Accuracy']
  
}

plot(m_1,accuracy)
