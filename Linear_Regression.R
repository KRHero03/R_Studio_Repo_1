
'n <- c(100,500,1000,5000,10000)

pro <- function(x){
  Sigma <- 9*matrix(c(1.0, 0.95, 0.95, 1.0), 2, 2)
  dat <- MASS::mvrnorm(n = x, c(69, 69), Sigma) %>%
    data.frame() %>% setNames(c("x", "y"))
  
  B <- 100
  rmse <- replicate(B,{
    test_index <- createDataPartition(dat$y,times=1,p=0.5,list=FALSE)
    train_set <- dat %>% slice(-test_index)
    test_set <- dat %>% slice(test_index)
    fit <- lm(y~x,data=train_set)
    y_hat <- predict(fit,test_set)
    sqrt(mean((y_hat-test_set$y)^2))
  })
  mean(rmse)
}


set.seed(1,sample.kind = "Rounding")
Sigma <- 9*matrix(c(1.0, 0.95, 0.95, 1.0), 2, 2)
dat <- MASS::mvrnorm(n = 100, c(69, 69), Sigma) %>%
  data.frame() %>% setNames(c("x", "y"))

B <- 100
rmse <- replicate(B,{
  test_index <- createDataPartition(dat$y,times=1,p=0.5,list=FALSE)
  train_set <- dat %>% slice(-test_index)
  test_set <- dat %>% slice(test_index)
  fit <- lm(y~x,data=train_set)
  y_hat <- predict(fit,test_set)
  sqrt(mean((y_hat-test_set$y)^2))
})
mean(rmse)
sd(rmse)'


set.seed(2,sample.kind = "Rounding")
Sigma <- matrix(c(1.0, 0.75, 0.75, 0.75, 1.0, 0.25, 0.75, 0.25, 1.0), 3, 3)
dat <- MASS::mvrnorm(n = 100, c(0, 0, 0), Sigma) %>%
  data.frame() %>% setNames(c("y", "x_1", "x_2"))
cor(dat)

test_index <- createDataPartition(dat$y,times=1,p=0.5,list=FALSE)
test_set <- dat %>% slice(test_index)
train_set <- dat %>% slice(-test_index)

fit_x1<-lm(y~x_1,data=train_set)
y_hat_x1 <-predict(fit_x1,test_set)
rmse_x1 <-sqrt(mean((y_hat_x1-test_set$y)^2))

fit_x2<-lm(y~x_2,data=train_set)
y_hat_x2 <-predict(fit_x2,test_set)
rmse_x2 <-sqrt(mean((y_hat_x2-test_set$y)^2))

fit_x12<-lm(y~x_1+x_2,data=train_set)
y_hat_x12 <-predict(fit_x12,test_set)
rmse_x12 <-sqrt(mean((y_hat_tp-test_set$y)^2))
rmse_x1
rmse_x2
rmse_x12
