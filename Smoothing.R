library(tidyverse)
library(caret)
library(dslabs)
total_days <- diff(range(polls_2008$day))
span <- 21/total_days
fit <- with(polls_2008,ksmooth(day,margin,x.points=day,kernel="box",bandwidth=7))

fit<- loess(margin ~ day ,degree=1,span=span,data=polls_2008)
polls_2008 %>%  mutate(smooth=fit$fitted) %>% ggplot(aes(day,margin))+geom_point(size=3,alpha=0.5,color="grey")+geom_line(aes(day,smooth),color="red")+geom_smooth()
