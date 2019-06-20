library(dslabs)
library(caret)
data("tissue_gene_expression")

set.seed(1993, sample.kind = "Rounding")
ind <- which(tissue_gene_expression$y %in% c("cerebellum", "hippocampus"))
'y <- droplevels(tissue_gene_expression$y[ind])
x <- tissue_gene_expression$x[ind, ]
x <- x[, sample(ncol(x), 10)]'
y <- tissue_gene_expression$y
x <- tissue_gene_expression$x
x <- x[, sample(ncol(x), 10)]

train_lda <- train(x, y, method="lda",preProcess = "center") 
train_lda$finalModel
show(train_lda)
t(train_lda$finalModel$means) %>% data.frame() %>%
  mutate(predictor_name = rownames(.)) %>%
  ggplot(aes(cerebellum, hippocampus, label = predictor_name)) +
  geom_point() +
  geom_text() +
  geom_abline()

'train_qda <- train(x,y,method="qda")
show(train_qda)
t(train_qda$finalModel$means) %>% data.frame() %>%
  mutate(predictor_name = rownames(.)) %>%
  ggplot(aes(cerebellum, hippocampus, label = predictor_name)) +
  geom_point() +
  geom_text() +
  geom_abline()'
