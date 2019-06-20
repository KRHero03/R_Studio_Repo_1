library(devtools)
library(genefilter)
library(tidyverse)
library(caret)
set.seed(1996,sample.kind="Rounding")
n <- 1000
p <- 10000
x <- matrix(rnorm(n*p), n, p)
colnames(x) <- paste("x", 1:ncol(x), sep = "_")
y <- rbinom(n, 1, 0.5) %>% factor()
x_subset <- x[ ,sample(p, 100)]
fit <- train(x_subset, y, method = "glm")

tt <- colttests(x, y)
ind <- rownames(tt[tt$p.value<=0.01,])
x_subset <- x[,ind]
fitX <- train(tissue_gene_expression$x, tissue_gene_expression$y, method = "knn", tuneGrid = data.frame(k = seq(1,7,2)))

