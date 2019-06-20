'set.seed(1,sample.kind="Rounding")
B <- 10000
quant <- replicate(B,{
  y <- rnorm(100, 0, 1)
  quantile(y, 0.75)
})
mean(quant)
sd(quant)'

set.seed(1, sample.kind="Rounding")
y <- rnorm(100,0,1)
set.seed(1,sample.kind="Rounding")
index <- createResample(y,10)
output <- matrix(ncol=1, nrow=10)
count <- 1
for(i in index){
  tp <- y[i]
  output[count,] <- quantile(tp, 0.75)
  count <- count +1 
}
q_star <- data.frame(output)
mean(q_star$output)
sd(q_star$output)

