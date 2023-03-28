## -----------------------------------------------------------------------------
library(SparseFunClust)
set.seed(24032023)
n <- 50
x <- seq(0,1,len=500)
out <- generate.data.FV17(n, x)
data <- out$data
trueClust <- out$true.partition
matplot(x, t(data), type='l', col=trueClust,
        xlab = 'x', ylab = 'data', main = 'Simulated data')

## -----------------------------------------------------------------------------
K <- 2            # run with 2 groups only
method <- 'kmea'  # version with K-means clustering
tuning.m <- FALSE # don't perform tuning of the sparsity parameter (faster)
result <- SparseFunClust(data, x, K = K, do.alignment = FALSE,
                         clust.method = method, tuning.m = tuning.m)

## -----------------------------------------------------------------------------
table(trueClust,result$labels)
cer(trueClust,result$labels)

## -----------------------------------------------------------------------------
matplot(x,t(data),type='l',lty=1,col=result$labels+1,ylab='',
        main='clustering results')
lines(x,colMeans(data[which(result$labels==1),]),lwd=2)
lines(x,colMeans(data[which(result$labels==2),]),lwd=2)
plot(x,result$w,type='l',lty=1,lwd=2,ylab='',
     main='estimated weighting function')
abline(v=0.5)

