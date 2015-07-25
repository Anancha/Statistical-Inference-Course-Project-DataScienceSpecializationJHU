set.seed(1)
lambda <- 0.2
n  <- 40


mns = NULL
vrs= NULL
for (i in 1 : 1000) {
        mySample <- rexp(n,lambda)
        mns <- c(mns, mean(mySample))
        vrs <- c(vrs, var(mySample))
}

myMean <- mean(mns)
theoMean <- 1/lambda
myVar<- mean(vrs)
theoVar <- (1/lambda)^2

hist(mns, breaks = 20, xlab = 'Mean', main = 'Histogram of Means')
abline(v = theoMean, col = 3, lwd = 3)
abline(v = myMean, col = 2, lwd = 3, lty = 2)
legend(6, 100, c('Theoretical Mean', 'Sample Mean'), lwd = c(3,3), lty = c(1,2), col = c(3,2))
curve(dnorm(x, mean=myMean, sd=sd(mns))*200, col="darkblue", lwd=2, add=TRUE)

hist(vrs, breaks = 14, xlab = 'Variance', main = 'Histogram of Variance')
abline(v = theoVar, col = 3, lwd = 3)
abline(v = myVar, col = 2, lwd = 3, lty = 2)
legend(60, 200, c('Theoretical Variance', 'Sample Variance'), lwd = c(3,3), lty = c(1,2), col = c(3,2))
curve(dnorm(x, mean=myVar, sd=sd(vrs))*6000, col="darkblue", lwd=2, add=TRUE)
