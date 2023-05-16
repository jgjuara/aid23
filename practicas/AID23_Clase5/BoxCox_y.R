library(MASS)
y = rf(500,10,30) #500 números random de distribución F

result = boxcox(y~1, lambda = seq(-5,5,0.5))

valor_lambda = result$x[which.max(result$y)]
valor_lambda

nuevo_y = (y^valor_lambda-1)/valor_lambda
hist(y,breaks = 15,xlim = c(-2,4), col='blue')
hist(nuevo_y, breaks=12, add=T, col='red')
legend("topright", c("y", "y_transf"), col=c("blue", "red"), lwd=2)
