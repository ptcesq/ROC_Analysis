# Sigmoid Transformation Example 

x <- seq(-10, 10, by=0.5)
b = 0 # intercept
m = 1 # slope
y <- exp((b + m*x)) / (1 + exp((b + m*x)))
plot(x, y, xlab="X", ylab="P(Y=1)", pch=20, col="blue")
title(main="Sigmoid (Logistic) Function")
xy.glm <- glm(y~x, )
lines(x, xy.glm$fitted, col="black")

loess <- loess(y~x)
hat <- predict(loess)
#plot(y~x)
lines(x[order(x)], hat[order(hat)], col="red")


