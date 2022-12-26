
cancer <- read.csv("/Users/lukegeel/Desktop/School/Spring 2021/Stats 525/cancer project/data/cancer csv.csv")

Y <- cancer$Mean_Area
X1 <- cancer$Mean_mean_R
X2 <- cancer$Mean_mean_G
X3 <- cancer$Mean_mean_B
X4 <- cancer$Mean_mean_HSV
X5 <- cancer$Mean_mean_Lab
X6 <- cancer$Mean_mean_HE
X7 <- cancer$Mean_mean_BR

lm.1 <- lm(Y~X1)
lm.2 <- lm(Y~X2)
lm.3 <- lm(Y~X3)
lm.4 <- lm(Y~X4)
lm.5 <- lm(Y~X5)
lm.6 <- lm(Y~X6)
lm.7 <- lm(Y~X7)

plot(X1, Y, pch = 16, col='red',
     xlab = "Mean Red Intensity", ylab = "Mean Area", cex.lab = 1.5,
     cex.axis = 1.5)
abline(a=lm.1$coef[1], b=lm.1$coef[2], lty=3)

plot(X2, Y, pch = 16, col='green',
     xlab = "Mean Green Intensity", ylab = "Mean Area", cex.lab = 1.5,
     cex.axis = 1.5)
abline(a=lm.2$coef[1], b=lm.2$coef[2], lty=3)

plot(X3, Y, pch = 16, col='blue',
     xlab = "Mean Blue Intensity", ylab = "Mean Area", cex.lab = 1.5,
     cex.axis = 1.5)
abline(a=lm.3$coef[1], b=lm.3$coef[2], lty=3)

