
cancer <- read.csv("/Users/lukegeel/Desktop/School/Spring 2021/Stats 525/cancer project/data/cancer csv.csv")

Y <- cancer$Mean_Area
X1 <- cancer$Mean_mean_R
X2 <- cancer$Mean_mean_G
X3 <- cancer$Mean_mean_B
X4 <- cancer$Mean_mean_HSV
X5 <- cancer$Mean_mean_Lab
X6 <- cancer$Mean_mean_HE
X7 <- cancer$Mean_mean_BR

lm.canc <- lm(Y ~ X1 + X2 + X3 + X4 + X5 + X6 + X7)

#extract b-values
b0 <- summary(lm.canc)$coef[1,1]
b1 <- summary(lm.canc)$coef[2,1]
b2 <- summary(lm.canc)$coef[3,1]
b3 <- summary(lm.canc)$coef[4,1]
b4 <- summary(lm.canc)$coef[5,1]
b5 <- summary(lm.canc)$coef[6,1]
b6 <- summary(lm.canc)$coef[7,1]
b7 <- summary(lm.canc)$coef[8,1]

#extract residuals
e <- lm.canc$residuals
sum(abs(e))
sd(abs(e))
#extract fitted values
Y.hat <- lm.canc$fitted.values
#histogram of residuals
hist(e, xlab = "Residuals", ylab = "Frequency", cex.lab = 1.5,
     cex.axis = 1.5)

#plot of residuals vs fitted values
plot(Y.hat, e, pch = 16,
     xlab = "Fitted Values", ylab = "Residuals", cex.lab = 1.5,
     cex.axis = 1.5)
abline(h = 0, lty = 3)

#plot of residuals vs mean red intensity
plot(X1, e, pch = 16, col = 'red',
     xlab = "Mean Red Intensity", ylab = "Residuals", cex.lab = 1.5,
     cex.axis = 1.5)
abline(h = 0, lty = 3)

#plot of residuals vs mean green intensity
plot(X2, e, pch = 16, col = 'green',
     xlab = "Mean Green Intensity", ylab = "Residuals", cex.lab = 1.5,
     cex.axis = 1.5)
abline(h = 0, lty = 3)

#plot of residuals vs mean blue intensity
plot(X3, e, pch = 16, col = 'blue',
     xlab = "Mean Blue Intensity", ylab = "Residuals", cex.lab = 1.5,
     cex.axis = 1.5)
abline(h = 0, lty = 3)

#plot of residuals vs mean hsv intensity
plot(X4, e, pch = 16,
     xlab = "Mean HSV Intensity", ylab = "Residuals", cex.lab = 1.5,
     cex.axis = 1.5)
abline(h = 0, lty = 3)

##plot of residuals vs mean lab intensity
plot(X5, e, pch = 16,
     xlab = "Mean Lab Intensity", ylab = "Residuals", cex.lab = 1.5,
     cex.axis = 1.5)
abline(h = 0, lty = 3)

#plot of residuals vs mean he intensity
plot(X6, e, pch = 16,
     xlab = "Mean HE Intensity", ylab = "Residuals", cex.lab = 1.5,
     cex.axis = 1.5)
abline(h = 0, lty = 3)

#plot of residuals vs mean br intensity
plot(X7, e, pch = 16,
     xlab = "Mean BR Intensity", ylab = "Residuals", cex.lab = 1.5,
     cex.axis = 1.5)
abline(h = 0, lty = 3)
















