cancer <- read.csv("/Users/lukegeel/Desktop/School/Spring 2021/Stats 525/cancer project/data/cancer csv.csv")
Y = cancer$Mean_Area
R = cancer$Mean_mean_R
G = cancer$Mean_mean_G
B = cancer$Mean_mean_B
HSV = cancer$Mean_mean_HSV
Lab = cancer$Mean_mean_Lab
HE = cancer$Mean_mean_HE
BR = cancer$Mean_mean_BR







linmod = lm(Y~R+G+B+HSV+Lab+HE+BR)
e = linmod$residuals
t = c(1:167)
plot(t, e, pch = 16, 
     xlab = "Time",
     ylab = "Residuals",
     cex.lab = 1.5, cex.axis = 1.5)




#STEP 3

n = nrow(cancer)
alpha = 0.05

plot(Y, R)
linmod.R <- lm(Y~R, data = cancer)
  R.b0 <- linmod.R$coeff[1]
  R.b1 <- linmod.R$coeff[2]
  R.s.b1 <- summary(linmod.R)$coef[2, 2]
  R.b1.lower <- R.b1 + R.s.b1 * qt(alpha/2, n - 2)
  R.b1.upper <- R.b1 - R.s.b1 * qt(alpha/2, n - 2)

plot(Y, G)
linmod.G <- lm(Y~G, data = cancer)
  G.b0 <- linmod.G$coeff[1]
  G.b1 <- linmod.G$coeff[2]
  G.s.b1 <- summary(linmod.G)$coef[2, 2]
  G.b1.lower <- G.b1 + G.s.b1 * qt(alpha/2, n - 2)
  G.b1.upper <- G.b1 - G.s.b1 * qt(alpha/2, n - 2)

plot(Y, B)
linmod.B <- lm(Y~B, data = cancer)
  B.b0 <- linmod.B$coeff[1]
  B.b1 <- linmod.B$coeff[2]
  B.s.b1 <- summary(linmod.B)$coef[2, 2]
  B.b1.lower <- B.b1 + B.s.b1 * qt(alpha/2, n - 2)
  B.b1.upper <- B.b1 - B.s.b1 * qt(alpha/2, n - 2)

plot(Y, HSV)
  linmod.HSV <- lm(Y~HSV, data = cancer)
  HSV.b0 <- linmod.HSV$coeff[1]
  HSV.b1 <- linmod.HSV$coeff[2]
  HSV.s.b1 <- summary(linmod.HSV)$coef[2, 2]
  HSV.b1.lower <- HSV.b1 + HSV.s.b1 * qt(alpha/2, n - 2)
  HSV.b1.upper <- HSV.b1 - HSV.s.b1 * qt(alpha/2, n - 2)
  
plot(Y, Lab)
  linmod.Lab <- lm(Y~Lab, data = cancer)
  Lab.b0 <- linmod.Lab$coeff[1]
  Lab.b1 <- linmod.Lab$coeff[2]
  Lab.s.b1 <- summary(linmod.Lab)$coef[2, 2]
  Lab.b1.lower <- Lab.b1 + Lab.s.b1 * qt(alpha/2, n - 2)
  Lab.b1.upper <- Lab.b1 - Lab.s.b1 * qt(alpha/2, n - 2)
  
plot(Y, HE)
  linmod.HE <- lm(Y~HE, data = cancer)
  HE.b0 <- linmod.HE$coeff[1]
  HE.b1 <- linmod.HE$coeff[2]
  HE.s.b1 <- summary(linmod.HSV)$coef[2, 2]
  HE.b1.lower <- HE.b1 + HE.s.b1 * qt(alpha/2, n - 2)
  HE.b1.upper <- HE.b1 - HE.s.b1 * qt(alpha/2, n - 2)
  
plot(Y, BR)
  linmod.BR <- lm(Y~BR, data = cancer)
  BR.b0 <- linmod.BR$coeff[1]
  BR.b1 <- linmod.BR$coeff[2]
  BR.s.b1 <- summary(linmod.BR)$coef[2, 2]
  BR.b1.lower <- BR.b1 + BR.s.b1 * qt(alpha/2, n - 2)
  BR.b1.upper <- BR.b1 - BR.s.b1 * qt(alpha/2, n - 2)
  
  
  
  
  
  
  



