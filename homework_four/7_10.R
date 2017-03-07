graphics.off()
setwd("~/Projects/classes/aml/homework_four")
dat <- read.table("physical.txt", header = TRUE, sep = "\t")

lm_reg <- glm(dat)
plot(lm_reg$fitted.values ,lm_reg$residuals, xlab = "Fitted Values", ylab = "Residuals", main = "Residuals VS Fitted Values in Standard Coordinates")
abline(glm(lm_reg$residuals~lm_reg$fitted.values), col=c("red"))
dev.copy(png,"710_A.png",width=10,height=6,units="in",res=150)
dev.off()
graphics.off()

par(mfrow=c(1,2))
dat$Mass <- dat$Mass^(1/3)
lm_reg <- glm(dat)
plot(lm_reg$fitted.values ,lm_reg$residuals, xlab = "Fitted Values", ylab = "Residuals", main = "Residuals VS Fitted Values in Cube Root Coordinates")
abline(glm(lm_reg$residuals~lm_reg$fitted.values), col=c("red"))
# Build new predictor for regular coordinate plotting
p_d <- lm_reg$coefficients[['(Intercept)']] + 
        ((lm_reg$coefficients[['Fore']] * dat$Fore) +
         (lm_reg$coefficients[['Bicep']] * dat$Bicep) +
         (lm_reg$coefficients[['Chest']] * dat$Chest) +
         (lm_reg$coefficients[['Neck']] * dat$Neck) +
         (lm_reg$coefficients[['Shoulder']] * dat$Shoulder) +
         (lm_reg$coefficients[['Waist']] * dat$Waist) +
         (lm_reg$coefficients[['Height']] * dat$Height) +
         (lm_reg$coefficients[['Calf']] * dat$Calf) +
         (lm_reg$coefficients[['Thigh']] * dat$Thigh) +
         (lm_reg$coefficients[['Head']] * dat$Head))
dat$Mass <- dat$Mass^3

plot((lm_reg$fitted.values)^3 ,dat$Mass - p_d^3, xlab = "Fitted Values", ylab = "Residuals", main = "Residuals VS Fitted Values in Standard Coordinates")
abline(glm(lm_reg$residuals~(dat$Mass - p_d)), col=c("red"))
dev.copy(png,"710_B.png",width=12,height=6,units="in",res=150)
dev.off()
