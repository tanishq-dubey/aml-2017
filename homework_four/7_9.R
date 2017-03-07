graphics.off()
setwd("~/Projects/classes/aml/homework_four")

dat <- read.table("brunhild.txt", header = TRUE, sep = "\t")
dat <- log(dat)
lm_reg <- glm(dat$Sulfate~dat$Hours)

par(mfrow=c(1,2))
plot(dat, main = "Sulfate VS Hours in Log-Log Coordinates")
plot(dat, main = "Sulfate VS Hours in Log-Log Coordinates")
abline(glm(dat$Sulfate~dat$Hours), col=c("red"))
dev.copy(png,"79_A.png",width=10,height=6,units="in",res=150)
dev.off()
graphics.off()

par(mfrow=c(1,2))
plot(exp(dat), main = "Sulfate VS Hours in Standard Coordinates")
plot(exp(dat), main = "Sulfate VS Hours in Standard Coordinates")
lines(exp(dat$Hours), exp(predict(lm_reg, data.frame(x = dat$Hours))), col = c("red"))
dev.copy(png,"79_B.png",width=10,height=6,units="in",res=150)
dev.off()
graphics.off()

par(mfrow=c(1,2))
plot(lm_reg$fitted.values ,lm_reg$residuals, xlab = "Fitted Values", ylab = "Residuals", main = "Residuals VS Fitted Values in Log-Log Coordinates")
abline(glm(lm_reg$residuals~lm_reg$fitted.values), col=c("red"))

p_d <- lm_reg$coefficients[['(Intercept)']] + (lm_reg$coefficients[['dat$Hours']] * dat$Hours)

plot(exp(lm_reg$fitted.values) ,exp(dat$Sulfate) - exp(p_d), xlab = "Fitted Values", ylab = "Residuals", main = "Residuals VS Fitted Values in Standard Coordinates")
abline(glm((exp(dat$Sulfate) - exp(p_d))~exp(lm_reg$fitted.values)), col=c("red"))
dev.copy(png,"79_C.png",width=12,height=6,units="in",res=150)
dev.off()

