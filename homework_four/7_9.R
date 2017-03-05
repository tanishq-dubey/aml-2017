graphics.off()

setwd("~/Projects/classes/aml/homework_four")
dat <- read.table("brunhild.txt", header = TRUE, sep = "\t")
par(mfrow=c(1,2))
dat <- log(dat)

lm_reg <- glm(dat$Sulfate~dat$Hours)

plot(dat, main = "Sulface VS Hours in Log-Log Coordinates")

plot(dat, main = "Sulface VS Hours in Log-Log Coordinates")
abline(glm(dat$Sulfate~dat$Hours), col=c("red"))

invisible(readline(prompt="Press [enter] to continue"))

plot(exp(dat), main = "Sulface VS Hours in Standard Coordinates")

plot(exp(dat), main = "Sulface VS Hours in Standard Coordinates")
lines(exp(dat$Hours), exp(predict(lm_reg, data.frame(x = dat$Hours))), col = c("red"))

invisible(readline(prompt="Press [enter] to continue"))

# Show log-log residual plot

plot(lm_reg$fitted.values ,lm_reg$residuals, xlab = "Fitted Values", ylab = "Residuals", main = "Residuals VS Fitted Values in Log-Log Coordinates")
abline(glm(lm_reg$residuals~lm_reg$fitted.values), col=c("red"))

plot(exp(lm_reg$fitted.values) ,exp(lm_reg$residuals), xlab = "Fitted Values", ylab = "Residuals", main = "Residuals VS Fitted Values in Standard Coordinates")
