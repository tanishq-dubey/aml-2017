graphics.off()

setwd("~/Projects/classes/aml/homework_four")
dat <- read.table("brunhild.txt", header = TRUE, sep = "\t")
par(mfrow=c(2,2))
lm_reg <- lm(dat$Sulfate~dat$Hours)

plot(dat, log = "xy", main = "Sulface VS Hours in Log-Log Coordinates")
abline(lm(log10(dat$Sulfate)~log10(dat$Hours)))


plot(dat, main = "Sulface VS Hours in Standard Coordinates")
abline(lm(dat$Sulfate~dat$Hours))

# Show log-log residual plot

plot(lm_reg$fitted.values ,lm_reg$residuals, xlab = "Fitted Values", ylab = "Residuals", main = "Residuals VS Fitted Values in Standard Coordinates")
plot(lm_reg$fitted.values ,lm_reg$residuals, log = "xy", xlab = "Fitted Values", ylab = "Residuals", main = "Residuals VS Fitted Values in Log-Log Coordinates")