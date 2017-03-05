graphics.off()

gep3Rt <- function(x) {
    sign(x) * abs(x)^(1/3)
}

setwd("~/Projects/classes/aml/homework_four")
dat <- read.table("physical.txt", header = TRUE, sep = "\t")

lm_reg <- glm(dat)

plot(lm_reg$fitted.values ,lm_reg$residuals, xlab = "Fitted Values", ylab = "Residuals", main = "Residuals VS Fitted Values in Standard Coordinates")
abline(glm(lm_reg$residuals~lm_reg$fitted.values), col=c("red"))

invisible(readline(prompt="Press [enter] to continue"))

par(mfrow=c(1,2))

dat$Mass <- gep3Rt(dat$Mass)
lm_reg <- glm(dat)

plot(lm_reg$fitted.values ,lm_reg$residuals, xlab = "Fitted Values", ylab = "Residuals", main = "Residuals VS Fitted Values in Standard Coordinates")
abline(glm(lm_reg$residuals~lm_reg$fitted.values), col=c("red"))