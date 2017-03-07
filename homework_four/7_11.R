setwd("~/AML/HW4")

library(plyr)
library(glmnet)

#get data
dat <- read.csv("abalone.data", header = F)
dat2 = dat

#part a

#get linear regression
fit <-lm(V9 ~ V2+ V3+ V4+ V5+ V6+ V7+ V8, data = dat)

plot(fit)

#part b
#assign numbers to gender
temp <-mapvalues(dat$V1, from = c("M", "F", "I"), to = c(1,-1,0))
dat$V1 = temp

#get linear regression
fit <-lm(V9 ~ V1 + V2+ V3+ V4+ V5+ V6+ V7+ V8, data = dat)

plot(fit)

#part c

#get log of ages
dat$v9 = log(dat$V9)

#get linear regression
fit <-lm(V9 ~ V2+ V3+ V4+ V5+ V6+ V7+ V8, data = dat)

plot(fit)

#part d
#get linear regression
fit <-lm(V9 ~ V1 + V2+ V3+ V4+ V5+ V6+ V7+ V8, data = dat)

plot(fit)

#part f using glmnet to plot error
#fa)
x <- as.matrix(dat2[-c(1,9)])
ft<-cv.glmnet(x,dat2$V9)
plot(ft)

#fb)
dat2$V1 <- as.numeric(dat2$V1)
x <- as.matrix(dat2[-c(9)])
ft<-cv.glmnet(x,dat2$V9)
plot(ft)

#fc)
dat2$v9 = log(dat2$V9)
x <- as.matrix(dat2[-c(1,9)])
ft<-cv.glmnet(x,dat2$V9)
plot(ft)

#fd)
dat2$V1 <- as.numeric(dat2$V1)
x <- as.matrix(dat2[-c(9)])
ft<-cv.glmnet(x,dat2$V9)
plot(ft)



