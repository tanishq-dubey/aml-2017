# CIFAR-10 is a 32 x 32 images in 10 categories
# Downloaded from https://www.cs.toronto.edu/~kriz/cifar.html
# 
# (a) For each category, compute the mean image and the first 20 principle components
# 
library(grid)
library(abind)
library(scales)
library(labdsv)

setwd("~/AML/HW3/cifar-10-batches-bin")
labels <- read.table("~/AML/HW3/cifar-10-batches-bin/batches.meta.txt")
images.rgb <- list()
images.label <- list()
num.images = 10000 # Set to 10000 to retrieve all images per file to memory

for (f in 1:5) {
  to.read <-file(paste("~/AML/HW3/cifar-10-batches-bin/data_batch_", f, ".bin", sep=""), "rb")
  for(i in 1:num.images) {
    l <- readBin(to.read, integer(), size=1, n=1, endian="big")
    r <- as.integer(readBin(to.read, raw(), size=1, n=1024, endian="big"))
    g <- as.integer(readBin(to.read, raw(), size=1, n=1024, endian="big"))
    b <- as.integer(readBin(to.read, raw(), size=1, n=1024, endian="big"))
    index <- num.images * (f-1) + i
    images.rgb[[index]] = data.frame(r, g, b)
    images.label[[index]] = l+1
  }
  close(to.read)
  remove(l,r,g,b,f,i,index, to.read)
}

labeln <- dim(labels)[1]
average.rgb <- list()
collection <- array(0, c(5000,3072,labeln))
for(f in 1:labeln) {
  classindex <- f == images.label
  classarray <- images.rgb[classindex]
  ca <- matrix(unlist(classarray), nrow=5000, byrow=T)
  collection[,,f] <- ca
  average.rgb[[f]] = colMeans(ca)
}

class_average <- data.frame(average.rgb[[1]], average.rgb[[2]], average.rgb[[3]], average.rgb[[4]], average.rgb[[5]], average.rgb[[6]], average.rgb[[7]], average.rgb[[8]], average.rgb[[9]], average.rgb[[10]])

for(f in 1:labeln) {
  img <- average.rgb[[f]]
  img.r <- matrix(img[1:1024], ncol=32, byrow = TRUE) 
  img.g <- matrix(img[1025:2048], ncol=32, byrow = TRUE) 
  img.b <- matrix(img[2049:3072], ncol=32, byrow = TRUE) 
  
  img.visual <- rgb(img.r, img.g, img.b, maxColorValue=255)
  dim(img.visual) <- dim(img.r)
  # Plot and output label
  # savePlot("average_",labels[f,],".png")
  #savePlot(paste("average_",labels[f,],".png", png, sep = "", dev.cur()))
}
drawImage <- function(rmat, gmat, bmat) {
  img.col.mat <- rgb(rmat, gmat, bmat, maxColorValue = 255)
  dim(img.col.mat) <- dim(rmat)
  
  # Plot and output label
  grid.raster(img.col.mat, interpolate=FALSE)
  
  # clean up
  remove(img.col.mat)
}




D <-array(0,dim=c(10,10))
for(i in 1:labeln){
  for(j in 1:labeln){
    D[i,j] = dist(rbind(class_average[,i],class_average[,j]))
  }
}

pcotest <- pco(D,k=2)
pcoplot <- data.frame(labels,pcotest$points)
plot(pcoplot$X1,pcoplot$X2,xlim=c(-1000,1600), main="pcoplot",xlab = "x", ylab = "y")
text(pcoplot$X1, pcoplot$X2, pcoplot$V1, cex=0.5, pos=4, col="red")
