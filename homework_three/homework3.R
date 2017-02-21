setwd("~/Projects/classes/aml/homework_three")

labels <- read.table("batches.meta.txt")
images.rgb <- list()
images.label <- list()
num.images = 10000 # Set to 10000 to retrieve all images per file to memory

for (f in 1:5) {
  to.read <- file(paste("data_batch_", f, ".bin", sep=""), "rb")
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
for(f in 1:labeln) {
  classindex <- f == images.label
  classarray <- images.rgb[classindex]
  ca <- data.frame(matrix(unlist(classarray), nrow=5000, byrow=T),stringsAsFactors=FALSE)
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
  library(grid)
  grid.raster(img.visual, interpolate=FALSE)
  # savePlot("average_",labels[f,],".png")
  #savePlot(paste("average_",labels[f,],".png", png, sep = "", dev.cur()))
}

# scaling
for(f in 1:labeln) {
  
}
