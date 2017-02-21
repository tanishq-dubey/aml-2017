library(grid)
library(scales)
if(!is.element("abind", installed.packages()[,1])) {
    install.packages("abind", dependencies = TRUE)
}
library(abind)
setwd("~/Projects/classes/aml/homework_three")

drawImage <- function(rmat, gmat, bmat) {
    img.col.mat <- rgb(rmat, gmat, bmat, maxColorValue = 255)
    dim(img.col.mat) <- dim(rmat)
    
    # Plot and output label
    grid.raster(img.col.mat, interpolate=FALSE)
    
    # clean up
    remove(img.col.mat)
}

rot <- function(x) t(apply(x, 2, rev))

renderImg <- function(k,p) {
    graphics.off()
    imgs.reconst[,,1,k] <- matrix(data=rev(rest.r[k,]), nrow=32, ncol=32)
    imgs.orig.r <- rot(rot(matrix(dta.subset[k, 1:1024], nrow = 32)))
    imgs.reconst[,,2,k] <- matrix(data=rev(rest.g[k,]), nrow=32, ncol=32)
    imgs.orig.g <- rot(rot(matrix(dta.subset[k, 1025:2048], nrow = 32)))
    imgs.reconst[,,3,k] <- matrix(data=rev(rest.b[k,]), nrow=32, ncol=32)
    imgs.orig.b <- rot(rot(matrix(dta.subset[k, 2049:3072], nrow = 32)))
    return(dist(rbind(matrix(imgs.reconst[,,1,k], ncol = 1024), matrix(imgs.orig.r, ncol = 1024))) +
               dist(rbind(matrix(imgs.reconst[,,2,k], ncol = 1024), matrix(imgs.orig.g, ncol = 1024))) +
               dist(rbind(matrix(imgs.reconst[,,3,k], ncol = 1024), matrix(imgs.orig.b, ncol = 1024))))
}

rotate <- function(x) t(apply(x, 2, rev))

labels <- read.table("batches.meta.txt")

# Load the data
if(!exists("dta.orig")) {
    # Create an array of each catagory
    dta.orig  <- array(dim = c(50000, 3072))
    dta.label <- array(dim = c(50000))
    
    num.images = 10000 # Set to 10000 to retrieve all images per file to memory
    img.num = 1
    
    # Cycle through all 5 binary files
    for (f in 1:5) {
        to.read <- file(paste("data_batch_", f, ".bin", sep=""), "rb")
        for(i in 1:num.images) {
            l <- readBin(to.read, integer(), size=1, n=1, endian="big")
            l <- l + 1
            r <- as.integer(readBin(to.read, raw(), size=1, n=1024, endian="big"))
            g <- as.integer(readBin(to.read, raw(), size=1, n=1024, endian="big"))
            b <- as.integer(readBin(to.read, raw(), size=1, n=1024, endian="big"))
            index <- num.images * (f-1) + i
            dta.label[index] <- l
            dta.orig[index, 1:1024] <- as.integer(r)
            dta.orig[index, 1025:2048] <- as.integer(g)
            dta.orig[index, 2049:3072] <- as.integer(b)
        }
        close(to.read)
    }
    remove(l,r,g,b,f,i,to.read, num.images, index)
}

graphics.off()

err <- array(dim = c(10, 5000))
imgs.reconst <- array(dim = c(32, 32, 3, 5000))
dta.subset <- array(dim = c(5000, 3072))


imgs.average <- array(dim = c(10, 3072))

imgs.covar <- array(dim = c(10, 10, 5000))

cat("Data loaded...\n")

for(p in 1:10) {
    i <- 1
    j <- 1
    while(i <= 5000) {
        if(dta.label[j] == p) {
            dta.subset[i, ] <- dta.orig[j, ]
            i <- i + 1
        }
        j <- j + 1
    }
    
    cat("Computing Average Images...\n")
    imgs.average[p, ] <- colMeans(dta.subset)
}

for(p in 1:10) {
    cat(sprintf("Doing run %d\n", p))
    i <- 1
    j <- 1
    while(i <= 5000) {
        if(dta.label[j] == p) {
            dta.subset[i, ] <- dta.orig[j, ]
            i <- i + 1
        }
        j <- j + 1
    }
    
    cat("Calculating principal components...\n")
    pca.r <- prcomp(dta.subset[ , 1:1024])
    cat("R Channel Done...\n")
    pca.g <- prcomp(dta.subset[ , 1025:2048])
    cat("G Channel Done...\n")
    pca.b <- prcomp(dta.subset[ , 2049:3072])
    cat("B Channel Done...\n")
    
    cat("Calculating structures...\n")
    rest.r <- pca.r$x[,1:20] %*% t(pca.r$rotation[,1:20])
    rest.g <- pca.g$x[,1:20] %*% t(pca.g$rotation[,1:20])
    rest.b <- pca.b$x[,1:20] %*% t(pca.b$rotation[,1:20])
    
    cat("Scaling...\n")
    rest.r <- rescale(rest.r, to=c(0,255), from = range(rest.r))
    rest.g <- rescale(rest.g, to=c(0,255), from = range(rest.g))
    rest.b <- rescale(rest.b, to=c(0,255), from = range(rest.b))
    
    for (q in 1:10) {
        cat("Calculating Errors...\n")
        imgs.temp <- array(dim = c(32,32,3))
        imgs.temp[,,1] <- matrix(imgs.average[q, 1:1024], nrow = 32, ncol = 32)
        imgs.temp[,,2] <- matrix(imgs.average[q, 1025:2048], nrow = 32, ncol = 32)
        imgs.temp[,,3] <- matrix(imgs.average[q, 2049:3072], nrow = 32, ncol = 32)
        for(k in 1:5000) {
            imgs.reconst[,,1,k] <- matrix(data=rev(rest.r[k,]), nrow=32, ncol=32)
            imgs.reconst[,,2,k] <- matrix(data=rev(rest.g[k,]), nrow=32, ncol=32)
            imgs.reconst[,,3,k] <- matrix(data=rev(rest.b[k,]), nrow=32, ncol=32)
            imgs.covar[p,q,k] <- dist(rbind(matrix(imgs.temp[,,1], ncol = 1024), matrix(imgs.reconst[,,1,k], ncol = 1024))) +
                dist(rbind(matrix(imgs.temp[,,2], ncol = 1024), matrix(imgs.reconst[,,2,k], ncol = 1024))) +
                dist(rbind(matrix(imgs.temp[,,3], ncol = 1024), matrix(imgs.reconst[,,3,k], ncol = 1024)))
        }
    }
}
