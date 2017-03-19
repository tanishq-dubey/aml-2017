setwd("~/Projects/classes/aml/homework_five")
library(MASS) # Needed for Box-Cox
library(glmnet)

mdat <- read.csv("Geographical Original of Music/default_plus_chromatic_features_1059_tracks.txt", header = FALSE)

lat <- mdat[,117]
lon <- mdat[,118]
fet <- mdat[,-c(117, 118)]

# To prepare for BCT, we move all coordinates up by the min (Note that this has to be
#   greater than the mid of Y, hence the "+1")
#   In addition, this should not effect our final regression values
offset_lat <- abs(min(lat)) + 1
offset_lon <- abs(min(lon)) + 1
lat <- lat + abs(min(lat)) + 1
lon <- lon + abs(min(lon)) + 1


# Build a straightforward linear regression of lat vs features and long vs features
lm_lat <- lm(lat ~ rowSums(fet))
lm_lon <- lm(lon ~ rowSums(fet))

par(mfrow=c(2,2))
plot(lm_lat)
dev.copy(png,"P1_LAT_REGULAR.png",width=8,height=6,units="in",res=300)
dev.off()
graphics.off()

par(mfrow=c(2,2))
plot(lm_lon)
dev.copy(png,"P1_LON_REGULAR.png",width=8,height=6,units="in",res=300)
dev.off()
graphics.off()


# Take Box-Cox Tranformation of each
bc_lat <- boxcox(lat ~ rowSums(fet))
bc_lon <- boxcox(lon ~ rowSums(fet))

# Apply the transformation
transform_lat <- bc_lat$x[which.max(bc_lat$y)]
transform_lon <- bc_lon$x[which.max(bc_lon$y)]

lm_lat_new <- lm(lat^transform_lat ~ rowSums(fet))
lm_lon_new <- lm(lon^transform_lon ~ rowSums(fet))

par(mfrow=c(2,2))
plot(lm_lat_new)
dev.copy(png,"P1_LAT_BCT.png",width=8,height=6,units="in",res=300)
dev.off()
graphics.off()

par(mfrow=c(2,2))
plot(lm_lon_new)
dev.copy(png,"P1_LON_BCT.png",width=8,height=6,units="in",res=300)
dev.off()
graphics.off()

# Perform GLMNET Ridge and Lasso for latitude regression
cv_lat_ridge <- cv.glmnet(as.matrix(fet), lat, alpha=0, standardize=TRUE, type.measure='auc')
cv_lat_lasso <- cv.glmnet(as.matrix(fet), lat, alpha=1, standardize=TRUE, type.measure='auc')


# Perform GLMNET Ridge and Lasso for longitude regression
cv_lon_ridge <- cv.glmnet(as.matrix(fet), lon, alpha=0, standardize=TRUE, type.measure='auc')
cv_lon_lasso <- cv.glmnet(as.matrix(fet), lon, alpha=1, standardize=TRUE, type.measure='auc')

par(mfrow=c(2,2))
plot(cv_lat_ridge)
plot(cv_lat_lasso)
plot(cv_lon_ridge)
plot(cv_lon_lasso)
dev.copy(png,"P1_GLM_ALL.png",width=8,height=6,units="in",res=300)
dev.off()
