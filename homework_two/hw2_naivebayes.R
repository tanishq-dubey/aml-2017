#setwd('C://Users/Benson/Desktop/aml things/Data Sets/')
#wdat<-read.csv('K9.data', header=FALSE)

library(klaR)
library(caret)
library(stringr)
library(varhandle)
library(e1071)

# Load data in chunks
dt <- read.table(file="K9.data", header=FALSE, nrows = 800, stringsAsFactors = FALSE, sep = ",")
# Drop last column (its garbage)
dt <-  dt[1:5409]
#dt <- unfactor(dt)

# Remove question marks, set to NA
idx <- dt == "?"
is.na(dt) <- idx
rm(idx)
dt <- dt[complete.cases(dt), ]

# Grab Features
features <- dt[,-c(5409:ncol(dt))]
# Coerce data to num
features <- as.data.frame(sapply(features, as.numeric))

# Grab Labels
labels <- as.factor(dt[,5409])

# Normalize featues
featuresNormal <- features#scale(features)

#convert labels into 1 or -1
labels.n = rep(0,length(labels))
labels.n[labels=="inactive"] = TRUE
labels.n[labels=="active"] = FALSE
labels = labels.n
rm(labels.n)
rm(features)

# Split the data into training and testing
training_idx <- createDataPartition(y=labels, p=.9, list=FALSE)
training_features <- featuresNormal[training_idx,]
training_labels <- labels[training_idx]
training_labels <- as.factor(training_labels)

model<-train(training_features, training_labels, 'nb', trControl=trainControl(method='cv', number=1))
teclasses<-predict(model,newdata=training_idx[-training_idx,])
print(confusionMatrix(data=teclasses, training_labels[-training_idx]))
