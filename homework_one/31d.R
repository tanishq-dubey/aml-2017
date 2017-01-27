# Optionally set working directory
# setwd("~/Projects/classes/aml/homework_one")

# Open helper libaries
library(klaR)
library(caret)

# Open the data file
all_data <- read.csv('pima-indians-diabetes.data', header = FALSE)

# Seperate data into catagories and labels
cata <- all_data[,-c(9)]
labl <- all_data[,9]

# Partition Data
part_idx <- createDataPartition(y = labl, p = .8, list = FALSE)

# Build SVM
svm <- svmlight(cata[part_idx,], labl[part_idx])

# Test on training data
labels_prediction <- predict(svm, cata[-part_idx, ])

# Get Results
final <- labels_prediction$class

# Calculate Accuracy
acc <- sum(final == labl[-part_idx]) / (sum(final == labl[-part_idx]) + sum (!(final == labl[-part_idx])))

# Print
print("Accuracy:")
print(acc)