# Optionally set working directory
# setwd("~/Projects/classes/aml/homework_one")

# Open helper libaries
library(klaR)
library(caret)

num_runs = 10

# Open the data file
all_data <- read.csv('pima-indians-diabetes.data', header = FALSE)

# Seperate data into catagories and labels
cata <- all_data[,-c(9)]
labl <- all_data[,9]

test_score <- array(dim = num_runs)

for(i in 1:num_runs) {
  # Partition Data
  part_idx <- createDataPartition(y = labl, p = .8, list = FALSE)
  
  # Build SVM
  svm <- svmlight(cata[part_idx,], labl[part_idx])
  
  # Test on training data
  labels_prediction <- predict(svm, cata[-part_idx, ])
  
  # Get Results
  final <- labels_prediction$class
  
  # Calculate Accuracy
  test_score[i] <- sum(final == labl[-part_idx]) / (sum(final == labl[-part_idx]) + sum (!(final == labl[-part_idx])))
}

# Print
cat(sprintf("Accuracy over %d runs: %f    SD: %f", num_runs, mean(test_score), sd(test_score)))