# Optionally set working directory
# setwd("~/Projects/classes/aml/homework_one")

# Open helper libaries
library(klaR)
library(caret)

# Number of runs
num_runs = 100

# Open the data file
all_data <- read.csv('pima-indians-diabetes.data', header = FALSE)

# Seperate data into catagories and labels
cata <- all_data[,-c(9)]
labl <- as.factor(all_data[,9])

# Partition Data 
part_idx <- createDataPartition(y = labl, p = .8, list = FALSE)

# Get catagories and labels of partitioned data
cata_part <- cata[part_idx, ]
labl_part <- labl[part_idx]

# klaR training 
model <- train (cata_part, labl_part, 'nb', trControl = trainControl(method='cv', number = num_runs))
test_classes <- predict(model, newdata = cata[-part_idx, ])
cm <- confusionMatrix(data = test_classes, labl[-part_idx])
print(cm)