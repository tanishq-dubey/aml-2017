# Optionally set working directory
# setwd("~/Projects/classes/aml/homework_one")

# Open helper libaries
library(klaR)
library(caret)

# Number of runs
num_runs = 10

# Open the data file
all_data <- read.csv('processed-cleveland.data', header = FALSE)

# Seperate data into catagories and labels
cata <- all_data[,-c(dim(all_data)[2])]
labl <- all_data[,dim(all_data)[2]]
labl[labl > 0 ] <- 1

labl <- as.factor(labl)


# Partition Data 
part_idx <- createDataPartition(y = labl, p = .85, list = FALSE)

# Get catagories and labels of partitioned data
cata_part <- cata[part_idx, ]
labl_part <- labl[part_idx]

# klaR training 
model <- train (cata_part, labl_part, 'nb', std=TRUE,verbose = TRUE, trControl = trainControl(method='cv', number = num_runs))
test_classes <- predict(model, newdata = cata[-part_idx, ])
cm <- confusionMatrix(data = test_classes, labl[-part_idx])
cat(sprintf("Standard Deviation: %f\n", sd(model$resample$Accuracy)[1]))
print(cm)