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
labl <- all_data[,9]

# Arrays for holding error data over all runs
train_score <- array(dim = num_runs)
test_score <- array(dim = num_runs)

# Do 10 times
for(i in 1:num_runs) {
    # Partition Data 
    part_idx <- createDataPartition(y = labl, p = .8, list = FALSE)
    
    # Get catagories and labels of partitioned data
    cata_part <- cata[part_idx, ]
    labl_part <- labl[part_idx]
    
    # Get all the positive training examples
    pos_exmp_idx <- labl_part > 0
    
    # Split the data based on positive and negative labels
    pos_cata <- cata_part[pos_exmp_idx, ]
    neg_cata <- cata_part[!pos_exmp_idx, ]
    
    # Calculate means for each catagory/column
    pos_cata_means <- sapply(pos_cata, mean)
    neg_cata_means <- sapply(neg_cata, mean)
    
    # Calculate std for each catagory/column
    pos_cata_std <- sapply(pos_cata, sd)
    neg_cata_std <- sapply(neg_cata, sd)
    
    # Find distances from mean
    pos_mean_dist <- t(t(cata_part) - pos_cata_means)
    neg_mean_dist <- t(t(cata_part) - neg_cata_means)
    
    # Convert distances into STD units
    pos_sd_dist <- t(t(pos_mean_dist) / pos_cata_std)
    neg_sd_dist <- t(t(neg_mean_dist) / neg_cata_std)
    
    # Build distributions
    pos_dist <- -(1/2) * rowSums(apply(pos_sd_dist, c(1,2), function(x)x^2)) - sum(log(pos_cata_std))
    neg_dist <- -(1/2) * rowSums(apply(neg_sd_dist, c(1,2), function(x)x^2)) - sum(log(neg_cata_std))
    
    # Add log probability of label to dist
    pos_dist_wlog <- pos_dist + log(length(which(pos_exmp_idx))/length(pos_exmp_idx))
    neg_dist_wlog <- neg_dist + log(1 - (length(which(pos_exmp_idx))/length(pos_exmp_idx)))
    
    # Calculate training score
    pos_data_labl <- pos_dist_wlog > neg_dist_wlog
    correct_vals <- pos_data_labl == labl_part
    
    # Add training score percent to array
    train_score[i] <- sum(correct_vals)/(sum(correct_vals) + sum(!correct_vals))
    
    # Build testing arrays
    test_cata <- cata[-part_idx, ]
    test_labl <- labl[-part_idx]
    
    # Calculate testing distances from mean
    pos_test_mean_dist <- t(t(test_cata) - pos_cata_means)
    neg_test_mean_dist <- t(t(test_cata) - neg_cata_means)
    
    # Calculate testing distances from mean in STDs
    pos_test_dist_sd <- t(t(pos_test_mean_dist)/pos_cata_std)
    neg_test_dist_sd <- t(t(neg_test_mean_dist)/neg_cata_std)
    
    # Build testing distributions
    pos_test_dist <- -(1/2) * rowSums(apply(pos_test_dist_sd, c(1,2), function(x)x^2)) - sum(log(pos_cata_std))
    neg_test_dist <- -(1/2) * rowSums(apply(neg_test_dist_sd, c(1,2), function(x)x^2)) - sum(log(neg_cata_std))
    
    # Add log probability of label to test dist
    pos_test_exmp_idx = test_labl > 0
    pos_test_dist_wlog <- pos_test_dist + log(length(which(pos_test_exmp_idx))/length(pos_test_exmp_idx))
    neg_test_dist_wlog <- neg_test_dist + log(1 - (length(which(pos_test_exmp_idx))/length(pos_test_exmp_idx)))
    
    # Calculate testing score percent
    pos_test_labl <- pos_test_dist_wlog > neg_test_dist_wlog
    correct_test_vals <- pos_test_labl == test_labl
    
    # Calculate score
    test_score[i] <- sum(correct_test_vals)/(sum(correct_test_vals) + sum(!correct_test_vals))
}

# Print info
cat(sprintf("Average training score over %d runs: %f (SD: %f)\n", num_runs, mean(train_score), sd(train_score)))
cat(sprintf("Average testing score over %d runs: %f (SD: %f)\n", num_runs, mean(test_score), sd(test_score)))

