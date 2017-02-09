setwd("~/Desktop/Data Sets")

library(klaR)
library(caret)
library(stringr)
library(varhandle)

chunk_size = 500

# Load data in chunks
dt <- read.table(file="K9.data", header=FALSE, stringsAsFactors = FALSE, sep = ",")
# Drop last column (its garbage)
dt <- dt[1:5409]
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
featuresNormal <- scale(features)

#convert labels into 1 or -1
labels.n = rep(0,length(labels))
labels.n[labels=="inactive"] = -1
labels.n[labels=="active"] = 1
labels = labels.n
rm(labels.n)
rm(features)

# Split the data into training and testing
training_idx <- createDataPartition(y=labels, p=.1, list=FALSE)
training_features <- featuresNormal[training_idx,]
training_labels <- labels[training_idx]

# Get testing data
testing_labels <- labels[-training_idx]
testing_features <- featuresNormal[-training_idx,]

# Split training data into validation and training
training_lambda_idx <- createDataPartition(y=training_labels, p=.8, list=FALSE)
training_lambda_features <- training_features[training_lambda_idx,]
training_lambda_labels <- training_labels[training_lambda_idx]

testing_lambda_features <- training_features[-training_lambda_idx,] 
testing_lambda_labels <- training_labels[-training_lambda_idx]

# Create set of lambdas, we have pre chosen 5 values:
lambdas <- c(.01, .1, 1, 10, 100)

# Choose constants
ni <- 50    # Items per batch
ns <- 100   # Number of seasons
nt <- 300   # Number of steps per season

# Create array to hold lambda errors
lambda_accuracy <- c(1:5)

# Create matrix to log "a" and "b" values to store best
a_vals = matrix(data = NA, nrow = length(lambdas), ncol = 5408)
b_vals = matrix(data = NA, nrow = length(lambdas), ncol = 1)

# Create matrix to log error per season, useful for making graphs
season_accuracy = matrix(data = NA, nrow = length(lambdas), ncol = ns)

for(i in 1:length(lambdas)) {
    # Pick random a and b values
    # A is the same size as an example from the set of features
    a <- runif(5408, 0.0, 1.0) 
    b <- runif(1, 0.0, 1.0)
    
    # Select lambda to test
    current_lambda = lambdas[i]
    
    
    for(j in 1:ns) {
        # Compute Step Length
        # M and N chosen so we have a step size of 7.7% starting, and 1.6% ending
        e <- 2/(j + 25)
        
        # Per season number correct
        season_num_correct <- 0
        
        # Perform gradient descent steps nt times
        for(k in 1:nt) {
            # Get the current example and label
            # Note the modulo calculation so we can wrap around if needed
            curr_idx <- ((nt * (j - 1)) + k)%% (dim(training_lambda_features)[1])
            if(curr_idx == 0) {
                curr_idx <- 1
            }
            current_example <- training_lambda_features[curr_idx,]
            current_label <- training_lambda_labels[curr_idx]
            
            # Compute the current value of the step (gamma * label)
            current_gamma <- (t(a)%*%current_example)[1, 1] + b
            if(is.nan(current_gamma)) {
                current_gamma = 0;
            }
            current_step_val <- current_gamma * current_label
            
            # Calculate gradient
            if (current_step_val >= 1) {
                delta_a <- current_lambda * a
                delta_b <- 0
                season_num_correct <- season_num_correct + 1
            } else {
                delta_a <- (current_lambda * a) - (current_label - current_example)
                delta_b <- -1 * current_label
            }
            
            # Apply gradient with step length to our "a" and "b" vector
            a <- a - (e * delta_a)
            b <- b - (e * delta_b)
            cat(sprintf("Current Season: %f, Current Step: %f\n", j, k))
        }
        
        # Store season accuracy percent
        season_accuracy[i, j] = season_num_correct/nt
    }
    
    # Compute Error (test against validation set)
    testing_lambda_vals <- (testing_lambda_features %*% a) + b
    lambda_predictions <- sign(testing_lambda_vals)
    
    # Store accuracy
    lambda_accuracy[i] <- sum(lambda_predictions == testing_lambda_labels) / length(testing_lambda_labels)
    
    # Store "a" and "b"
    a_vals[i, ] <- a
    b_vals[i] <- b
    
}

# Pick best lambda, a, b
best_idx <- which.max(lambda_accuracy)

best_lambda <- lambdas[best_idx]
best_a <- a_vals[best_idx, ]
best_b <- b_vals[best_idx]

# Run testing set against best lamba (and correspondin "a" and "b" values)
final_test_vals <- (testing_features %*% best_a) + best_b
final_predictions <- sign(final_test_vals)

final_accuracy <- sum(final_predictions == testing_labels) / length(testing_labels)

# Report error/accuracy
cat(sprintf("Final Accuracy: %f\n", final_accuracy))
cat(sprintf("Chosen Lambda value: %f\n", best_lambda))

# Plot results
par(xpd=T, mar=par()$mar+c(0,0,0,5))
matplot(t(season_accuracy), type = "l", lty=1, ylab = "Accuracy", xlab = "Season Number", bty='L', main="Accuracy over 100 seasons for various lambdas")
legend(2.8, -1, legend = c(".01", ".1", "1", "10", "100"), col=(1:5), lty = 1, lwd = 1)