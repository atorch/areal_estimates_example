## Imagine a classification problem with classes
## forest, pasture, crops (a land cover classification problem with satellite imagery, for example) or
## background, healthy crop, diseased crop (a classification problem with drone imagery)
## Total area is proportional to these shares, by definition (they are fractions of total area)
true_shares <- c(0.65, 0.30, 0.05)

## This is a probability distribution, it needs to sum to 1.0
sum(true_shares)

## Entry i, j is Pr[ classifier predicts class j | truth is class i ]
recall <- rbind(c(0.88, 0.07, 0.05),
                c(0.06, 0.92, 0.02),
                c(0.01, 0.09, 0.90))
recall[1, ]

## Each row is a probability distribution over predicted classes,
## so rows need to sum to 1.0
rowSums(recall)

## These are the class-specific recall rates that you'd see in a classification report
diag(recall)

predicted_shares <- as.vector(true_shares %*% recall)

## This should sum to 1.0 (it's a probability distribution)
sum(predicted_shares)

## Note that we over-predict class 3 by a considerable percentage
data.frame(true_share=true_shares, predicted_share=predicted_shares)

## Suppose we observe predicted_share -- how could we figure out true_share?
## If we have a _perfect_ estimate of the recall matrix, we can do this:
recall_inverse <- solve(recall)
predicted_shares %*% recall_inverse

## How do we know the recall matrix is invertible?
## See https://en.wikipedia.org/wiki/Diagonally_dominant_matrix
## Usually (but not always), the recall matrix is diagonally dominant

## Suppose instead that we need to estimate the recall matrix using a small test set
n_test_obs <- 300
test_set <- data.frame(true_class=sample(1:ncol(recall), size=n_test_obs, prob=true_shares, replace=TRUE))
test_set$predicted_class <- sapply(test_set$true_class, function(x) {
    sample(1:ncol(recall), size=1, prob=recall[x, ])
})

confusion_matrix <- table(test_set$true_class, test_set$predicted_class)

## Sanity check
confusion_matrix[2, 3]
sum(test_set$true_class == 2 & test_set$predicted_class == 3)

recall_hat <- confusion_matrix / rowSums(confusion_matrix)
round(recall_hat, 3)
round(recall - recall_hat, 3)

## This perfectly recovers true_shares
predicted_shares %*% recall_inverse

## This has an error  # TODO Could be interesting to look at its sampling distribution
predicted_shares %*% solve(recall_hat)

## Example of a recall matrix that is _not_ invertible
recall_not_invertible <- rbind(c(0.88, 0.07, 0.05),
                               c(0.88, 0.07, 0.05),
			       c(0.01, 0.09, 0.90))
solve(recall_not_invertible)
eigen(recall_not_invertible)
round(eigen(recall_not_invertible)$values, 5)
