set.seed(123)
library(tidyverse)
library(gridExtra)

# Set lenght and dimensions
l <- 333
d <- 4

# Generate random numbers and compute the n-th root
root_X <- as.data.frame(runif(l)^(1/d))
colnames(root_X) <- "value"

# Compute the max between n random generated numbers 
max_X <- as.data.frame(vector(length = l))
for (i in 1:l) {
  max_value <- max(sapply(1:d, function(j) runif(1))) 
  max_X[i, 1] <- max_value
}
colnames(max_X) <- "value"

# Create the histogram plot
p2 <- ggplot(max_X, aes(x = value)) + 
  geom_histogram(aes(y = after_stat(count/sum(count))), binwidth = 0.01, fill = "lightgreen", color = "black") + scale_y_continuous(labels = scales::percent) +
  labs(title = paste("Relative frequency of the maximum between ", d, " random numbers between 0 and 1", sep = ""), 
       y = "Relative Frequency") + xlim(0, 0.999999999) + theme_minimal()

p1 <- ggplot(root_X, aes(x = value)) + 
  geom_histogram(aes(y = after_stat(count/sum(count))), binwidth = 0.01, fill = "lightblue", color = "black") + scale_y_continuous(labels = scales::percent) +
  labs(title = paste("Relative frequency of ", d, "-th root for random number between 0 and 1", sep = ""), 
       y = "Relative Frequency") + xlim(0, 0.999999999) + theme_minimal()

grid.arrange(p1, p2, nrow = 2)

# Sort in ascending order the observations of the two DGPs and compute the squared difference between Ai and Bi

max_X_sorted <- max_X %>% arrange(value) 
root_X_sorted <- root_X %>% arrange(value)
abs_diff <- abs(max_X_sorted - root_X_sorted)
ggplot(abs_diff, aes(x = value)) + geom_histogram(aes(y = after_stat(count/sum(count))), bins = 40,  fill = "orange", color = "black") +
  scale_y_continuous(labels = scales::percent) + labs(title = paste("Relative frequency of the absolute difference between sorted samples"), 
       y = "Relative Frequency") + theme_minimal()

# Do these Data Generation Processes give the same frequency distribution?
# Let X be a random variable generated from a uniform distribution:
# X ~ Uniform(0, 1)
# Define two different probability distributions:
# a) P(X^(1/d) <= z) = z^d for d ∈ ℕ (natural numbers)
# b) P(max(X1, ..., Xd) <= z) = z^d for d ∈ ℕ (natural numbers)
# Perform the Wilcoxon rank-sum test to compare the two distributions:
# Null Hypothesis (H0): F_a(X) = F_b(X) for all x

test <- wilcox.test(max_X$value, root_X$value)
test
if (test$p.value < .05 ) {
  cat("Reject H0")
} else {
  cat("Fail to reject H0")
}

rm(list=ls())
cat("\014")
dev.off()
