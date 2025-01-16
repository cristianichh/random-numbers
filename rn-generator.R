set.seed(123)
library(tidyverse)
library(gridExtra)

# Set lenght and dimensions
l <- 222
d <- 3

# Generate random numbers and compute the n-th root
a <- as.data.frame(runif(l)^(1/d))
colnames(a) <- "value"

# Compute the max between n random generated numbers 
b <- as.data.frame(vector(length = l))
for (i in 1:l) {
  max_value <- max(sapply(1:d, function(j) runif(1))) 
  b[i, 1] <- max_value
}
colnames(b) <- "value"

# Create the histogram plot
p2 <- ggplot(b, aes(x = value)) + 
  geom_histogram(aes(y = after_stat(count/sum(count))), binwidth = 0.01, fill = "lightgreen", color = "black") + scale_y_continuous(labels = scales::percent) +
  labs(title = paste("Relative frequency of the maximum between ", d, " random numbers between 0 and 1", sep = ""), 
       y = "Relative Frequency") + xlim(0, 0.999999999) + theme_minimal()

p1 <- ggplot(a, aes(x = value)) + 
  geom_histogram(aes(y = after_stat(count/sum(count))), binwidth = 0.01, fill = "lightblue", color = "black") + scale_y_continuous(labels = scales::percent) +
  labs(title = paste("Relative frequency of ", d, "-th root for random number between 0 and 1", sep = ""), 
       y = "Relative Frequency") + xlim(0, 0.999999999) + theme_minimal()

grid.arrange(p1, p2, nrow = 2)

# Sort in ascending order the observations of the two DGPs and compute the squared difference between Ai and Bi

a_ord <- a %>% arrange(a) 
b_ord <- b %>% arrange(b)
c <- abs(a_ord - b_ord)
ggplot(c, aes(x = value)) + geom_histogram(aes(y = after_stat(count/sum(count))), fill = "orange", color = "black") +
  scale_y_continuous(labels = scales::percent) + labs(title = paste("Relative frequency of the absolute difference between sorted samples"), 
       y = "Relative Frequency") + theme_minimal()

# Do these Data Generation Processes give the same frequency distribution?
# Xi = runif(min=0, max=1)
# a <- P( X1^(1/d) <= z) = z^d
# b <- P( max(X1,...,Xd) <= z) = z^d
# Perform Wilcoxon test (H0: Fa(X) = Fb(X) for all x)
# Reject H0 if p-value < 0.05

wilcox.test(a[,1], b[,1])


rm(list=ls())
cat("\014")
dev.off()
