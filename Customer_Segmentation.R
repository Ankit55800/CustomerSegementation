# Load necessary libraries
library(purrr)
library(cluster)
library(NbClust)
library(factoextra)
library(plotrix)

# Load the data
data <- read.csv("Mall_Customers.csv")

# Explore the data
str(data)
names(data)
head(data)

# Summary statistics
summary(data$Age)
sd(data$Age)
summary(data$Annual.Income..k..)
sd(data$Annual.Income..k..)
summary(data$Spending.Score..1.100.)
sd(data$Spending.Score..1.100.)

# Split the data into training and testing sets (e.g., 70% training, 30% testing)
set.seed(123)
sample_indices <- sample(1:nrow(data), 0.7 * nrow(data))
train_data <- data[sample_indices, ]
test_data <- data[-sample_indices, ]

# Perform K-means clustering on the training data
k <- 6  # Choose the desired number of clusters
set.seed(123)
kmeans_model <- kmeans(train_data[, 3:5], centers = k, nstart = 100)

# Apply the clustering model to the test data
test_clusters <- predict(kmeans_model, newdata = test_data[, 3:5])

# Determine the optimal number of clusters
fviz_nbclust(data[, 3:5], kmeans, method = "silhouette")
set.seed(125)
stat_gap <- clusGap(data[, 3:5], FUN = kmeans, nstart = 25, K.max = 10, B = 50)
fviz_gap_stat(stat_gap)

# Perform K-means clustering with the chosen number of clusters (e.g., k = 6)
k6 <- kmeans(data[, 3:5], 6, iter.max = 100, nstart = 50, algorithm = "Lloyd")

# Visualize the clustering results and data exploration

# Customer Gender Visualization
a <- table(data$Gender)
barplot(a, main = "Gender Comparison", ylab = "Count", xlab = "Gender", col = rainbow(2), legend = rownames(a))

# Visualization of Age Distribution
hist(data$Age, col = "blue", main = "Age Class Distribution", xlab = "Age Class", ylab = "Frequency")

# Analysis of Annual Income
hist(data$Annual.Income..k.., col = "#660033", main = "Annual Income Distribution", xlab = "Annual Income Class", ylab = "Frequency")
plot(density(data$Annual.Income..k..), col = "black", main = "Density Plot for Annual Income", xlab = "Annual Income Class", ylab = "Density")

# BoxPlot for Descriptive Analysis of Spending Score
boxplot(data$Spending.Score..1.100., horizontal = TRUE, col = "#990000", main = "Spending Score Analysis")
hist(data$Spending.Score..1.100., col = "#6600cc", main = "Spending Score Distribution", xlab = "Spending Score Class", ylab = "Frequency")

# Function to calculate total intra-cluster sum of squares
iss <- function(k) {
  kmeans(data[, 3:5], k, iter.max = 100, nstart = 100, algorithm = "Lloyd")$tot.withinss
}

k.values <- 1:10
iss_values <- map_dbl(k.values, iss)

# Plotting the total intra-cluster sum of squares
plot(k.values, iss_values, type = "b", pch = 19, frame = FALSE, xlab = "Number of clusters K", ylab = "Total intra-cluster sum of squares")

# Visualizing the Clustering Results using the First Two Principal Components
pcclust <- prcomp(data[, 3:5], scale = FALSE)  # Principal component analysis
summary(pcclust)

# Plotting clusters in the first two principal components
digCluster <- k6$cluster
dignm <- as.character(digCluster)

# 
# # Custom function to generate colors
# kCols <- function(vec) {
#   cols <- rainbow(length(unique(vec))
#                   return(cols[as.numeric(as.factor(vec))])
# }
# 
# point_colors <- kCols(digCluster)
# 
# plot(pcclust$x[, 1:2], col = point_colors, pch = 19, xlab = "K-means", ylab = "Classes")
# 
# # Create a legend
# legend("bottomleft", legend = unique(dignm), fill = unique(point_colors))
