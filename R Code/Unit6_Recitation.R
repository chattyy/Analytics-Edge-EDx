# Unit 6 - Recitation
# Video 2

flower = read.csv("/Users/eshanchatty/Downloads/Clustering/flower.csv", header=FALSE)
str(flower)

# Change the data type to matrix
flowerMatrix = as.matrix(flower)
str(flowerMatrix)

# Turn matrix into a vector
flowerVector = as.vector(flowerMatrix)
str(flowerVector)

flowerVector2 = as.vector(flower)
str(flowerVector2)

# Compute distances
distance = dist(flowerVector, method = "euclidean")



# Video 3

# Hierarchical clustering
clusterIntensity = hclust(distance, method="ward")
#As a reminder, the Ward’s method
#is a minimum variance method, which
#tries to find compact and spherical clusters.
#We can think about it as trying to minimize the variance
#within each cluster and the distance among clusters.

# Plot the dendrogram
plot(clusterIntensity)

# Select 3 clusters
rect.hclust(clusterIntensity, k = 3, border = "red")
#We can actually visualize the cuts
#by plotting rectangles around the clusters on this tree
flowerClusters = cutree(clusterIntensity, k = 3)
flowerClusters

# Find mean intensity values # clustervalues, 0.008 is lowest colour, so on.
tapply(flowerVector, flowerClusters, mean)

# Plot the image and the clusters
dim(flowerClusters) = c(50,50)
image(flowerClusters, axes = FALSE)

# Original image
image(flowerMatrix,axes=FALSE,col=grey(seq(0,1,length=256)))



# Video 4

# Let's try this with an MRI image of the brain

healthy = read.csv("/Users/eshanchatty/Downloads/Clustering/healthy.csv", header=FALSE)
healthyMatrix = as.matrix(healthy)
str(healthyMatrix)

# Plot image
image(healthyMatrix,axes=FALSE,col=grey(seq(0,1,length=256)))

# Hierarchial clustering
healthyVector = as.vector(healthyMatrix)
distance = dist(healthyVector, method = "euclidean")

# We have an error - why?
str(healthyVector)
#that for R to calculate the pairwise distances,
#it would actually need to calculate n*(n-1)/2 and then
#store them in the distance matrix.


# Video 5

# Specify number of clusters
k = 5

# Run k-means
set.seed(1)
KMC = kmeans(healthyVector, centers = k, iter.max = 1000)
str(KMC)

# Extract clusters
healthyClusters = KMC$cluster
KMC$centers[2]

# Plot the image with the clusters
dim(healthyClusters) = c(nrow(healthyMatrix), ncol(healthyMatrix))

image(healthyClusters, axes = FALSE, col=rainbow(k))



# Video 6

# Apply to a test image
 
tumor = read.csv("/Users/eshanchatty/Downloads/Clustering/tumor.csv", header=FALSE)
tumorMatrix = as.matrix(tumor)
tumorVector = as.vector(tumorMatrix)

# Apply clusters from before to new image, using the flexclust package
library(flexclust)

KMC.kcca = as.kcca(KMC, healthyVector)
tumorClusters = predict(KMC.kcca, newdata = tumorVector)
#which stands for K-Centroids Cluster Analysis.
#We need to convert the information from the clustering
#algorithm to an object of the class KCCA.
#And this conversion is needed before we
#can use the predict function on the test set tumorVector.
# Visualize the clusters
dim(tumorClusters) = c(nrow(tumorMatrix), ncol(tumorMatrix))

image(tumorClusters, axes = FALSE, col=rainbow(k))
#yellow is the white, grey is the purple, blue is the tumour.
