# Document clustering, or text clustering, is a very popular application of clustering algorithms. A web search engine, like Google, often returns thousands of results for a simple query. For example, if you type the search term "jaguar" into Google, around 200 million results are returned. This makes it very difficult to browse or find relevant information, especially if the search term has multiple meanings. If we search for "jaguar", we might be looking for information about the animal, the car, or the Jacksonville Jaguars football team. 
# 
# Clustering methods can be used to automatically group search results into categories, making it easier to find relevant results. This method is used in the search engines PolyMeta and Helioid, as well as on FirstGov.gov, the official Web portal for the U.S. government. The two most common algorithms used for document clustering are Hierarchical and k-means. 
# 
# In this problem, we'll be clustering articles published on Daily Kos, an American political blog that publishes news and opinion articles written from a progressive point of view. Daily Kos was founded by Markos Moulitsas in 2002, and as of September 2014, the site had an average weekday traffic of hundreds of thousands of visits. 
# 
# The file dailykos.csv contains data on 3,430 news articles or blogs that have been posted on Daily Kos. These articles were posted in 2004, leading up to the United States Presidential Election. The leading candidates were incumbent President George W. Bush (republican) and John Kerry (democratic). Foreign policy was a dominant topic of the election, specifically, the 2003 invasion of Iraq. 
# 
# Each of the variables in the dataset is a word that has appeared in at least 50 different articles (1,545 words in total). The set of  words has been trimmed according to some of the techniques covered in the previous week on text analytics (punctuation has been removed, and stop words have been removed). For each document, the variable values are the number of times that word appeared in the document. 

dailykos=read.csv("/Users/eshanchatty/Downloads/Clustering/dailykos.csv")
View(dailykos)
dim(dailykos)
#compute the distances (using method="euclidean")
kosDist<- dist(dailykos,method="euclidean")

#use hclust to build the model (using method="ward.D"),clustering on all of the variables
kosHierClust<- hclust(kosDist, method="ward.D")
plot(kosHierClust)
hierGroups<-cutree(kosHierClust, k=7)

#Then, you can use the subset function 7 times to split the data into the 7 clusters(we don't really want to run tapply on every single variable when we have over 1,000 different variables):
HierCluster1 = subset(dailykos, hierGroups == 1)
nrow(HierCluster1)
HierCluster2 = subset(dailykos, hierGroups == 2)
nrow(HierCluster2)
HierCluster3 = subset(dailykos, hierGroups == 3)
nrow(HierCluster3)
HierCluster4 = subset(dailykos, hierGroups == 4)
nrow(HierCluster4)
HierCluster5 = subset(dailykos, hierGroups == 5)
nrow(HierCluster5)
HierCluster6 = subset(dailykos, hierGroups == 6)
nrow(HierCluster6)
HierCluster7 = subset(dailykos, hierGroups == 7)
nrow(HierCluster7)

# Instead of looking at the average value in each variable individually, we'll just look at the top 6 words in each cluster. To do this for cluster 1, type the following in your R console (where "HierCluster1" should be replaced with the name of your first cluster subset):
# 
# tail(sort(colMeans(HierCluster1)))
# 
# This computes the mean frequency values of each of the words in cluster 1, and then outputs the 6 words that occur the most frequently. The colMeans function computes the column (word) means, the sort function orders the words in increasing order of the mean values, and the tail function outputs the last 6 words listed, which are the ones with the largest column means.
# 
# What is the most frequent word in this cluster, in terms of average value? Enter the word exactly how you see it in the output:
tail(sort(colMeans(HierCluster1)))
tail(sort(colMeans(HierCluster2)))
tail(sort(colMeans(HierCluster3)))
tail(sort(colMeans(HierCluster4)))
tail(sort(colMeans(HierCluster5)))
tail(sort(colMeans(HierCluster6)))
tail(sort(colMeans(HierCluster7)))

#K-means Clustering.
k = 7

# Run k-means
set.seed(1000)
KmeansCluster = kmeans(dailykos, centers = k)
str(KmeansCluster)
table(KmeansCluster$cluster)

#Now, output the six most frequent words in each cluster, like we did in the previous problem, for each of the k-means clusters.
KmeansClusterspl<-split(dailykos,KmeansCluster$cluster)

#most frequent words from each kmeans cluster:
kmTopWords <- lapply(KmeansClusterspl, function(c) tail(sort(colMeans(c))))
kmTopWords
#For the rest of this problem, we'll ask you to compare how observations were assigned to clusters in the two different methods. Use the table function to compare the cluster assignment of hierarchical clustering to the cluster assignment of k-means clustering.
table(hierGroups,KmeansCluster$cluster)






