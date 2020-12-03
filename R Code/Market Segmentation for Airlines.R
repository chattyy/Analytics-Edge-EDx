# market Segmentation For Airlines
# Market segmentation is a strategy that divides a broad target market of customers into smaller,
# more similar groups, and then designs a marketing strategy specifically for each group. 
# Clustering is a common technique for market segmentation since it 
# automatically finds similar groups given a data set. 
# In this problem, we'll see how clustering can be used to find similar groups of customers 
#who belong to an airline's frequent flyer program. The airline is trying to learn more about its customers so that it can target different customer segments with different types of mileage offers. 
# 
# The file AirlinesCluster.csv contains information on 3,999 members of the frequent flyer program. 
# This data comes from the textbook "Data Mining for Business Intelligence," by Galit Shmueli, Nitin R. Patel, and Peter C. Bruce. For more information, see the website for the book.
# There are seven different variables in the dataset, described below:
#     
# Balance = number of miles eligible for award travel
# QualMiles = number of miles qualifying for TopFlight status
# BonusMiles = number of miles earned from non-flight bonus transactions in the past 12 months
# BonusTrans = number of non-flight bonus transactions in the past 12 months
# FlightMiles = number of flight miles in the past 12 months
# FlightTrans = number of flight transactions in the past 12 months
# DaysSinceEnroll = number of days since enrolled in the frequent flyer program


airlines<- read.csv("/Users/eshanchatty/Downloads/Clustering/AirlinesCluster.csv")
summary(airlines)
colMeans(airlines)
library(caret)
#Normalisation
preproc = preProcess(airlines)
airlinesNorm = predict(preproc, airlines)
summary(airlinesNorm)
apply(airlinesNorm, 2, max)
distances<-dist(airlinesNorm,method="euclidean")
airlineClust<-hclust(distances, method="ward.D")
plot(airlineClust)
clusterGroups<-cutree(airlineClust,k=5)
airlineClusters<-split(airlinesNorm, clusterGroups)
nrow(airlineClusters[[1]])
table(clusterGroups)
tapply(airlines$Balance, clusterGroups, mean)
tapply(airlines$QualMiles, clusterGroups, mean)
tapply(airlines$BonusMiles,clusterGroups, mean)
#so on
airlinesUnnormClusters<-split(airlines,clusterGroups)
round(sapply(airlinesUnnormClusters,colMeans),4)

#Kmeans
set.seed(88)
kmeansClust<-kmeans(airlinesNorm,centers = 5,iter.max = 1000)
table(kmeansClust$cluster)
#PROBLEM 3.2 - K-MEANS CLUSTERING  

#Now, compare the cluster centroids to each other either by dividing the data points into groups and then using tapply, or by looking at the output of kmeansClust$centers, where "kmeansClust" is the name of the output of the kmeans function. (Note that the output of kmeansClust$centers will be for the normalized data. If you want to look at the average values for the unnormalized data, you need to use tapply like we did for hierarchical clustering.)

#the centroids of the clusters (Normalised data)

#Hierarchial clusters centroid
airlinesUnnormClusters<-split(airlinesNorm,clusterGroups)
round(sapply(airlinesUnnormClusters,colMeans),4)
#Kmeans centroid
kmeansClust$centers
