# This mini-project is based on the K-Means exercise from 'R in Action'
# Go here for the original blog post and solutions
# http://www.r-bloggers.com/k-means-clustering-from-r-in-action/

# Exercise 0: Install these packages if you don't have them already

#install.packages(c("cluster", "rattle","NbClust"))

# Now load the data and look at the first few rows
data(wine, package="rattle")
head(wine)
str(wine)
# Exercise 1: Remove the first column from the data 
wine_new <- wine[-1]
# Scale it using the scale() function
scale(wine_new)

# Now we'd like to cluster the data using K-Means.
#km_wine <- kmeans(wine_new, 3)
#km_wine <- kmeans(wine_new, 26)
km_wine<- kmeans(wine_new,centers = 3, nstart = 20)
#Plot for3
plot(Alcohol ~ Proline, data = wine_new, col = km_wine$cluster)
#Plot for26
# How do we decide how many clusters to use if you don't know that already?

# We'll try two methods.

# Method 1: A plot of the total within-groups sums of squares against the 
# number of clusters in a K-means solution can be helpful. A bend in the 
# graph can suggest the appropriate number of clusters. 

wssplot <- function(data, nc=15, seed=1234){
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers=i)$withinss)}
  
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")
}

wssplot(wine_new)

# Exercise 2:
#   * How many clusters does this method suggest?
km_wine$tot.withinss / km_wine$betweenss
#Ans:3, After 3 there is a sharp drop in the curve without further change.
#   * Why does this method work? What's the intuition behind it?

#   * Look at the code for wssplot() and figure out how it works

# Method 2: Use the NbClust library, which runs many experiments
# and gives a distribution of potential number of clusters.

library(NbClust)
set.seed(1234)
nc <- NbClust(wine_new, min.nc=2, max.nc=15, method="kmeans")
barplot(table(nc$Best.n[1,]),
	          xlab="Numer of Clusters", ylab="Number of Criteria",
		            main="Number of Clusters Chosen by 26 Criteria")


# Exercise 3: How many clusters does this method suggest?
Ans:3

# Exercise 4: Once you've picked the number of clusters, run k-means 
# using this number of clusters. Output the result of calling kmeans()
# into a variable fit.km

fit.km <- kmeans(wine_new, 3)
fit.km
# Now we want to evaluate how well this clustering does.

# Exercise 5: using the table() function, show how the clusters in fit.km$clusters
table(km_wine$cluster, wine_new)
# compares to the actual wine types in wine$Type. Would you consider this a good
# clustering?
library(fpc)
plotcluster(wine_new, fit.km$cluster)
library(MASS)
parcoord(wine_new, fit.km$cluster)
# Exercise 6:
# * Visualize these clusters using  function clusplot() from the cluster library
# * Would you consider this a good clustering?
library(cluster)
clusplot(wine_new, fit.km$cluster)
clusplot((pam(wine_new,3)))
