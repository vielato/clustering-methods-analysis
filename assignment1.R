####Load Libraries Used
library(tidyverse)  # data manipulation
library(cluster)    # clustering algorithms
library(factoextra) # clustering algorithms & visualization
#install.packages('corrplot') 
library(corrplot)
#install.packages('mclust')
library(mclust)
library(gridExtra)


####Dataset Preparation
data <- read.csv("market.csv")
head(data,5)   
data <- na.omit(data)
str(data)
summary(data)
data <- scale(data)
head(data,5)
boxplot(data)

#remove outliers
data <- data.frame(data)
outliers <- function(x) {

  Q1 <- quantile(x, probs=.25)
  Q3 <- quantile(x, probs=.75)
  iqr = Q3-Q1

 upper_limit = Q3 + (iqr*1.5)
 lower_limit = Q1 - (iqr*1.5)

 x > upper_limit | x < lower_limit
}

remove_outliers <- function(data, cols = names(data)) {
  for (col in cols) {
    data <- data[!outliers(data[[col]]),]
  }
  data
}
data<-remove_outliers(data, c('Milk','Grocery','Frozen', 'Detergents_Paper', 'Delicassen'))
boxplot(data)

market <- data
market$Region = NULL
market$Channel = NULL


####Kmeans Clustering
kmeans_clustering <- market 

k2 <- kmeans(kmeans_clustering, centers = 2, nstart = 25)
k3 <- kmeans(kmeans_clustering, centers = 3, nstart = 25)
k4 <- kmeans(kmeans_clustering, centers = 4, nstart = 25)
k5 <- kmeans(kmeans_clustering, centers = 5, nstart = 25)
k6 <- kmeans(kmeans_clustering, centers = 6, nstart = 25)
k7 <- kmeans(kmeans_clustering, centers = 7, nstart = 25)
k8 <- kmeans(kmeans_clustering, centers = 8, nstart = 25)
k9 <- kmeans(kmeans_clustering, centers = 9, nstart = 25)
k10<-kmeans(kmeans_clustering, centers = 10, nstart = 25)

# plots to compare
p2 <- fviz_cluster(k2, geom = "point", data = kmeans_clustering) + ggtitle("k = 2")
p3 <- fviz_cluster(k3, geom = "point", data = kmeans_clustering) + ggtitle("k = 3")
p4 <- fviz_cluster(k4, geom = "point", data = kmeans_clustering) + ggtitle("k = 4")
p5 <- fviz_cluster(k5, geom = "point", data = kmeans_clustering) + ggtitle("k = 5")
p6 <- fviz_cluster(k6, geom = "point", data = kmeans_clustering) + ggtitle("k = 6")
p7 <- fviz_cluster(k7, geom = "point", data = kmeans_clustering) + ggtitle("k = 7")
p8 <- fviz_cluster(k8, geom = "point", data = kmeans_clustering) + ggtitle("k = 8")
p9 <- fviz_cluster(k9, geom = "point", data = kmeans_clustering) + ggtitle("k = 9")
p10<- fviz_cluster(k10,geom = "point", data = kmeans_clustering) + ggtitle("k = 10")

#display plots in the same image
grid.arrange(p2, p3, p4, p5, p6, p7, p8, p9, p10, nrow = 2)

#elbow method
set.seed(123)
fviz_nbclust(kmeans_clustering, kmeans, method = "wss")

#silhouette methode
set.seed(123)
fviz_nbclust(kmeans_clustering, kmeans, method = "silhouette")

#gap statistic methode
set.seed(123)
gap_stat <- clusGap(kmeans_clustering, FUN = kmeans, nstart = 25,K.max = 10, B = 26)
print(gap_stat, method = "firstmax")
fviz_gap_stat(gap_stat)

#Compute k-means clustering with k = 3
set.seed(123)
final <- kmeans(kmeans_clustering, 3, nstart = 25)
## Total Within cluster sum of square
final$tot.withinss
## Cluster sizes
final$size
print(final)
fviz_cluster(final, data = kmeans_clustering)


####Hierarchical Clustering ~ Agglomerative HC 
hc1 <- market 

#define linkage methods
m <- c( "average", "single", "complete", "ward")
names(m) <- c( "average", "single", "complete", "ward")
    
#function to compute agglomerative coefficient
ac <- function(x) {
    agnes(hc1, method = x)$ac
}

#calculate agglomerative coefficient for each clustering linkage method
sapply(m, ac)

#perform hierarchical clustering using Ward's minimum variance
clust <- agnes(hc1, method = "ward")

#produce dendrogram
pltree(clust, cex = 0.6, hang = -1, main = "Dendrogram") 

#cut the dendrogram into 3 clusters
groups <- cutree(clust, k=3)
#find number of observations in each cluster
table(groups)
fviz_cluster(list(data = hc1, cluster = groups)) 

#append cluster labels to original data
final_data <- cbind(hc1, cluster = groups)
plot(final_data)
#display first six rows of final data
head(final_data)


####Model-Based  Clustering
mbc <- Mclust(market)
summary(mbc)
# BIC values used for choosing the number of clusters
fviz_mclust(mbc, "BIC", palette = "jco")
plot(mbc, what="density")
# Classification: plot showing the clustering
fviz_mclust(mbc, "classification", geom = "point", pointsize = 1.5, palette = "jco")


####Extras for the Winning Clustering Algorithm

#append cluster labels to the original data table, containing the columns Region, Channel
data<-data.frame(data)
win_clust <- cbind(data, cluster = groups)
plot(win_clust)

corrmatrix <- cor(win_clust)
corrplot(corrmatrix, method = 'number')

