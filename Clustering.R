# Read the data and remove unnecessary columns
getwd()

setwd("C:/Users/rbart/Desktop/BI/CSVDatasets")
dungaree<-read.csv("dungaree.csv")
head(dungaree)
rownames(dungaree)<-dungaree[,"STOREID"]
dung_use <- dungaree[,-c(1,6)]
head(dung_use)
summary(dung_use)

#There are still NA's.  Let's remove them.  Five rows will be deleted.
dung_nm <- na.omit(dung_use)
dim(dung_nm)

##pairwise distance
dung_dist <- dist(dung_nm)
cars_dist <- dist(dung_nm, method="minkowski",p=2) # same as Euclidean

#scaling inputs and distances
dung_scaled <- scale(dung_nm)
dung_scaled_dist<-dist(dung_scaled)

#Hierarchical clustering with default method = "complete"
complete.out = hclust(dung_dist)

ward.out = hclust(cars_dist, method="ward.D")

complete2.out = hclust(dung_scaled_dist)

ward2.out = hclust(cars_scaled_dist, method="ward.D")




plot(complete.out, main='Dendrogram using complete linkage', cex=0.7)
plot(complete2.out, main='Dendrogram using complete linkage', cex=0.7)
plot(ward.out, main='Dendrogram using Ward\'s method', cex=0.7)
plot(ward2.out, main='Dendrogram using Ward\'s method', cex=0.7)

#Playing with dendrogram objects
dndr_ward = as.dendrogram(ward2.out)
plot(dndr_ward, type="triangle")
plot(cut(dndr_ward, h = 7)$upper, main = "Upper tree of cut at h=7")
plot(cut(dndr_ward, h = 7)$lower[[4]], main = "Fourth branch of lower tree with cut at h=7") 


#Cut the dendrogram tree to create three clusters.
single_cl3.out<-cutree(single2.out,3)
table(single_cl3.out)
complete_cl3.out<-cutree(complete2.out,3)
table(complete_cl3.out)
ward_cl3.out<-cutree(ward2.out,3)
table(ward_cl3.out)


# members in each cluster for "hierarchical" clustering
counts = sapply(2:10,function(ncl)table(cutree(ward2.out, ncl)))
names(counts) = 2:10
counts

cl8.out=cutree(ward2.out,8)
table(cl8.out)
sapply(unique(cl8.out),function(g)rownames(cars_nm)[cl8.out == g])

# k-means algorithm
set.seed(1)
km.out = kmeans(cars_nm, 5, nstart = 25)
table(km.out$cluster)

# cheking cluster membership and size
km.out
km.out$cluster
km.out$size


counts = sapply(2:10,function(ncl)table(kmeans(cars_scaled, ncl,nstart=25)$cluster))
names(counts) = 2:10
counts

#clusplot
par(mfrow=c(1,2))
require("cluster")
km_cl3.out = kmeans(cars_scaled, 3, nstart = 25) # 3 clusters
clusplot(cars_scaled_dist, diss = TRUE, km_cl3.out$cluster, color = TRUE, shade = TRUE, labels = 2, lines = 0)
clusplot(cars_scaled_dist, diss = TRUE, complete_cl3.out, color = TRUE, shade = TRUE, labels = 2, lines = 0)

# Plots
km_cl3.out = kmeans(cars_nm, 3, nstart = 1)
plot(cars_nm,col=(km_cl3.out$cluster+1))

# different parameter values
km.out = kmeans(cars_nm, 5, nstart = 1)
km_scaled.out<-kmeans(cars_scaled, 5, nstart = 1)
km_n25.out<-kmeans(cars_nm, 5, nstart = 1)
km_scaled_n25.out<-kmeans(cars_scaled, 5, nstart = 25)

par(mfrow=c(2,2))
plot(cars_dist,col=(km.out$cluster+1), main = "Cars")
plot(cars_scaled_dist,col=(km_scaled.out$cluster+1), main = "Cars scaled")
plot(cars_dist,col=(km_n25.out$cluster+1), main = "Cars nstart=25")
plot(cars_scaled_dist,col=(km_scaled_n25.out$cluster+1), main = "Cars scaled & nstart=25")
par(mfrow=c(1,1))

# Function definition for within sum of squares plot
wssplot <- function(data, nc=15, seed=1234)
{
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers=i, nstart=25)$withinss)}
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")
}

wssplot (cars_scaled)

#Determining number of clusters
if(!require("NbClust")) install.packages("NbClust")
library(NbClust)
nbcl.out = NbClust(cars_scaled, distance = "euclidean", 
                   min.nc = 2, max.nc = 10, method = "ward.D2", index = "all")

nbcl.out$Best.nc
par(mfrow=c(1,1))

#Silhouette
km_sil<-silhouette(km_scaled_n25.out$cluster, cars_scaled_dist)
plot(km_sil, main = "Silhouette plot for k-means result")
summary(km_sil)
summary(km_sil)$avg.width

ward_sil<-silhouette(cutree(ward2.out,5), cars_scaled_dist)
plot(ward_sil, main = "Silhouette plot for Ward's method result")
complete_sil<-silhouette(cutree(complete2.out,5), cars_scaled_dist)
plot(complete_sil, main = "Silhouette plot for complete linkage result")
single_sil<-silhouette(cutree(single2.out,5), cars_scaled_dist)
plot(single_sil, main = "Silhouette plot for single linkage result")


# comparison of cluster membership
kmeans_cl5<-km_scaled_n25.out$cluster
complete_cl5<-cutree(complete2.out,5)
ward_cl5<-cutree(ward2.out,5)
table (kmeans_cl5, complete_cl5)
table (kmeans_cl5, ward_cl5)
table (ward_cl5, complete_cl5)