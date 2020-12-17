install_packages = FALSE
if(install_packages) {
  install.packages("BiocManager")
  BiocManager::install("rhdf5")
  pkgs <- c("factoextra",  "NbClust")
  install.packages(pkgs)
  install.packages("e1071")
  install.packages("dbscan")
  install.packages("mclust")
  install.packages("DDoutlier")
}

rm(list = ls())

require(tidyverse)
library(rhdf5)
library(factoextra)
library(NbClust)
library(e1071)
library(dbscan)
library(mclust)
library(DDoutlier)

dataset_files = c('./wine.mat', './smtp.mat')
fields = c("classification", "clustering")

# classification
methods = c("knn")

# clustering
methods = c("kmeans", "hcluster", "cmeans", "dbscan", "mclust")
kmeans_algorithms = c("Hartigan-Wong", "MacQueen", "Lloyd")
hclust_metrics = c("euclidean", "manhattan", "maximum", "canberra", "binary", "minkowski")
hclust_algorithms = c("average", "ward.D", "ward.D2", "single", "complete", "mcquitty", "median", "centroid")
cmeans_metrics = c("euclidean", "manhattan")

file = "./smtp.mat"
field = "classification"
metric = "euclidean"
method = "mclust"
algorithm = "average"
no_records = 4000  

data = H5Fopen(file)
anomaly_class = as_tibble(data$y)
attributes = head(as_tibble(data$X), no_records)
h5closeAll()
if(field=="clustering") {
  if(method == "kmeans") {
    # fviz_nbclust(smtp_attributes, kmeans, method = "wss")
    clusters = fviz_nbclust(attributes, kmeans, method = "silhouette")
    # fviz_nbclust(smtp_attributes, kmeans, nstart = 25,  method = "gap_stat", nboot = 50)
    # NbClust(smtp_attributes, method="kmeans")
    no_clusters = which.max(clusters$data$y)
    cluster_data = kmeans(attributes, no_clusters, algorithm=algorithm)
    fviz_cluster(cluster_data, data = attributes)
  } else if(method == "hcluster") {
    diss_matrix = dist(attributes, method=metric)
    hclust_avg = hclust(diss_matrix, method=algorithm)
    plot(hclust_avg)
  } else if(method == "cmeans") {
    clusters = fviz_nbclust(attributes, kmeans, method = "silhouette")
    no_clusters = which.max(clusters$data$y)
    cluster_data = cmeans(attributes, no_clusters, dist=metric)
    fviz_cluster(list(data = attributes, cluster=cluster_data$cluster),
                 ggtheme = theme_minimal())
  } else if(method == "dbscan") {
    diss_matrix = dist(attributes, method=metric)
    cluster_data = dbscan(diss_matrix, eps=0.875)
    fviz_cluster(list(data = attributes, cluster=cluster_data$cluster),
                 ggtheme = theme_minimal())
  } else if(method == "mclust") {
    BIC = mclustBIC(attributes)
    model = Mclust(attributes, x = BIC)
    plot(model, what = "classification")
  }
} else if (field=="classification"){
  distances = dist(attributes, method=metric)
  knns = kNN(distances, k=10, sort=FALSE)
  outlier_score = ln(COF(attributes, k=10))
  boxplot(outlier_score)
  boxplot.stats(outlier_score)$out
  plot(knns, attributes)
}
