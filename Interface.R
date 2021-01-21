require(tidyverse)
library(rhdf5)
library(factoextra)
library(NbClust)
library(e1071)
library(dbscan)
library(mclust)
library(DDoutlier)

#data_test & data_train - subsets of data, where data_test + data_train = data && data_test inner join data_train = 0
#data_train = subset of data
#data_test = subset of data

#metric = "euclidean"
#method = "kmeans"
#args = c("arg1", "arg2")
#dissimilarity_measure = "distance"
#outlier_threshold_train = 0.8
#outlier_threshold_test = 0.8


# dissimilarity measures should later include LOF, CBLOF, uCBLOF, LDCOF
# allowed metrics depends on used method

#args for kmeans: 1 - number_of_clusters, 2 - kmeans_algorithm
#args for dbscan: 1 - scan_distance, 2 - fraction_of_outliers



## @knitr nazwachunka

data_train_x = data_train %>% select(1:6)
data_train_y = data_train %>% select(7)
data_test_x = data_test %>% select(1:6)
data_test_y = data_test %>% select(7)

show_stats <- function(data){
  #roc curve
  pred = prediction(predictions=data$pred, labels=data$class)
  perf = performance(pred, "tpr", "fpr")
  plot(perf, main="ROC curve")
  abline(0,1, col="#0000ff")
  
  #f1-measure
  data = data %>% mutate(tp = class & pred_int)
  data = data %>% mutate(fp = !class & pred_int)
  data = data %>% mutate(tn = !class & !pred_int)
  data = data %>% mutate(fn = class & !pred_int)
  recall = tally(data, tp) / (tally(data, tp) + tally(data, fn))
  precision = tally(data, tp) / (tally(data, tp) + tally(data, fp))
  f_value = 2 * recall * precision / (recall + precision)
  print(paste("F-value:", f_value))
}

if(method == "kmeans") {
  
  cluster_data = kmeans(data_train_x, as.integer(args[1]), algorithm=args[2])
  plot(fviz_cluster(cluster_data, data = data_train_x))
  if(dissimilarity_measure == "distance") {
    cluster_centers = cluster_data$centers
    data = data_train
    data %<>% mutate(group = cluster_data$cluster) %>% mutate(dist = sqrt((V1-cluster_centers[group, 1])^2 + (V2-cluster_centers[group, 2])^2 + (V3-cluster_centers[group, 3])^2 + (V4-cluster_centers[group, 4])^2)) %>% arrange(dist)
    data$dist = data$dist / max(data$dist)
    data = data %>% mutate(pred = dist)
    data = data %>% mutate(pred_int = as.integer(dist > outlier_threshold_train))
    plot(data$dist, main="train set dissimilarity distribution")
    
    predict.kmeans <- function(object, newdata){
      centers <- object$centers
      n_centers <- nrow(centers)
      dist_mat <- as.matrix(dist(rbind(centers, newdata)))
      dist_mat <- dist_mat[-seq(n_centers), seq(n_centers)]
      max.col(-dist_mat)
    }
    
    groups = predict(cluster_data, data_test_x)
    data = data_test %>% mutate(group = groups)
    data %<>% mutate(dist = sqrt((V1-cluster_centers[group, 1])^2 + (V2-cluster_centers[group, 2])^2 + (V3-cluster_centers[group, 3])^2 + (V4-cluster_centers[group, 4])^2)) %>% arrange(dist)
    data$dist = data$dist / max(data$dist)
    data = data %>% mutate(pred = dist)
    data = data %>% mutate(pred_int = as.integer(dist > outlier_threshold_test))
    plot(data$dist, main="test set dissimilarity distribution")
    
    show_stats(data)
  }
  
} else if(method == "dbscan") {
  
  diss_matrix = dist(data_train_x, method=metric)
  cluster_data = dbscan(diss_matrix, eps=as.numeric(args[1]))
  plot(
    fviz_cluster(
      list(data = data_train_x, 
      cluster=cluster_data$cluster),
      ggtheme = theme_minimal()
    )
  )
  
  data = data_test
  
  if(dissimilarity_measure == 'DB') {
    diss = data %>% select(1:6)
    data$pred = DB(diss, d = as.numeric(args[1]), fraction = as.numeric(args[2]))$classification
    data = data %>% mutate(pred = as.integer(pred == 'Outlier'))
    data = data %>% mutate(pred_int = as.integer(pred == 'Outlier'))
  }
  
  
  show_stats(data)
  
} 
