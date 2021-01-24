require(tidyverse)
library(rhdf5)
library(factoextra)
library(NbClust)
library(e1071)
library(dbscan)
library(mclust)
library(DDoutlier)
require(ROCR)

#data_test & data_train - subsets of data, where data_test + data_train = data && data_test inner join data_train = 0
#data_train = subset of data
#data_test = subset of data

#Example parameters

#metric = "euclidean"
#method = "kmeans"
#dissimilarity_measure = "CBLOF"
#outlier_threshold = 0.8
#algorithm = "Hartigan-Wong"
#no_clusters = 5

#Dissimilarity measures:
#CBLOF : (odleg³oœæ od najbli¿szego centroidu) * (rozmiar grupy tego centroidu)
#uCBLOR: (odleg³oœæ od najbli¿szego centroidu)
#LDCOF: (odleg³oœæ od najbli¿szego centroidu) / (œrednie odleg³oœci od tego centroidu)

# dissimilarity measures should later include LOF, CBLOF, uCBLOF, LDCOF
# allowed metrics depends on used method

#args for kmeans: 1 - number_of_clusters, 2 - kmeans_algorithm
#args for dbscan: 1 - scan_distance, 2 - fraction_of_outliers


f_value <- function (cm) {
  fp = cm[1,2]
  fn = cm[2,1]
  tp = cm[2,2]
  recall = tp / (tp+fn)
  precision = tp / (tp+fp)
  f = (2 * recall * precision) / (recall + precision)
  return(f)
}


predict.kmeans <- function(object, newdata, metric){
  centers <- object$centers
  n_centers <- nrow(centers)
  dist_mat <- as.matrix(dist(rbind(centers, newdata), method=metric))
  dist_mat <- dist_mat[-seq(n_centers), seq(n_centers)]
  max.col(-dist_mat)
}

normalize_outlier_score <- function (data) {
  data$outlier_score / max(data$outlier_score)
}

## @knitr nazwachunka

data_train_x = data_train %>% select(1:6)
data_train_y = data_train %>% select(7)
data_test_x = data_test %>% select(1:6)
data_test_y = data_test %>% select(7)

show_stats <- function(data){
  #prepare data
  data$outlier_score = normalize_outlier_score(data)
  data = data %>% mutate(pred = outlier_score)
  data = data %>% mutate(pred_int = as.integer(outlier_score > outlier_threshold))
  data = data %>% arrange(outlier_score)

  #dissimilarity measure distribution
  plot(data$outlier_score, main="dissimilarity distribution")
  
  #roc curve
  pred = prediction(predictions=data$pred, labels=data$class)
  perf = performance(pred, "tpr", "fpr")
  plot(perf, main="ROC curve")
  abline(0,1,col="#000000", lty=2)
  
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
  
  cluster_data = kmeans(data_train_x, no_clusters, algorithm=algorithm)
  plot(fviz_cluster(cluster_data, data = data_train_x))
  groups = predict.kmeans(cluster_data, data_test_x, metric)
  data = data_test %>% mutate(group = groups)
  
} else if(method == "cmeans") {
  
  cluster_data = cmeans(data_train_x, no_clusters, dist=metric)
  plot(fviz_cluster(list(data = data_train_x, cluster=cluster_data$cluster), ggtheme = theme_minimal()))
  data = data_test
  centers = cluster_data$centers
  data$group = cmeans(data_test_x, centers, dist=metric)$cluster
}

cluster_centers = cluster_data$centers

if(dissimilarity_measure == "uCBLOF") {
  
  data = data %>% mutate(outlier_score = sqrt((V1-cluster_centers[group, 1])^2 + (V2-cluster_centers[group, 2])^2 + (V3-cluster_centers[group, 3])^2 + (V4-cluster_centers[group, 4])^2 + (V5-cluster_centers[group, 5])^2 + (V6-cluster_centers[group, 6])^2))
  
} else if(dissimilarity_measure == "LDCOF") {
  
  data = data %>% mutate(dist = sqrt((V1-cluster_centers[group, 1])^2 + (V2-cluster_centers[group, 2])^2 + (V3-cluster_centers[group, 3])^2 + (V4-cluster_centers[group, 4])^2 + (V5-cluster_centers[group, 5])^2 + (V6-cluster_centers[group, 6])^2))
  average_dist = data %>% group_by(group) %>% summarise(avg_dist = mean(dist))
  data = data %>% mutate(outlier_score = dist / average_dist[group,2]$avg_dist)
  
} else if(dissimilarity_measure == "CBLOF") {
  
  data = data %>% mutate(dist = sqrt((V1-cluster_centers[group, 1])^2 + (V2-cluster_centers[group, 2])^2 + (V3-cluster_centers[group, 3])^2 + (V4-cluster_centers[group, 4])^2 + (V5-cluster_centers[group, 5])^2 + (V6-cluster_centers[group, 6])^2))
  group_count = data %>% group_by(group) %>% summarise(count = n())
  data = data %>% mutate(outlier_score = dist * group_count[group,2]$count)
  
}
show_stats(data)

#} else if(method == "dbscan") {
#  diss_matrix = dist(data_train_x, method=metric)
#  cluster_data = dbscan(diss_matrix, eps=as.numeric(args[1]))
#  plot(
#    fviz_cluster(
#      list(data = data_train_x, 
#      cluster=cluster_data$cluster),
#      ggtheme = theme_minimal()
#    )
#  )
#  data = data_test
#  if(dissimilarity_measure == 'DB') {
#    diss = data %>% select(1:6)
#    data$pred = DB(diss, d = as.numeric(args[1]), fraction = as.numeric(args[2]))$classification
#    data = data %>% mutate(pred = as.integer(pred == 'Outlier'))
#    data = data %>% mutate(pred_int = as.integer(pred == 'Outlier'))
#  }
#  show_stats(data)
#} 
