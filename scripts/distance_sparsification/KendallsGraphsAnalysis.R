create_mutual_knn_graph <- function(dist_matrix, k) {
  n <- nrow(dist_matrix)
  adj_matrix <- matrix(0, n, n)
  
  for (i in 1:n) {
    # Encontrar los índices de los k vecinos más cercanos de i
    neighbors_i <- order(dist_matrix[i, ])[1:k]
    for (j in neighbors_i) {
      # Verificar si i está entre los k vecinos más cercanos de j
      neighbors_j <- order(dist_matrix[j, ])[1:k]
      if (i %in% neighbors_j) {
        adj_matrix[i, j] <- 1
        adj_matrix[j, i] <- 1 # Hacerlo simétrico
      }
    }
  }
  
  return(adj_matrix)
}

create_knn_graph <- function(dist_matrix, k) {
  n <- nrow(dist_matrix)
  adj_matrix <- matrix(0, n, n)
  
  for (i in 1:n) {
    # Encontrar los índices de los k vecinos más cercanos
    neighbors <- order(dist_matrix[i, ])[1:k]
    adj_matrix[i, neighbors] <- 1
  }
  
  # Asegurarse de que la matriz sea simétrica
  adj_matrix <- pmax(adj_matrix, t(adj_matrix))
  return(adj_matrix)
}




# Transpose the matrix so that athletes are the rows, and segments are the columns
pca_matrix_t <- t(pca_matrix)

# Compute the correlation matrix between athletes
kendall_distance <- 1 - cor(t(pca_matrix), method = "kendall", use = "pairwise.complete.obs")


library(psych)
library(igraph)

distancematrix <- kendall_distance

hiperparameterAux = 50

coef_clust_knn <- numeric(hiperparameterAux)
coef_clust_knn_mutual <- numeric(hiperparameterAux)
coef_clust_threshold <- numeric(hiperparameterAux)

connected_componentst_knn <- numeric(hiperparameterAux)
connected_componentst_knn_mutual <- numeric(hiperparameterAux)
connected_componentst_threshold <- numeric(hiperparameterAux)

avg_degree_knn <- numeric(hiperparameterAux)
avg_degree_knn_mutual <- numeric(hiperparameterAux)
avg_degree_threshold <- numeric(hiperparameterAux)

diameter_value_knn <- numeric(hiperparameterAux)
diameter_value_knn_mutual <- numeric(hiperparameterAux)
diameter_value_threshold <- numeric(hiperparameterAux)

avg_path_length_knn <- numeric(hiperparameterAux)
avg_path_length_knn_mutual <- numeric(hiperparameterAux)
avg_path_length_threshold <- numeric(hiperparameterAux)

aux = 0.3

for(i in 1:hiperparameterAux){
  
  adj_matrix_knn <- create_knn_graph(as.matrix(distancematrix), k = i)
  adj_matrix_knn_mutual <- create_mutual_knn_graph(as.matrix(distancematrix), k = i)
  adj_matrix_threshold <- as.matrix(distancematrix)
  adj_matrix_threshold <- ifelse(distancematrix < aux, 1, 0)
  diag(adj_matrix_knn) <- 0
  diag(adj_matrix_knn_mutual) <- 0
  diag(adj_matrix_threshold) <- 0
  
  g_knn <- graph_from_adjacency_matrix(adj_matrix_knn, mode = "undirected")
  g_knn_mutual <- graph_from_adjacency_matrix(adj_matrix_knn_mutual, mode = "undirected")
  g_threshold <- graph_from_adjacency_matrix(adj_matrix_threshold, mode = "undirected", diag = FALSE)
  
  
  coef_clust_knn[i] <-  transitivity(g_knn, type = "global")
  coef_clust_knn_mutual[i] <- transitivity(g_knn_mutual, type = "global")  
  coef_clust_threshold[i] <- transitivity(g_threshold, type = "global")
  
  connected_componentst_knn[i] <- components(g_knn)$no
  connected_componentst_knn_mutual[i] <- components(g_knn_mutual)$no
  connected_componentst_threshold[i] <- components(g_threshold)$no
  
  avg_degree_knn[i] <- mean(degree(g_knn))
  avg_degree_knn_mutual[i] <- mean(degree(g_knn_mutual))
  avg_degree_threshold[i] <- mean(degree(g_threshold))
  
  diameter_value_knn[i] <- diameter(g_knn, directed = FALSE)
  diameter_value_knn_mutual[i] <- diameter(g_knn_mutual, directed = FALSE)
  diameter_value_threshold[i] <- diameter(g_threshold, directed = FALSE)
  
  avg_path_length_knn[i] <- mean_distance(g_knn, directed = FALSE)
  avg_path_length_knn_mutual[i] <- mean_distance(g_knn_mutual, directed = FALSE)
  avg_path_length_threshold[i] <- mean_distance(g_threshold, directed = FALSE)
  
  
  aux = aux+0.02
}

library(ggplot2)
library(gridExtra)  # Para organizar múltiples gráficos

# Crear data frames individuales para cada métrica
data_clustering <- data.frame(
  k = 1:length(coef_clust_knn),
  k_Nearest = coef_clust_knn,
  k_Nearest_Mutual = coef_clust_knn_mutual,
  Threshold = coef_clust_threshold
)

data_connected <- data.frame(
  k = 1:length(connected_componentst_knn),
  k_Nearest = connected_componentst_knn,
  k_Nearest_Mutual = connected_componentst_knn_mutual,
  Threshold = connected_componentst_threshold
)

data_degree <- data.frame(
  k = 1:length(avg_degree_knn),
  k_Nearest = avg_degree_knn,
  k_Nearest_Mutual = avg_degree_knn_mutual,
  Threshold = avg_degree_threshold
)

data_diameter <- data.frame(
  k = 1:length(diameter_value_knn),
  k_Nearest = diameter_value_knn,
  k_Nearest_Mutual = diameter_value_knn_mutual,
  Threshold = diameter_value_threshold
)

data_path <- data.frame(
  k = 1:length(avg_path_length_knn),
  k_Nearest = avg_path_length_knn,
  k_Nearest_Mutual = avg_path_length_knn_mutual,
  Threshold = avg_path_length_threshold
)

# Crear funciones para gráficos individuales
plot_metric <- function(data, title, y_label) {
  ggplot(data, aes(x = k)) +
    geom_line(aes(y = k_Nearest, color = "k-Nearest")) +
    geom_line(aes(y = k_Nearest_Mutual, color = "k-Nearest-Mutual")) +
    geom_line(aes(y = Threshold, color = "Threshold")) +
    geom_point(aes(y = k_Nearest, color = "k-Nearest")) +
    geom_point(aes(y = k_Nearest_Mutual, color = "k-Nearest-Mutual")) +
    geom_point(aes(y = Threshold, color = "Threshold")) +
    labs(title = title, x = "k", y = y_label, color = "Legend") +
    theme_minimal() +
    theme(legend.position = "bottom")
}

# Crear gráficos para cada métrica
plot1 <- plot_metric(data_clustering, "Clustering Coefficient", "Value")
plot2 <- plot_metric(data_connected, "Connected Components", "Count")
plot3 <- plot_metric(data_degree, "Average Degree", "Value")
plot4 <- plot_metric(data_diameter, "Diameter Value", "Value")
plot5 <- plot_metric(data_path, "Average Path Length", "Value")

# Mostrar gráficos en una cuadrícula
grid.arrange(plot1, plot2, plot3, plot4, plot5, ncol = 2)