# Transpose the matrix so that athletes are the rows, and segments are the columns
pca_matrix_t <- t(pca_matrix)

# Compute the correlation matrix between athletes
correlation_matrix <- cor(pca_matrix_t, use = "pairwise.complete.obs", method = "pearson")


library(psych)
library(igraph)

distancematrix <- cor2dist(correlation_matrix)

library(igraph)

# Convertir la matriz de distancia a una lista de adyacencias k-NN
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

# Crear la matriz de adyacencia k-NN (k = 5)
knn_adj_matrix <- create_knn_graph(as.matrix(distancematrix), k = 5)
diag(knn_adj_matrix) <- 0
# Crear el grafo a partir de la matriz de adyacencia
net <- graph_from_adjacency_matrix(knn_adj_matrix, mode = "undirected")


plot(net,
     vertex.size = 5,         # Tamaño de los nodos (ajusta este valor según lo necesites)
     vertex.label.cex = 0.8,   # Tamaño de la etiqueta de los nodos (ajusta según lo necesites)
     vertex.color = "lightblue",  # Color de los nodos
     edge.color = "gray",      # Color de las aristas
     edge.width = 2,           # Ancho de las aristas
     main = "KNN; n=5")       # Título del grafo