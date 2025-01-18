# Transpose the matrix so that athletes are the rows, and segments are the columns
pca_matrix_t <- t(pca_matrix)

# Compute the correlation matrix between athletes
spearman_distance <- 1 - cor(t(pca_matrix), method = "spearman")



library(psych)
library(igraph)

distancematrix <- spearman_distance

library(igraph)

# Convertir la matriz de distancia a una lista de adyacencias k-NN
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


# Crear la matriz de adyacencia k-NN (k = 5)
knn_adj_matrix <- create_mutual_knn_graph(as.matrix(distancematrix), k = 9)
diag(knn_adj_matrix) <- 0

# Crear el grafo a partir de la matriz de adyacencia
net <- graph_from_adjacency_matrix(knn_adj_matrix, mode = "undirected")

plot(net,
     vertex.size = 5,         # Tamaño de los nodos (ajusta este valor según lo necesites)
     vertex.label.cex = 0.8,   # Tamaño de la etiqueta de los nodos (ajusta según lo necesites)
     vertex.color = "lightblue",  # Color de los nodos
     edge.color = "gray",      # Color de las aristas
     edge.width = 2,           # Ancho de las aristas
     main = "KNN_mutual; k=9")       # Título del grafo