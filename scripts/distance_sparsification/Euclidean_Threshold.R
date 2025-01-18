# Transpose the matrix so that athletes are the rows, and segments are the columns
pca_matrix_t <- t(pca_matrix)

# Compute the correlation matrix between athletes
correlation_matrix <- cor(pca_matrix_t, use = "pairwise.complete.obs", method = "pearson")


library(psych)
library(igraph)

distance_matrix <- dist(pca_matrix, method = "euclidean")
distance_matrix <- as.matrix(distance_matrix)
# Esto crea la matriz de distancias, ahoar necesitamos un criterio para obtener un grafo. Por ejemplo probamos con k-nearest


threshold <-2  # Ajusta según el rango de distancias
adjacency_matrix <- as.matrix(distance_matrix) <= threshold
g <- graph_from_adjacency_matrix(adjacency_matrix, mode = "undirected")


DM1 <- as.matrix(g)
## Zero out connections where there is low (absolute) correlation
## Keeps connection for cor ~ -1

# Esto tambien lo comento pq ns q es: source("tsClustAlgs.R")

net <- graph.adjacency(DM1, mode = "undirected", weighted = TRUE, diag = TRUE)
# get the net layout, just to plot the network nodes in the same position
net_layout = layout_components(net)
# plot the network
plot(net, layout=layout.fruchterman.reingold)