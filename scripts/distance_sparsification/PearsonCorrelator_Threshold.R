# Transpose the matrix so that athletes are the rows, and segments are the columns
pca_matrix_t <- t(pca_matrix)

# Compute the correlation matrix between athletes
correlation_matrix <- cor(pca_matrix_t, use = "pairwise.complete.obs", method = "pearson")


library(psych)
library(igraph)

distancematrix <- cor2dist(correlation_matrix)

DM1  <- ifelse(distancematrix < 1.06, 1, 0)
diag(DM1) <- 0
## Zero out connections where there is low (absolute) correlation
## Keeps connection for cor ~ -1


# Esto tambien lo comento pq ns q es: source("tsClustAlgs.R")

net <- graph.adjacency(DM1, mode = "undirected", weighted = TRUE, diag = TRUE)
# get the net layout, just to plot the network nodes in the same position
net_layout = layout_components(net)
# plot the network
plot(net,
     vertex.size = 5,         # Tamaño de los nodos (ajusta este valor según lo necesites)
     vertex.label.cex = 0.8,   # Tamaño de la etiqueta de los nodos (ajusta según lo necesites)
     vertex.color = "lightblue",  # Color de los nodos
     edge.color = "gray",      # Color de las aristas
     edge.width = 2,           # Ancho de las aristas
     main = "Threshold; t=1.06")       # Título del grafo