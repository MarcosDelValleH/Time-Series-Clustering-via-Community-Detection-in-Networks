# Transpose the matrix so that athletes are the rows, and segments are the columns
pca_matrix_t <- t(pca_matrix)
library(coop)
cosine_similarity <- coop::cosine(t(pca_matrix))
cosine_distance <- 1 - cosine_similarity



library(psych)
library(igraph)

distancematrix <- cosine_distance

#DM1  <- ifelse(distancematrix < 0.58, 1, 0) # Data set 1
DM1  <- ifelse(distancematrix < 0.45, 1, 0) # Data set 2
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
     main = "Threshold; t=0.58")       # Título del grafo