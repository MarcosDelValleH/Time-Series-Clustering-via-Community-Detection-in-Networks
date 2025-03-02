library(igraph)

cosine_similarity <- coop::cosine(t(pca_matrix))
cosine_distance <- 1 - cosine_similarity
distancematrix <- cosine_distance

net <- graph_from_adjacency_matrix(distancematrix, mode = "undirected", weighted= TRUE)

# Eliminamos las aristas con menos distancia para que louvain funcione
threshold <- quantile(E(net)$weight, 0)  # Mantener el 25% más alto
net <- delete_edges(net, E(net)[weight < threshold])

plot(net,
     vertex.size = 5,         # Tamaño de los nodos (ajusta este valor según lo necesites)
     vertex.label.cex = 0.8,   # Tamaño de la etiqueta de los nodos (ajusta según lo necesites)
     vertex.color = "lightblue",  # Color de los nodos
     edge.color = "gray",      # Color de las aristas
     edge.width = 2,           # Ancho de las aristas
     main = "Cosene graph")       # Título del grafo