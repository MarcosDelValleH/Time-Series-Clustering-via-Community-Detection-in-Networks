library(igraph)
library(visNetwork)

calculate_mst <- function(g) {
  # Calcular el árbol de recubrimiento mínimo (MST)
  mst <- mst(g)
  return(mst)
}

# Función para aplicar el algoritmo k-Spanning Tree
k_spanning_tree <- function(g, k) {
  # Calcular el MST
  mst <- calculate_mst(g)
  
  # Verificar que el MST tiene pesos
  if (length(E(mst)$weight) == 0) {
    stop("El MST no tiene pesos de arista. Asegúrate de que el grafo de entrada es ponderado.")
  }
  
  # Obtener los pesos de las aristas del MST
  mst_weights <- E(mst)$weight
  
  # Asegurarnos de que los pesos sean un vector numérico
  mst_weights <- as.numeric(mst_weights)
  
  # Ordenar las aristas por peso de mayor a menor
  mst_edges_sorted <- order(mst_weights, decreasing = TRUE)
  
  # Eliminar las k-aristas de mayor peso
  edges_to_remove <- mst_edges_sorted[1:k]
  mst <- delete_edges(mst, edges_to_remove)
  
  # Obtener las comunidades como componentes conexos
  communities <- components(mst)
  
  return(communities)
}

k <- 8
communities <- k_spanning_tree(net, k)

# Set up colors for each community
num_clusters <- length(unique(communities$membership))
cluster_colors <- rainbow(num_clusters, alpha = 0.7)

# Assign cluster colors and tooltip titles
V(net)$color <- cluster_colors[communities$membership]
V(net)$name <- pca_long$name  # Store athlete names as node names for tooltips
V(net)$size <- 5 + degree(net) * 0.5  # Size based on degree

# Convert igraph object to visNetwork data
network_data <- toVisNetworkData(net)

# Explicitly assign athlete names to the title attribute for hover display
network_data$nodes$title <- V(net)$name  # Tooltip text on hover
network_data$nodes$label <- NULL  # Hide labels by default

# Plot using visNetwork with hover functionality
visNetwork(network_data$nodes, network_data$edges) %>%
  visNodes(color = list(border = "black")) %>%
  visOptions(highlightNearest = list(enabled = TRUE, degree = 1, hover = TRUE),
             nodesIdSelection = TRUE) %>%
  visLayout(randomSeed = 42) %>%
  visPhysics(stabilization = TRUE)