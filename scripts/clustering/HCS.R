library(igraph)
library(visNetwork)

# Función para encontrar el corte mínimo
find_min_cut <- function(g) {
  # Usamos la función min_cut para obtener información sobre el corte mínimo
  min_cut_result <- min_cut(g, value.only = FALSE)
  return(list(value = min_cut_result$value, partition = min_cut_result$partition))
}

# Función para determinar si un grafo es altamente conexo
is_highly_connected <- function(g) {
  # Obtener el corte mínimo
  min_cut_result <- find_min_cut(g)
  
  # Extraer las particiones del corte mínimo
  cut_partition <- min_cut_result$partition
  
  # Verificar si alguna partición tiene más de la mitad de los nodos
  if (max(lengths(cut_partition)) > vcount(g) / 2) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

# Función recursiva para HCS Clustering
hcs_clustering <- function(g) {
  # Almacenar las comunidades
  communities <- list()
  
  # Si el grafo es altamente conexo, es una comunidad
  if (is_highly_connected(g)) {
    communities <- list(g)
    return(communities)
  } else {
    # Si no es altamente conexo, encontrar el corte mínimo y separar el grafo
    min_cut_result <- find_min_cut(g)
    
    # Obtener las particiones del corte
    partitions <- min_cut_result$partition
    subgraph_1 <- induced_subgraph(g, partitions[[1]])
    subgraph_2 <- induced_subgraph(g, partitions[[2]])
    
    # Aplicar recursivamente el algoritmo a los subgrafos
    communities <- c(communities, hcs_clustering(subgraph_1))
    communities <- c(communities, hcs_clustering(subgraph_2))
    
    return(communities)
  }
}

communities <- hcs_clustering(net)
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