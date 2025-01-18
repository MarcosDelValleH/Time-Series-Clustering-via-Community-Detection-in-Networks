library(igraph)

# Función para calcular el número de vecinos comunes entre dos nodos
shared_neighbors <- function(node1, node2, net) {
  # Obtener los vecinos de los nodos
  neighbors1 <- neighbors(net, node1)
  neighbors2 <- neighbors(net, node2)
  
  # Calcular la intersección de los vecinos
  length(intersect(neighbors1, neighbors2))
}

# Función para realizar clustering basado en Shared Nearest Neighbors (SNN)
cluster_snn <- function(net, tau) {
  # Número de nodos
  n <- vcount(net)
  
  # Crear una matriz de SNN
  snn_matrix <- matrix(0, n, n)
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      # Calcular el número de vecinos comunes
      snn_matrix[i, j] <- shared_neighbors(i, j, net)
      snn_matrix[j, i] <- snn_matrix[i, j]  # La matriz es simétrica
    }
  }
  
  # Construir el grafo SNN: si los nodos tienen al menos 'tau' vecinos en común, se conectan
  snn_graph <- graph.adjacency(snn_matrix >= tau, mode = "undirected", diag = FALSE)
  
  # Aplicar componentes conexas para detectar las comunidades
  communities <- components(snn_graph)
  
  return(communities)
}

# Ejemplo de uso
tau <- 6  # Umbral de vecinos comunes
communities <- cluster_snn(net, tau)


# Paso 4: Asignar colores a cada comunidad
num_clusters <- communities$no
cluster_colors <- rainbow(num_clusters, alpha = 0.7)

V(net)$color <- cluster_colors[communities$membership]
V(net)$name <- pca_long$name  # Usar nombres de atletas como nombres de nodos
V(net)$size <- 5 + degree(net) * 0.5  # Tamaño basado en el grado del nodo

# Paso 5: Convertir el grafo a formato visNetwork
network_data <- toVisNetworkData(net)

# Asignar atributos para mostrar tooltips al pasar el ratón
network_data$nodes$title <- V(net)$name  # Texto del tooltip
network_data$nodes$label <- NULL  # Ocultar etiquetas por defecto

# Paso 6: Graficar usando visNetwork
visNetwork(network_data$nodes, network_data$edges) %>%
  visNodes(color = list(border = "black")) %>%
  visOptions(highlightNearest = list(enabled = TRUE, degree = 1, hover = TRUE),
             nodesIdSelection = TRUE) %>%
  visLayout(randomSeed = 42) %>%
  visPhysics(stabilization = TRUE)
