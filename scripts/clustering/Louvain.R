library(igraph)
library(visNetwork)
library(ggraph)
library(ggplot2)
# Community detection using Louvain algorithm
communities <- cluster_louvain(net)

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


ggraph(net, layout = "fr") +  # Usa layout de Fruchterman-Reingold
  geom_edge_link(aes(alpha = 0.5), color = "grey") +  # Enlaces en gris
  geom_node_point(aes(color = as.factor(communities$membership), size = degree(net))) +  # Nodos coloreados por cluster
  scale_color_manual(values = cluster_colors) +  # Escala de colores personalizada
  theme_void() +  # Tema limpio sin ejes
  ggtitle("") +
  theme(legend.position = "none")  # Ocultar la leyenda