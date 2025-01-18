library(ggplot2)
library(dplyr)
library(igraph)

# Paso 1: Filtrar y preparar los datos con clusters
pca_data_with_clusters <- pca_data %>%
  inner_join(data.frame(name = pca_long$name, cluster = communities$membership), by = "name")

# Paso 2: Calcular el grado de cada nodo en el grafo
degree_data <- data.frame(name = V(net)$name, degree = degree(net))

# Paso 3: Unir la información de grado con los datos de clusters
pca_data_with_degrees <- pca_data_with_clusters %>%
  inner_join(degree_data, by = "name")

# Paso 4: Identificar los centroides por cluster (nodos con mayor grado)
centroid_data <- pca_data_with_degrees %>%
  group_by(cluster) %>%
  filter(degree == max(degree)) %>%
  dplyr::select(cluster, number, relative_time_improvement)

# Paso 5: Definir colores para los clusters
num_clusters <- length(unique(communities$membership))
cluster_colors <- rainbow(num_clusters, alpha = 0.7)

# Paso 6: Graficar las series temporales con overlay de clusters
ggplot() +
  # Líneas de series temporales de los atletas con transparencia
  geom_smooth(data = pca_data_with_clusters, aes(x = number, y = relative_time_improvement, group = name, color = as.factor(cluster)), 
              method = "loess", se = FALSE, size = 0.5, alpha = 0.1) +
  # Líneas suavizadas de los centroides
  geom_smooth(data = centroid_data, aes(x = number, y = relative_time_improvement, color = as.factor(cluster)), 
              method = "loess", se = TRUE, size = 1.5) +
  # Personalización de colores
  scale_color_manual(values = cluster_colors) +
  # Facetado por cluster
  facet_wrap(~ cluster, scales = "free_y", ncol = 3) +  # Ajustar ncol para el diseño deseado
  # Estética adicional
  labs(title = "Time Series by Cluster (1500m Races)",
       x = "Segment",
       y = "Relative Time Improvement",
       color = "Cluster") +
  theme_minimal() +
  theme(legend.position = "right", 
        strip.text = element_text(face = "bold", size = 10),
        axis.text = element_text(size = 8),
        panel.grid = element_blank())  # Opcional: Eliminar la cuadrícula para un diseño más limpio


ggsave("output/cluster_analysis/output_graph.png", width = 10, height = 6, dpi = 300)