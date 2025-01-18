library(ggplot2)
library(dplyr)
library(igraph)

# Merge cluster information into the time series data
pca_data_with_clusters <- pca_data %>%
  inner_join(data.frame(name = pca_long$name, cluster = communities$membership), by = "name")

# Calculate the degree for each node in the graph
degree_data <- data.frame(name = V(net)$name, degree = degree(net))

# Merge degree information into the data with clusters
pca_data_with_degrees <- pca_data_with_clusters %>%
  inner_join(degree_data, by = "name")

# Identify the centroid for each cluster (node with the highest degree)
centroid_data <- pca_data_with_degrees %>%
  group_by(cluster) %>%
  filter(degree == max(degree)) %>%
  dplyr::select(cluster, number, relative_time_improvement)

# Define colors for each cluster (use the same colors as in your network plot)
num_clusters <- length(unique(communities$membership))
cluster_colors <- rainbow(num_clusters, alpha = 0.7)

# Plot the time series with cluster overlay
ggplot() +
  # Plot athletes' time series lines with transparency
  geom_smooth(data = pca_data_with_clusters, aes(x = number, y = relative_time_improvement, group = name, color = as.factor(cluster)), method = "loess", se = FALSE, size = 0.5, alpha = 0.1) +
  # Plot smoothed centroid lines using nodes with the highest degree
  geom_smooth(data = centroid_data, aes(x = number, y = relative_time_improvement, color = as.factor(cluster)), method = "loess", se = TRUE, size = 1.5) +
  # Customizing the colors
  scale_color_manual(values = cluster_colors) +
  # Facet by cluster with horizontal arrangement
  facet_wrap(~ cluster, scales = "free_y", ncol = 3) +  # Adjust ncol for desired layout
  # Additional aesthetics
  labs(title = "Time Series by Cluster using Pearson Correlation + Threshold",
       x = "Segment",
       y = "Relative Time Improvement",
       color = "Cluster") +
  theme_minimal() +
  theme(legend.position = "right", 
        strip.text = element_text(face = "bold", size = 10),
        axis.text = element_text(size = 8),
        panel.grid = element_blank())  # Optionally remove grid for a cleaner look