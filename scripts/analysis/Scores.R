library(cluster)
library(clusterCrit) # Para Davies-Bouldin Index
library(stats) # Para cmdscale

# Asegúrate de que la matriz de distancias tiene valores finitos
if (!all(is.finite(as.vector(distancematrix)))) {
  stop("La matriz de distancias contiene valores no finitos. Revisa el cálculo.")
}

# Extraer las etiquetas de las comunidades
labels <- membership(communities)

# Calcular el puntaje Silhouette
silhouette_scores <- silhouette(labels, dist(distancematrix))

# Calcular el promedio del Silhouette Score
silhouette_avg <- mean(silhouette_scores[, "sil_width"])

# Mostrar el resultado
print(paste("Silhouette Score:", silhouette_avg))


data <- cmdscale(distancematrix, k = 2) # k es el número de dimensiones a reconstruir

# Davies-Bouldin Index
db_index <- intCriteria(as.matrix(data), as.integer(labels), "Davies_Bouldin")
print(paste("Davies-Bouldin Index:", db_index[[1]]))

# Calinski-Harabasz Index
ch_index <- intCriteria(as.matrix(data), as.integer(labels), "Calinski_Harabasz")
print(paste("Calinski-Harabasz Index:", ch_index[[1]]))

cat("&", sprintf("%.4f", silhouette_avg), 
    "&", sprintf("%.4f", db_index[[1]]), 
    "&", sprintf("%.4f", ch_index[[1]]), "\\\\\n") # formato para copiar en la tabla de latex
