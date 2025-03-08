library(dplyr)

# Función para calcular la entropía de Shannon
calculate_entropy <- function(x) {
  probs <- table(x) / length(x)  # Probabilidades de cada comunidad
  -sum(probs * log2(probs), na.rm = TRUE)  # Fórmula de entropía de Shannon
}

# Definir athlete_race_mapping con la relación atleta-carrera
athlete_race_mapping <- pca_data %>%
  select(athlete_id, race_id) %>%
  distinct()  # Asegurar que no haya duplicados

# Unir con los nombres de los atletas
athlete_race_mapping <- athlete_race_mapping %>%
  left_join(pca_data %>% select(athlete_id, firstname, lastname), by = "athlete_id") %>%
  mutate(race_id = paste(firstname, lastname, race_id, sep = " ")) %>%
  select(athlete_id, race_id)  # Mantener solo las columnas necesarias

# Extraer asignaciones de comunidad por carrera
race_communities <- data.frame(
  race_id = pca_long$name,  # Los nombres en pca_long corresponden a las carreras
  community = communities$membership  # Asignación de cluster
)

# Hacer el left join para asignar comunidades a los atletas
athlete_communities <- athlete_race_mapping %>%
  left_join(race_communities, by = "race_id")

# Asegurar que cada atleta tiene una única fila por carrera
athlete_communities <- athlete_communities %>%
  distinct(athlete_id, race_id, community)  # Elimina filas repetidas

# Calcular la pureza y entropía por atleta
athlete_metrics <- athlete_communities %>%
  group_by(athlete_id) %>%
  summarise(
    total_races = n(),  # Número total de carreras del atleta
    most_common_community = names(which.max(table(community))),  # Comunidad predominante
    races_in_common_community = max(table(community)),  # Número de carreras en la comunidad predominante
    purity = races_in_common_community / total_races,  # Pureza del atleta
    entropy = calculate_entropy(community)  # Entropía de Shannon
  )

# Mostrar pureza y entropía
print(athlete_metrics)
