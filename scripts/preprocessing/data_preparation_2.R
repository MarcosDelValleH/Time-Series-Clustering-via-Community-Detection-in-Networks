races<- races %>%
  filter(competition == '1500m') %>%
  filter(error_tag == 0)

races_id <- races$id

races


laps_id <- race_athlete %>%
  filter(race_id %in% races_id)

# Change time to seconds

# No se muy bien para que vale esto pero de momento lo voy a comentar: laps_id$time <- as.numeric(sapply(laps_id$time, convert_time))




# Step 1: Filter and join laps_id to laps, ensuring athlete and race IDs are included
laps_1500 <- laps %>%
  filter(race_athlete_id %in% laps_id$id) %>%
  left_join(laps_id %>% dplyr::select(id, race_id), by = c("race_athlete_id" = "id")) %>%
  left_join(laps_id %>% dplyr::select(id, athlete_id), by = c("race_athlete_id" = "id")) %>%
  dplyr::select (-c(race_athlete_id,id))

laps_1500$time <- as.numeric(laps_1500$time)




# Step 2: Summarize the total time of previous laps for each athlete in each race
laps_summary <- laps_1500 %>%
  group_by(athlete_id, race_id) %>%
  summarise(total_previous_time = sum(as.numeric(time), na.rm = TRUE))  # sum time for each athlete per race




library(tidyverse)

# Merge laps_1500 with athletes on athlete_id
merged_data <- laps_1500 %>%
  left_join(athletes, by = c("athlete_id" = "id")) %>%
  left_join(races %>% dplyr::select(id,temperature,humidity,conditions,competition,stadium_id,round), by = c("race_id" = "id")) %>%
  left_join(races %>% dplyr::select(id,temperature,humidity,conditions,competition,stadium_id,round), by = c("race_id" = "id")) %>%
  filter(time >= 10 & time <= 30) # Filter out wrong data

merged_data <- merged_data %>%
  mutate(sex = case_when(
    sex == "Women" ~ "FEMALE",
    sex == "Men" ~ "MALE",
    TRUE ~ sex
  ))






ids <- c("14653717", "14905834", "14533464", "14798735", "14643526", "15092355", "14970747", "14910035", "14705412", "14513077", "14701882", "14910032", "14640588", "14756630", "14997479", "14910032", "14457638", "14997479", "14756630")  # Sustituye por los nombres que desees

# Filter athletes by id
filtered_races <- merged_data %>%
  filter(world_athletics_id %in% ids)  # Quedarse solo con los nombres seleccionados

pca_data <- filtered_races %>%
  filter(!is.na(sex)) %>%
  filter(sex == 'MALE' ) %>%
  filter(number %in% 1:15) %>%
  group_by(athlete_id, race_id) %>%
  mutate(relative_time_improvement = c(0,diff(time)),
         name = paste(firstname,lastname,race_id,sep = " ")) %>% # Calculate relative improvement
  ungroup() 


# Save each race
pca_long <- pca_data %>%
  group_by(name, number) %>%
  summarise(relative_time_improvement = first(na.omit(relative_time_improvement))) %>%
  ungroup() %>%
  pivot_wider(names_from = number, values_from = relative_time_improvement, names_prefix = "Segment_")%>%
  dplyr::select(-Segment_1)


pca_long[is.na(pca_long)] <- 0

# Create PCA matrix
pca_matrix <- pca_long %>%
  as.data.frame()

# Set names as rows
row.names(pca_matrix) <- pca_long$name
pca_matrix <- pca_matrix[, -1]  # Remove 'name' column from matrix

















