library(tidyverse)
library(readr)
library(dplyr)
library(ggplot2)
library(corrplot)
library(RColorBrewer)
library(reshape2)
library(cluster)  # For clustering algorithms
install.packages("factoextra")
library(factoextra)  # For visualizing clusters

# Load the data
spotify_data = read.csv("spotify-2023.csv")

# Check the structure of the dataset
str(spotify_data)

# Renaming columns by specifying old_name = new_name
spotify_data <- spotify_data %>%
  rename(artist = artist.s._name,
         valance = valence_.,
         energy = energy_.,
         danceability = danceability_.,
         acousticness = acousticness_.,
         instrumentalness = instrumentalness_.,
         liveness = liveness_.,
         speechiness = speechiness_.)


# Check for unique non-numeric values 
unique(grep("[^0-9]", spotify_data$in_shazam_charts, value = TRUE))
unique(grep("[^0-9]", spotify_data$in_deezer_playlists, value = TRUE))
unique(grep("[^0-9]", spotify_data$streams, value = TRUE))

# Clean the data and convert to integers
spotify_data1 <- spotify_data %>%
  mutate(
    in_shazam_charts = as.integer(gsub(",", "", in_shazam_charts)),
    in_deezer_playlists = as.integer(gsub(",", "", in_deezer_playlists)),
    streams = as.integer(gsub(",", "", streams))
  )

# Check the structure to ensure the conversion was successful
str(spotify_data1)

# Check for unique non-numeric values 
unique(grep("[^0-9]", spotify_data1$in_shazam_charts, value = TRUE))
unique(grep("[^0-9]", spotify_data1$in_deezer_playlists, value = TRUE))
unique(grep("[^0-9]", spotify_data1$streams, value = TRUE))

#Visualizaion Part

# Visualization 1
# Top 20
# Filter for top 20 songs of all time
top_songs_all_time <- spotify_data1 %>%
  arrange(desc(streams)) %>%
  head(20)

# Filter for top 20 songs of 2023
top_songs_2023 <- spotify_data1 %>%
  filter(released_year == 2023) %>%
  arrange(desc(streams)) %>%
  head(20)

# Plot for top 20 songs of all time
ggplot(top_songs_all_time, aes(x = reorder(track_name, streams), y = streams, fill = artist)) +
  geom_bar(stat = "identity") +
  labs(title = "Top 20 Best Songs of All Time by Streams", x = "Song", y = "Streams") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Plot for top 20 songs of 2023
ggplot(top_songs_2023, aes(x = reorder(track_name, streams), y = streams, fill = artist)) +
  geom_bar(stat = "identity") +
  labs(title = "Top 20 Best Songs of 2023 by Streams", x = "Song", y = "Streams") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Visualization 2
# Top 10 Collaboration 
# Prepare the data by aggregating artist names and calculating total streams per song
prepared_data <- spotify_data1 %>%
  group_by(track_name) %>%
  summarise(
    artists = paste(unique(artist), collapse = ", "),
    total_streams = sum(streams, na.rm = TRUE),
    artist_count = n_distinct(artist),
    .groups = 'drop'
  ) %>%
  filter(artist_count > 1)  # focus on collaboration songs

# Top 10 collaboration song by total stream
top_songs <- prepared_data %>%
  arrange(desc(total_streams)) %>%
  slice_head(n = 10)

# Visualization top 10 collaboration songs
ggplot(top_songs, aes(x = reorder(paste(track_name, artists, sep = " - "), total_streams), y = total_streams, fill = track_name)) +
  geom_bar(stat = "identity") +
  labs(title = "Top 10 Songs with More Than One Artist by Streams", x = "Song - Artists", y = "Streams") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Memutar label x untuk keterbacaan yang lebih baik

#Visualization 3
# Prepare the data by summing streams per artist
artist_streams <- spotify_data1 %>%
  group_by(artist) %>%
  summarise(total_streams = sum(streams, na.rm = TRUE), .groups = 'drop') %>%
  arrange(desc(total_streams)) %>%
  slice_head(n = 10)  # Select top 10 artists by total streams

# Plotting the top 10 artists by total streams
ggplot(artist_streams, aes(x = reorder(artist, total_streams), y = total_streams, fill = artist)) +
  geom_bar(stat = "identity") +
  labs(title = "Top 10 Artists by Total Streams", x = "Artist", y = "Total Streams") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate labels for better readability

#Visualization 4
data_selected <- spotify_data1 %>%
  select(in_spotify_playlists, in_spotify_charts, streams, in_apple_playlists, in_apple_charts, in_deezer_playlists, in_deezer_charts, in_shazam_charts)

# Convert columns from character to numeric if necessary
data_selected <- data_selected %>%
  mutate(across(everything(), as.numeric))

# Compute the correlation matrix
cor_matrix <- cor(data_selected, use = "pairwise.complete.obs")  # Handles missing values by considering pair-wise observations

# Visualize the correlation matrix
corrplot(cor_matrix, method = "color", type = "upper", order = "hclust",
         tl.col = "black", tl.srt = 100,  # Text label color and rotation
         addCoef.col = "black")  # Add coefficient color

#Visualization 5
# Select relevant columns and filter the top 50 songs
top_songs <- spotify_data1%>%
  select(track_name, streams, valance, energy, acousticness, instrumentalness, liveness, speechiness) %>%
  mutate(across(valance: speechiness, ~ . / sum(.))) %>%  # Normalize attributes to sum up to 1 for percentage calculation
  arrange(desc(streams)) %>%
  slice_head(n = 50)

# Melt the data for visualization
top_songs_melted <- top_songs %>%
  pivot_longer(cols = valance:speechiness, names_to = "attribute", values_to = "percentage")

# Plotting
ggplot(top_songs_melted, aes(x = track_name, y = percentage, fill = attribute)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_brewer(palette = "Set3") +  # Using a colorblind-friendly palette
  labs(title = "Distribution of Song Attributes for Top 50 Songs by Streams",
       x = "Song",
       y = "Percentage") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))  # Rotate x-axis labels for readability

#visualization 6
# Select relevant features
song_features <- spotify_data1 %>% 
  select(energy, valance, danceability, acousticness, instrumentalness,liveness,speechiness) %>%
  na.omit()  # Remove missing values

# Scale the features
song_features_scaled <- scale(song_features)

# Determining the optimal number of clusters
set.seed(123)  # for reproducibility
fviz_nbclust(song_features_scaled, kmeans, method = "wss")

# Execute K-means clustering with a chosen number of clusters
k <- 7 # this number might change based on your analysis
km_results <- kmeans(song_features_scaled, centers = k, nstart = 25)

# Visualize clusters
fviz_cluster(km_results, data = song_features_scaled, ellipse.type = "norm")
