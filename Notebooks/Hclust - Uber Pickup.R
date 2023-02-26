library(dplyr)
library(ggplot2)
library(stats)
library(geosphere)
library(leaflet)
library(geosphere)
library(factoextra)
library(dendextend)
library(gridExtra)
library(ggdendro)

# Load the data
data_p <- read.csv("C:/Users/Sanchu/Downloads/Uber data/data-society-uber-pickups-in-nyc/uber-raw-data-sep14.csv")

data_sample <- data_p[sample(nrow(data_p), 100, replace = FALSE),] 
head(data_sample)

# Extract the latitude and longitude columns
locations <- data_p %>%
  select(Lat, Lon)

locations2 <- data_sample %>%
  select(Lat, Lon)

# Compute the distance matrix
dist_matrix <- dist(distCosine(locations))

dist_matrix2 <- dist(distCosine(locations2))

# Apply hierarchical clustering
hc <- hclust(dist_matrix, method = "ward.D2")

hc2 <- hclust(dist_matrix2, method = "ward.D2")

# Convert the dendrogram to a dendextend object
dend <- as.dendrogram(hc)

dend2 <- as.dendrogram(hc2)
dend2
# Beautify dendrogram
dend2 <- color_branches(dend2, h = 0.25, k = 7)
dend2 <- set(dend2, "branches_k_color", k = 7)
dend2 <- set(dend2, "branches_lwd", c(1, 1, 1, 2, 2))
dend2 <- set(dend2, "branches_lty", c(1, 1, 2, 1, 2))
dend2 <- set(dend2, "leaves_pch", 19)
dend2 <- set(dend2, "leaves_cex", 0.6)
dend2 <- set(dend2, "labels_cex", 0.6)

dend2

# Plot dendrogram
ggdendrogram(dend2, rotate = FALSE , Labels = TRUE) +
  labs(title = "Dendrogram NYC Uber Pickups") 

ggdendrogram(hc2, rotate = TRUE, size = 2) + 
  labs(title = "Dendrogram NYC Uber Pickups") +
  geom_segment(aes(x = "Lat", y = "Lon"))

plot.new()

# Add color bar
color_key <- ggplot(data.frame(x = 1:3), aes(x = x, y = 1, fill = as.factor(x))) +
  geom_rect(show.legend = FALSE, width = 1, height = 0.25) +
  scale_fill_brewer(palette = "Set1") +
  theme_void() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        panel.grid = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        plot.margin = unit(c(0, 0, -0.5, 0), "cm"))

# Combine plot and color bar
grid.arrange(p1, color_key, ncol = 2, widths = c(0.8, 0.2))


# Plot the dendrogram
plot(hc2, cex = 0.6, hang = -1, main = "Dendrogram of NYC Uber Pickups",engine="htmlwidget")

# Cut the dendrogram to get the clusters
k <- 7
clusters <- cutree(hc2, k)

# Add the cluster information to the data
data_p$cluster <- clusters

# Plot the clusters on a map
palette <- brewer.pal(k, "Set1")
colors <- palette[clusters]

map_center <- c(40.7128, -74.0060)
m <- leaflet(data_p) %>%
  addTiles() %>%
  setView(lng = map_center[2], lat = map_center[1], zoom = 11) %>%
  addCircleMarkers(lng = ~Lon, lat = ~Lat, color = colors, radius = 3)

m
