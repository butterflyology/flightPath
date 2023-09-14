# Project to map flight path of DAL 79 on 2023-09-02

# Chris Hamm
# 2023-09-13


# Preliminaries ----
set.seed(1138)

library(tidyverse)
library(rgl)
library(transformr)
library(gganimate)
library(rayshader)


# Load & munge data ----
DL79 <- read_csv("data/DL79_31d7299e.csv", col_names = TRUE)


# Split the Position column into Latitude and Longitude columns
coordinates <- strsplit(DL79$Position, ",")
DL79$Latitude <- as.numeric(sapply(coordinates, "[[", 1))
DL79$Longitude <- as.numeric(sapply(coordinates, "[[", 2))
DL79$Position <- NULL


# Plots ----

## Create a 3D scatter plot ----
open3d()
plot3d(DL79$Longitude, DL79$Latitude, DL79$Altitude, 
       xlab = "Longitude", ylab = "Latitude", zlab = "Altitude",
       type = "s", col = "blue", size = 1)

# Save the 3D plot as a PNG image
rgl.bringtotop()
rgl.snapshot(filename = "output/3d_scatter_plot.png", fmt = "png")
# Close the 3D plot window
close3d()


## Animated plot
animated_plot <- DL79 %>%
  ggplot(aes(x = Longitude, y = Latitude, z = Altitude)) +
  geom_line(aes(color = Timestamp), size = 3) +
  scale_color_gradient(low = "blue", high = "yellow") +
  labs(title = "3D Path Animation", x = "Longitude", y = "Latitude", z = "Altitude") +
  theme_minimal() +
  theme(legend.position = "none")

# Animate the plot
animated_plot <- animated_plot +
  transition_states(Timestamp, transition_length = 2, state_length = 1) +
  enter_fade() +
  exit_fade()

animated_plot

# Save or display the animation
animate(animated_plot, renderer = gifski_renderer())


## Another way at it
open3d()
# Create the 3D line plot
lines3d(DL79$Longitude, DL79$Latitude, DL79$Altitude, col = colorRampPalette(c("blue", "yellow"))(length(DL79$Timestamp)))

# Add labels to axes
axes3d("x")
axes3d("y")
axes3d("z")

# Set axis labels
text3d(x = max(DL79$Longitude), y = min(DL79$Latitude), z = min(DL79$Altitude), text = "Longitude", adj = c(-0.2, 0.5))
text3d(x = min(DL79$Longitude), y = max(DL79$Latitude), z = min(DL79$Altitude), text = "Latitude", adj = c(-0.2, 0.5))
text3d(x = min(DL79$Longitude), y = min(DL79$Latitude), z = max(DL79$Altitude), text = "Altitude", adj = c(-0.2, 0.5))

# Animate the plot (assuming 'animation' package is loaded)
library(animation)
rgl.snapshot("output/3d_animation.png", fmt = "png")
ani.options(interval = 0.1)
saveGIF({
  for (i in seq_along(DL79$Timestamp)) {
    subset_data <- DL79[1:i, ]
    lines3d(subset_data$Longitude, subset_data$Latitude, subset_data$Altitude, 
            col = colorRampPalette(c("blue", "yellow"))(length(subset_data$Timestamp)))
  }
}, movie.name = "output/3d_animation.gif", ani.width = 800, ani.height = 600, fps = 10)
close3d()
