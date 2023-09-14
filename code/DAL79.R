# Project to map flight path of DAL 79 on 2023-09-02

# Chris Hamm
# 2023-09-13


# Preliminaries ----
set.seed(1138)

library(tidyverse)
library(rgl)
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
animated_plot <- ggplot(DL79, aes(x = Longitude, y = Latitude, z = Altitude)) +
  geom_line(aes(group = Timestamp, color = Timestamp), linewidth = 2) +
  scale_color_gradient(low = "blue", high = "yellow") +
  labs(title = "3D Path Animation", x = "Longitude", y = "Latitude", z = "Altitude") +
  theme_minimal()

# Animate the plot
animated_plot <- animated_plot +
  transition_states(Timestamp, transition_length = 2, state_length = 1) +
  enter_fade() +
  exit_fade()

animated_plot

# Save or display the animation
animate(animated_plot, renderer = gifski_renderer("output/3d_animation.gif"))
