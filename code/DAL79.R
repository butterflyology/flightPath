# Project to map flight path of DAL 79 on 2023-09-02

# Chris Hamm
# 2023-09-13


# Preliminaries ----
set.seed(1138)

library(tidyverse)
library(rgl)
library(transformr)
library(plotly)
library(scales)
library(gganimate)
library(rayshader)


# Load & munge data ----
DL79 <- read_csv("data/DL79_31d7299e.csv", col_names = TRUE)
str(DL79)
head(DL79)


# Split the Position column into Latitude and Longitude columns
coordinates <- strsplit(DL79$Position, ",")
DL79$Latitude <- as.numeric(sapply(coordinates, "[[", 1))
DL79$Longitude <- as.numeric(sapply(coordinates, "[[", 2))
DL79$Position <- NULL

str(DL79)
head(DL79)

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


# plotly 
# create a color spectrum
DL79 <- DL79 %>% 
  arrange(Timestamp) %>% 
  mutate(color = colorRampPalette(c("blue", "yellow"))(nrow(DL79))[nrow(DL79):1])

plot_ly(DL79, x = ~Longitude, y = ~Latitude, z = ~Altitude, 
        text = ~paste("Time:", Timestamp, "<br>Speed:", Speed, "<br>Direction:", Direction), 
        marker = list(color = ~color, size = 5, opacity = 0.6),
        line = list(color = ~color, width = 4),
        type = 'scatter3d', mode = 'lines+markers') %>%
  layout(scene = list(xaxis = list(title = "Longitude"),
                      yaxis = list(title = "Latitude"),
                      zaxis = list(title = "Altitude")))

# now to animate
DL79 <- DL79 %>%
  arrange(Timestamp) %>%
  mutate(time_id = row_number())

p <- DL79 %>%
  ggplot(aes(x = Longitude, y = Latitude, linewidth = Altitude, color = time_id)) + 
  geom_path(aes(group = 1, frame = time_id, ids = Timestamp), lineend = "round") +
  scale_color_gradient(low = "blue", high = "yellow") +
  theme_minimal() + 
  labs(title = 'Time: {closest_state}', x = 'Longitude', y = 'Latitude') +
  theme(legend.position = "none") + 
  transition_states(states = Timestamp, transition_length = 2, state_length = 1) +
  view_follow(fixed_y = TRUE)

anim_save("animated_flight_path.gif", animate(p))


# Doesn't move
plot_ly(data = DL79, 
        x = ~Longitude, 
        y = ~Latitude, 
        z = ~Altitude, 
        ids = ~time_id, 
        type = 'scatter3d', 
        mode = 'lines+markers', 
        frame = ~time_id, 
        line = list(color = ~time_id, colorscale = list(c(0, 1), c("blue", "yellow")), width = 6)
) %>% 
  animation_opts(frame = 100, redraw = TRUE) %>%
  animation_slider(currentvalue = list(prefix = "Time: "))

# should move
DL79 <- DL79 %>%
  arrange(time_id) %>%
  mutate(lag_Longitude = lag(Longitude),
         lag_Latitude = lag(Latitude),
         lag_Altitude = lag(Altitude))

plot_ly(data = DL79) %>%
  add_segments(x = ~lag_Longitude, xend = ~Longitude,
               y = ~lag_Latitude, yend = ~Latitude,
               z = ~lag_Altitude, zend = ~Altitude,
               ids = ~time_id, 
               color = ~time_id,
               colorscale = list(c(0, 1), c("blue", "yellow")),
               lwd = 6,
               frame = ~time_id) %>%
  layout(scene = list(
    aspectmode = "cube",
    xaxis = list(range = range(DL79$Longitude)),
    yaxis = list(range = range(DL79$Latitude)),
    zaxis = list(range = range(DL79$Altitude))
  )) %>%
  animation_opts(frame = 100, redraw = FALSE) %>%
  animation_slider(currentvalue = list(prefix = "Time: "))

# cum data
# Create a cumulative dataset
DL79_cumulative <- DL79 %>%
  mutate(row_id = row_number()) %>%
  tidyr::expand(row_id, nesting(time_id, Longitude, Latitude, Altitude))

DL79_cumulative <- DL79_cumulative %>%
  group_by(row_id) %>%
  tidyr::fill(Longitude, Latitude, Altitude, .direction = "down") %>%
  ungroup()

# Generate the 3D animated plot
plot_ly(DL79_cumulative, x = ~Longitude, y = ~Latitude, z = ~Altitude, color = ~time_id, colors = c("yellow", "blue"), ids = ~time_id) %>%
  add_trace(type = 'scatter3d', mode = 'lines', line = list(width = 6)) %>%
  layout(scene = list(
    aspectmode = "cube",
    xaxis = list(range = range(DL79$Longitude)),
    yaxis = list(range = range(DL79$Latitude)),
    zaxis = list(range = range(DL79$Altitude))
  )) %>%
  animation_opts(frame = 100, redraw = TRUE) %>%
  animation_slider(currentvalue = list(prefix = "Time: "))



# Create a list of data frames, each one a subset of the full dataset up to a given point in time.
data_list <- lapply(1:nrow(DL79), function(i) DL79[1:i, ])

fig <- plot_ly()

# Add each subset to the plot as a frame
for(i in seq_along(data_list)) {
  fig <- fig %>% 
    add_trace(
      data = data_list[[i]],
      x = ~Longitude, y = ~Latitude, z = ~Altitude,
      type = 'scatter3d', mode = 'lines+markers',
      line = list(color = ~time_id, colorscale = list(c(0, 1), c("blue", "yellow")), width = 6),
      showlegend = FALSE,
      name = as.character(i)
    )  
}

# Adjust the plot layout and animation options
fig <- fig %>% 
  layout(
    scene = list(
      aspectmode = "cube",
      xaxis = list(range = range(DL79$Longitude)),
      yaxis = list(range = range(DL79$Latitude)),
      zaxis = list(range = range(DL79$Altitude))
    ),
    updatemenu = list(type = 'buttons', showactive = FALSE, buttons = list(list(method = 'animate', args = list(NULL, list(frame = list(duration = 100, redraw = TRUE), fromcurrent = TRUE)))))) %>%
  animation_opts(frame = 100, redraw = TRUE) %>%
  animation_slider(currentvalue = list(prefix = "Time: "))

fig

# Why is this so difficult?
# Create a fake "3D" perspective 

