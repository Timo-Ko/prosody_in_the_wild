# this script defines the overall figure theme for the project

library(ggplot2)
library(grid) # For additional spacing adjustments


theme_custom <- function(base_size = 20, base_family = "sans") {
  theme_minimal(base_size = base_size, base_family = base_family) +
    theme(
      # General settings
      panel.grid.major = element_blank(),     # Remove major grid lines
      panel.grid.minor = element_blank(),     # Remove minor grid lines
      panel.background = element_blank(),     # Remove panel background
      axis.line = element_line(color = "black", size = 0.3),  # Thinner axis lines
      axis.ticks = element_line(color = "black", size = 0.3), # Thinner axis ticks
      
      # Text settings
      axis.title = element_text(size = base_size + 2, face = "plain"),  # Axis titles
      axis.text = element_text(size = base_size, color = "black"),     # Axis text
      axis.text.x = element_text(margin = margin(t = 5), angle = 0, hjust = 0.5),
      axis.text.y = element_text(margin = margin(r = 5)),
      
      # Align plot title to the top-left
      plot.title = element_text(size = base_size + 4, face = "bold", hjust = 0, vjust = 1),
      plot.title.position = "plot",  # Ensures title starts at the top-left of the plot
      plot.subtitle = element_text(size = base_size + 2, hjust = 0, color = "grey30"),
      
      # Legend settings
      legend.title = element_text(size = base_size, face = "plain"),
      legend.text = element_text(size = base_size),
      legend.position = "right",
      legend.key = element_blank(),
      legend.background = element_blank(),
      
      # Strip settings (for faceted plots)
      strip.text = element_text(size = base_size + 1, face = "bold"),
      strip.background = element_blank(),
      
      # Plot margins
      plot.margin = margin(10, 10, 10, 10)
    )
}
