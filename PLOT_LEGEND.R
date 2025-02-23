# Custom order for legend items
custom_order <- c(
  "Random Sampling", 
  "SQBCR Squared 4", "SQBCR Squared 10", "SQBCR Squared 25",
  "SQBCR Absolute 4", "SQBCR Absolute 10", "SQBCR Absolute 25",
  "QBCR Absolute 4", "QBCR Absolute 10", "QBCR Absolute 25",
  "QBCR Squared 4", "QBCR Squared 10", "QBCR Squared 25"
)

plot_legend <- ggplot(plot_data_long, aes(x = n_training_data, y = Training_Error, color = Method, shape = Method)) +
  geom_line(size = 0.8) +
  geom_point(size = 2.5) +  # Adjust point size for better visibility
  labs(
    x = "No. of Iteration",
    y = "Training Error",
    title = dataset_name
  ) +
  scale_y_continuous(
    limits = c(
      min(plot_data_long$Training_Error, na.rm = TRUE),
      max(plot_data_long$Training_Error, na.rm = TRUE)
    )
  ) +
  scale_x_continuous(breaks = seq(min(n_training_data), max(n_training_data), by = 1)) +
  
  # Define custom shapes for each method
  scale_shape_manual(
    values = c("Random Sampling" = 4,
               "SQBCR Squared 4" = 15, "SQBCR Squared 10" = 16, "SQBCR Squared 25" = 17,
               "SQBCR Absolute 4" = 15, "SQBCR Absolute 10" = 16, "SQBCR Absolute 25" = 17,
               "QBCR Squared 4" = 15, "QBCR Squared 10" = 16, "QBCR Squared 25" = 17,
               "QBCR Absolute 4" = 15, "QBCR Absolute 10" = 16, "QBCR Absolute 25" = 17),
    limits = custom_order  # Set legend order for shapes
  ) +
  
  # Specify the colors using the hue palette, setting the RS color to red or a specific color
  scale_color_manual(
    values = c(
      "Random Sampling" = "black",  # Set "RS" to black
      setNames(hue_colors, c("QBCR Squared 4", "QBCR Squared 10", "QBCR Squared 25",
                             "QBCR Absolute 4", "QBCR Absolute 10", "QBCR Absolute 25",
                             "SQBCR Squared 4", "SQBCR Squared 10", "SQBCR Squared 25",
                             "SQBCR Absolute 4", "SQBCR Absolute 10", "SQBCR Absolute 25"
      ))
    ),
    limits = custom_order  # Set legend order for colors
  ) +
  
  theme_minimal() +
  theme(
    legend.position = "top",
    legend.box = "rect",
    legend.background = element_rect(fill = "white", color = "black"),
    legend.title = element_blank(),
    axis.title.y = element_blank(),
    legend.key.size = unit(0.5, "cm"),
    legend.text = element_text(size = 12),
    panel.border = element_rect(color = "black", fill = NA, size = 1),
    plot.title = element_text(hjust = 0.5)
  )

# Extract the legend
plot_legend <- get_legend(plot_legend)
