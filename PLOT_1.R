# Combine data into a data frame for ggplot
plot_data <- data.frame(
  n_training_data = n_training_data[-13],
  RS = sum_training_errors_rs[-c(1,13)],
  
  SQBCR_REG_SUB_4 = sum_training_errors_sqbcr_reg_4[-c(1,13)] ,
  SQBCR_REG_SUB_10 = sum_training_errors_sqbcr_reg_10[-c(1,13)] ,
  SQBCR_REG_SUB_25 = sum_training_errors_sqbcr_reg_25 [-c(1,13)],
  
  SQBCR_AREG_SUB_4 = sum_training_errors_sqbcr_areg_4[-c(1,13)] ,
  SQBCR_AREG_SUB_10 = sum_training_errors_sqbcr_areg_10[-c(1,13)] ,
  SQBCR_AREG_SUB_25 = sum_training_errors_sqbcr_areg_25[-c(1,13)] ,
  
  QBCR_AREG_SUB_4 = sum_training_errors_qbcr_areg_4[-c(1,13)] ,
  QBCR_AREG_SUB_10 = sum_training_errors_qbcr_areg_10[-c(1,13)] ,
  QBCR_AREG_SUB_25 = sum_training_errors_qbcr_areg_25[-c(1,13)] ,
  
  QBCR_REG_SUB_4 = sum_training_errors_qbcr_4[-c(1,13)] ,
  QBCR_REG_SUB_10 = sum_training_errors_qbcr_10[-c(1,13)] ,
  QBCR_REG_SUB_25 = sum_training_errors_qbcr_25[-c(1,13)]
)
names(plot_data) = c("n_training_data", "Random Sampling",
                     "SQBCR Squared 4", "SQBCR Squared 10", "SQBCR Squared 25",
                     "SQBCR Absolute 4", "SQBCR Absolute 10", "SQBCR Absolute 25",
                     "QBCR Squared 4", "QBCR Squared 10", "QBCR Squared 25",
                     "QBCR Absolute 4", "QBCR Absolute 10", "QBCR Absolute 25")
# Reshape the data into long format for ggplot
plot_data_long <- pivot_longer(
  plot_data,
  cols = -n_training_data,
  names_to = "Method",
  values_to = "Training_Error"
)

# Get the 12 distinct colors from the hue palette
hue_colors <- hue_pal()(12)

# Create the ggplot
plot_1 <- ggplot(plot_data_long, aes(x = n_training_data, y = Training_Error, color = Method, shape = Method)) +
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
               "QBCR Absolute 4" = 15, "QBCR Absolute 10" = 16, "QBCR Absolute 25" = 17)
  ) +
  
  # Specify the colors using the hue palette, setting the RS color to red or a specific color
  scale_color_manual(values = c(
    "Random Sampling" = "black",  # Set "RS" to red
    setNames(hue_colors, c("QBCR Squared 4", "QBCR Squared 10", "QBCR Squared 25",
                           "QBCR Absolute 4", "QBCR Absolute 10", "QBCR Absolute 25",
                           "SQBCR Squared 4", "SQBCR Squared 10", "SQBCR Squared 25",
                           "SQBCR Absolute 4", "SQBCR Absolute 10", "SQBCR Absolute 25"
    ))
  )) +
  
  theme_minimal() +
  theme(
    legend.position = "none",
    panel.background  = element_rect(fill = "white", color = "black"),
    legend.justification = c(1.05, 1.05),  # Anchor legend at its bottom-left corner
    legend.box = "rect",  # Make the legend a box
    legend.background = element_rect(fill = "white", color = "black"),  # Box with black border
    legend.title = element_blank(),  # Remove legend title
    axis.title.y = element_blank(),
    legend.key.size = unit(0.1, "cm"),  # Increase legend key (symbol) size
    legend.text = element_text(size = 6),  # Increase legend text size
    panel.border = element_rect(color = "black", fill = NA, size = 1),  # Add box around plot
    plot.title = element_text(hjust = 0.5),  # Center the title
    axis.title.x = element_text(size = 15),
    axis.text.x = element_text(size = 15),
    axis.text.y = element_text(size = 15)
  )