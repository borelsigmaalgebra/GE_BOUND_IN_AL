# Combine data into a data frame for ggplot
plot_data <- data.frame(
  n_training_data = n_training_data[-13],
  RS = term_rs[-c(1,13)],
  SQBCR = term_sqbcr[-c(1,13)],
  QBCR = term_qbcr[-c(1,13)],
  QBCR_REG_SUB_10 = term_qbcr_10[-c(1,13)],
  QBCR_AREG_SUB_4 = term_qbcr_areg[-c(1,13)],
  QBCR_REG_SUB_25 = term_qbcr_25[-c(1,13)]
)
names(plot_data) = c("n_training_data", "RS (Rep.)", "SQBCR (Inf./Rep.)", "QBCR (Inf.)",
                     "QBCR_REG_SUB_10 (Inf.)",
                     "QBCR_AREG_SUB_4 (Inf.)",
                     "QBCR_REG_SUB_25 (Inf.)")
# Reshape the data into long format for ggplot
plot_data_long <- pivot_longer(
  plot_data,
  cols = -n_training_data,
  names_to = "Method",
  values_to = "Term"
)

# Create the ggplot
plot_6 <- ggplot(plot_data_long, aes(x = n_training_data, y = Term, color = Method)) +
  geom_line() +
  geom_point() +
  labs(
    x = "No. of Iteration",
    y = "Term",
    title = dataset_name
  ) +
  scale_y_continuous(
    limits = c(
      min(plot_data_long$Term, na.rm = TRUE),
      max(plot_data_long$Term, na.rm = TRUE)
    )
  ) +
  scale_x_continuous(breaks = seq(min(n_training_data), max(n_training_data), by = 1)) +
  theme_minimal()+
  theme(
    panel.background  = element_rect(fill = "white", color = "black"),
    legend.position = c(1, 1),  # Move legend to top-right corner
    legend.justification = c(1.05, 1.05),  # Anchor legend at its bottom-left corner
    legend.box = "rect",  # Make the legend a box
    legend.background = element_rect(fill = "white", color = "black"),  # Box with black border
    legend.title = element_blank(),  # Remove legend title,
    axis.title.y = element_blank(),
    legend.key.size = unit(0.1, "cm"),  # Increase legend key (symbol) size
    legend.text = element_text(size = 6),  # Increase legend text size
    panel.border = element_rect(color = "black", fill = NA, size = 1),  # Add box around plot
    plot.title = element_text(hjust = 0.5),  # Center the title
    axis.title.x = element_text(size = 16)
    )


