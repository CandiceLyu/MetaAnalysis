# R script for comparative forest plot 
# between separate Bayesian and multi-state models

library(ggplot2)
library(dplyr)
library(grid)

# Estimates from separate Bayesian analyses
separate_estimates <- data.frame(
  Transition = c("0 -> 1", "0 -> 2", "0 -> 3", "0 -> 4", "0 -> 5"),
  Estimate = c(0.69, 0.69, 0.65, 0.60, 0.78),
  LowerCI = c(0.54, 0.49, 0.50, 0.41, 0.44),
  UpperCI = c(0.88, 0.91, 0.80, 0.80, 1.40),
  Model = "separate"
)

# Estimates from multi-state model
bc_estimates <- data.frame(
  Transition = c("0 -> 1", "0 -> 2", "0 -> 3", "0 -> 4", "0 -> 5",
                 "1 -> 2", "2 -> 3", "3 -> 4", "4 -> 5"),
  Estimate = c(0.69, 0.62, 0.58, 0.57, 0.78, 1.35, 0.69, 3.12, 1.35),
  LowerCI = c(0.56, 0.44, 0.41, 0.37, 0.44, 0.89, 0.40, 0.99, 0.64),
  UpperCI = c(0.83, 0.86, 0.79, 0.83, 1.29, 2.16, 1.09, 10.24, 2.54),
  Model = "multi-state"
)

# Combine all estimates for plotting
combined_data <- bind_rows(
  separate_estimates %>% 
    mutate(Transition = factor(Transition, 
                               levels = c("0 -> 1", "0 -> 2", "0 -> 3", 
                                          "0 -> 4", "0 -> 5"))),
  bc_estimates %>% 
    mutate(Transition = factor(Transition, 
                               levels = c("0 -> 1", "0 -> 2", "0 -> 3", 
                                          "0 -> 4", "0 -> 5", "1 -> 2", 
                                          "2 -> 3", "3 -> 4", "4 -> 5")))
)

# Set factor levels for ordered display in the plot
combined_data$Transition <- factor(combined_data$Transition,
                                   levels = rev(c("0 -> 1", "0 -> 2", 
                                                  "0 -> 3", "0 -> 4", 
                                                  "0 -> 5", "1 -> 2", 
                                                  "2 -> 3", "3 -> 4", 
                                                  "4 -> 5")))

# Generate comparative forest plot with reversed order
forest_plot <- ggplot(combined_data, aes(x = Estimate, y = Transition, 
                                         color = Model, shape = Model)) +
  geom_point(size = 4, position = position_dodge(width = 0.5)) +
  geom_errorbarh(aes(xmin = LowerCI, xmax = UpperCI), 
                 height = 0.2, size = 0.8, position = position_dodge(width = 0.5)) +
  theme_minimal() +
  labs(x = "Risk Ratio", y = "Transition") +
  coord_cartesian(xlim = c(0, 3.2)) +
  theme(legend.text = element_text(size = 20),
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20)) +
  geom_text(aes(x = 0.1, label = sprintf("%.2f (%.2f, %.2f)",
                                         Estimate, LowerCI, UpperCI)),
            hjust = 0.5, vjust = 0.7, 
            position = position_dodge(width = 0.5), size = 4) +
  scale_shape_manual(values = c(16, 17)) +
  geom_vline(xintercept = 1, color = "black", size = 1) +
  annotation_custom(grob = linesGrob(arrow = arrow(type = "closed", 
                                                   length = unit(0.2, "inches"),
                                                   ends = "last", angle = 30), 
                                     gp = gpar(col = "red", lwd = 2)),
                    xmin = 3.2, xmax = 3.3, ymin = 2, ymax = 2)

print(forest_plot)

