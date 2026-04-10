library(tidyverse)
library(scales)
library(ggpubr)

# Read temperature data
temp_labels <- read_csv('results/topic_labels_temperature_iterations.csv')
temp_sim <- read_csv('results/topic_model/temperature_average_similarity.csv')

# Plot 1: Number of unique labels by temperature
plt1 <-
  temp_labels |> 
  filter(Topic >= 0) |> 
  group_by(model_source, temperature, iteration) |> 
  summarise(n_labels = length(unique(label)), .groups = 'drop') |> 
  group_by(model_source, temperature) |> 
  summarise(
    mean_n_labels = mean(n_labels),
    sd_n_labels = sd(n_labels),
    n = n(), .groups = 'drop') %>%
  mutate(error_margin = qt(0.975, df = n - 1) * sd_n_labels / sqrt(n),
         lower_ci = mean_n_labels - error_margin,
         upper_ci = mean_n_labels + error_margin) |> 
  ggplot(aes(temperature, mean_n_labels, color = model_source, group = model_source, label = round(mean_n_labels, digits = 2))) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.02) +
  geom_line() +
  geom_hline(yintercept = 106, linetype = 'dashed') +
  # geom_text(hjust = -.2, size = 6) +
  lims(y = c(NA, 110)) +
  theme_minimal() +
  theme(text = element_text(size = 12), legend.position = 'bottom') +
  labs(y = 'number of\nunique names', x = 'Temperature', title = 'Label Diversity by Temperature', color = 'Model')

# Plot 2: Average number of labels per topic (Stability)
plt2 <-
  temp_labels |> 
  filter(Topic >= 0) |> 
  group_by(model_source, temperature, Topic) |> 
  summarise(n_labels = length(unique(label)), .groups = 'drop') |> 
  group_by(model_source, temperature) |> 
  summarise(average_n_labels = mean(n_labels), .groups = 'drop') |> 
  ggplot(aes(x = factor(temperature), y = average_n_labels, fill = model_source, label = round(average_n_labels, digits = 2))) +
  geom_col(position = 'dodge') +
  # geom_text(vjust = -.1, size = 4, position = position_dodge(width = 0.9)) +
  theme_minimal() +
  # scale_y_continuous(breaks = 1:10, limits = c(0, NA)) +
  theme(text = element_text(size = 12), legend.position = 'bottom') +
  labs(y = 'average number of\nlabels per topic', x = 'Temperature', title = 'Label Stability by Temperature', fill = 'Model')

# Plot 3: Similarity heatmap
plt3 <-
  temp_sim |> 
  ggplot(aes(x = factor(temperature1), y = factor(temperature2), fill = AverageSimilarity)) +
  geom_tile() +
  facet_wrap(~model_source, scales='free') +
  # geom_text(aes(color = AverageSimilarity < 0.71), size = 3, show.legend = FALSE) +
  # scale_color_manual(values = c("TRUE" = "white", "FALSE" = "black")) +
  theme_minimal() +
  labs(x = 'Temperature 1', y = 'Temperature 2', fill = 'Average\nSimilarity', title = 'Label Similarity Across Temperatures') +
  theme(
    text = element_text(size = 14),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.text.y = element_text(size = 14),
    strip.text = element_text(size = 14),
    legend.key.width = unit(1.5, 'cm'),
    legend.margin = margin(0, 0, 0, 0),
    legend.position = 'top'
  ) +
  scale_fill_continuous(type = 'viridis')
  # scale_fill_binned(type = 'viridis')

# Combine plots
# ggarrange(plt1, plt2, plt3, heights = c(1, 1, 1.5), labels = 'AUTO')
top_row <- ggarrange(plt1, plt2, ncol = 2, labels = c('A', 'B'), widths = c(1, 1),
                     common.legend = T,legend = 'bottom')
ggarrange(top_row, plt3, nrow = 2, labels = c('', 'C'), heights = c(1, 1.2))

ggsave('results/temperature_analysis.png', width = 12, height = 10, bg = 'white')

