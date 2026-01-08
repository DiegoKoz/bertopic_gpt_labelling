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
  group_by(temperature, iteration) |> 
  summarise(n_labels = length(unique(label)), .groups = 'drop') |> 
  group_by(temperature) |> 
  summarise(
    mean_n_labels = mean(n_labels),
    sd_n_labels = sd(n_labels),
    n = n()) %>%
  mutate(error_margin = qt(0.975, df = n - 1) * sd_n_labels / sqrt(n),
         lower_ci = mean_n_labels - error_margin,
         upper_ci = mean_n_labels + error_margin) |> 
  ggplot(aes(temperature, mean_n_labels, label = round(mean_n_labels, digits = 2))) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.02) +
  geom_line() +
  geom_hline(yintercept = 104, linetype = 'dashed') +
  # geom_text(hjust = -.2, size = 6) +
  lims(y = c(NA, 110)) +
  theme_minimal() +
  theme(text = element_text(size = 24)) +
  labs(y = 'number of\nunique names', x = 'Temperature', title = 'Label Diversity by Temperature')

# Plot 2: Average number of labels per topic (Stability)
plt2 <-
  temp_labels |> 
  filter(Topic >= 0) |> 
  group_by(temperature, Topic) |> 
  summarise(n_labels = length(unique(label)), .groups = 'drop') |> 
  group_by(temperature) |> 
  summarise(average_n_labels = mean(n_labels)) |> 
  ggplot(aes(temperature, average_n_labels, label = round(average_n_labels, digits = 2))) +
  geom_col(fill = 'steelblue') +
  geom_text(vjust = 0, size = 7) +
  theme_minimal() +
  scale_y_continuous(breaks = 1:10, limits = c(0, NA)) +
  theme(text = element_text(size = 24)) +
  labs(y = 'average number of\nlabels per topic', x = 'Temperature', title = 'Label Stability by Temperature')

# Plot 3: Similarity heatmap
# Convert temperature format for better display
temp_sim_plot <- temp_sim |> 
  mutate(
    temp1_label = paste0('T=', format(temperature1, nsmall = 2)),
    temp2_label = paste0('T=', format(temperature2, nsmall = 2))
  )

# Get unique temperatures in order
temp_order <- temp_sim_plot |> 
  select(temp1_label, temperature1) |> 
  distinct() |> 
  arrange(temperature1) |> 
  pull(temp1_label)

plt3 <-
  temp_sim_plot |> 
  mutate(
    temp1_label = factor(temp1_label, levels = temp_order),
    temp2_label = factor(temp2_label, levels = temp_order)
  ) |> 
  ggplot(aes(temp1_label, temp2_label, fill = AverageSimilarity, label = round(AverageSimilarity, digits = 2))) +
  geom_tile() +
  geom_text(size = 6) +
  theme_minimal() +
  labs(x = '', y = '', fill = 'Average\nSimilarity', title = 'Label Similarity Across Temperatures') +
  theme(
    text = element_text(size = 18),
    legend.position = 'bottom',
    legend.key.width = unit(1.5, 'cm'),
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  scale_fill_binned(type = 'viridis')

# Combine plots
# ggarrange(plt1, plt2, plt3, heights = c(1, 1, 1.5), labels = 'AUTO')
ggarrange(plt1, plt2, plt3, nrow = 1, labels = 'AUTO')

ggsave('results/temperature_analysis.png', width = 22, height = 6)
