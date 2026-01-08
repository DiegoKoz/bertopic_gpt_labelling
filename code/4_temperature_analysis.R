# read all the files in results/temperature_runs and combine them into one data frame
library(dplyr)
library(readr)
library(purrr)
library(tidyr)

# Get list of all CSV files in the directory
file_list <- list.files(path = "results/topic_model/temperature_runs/", pattern = "*.csv", full.names = TRUE)
# Read and combine all CSV files into one data frame
temperature_data <- file_list %>%
  map_dfr(read_csv)

  # consist all the column names like gpt4omini_lnp_t0_00 into 'label' they all start with gpt4omini_lnp_t
temperature_data %>%
  pivot_longer(cols = starts_with("gpt4omini_lnp_t"), names_to = "model", values_to = "label") |> 
  filter(!is.na(label)) |> 
  select(-Count,-Name,-Representative_Docs,-Representation) |> 
  write_csv('results/topic_labels_temperature_iterations.csv')

