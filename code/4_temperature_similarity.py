import pandas as pd
import numpy as np
import glob
from sentence_transformers import SentenceTransformer
from sklearn.metrics.pairwise import cosine_similarity

# Initialize sentence transformer model
sentence_model = SentenceTransformer("paraphrase-multilingual-MiniLM-L12-v2")

# 1. Read and combine all CSV files from temperature_runs directory
print("Reading temperature run files...")
file_list = glob.glob('../results/topic_model/temperature_runs/*.csv')
print(f"Found {len(file_list)} files")

# Read and combine all CSV files (they already have temperature and iteration columns)
temperature_data = pd.concat([pd.read_csv(file) for file in file_list], ignore_index=True)

# 2. Pivot longer: convert all columns starting with 'gpt4omini_lnp_t' into 'label'
print("Pivoting data to long format...")
# Get all column names that start with 'gpt4omini_lnp_t'
temp_columns = [col for col in temperature_data.columns if col.startswith('gpt4omini_lnp_t')]

# Keep Topic, temperature, and iteration columns and pivot temperature columns
id_cols = ['Topic', 'temperature', 'iteration']
temp_data_long = pd.melt(
    temperature_data,
    id_vars=id_cols,
    value_vars=temp_columns,
    var_name='model',
    value_name='label'
)

# Filter out NA labels and reset index to match embeddings array
temp_data_long = temp_data_long[temp_data_long['label'].notna()].reset_index(drop=True)

# Save the compiled temperature labels
print("Saving temperature labels to CSV...")
temp_data_long.to_csv('../results/topic_labels_temperature_iterations.csv', index=False)
print(f"Saved {len(temp_data_long)} rows to topic_labels_temperature_iterations.csv")

# 3. Encode label embeddings
print("\nEncoding label embeddings...")
label_embeddings = sentence_model.encode(temp_data_long.label.values, show_progress_bar=True)
print(f"Label embeddings shape: {label_embeddings.shape}")

# 4. Create cosine similarity matrix
print("Computing cosine similarity matrix...")
cosine_sim_matrix = cosine_similarity(label_embeddings)
print(f"Cosine similarity matrix shape: {cosine_sim_matrix.shape}")

# 5. Create a dictionary to map (Topic, temperature) to their indices
print("\nComputing average similarities per topic and temperature...")
topic_temp_indices = temp_data_long.groupby(['Topic', 'temperature'], group_keys=False).apply(lambda g: g.index.tolist(), include_groups=False).to_dict()

# Function to calculate average distance
def average_distance(indices1, indices2, cosine_sim_matrix):
    distances = []
    for i in indices1:
        for j in indices2:
            distances.append(cosine_sim_matrix[i, j])
    return np.mean(distances)

# Compute average distances for each Topic & temperature combination
results = []
for (topic, temp1), indices1 in topic_temp_indices.items():
    for (topic2, temp2), indices2 in topic_temp_indices.items():
        if topic == topic2:  # Ensure Topic1=Topic2
            avg_sim = average_distance(indices1, indices2, cosine_sim_matrix)
            results.append({
                'Topic': topic,
                'temperature1': temp1,
                'temperature2': temp2,
                'AverageSimilarity': avg_sim
            })

# Convert results to a DataFrame
similarities_df = pd.DataFrame(results)
print(f"Computed {len(similarities_df)} topic-temperature similarity combinations")

# 6. Calculate average similarity across all topics for each temperature pair
print("\nComputing average similarities across topics...")
average_similarity = (similarities_df
    .groupby(['temperature1', 'temperature2'])
    .agg({'AverageSimilarity': 'mean'})
    .reset_index())

# 7. Save results
print("\nSaving results...")
similarities_df.to_csv('../results/topic_model/temperature_topic_similarity.csv', index=False)
average_similarity.to_csv('../results/topic_model/temperature_average_similarity.csv', index=False)

print("\nDone!")
print(f"Topic-level similarities saved to: temperature_topic_similarity.csv")
print(f"Average similarities saved to: temperature_average_similarity.csv")
print(f"\nAverage similarity matrix shape: {average_similarity.shape}")
print("\nSample of average similarities:")
print(average_similarity.head(10))
