library(tidyverse)
library(readxl)
library(scales)

ti <- read_excel('results/topic_model/topic_info.xlsx', skip = 1)
# read_csv('results/topic_model/iterations_topic_distances.csv')
iter_sim = read_csv('results/topic_model/iterations_average_similarity.csv')


df <- ti |> 
  pivot_longer(flan_snp:openai4o_lnp, names_to = 'model', values_to = 'label') |> 
  select(Topic, Name,model,label)


df |> 
  filter(Topic!=-1) |> 
  group_by(model) |> 
  summarise(n_labels = length(unique(label))) |> 
  separate(model, into = c('model', 'prompt')) |> 
  mutate(prompt= case_match(prompt,
                            'lnp'~'long name',
                            'snp'~'short name'),
         model= case_match(model,.default = model,
                           'openai4m'~'GPT-4 mini',
                           'openai4o'~'GPT-4'),
  ) |> 
  ggplot(aes(prompt, n_labels, fill = model))+
  geom_col(position = position_dodge()) +
  geom_hline(yintercept = 105,linetype='dashed')+
  theme_minimal()+
  labs(y= 'number of unique names')

ggsave('results/distinct_labels.png',width = 14,height = 8)



### stability
df_iterations <- read_csv('results/topic_model/topic_info_all_iterations.csv')

df_iterations |> 
  pivot_longer(flan_snp:openai4o_lnp, names_to = 'model', values_to = 'label') |> 
  select(iteration, Topic, Name,model,label) |> 
  filter(Topic!=-1) |> 
  group_by(model, Topic) |> 
  summarise(n_labels = length(unique(label))) |> 
  group_by(model) |> 
  summarise(average_n_labels = mean(n_labels)) |> 
  separate(model, into = c('model', 'prompt')) |> 
  mutate(prompt= case_match(prompt,
                            'lnp'~'long name',
                            'snp'~'short name'),
         model= case_match(model,.default = model,
                           'openai4m'~'GPT-4 mini',
                           'openai4o'~'GPT-4'),
  ) |> 
  ggplot(aes(prompt, average_n_labels, fill = model))+
  geom_col(position = position_dodge()) +
  theme_minimal()+
  labs(y= 'average number of labels per topic')

ggsave('results/average_n_labels.png',width = 14,height = 8)


## Stability

models = c("flan_snp","flan_lnp", "openai4m_snp", "openai4o_snp", "openai4m_lnp", "openai4o_lnp")
models_labels <- c("flan\nshort name", "flan\nlong name", "GPT-4-mini\nshort name", "GPT-4\nshort name", "GPT-4-mini\nlong name", "GPT-4\nlong name")
# models = c("flan_snp", "openai4m_snp", "openai4o_snp", "flan_lnp", "openai4m_lnp", "openai4o_lnp")
# models_labels <- c("flan\nshort name", "GPT-4-mini\nshort name", "GPT-4\nshort name", "flan\nlong name", "GPT-4-mini\nlong name", "GPT-4\nlong name")


iter_sim |> 
  mutate(Model1 = factor(Model1, levels=models, labels=models_labels),
         Model2 = factor(Model2, levels=models, labels=models_labels)) |> 
  # mutate(Model1 = str_replace(Model1,'_','\n'),
  #        Model2 = str_replace(Model2,'_','\n')) |> 
  ggplot(aes(Model1,Model2, fill=AverageSimilarity,label=round(AverageSimilarity,digits = 2)))+
  geom_tile()+
  geom_text()+
  theme_minimal()+
  labs(x='',y='')+
  scale_fill_binned(type = 'viridis')

ggsave('results/labels_similarity.png',width = 14,height = 8)

