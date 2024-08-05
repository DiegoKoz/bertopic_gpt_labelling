library(tidyverse)
library(readxl)


ti <- read_excel('results/topic_model/topic_info.xlsx', skip = 1)


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

df_iterations <- read_excel('results/topic_model/topic_info_iteration.xlsx')

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



