library(tidyverse)
library(readxl)
library(scales)
library(ggpubr)

ti <- read_excel('results/topic_model/topic_info.xlsx', skip = 1)
# read_csv('results/topic_model/iterations_topic_distances.csv')
iter_sim = read_csv('results/topic_model/iterations_average_similarity.csv')
df_iterations <- read_csv('results/topic_model/topic_info_all_iterations.csv')


df <- ti |> 
  pivot_longer(flan_snp:openai4o_lnp, names_to = 'model', values_to = 'label') |> 
  select(Topic, Name,model,label)

plt1 <- df_iterations |> 
  filter(Topic>=0) |> 
  pivot_longer(flan_snp:openai4o_lnp, names_to = 'model', values_to = 'label') |> 
  select(iteration, Topic, Name,model,label) |> 
  filter(Topic!=-1) |> 
  group_by(model, iteration) |> 
  summarise(n_labels = length(unique(label))) |> 
  summarise(
    mean_n_labels = mean(n_labels),
    sd_n_labels = sd(n_labels),
    n = n()) %>%
  mutate(error_margin = qt(0.975, df = n - 1) * sd_n_labels / sqrt(n),
         lower_ci = mean_n_labels - error_margin,
         upper_ci = mean_n_labels + error_margin) |> 
  separate(model, into = c('model', 'prompt')) |> 
  mutate(
    prompt= case_match(prompt,
                       'lnp'~'long name',
                       'snp'~'short name'),
    model= case_match(model,.default = model,
                      'openai4m'~'GPT-4 mini',
                      'openai4o'~'GPT-4')
  ) |> 
  ggplot(aes(prompt, mean_n_labels, color = model))+
  geom_point(position = position_dodge(width = 0.8), size = 3) +
  geom_errorbar(position = position_dodge(width = 0.8), aes(ymin = lower_ci, ymax = upper_ci), width = 0.2) +
  geom_hline(yintercept = 104,linetype='dashed')+
  theme_minimal()+
  theme(text=element_text(size=24))+
  labs(y= 'number of\nunique names')

# ggsave('results/distinct_labels.png',width = 14,height = 8)



### stability

plt2 <-
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
  ggplot(aes(prompt, average_n_labels, fill = model, label=round(average_n_labels,digits = 2)))+
  geom_col(position = position_dodge()) +
  geom_text(position = position_dodge(width = 1),vjust=0, size=7)+
  theme_minimal()+
  scale_y_continuous(breaks =  1:5)+
  theme(text=element_text(size=24))+
  labs(y= 'average number of\nlabels per topic')

# ggsave('results/average_n_labels.png',width = 14,height = 8)





## Stability

models = c("flan_snp","flan_lnp", "openai4m_snp", "openai4o_snp", "openai4m_lnp", "openai4o_lnp")
models_labels <- c("flan\nshort name", "flan\nlong name", "GPT-4-mini\nshort name", "GPT-4\nshort name", "GPT-4-mini\nlong name", "GPT-4\nlong name")
# models = c("flan_snp", "openai4m_snp", "openai4o_snp", "flan_lnp", "openai4m_lnp", "openai4o_lnp")
# models_labels <- c("flan\nshort name", "GPT-4-mini\nshort name", "GPT-4\nshort name", "flan\nlong name", "GPT-4-mini\nlong name", "GPT-4\nlong name")


plt3 <- iter_sim |> 
  mutate(Model1 = factor(Model1, levels=models, labels=models_labels),
         Model2 = factor(Model2, levels=models, labels=models_labels)) |> 
  # mutate(Model1 = str_replace(Model1,'_','\n'),
  #        Model2 = str_replace(Model2,'_','\n')) |> 
  ggplot(aes(Model1,Model2, fill=AverageSimilarity,label=round(AverageSimilarity,digits = 2)))+
  geom_tile()+
  geom_text(size = 8)+
  theme_minimal()+
  labs(x='',y='', fill='Average\nSimilarity')+
  theme(text=element_text(size=24))+
  scale_fill_binned(type = 'viridis')

# ggsave('results/labels_similarity.png',width = 14,height = 8)


ggarrange(ggarrange(plt1,plt2,common.legend = TRUE,labels = 'AUTO'),plt3, ncol = 1, labels = c(NA,'C'))

ggsave('results/quant_results',width = 14,height = 8)

