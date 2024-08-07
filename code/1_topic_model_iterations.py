from bertopic import BERTopic
from hdbscan import HDBSCAN
from sklearn.feature_extraction.text import CountVectorizer
from umap import UMAP
from sentence_transformers import SentenceTransformer
import numpy as np
from sklearn.decomposition import PCA
from nltk.corpus import stopwords
import pickle
import pandas as pd
from bertopic.vectorizers import ClassTfidfTransformer
import os

from tqdm import tqdm

import openai

from bertopic.representation import KeyBERTInspired

from transformers import pipeline
from bertopic.representation import TextGeneration
from bertopic.representation import PartOfSpeech
from bertopic.representation import MaximalMarginalRelevance
from bertopic.representation import OpenAI
# this functions are only to save the embedding
def save(x, file_name):
    with open(file_name, 'wb') as handle:
        pickle.dump(x, handle, protocol=pickle.HIGHEST_PROTOCOL)

def restore(file_name):
    with open(file_name, 'rb') as handle:
        x = pickle.load(handle)
    return x
df = pd.read_table('../data/biology_abstracts.csv',sep=';')

df['text'] = df.title+ ' ' + df.abstract
df['text'] = df.text.fillna('')
text_df = df[-df.OST_BK.duplicated(keep='last')].reset_index(drop=True) #drop duplicates
# umap_model = UMAP(n_neighbors=15, n_components=3, min_dist=0.0, metric='cosine', low_memory = True, n_jobs=32)
sentence_model = SentenceTransformer("paraphrase-multilingual-MiniLM-L12-v2") #"paraphrase-MiniLM-L3-v2"

def rescale(x, inplace=False):
    """ Rescale an embedding so optimization will not have convergence issues.
    """
    if not inplace:
        x = np.array(x, copy=True)

    x /= np.std(x[:, 0]) * 10000

    return x


embeddings = restore('../results/embeddings_1.p')
pca_embeddings = rescale(PCA(n_components=5).fit_transform(embeddings))

## basic elements
umap_model = UMAP(
    n_neighbors=15,
    n_components=5,
    min_dist=0.0,
    metric="cosine",
    init=pca_embeddings,
    random_state=1234, n_jobs=4, low_memory=False)    
sw=stopwords.words(['english','spanish','french']) + ['elsevier']
vectorizer_model = CountVectorizer(ngram_range=(1,1), stop_words=sw,max_df=1.0, min_df=0.001)
# Setting HDBSCAN model
hdbscan_model = HDBSCAN(min_cluster_size=100,min_samples=1, metric='euclidean',cluster_selection_epsilon=0.05, 
                        cluster_selection_method='leaf', prediction_data=True,core_dist_n_jobs=4,memory='tmp/') #, min_samples=1
ctfidf_model  = ClassTfidfTransformer()
## Representation models
# The main representation of a topic
main_representation = KeyBERTInspired()
### LLM

openai_api_key = os.getenv("OPENAI_API_KEY")

client = openai.OpenAI(api_key=openai_api_key)

# Create your representation model
short_name_prompt = """
I have a corpus of Biology with 100 topics. I have a topic that is described by the following keywords: [KEYWORDS]
Based on the information above, extract a short topic label of a single word that can accurately represent the topic, in the following format:
topic: <topic label>
"""
long_name_prompt = """
I have a corpus of Biology with 100 topics. I have a topic that is described by the following keywords: [KEYWORDS]
Based on the information above, extract a short topic label between one and three words that can accurately represent the topic, in the following format:
topic: <topic label>
"""
# title_prompt = """
# I have a topic that is described by the following keywords: [KEYWORDS]
# Based on the previous keywords, please give me a title that can describe the topic
# """

generator = pipeline('text2text-generation', model='google/flan-t5-base')

flan_snp = TextGeneration(generator,prompt=short_name_prompt)
flan_lnp = TextGeneration(generator,prompt=long_name_prompt)

openai4m_snp = OpenAI(client, model="gpt-4o-mini", chat=True, prompt=short_name_prompt, nr_docs=50, delay_in_seconds=5)
openai4m_lnp = OpenAI(client, model="gpt-4o-mini", chat=True, prompt=long_name_prompt, nr_docs=50, delay_in_seconds=5)

openai4o_snp = OpenAI(client, model="gpt-4o", chat=True, prompt=short_name_prompt, nr_docs=50, delay_in_seconds=5)
openai4o_lnp = OpenAI(client, model="gpt-4o", chat=True, prompt=long_name_prompt, nr_docs=50, delay_in_seconds=5)

## Stability
#base model
topic_model = BERTopic(verbose=True,embedding_model=sentence_model, low_memory=True, calculate_probabilities=False, 
                       vectorizer_model=vectorizer_model, 
                       hdbscan_model=hdbscan_model,
                       umap_model=umap_model,
                       ctfidf_model=ctfidf_model)

def get_topic_info_clean(topic_model):
    topic_info= topic_model.get_topic_info()
    
    topic_info['Aspect1'] = topic_info['Aspect1'].apply(lambda x: x[0] if x else '').str.replace("'", "").str.replace('"', '')
    topic_info['Aspect2'] = topic_info['Aspect2'].apply(lambda x: x[0] if x else '').str.replace("'", "").str.replace('"', '')
    topic_info['Aspect3'] = topic_info['Aspect3'].apply(lambda x: x[0] if x else '').str.replace("'", "").str.replace('"', '')
    topic_info['Aspect4'] = topic_info['Aspect4'].apply(lambda x: x[0] if x else '').str.replace("'", "").str.replace('"', '')
    topic_info['Aspect5'] = topic_info['Aspect5'].apply(lambda x: x[0] if x else '').str.replace("'", "").str.replace('"', '')
    topic_info['Aspect6'] = topic_info['Aspect6'].apply(lambda x: x[0] if x else '').str.replace("'", "").str.replace('"', '')
    
    topic_info= topic_info.rename(columns={
        "Aspect1": "flan_snp",
        "Aspect2": "flan_lnp",
        "Aspect3": "openai4m_snp",
        "Aspect4": "openai4m_lnp",
        "Aspect5": "openai4o_snp",
        "Aspect6": "openai4o_lnp"})
    
    topic_info=topic_info[['Topic', 'Count', 'Name','Representative_Docs', 'Representation', 'flan_snp',
           'openai4m_snp', 'openai4o_snp', 'flan_lnp', 'openai4m_lnp',
           'openai4o_lnp']]
    return topic_info

# Add all models together to be run in a single `fit`
representation_models = {
   "Main": main_representation,
    "Aspect1": flan_snp,
    "Aspect2": flan_lnp,
    "Aspect3": openai4m_snp,
    "Aspect4": openai4m_lnp,
    "Aspect5": openai4o_snp,
    "Aspect6": openai4o_lnp
}

topics, probabilities  = topic_model.fit_transform(text_df.text.values, embeddings)

for i in range(4,21):
    topic_model.load('../results/topic_model/basic_model') # I ensure I have the same basic model
    topic_model.update_topics(text_df.text.values,representation_model=representation_models)
    ti = get_topic_info_clean(topic_model)
    ti['iteration'] = i
    ti.to_csv('../results/topic_model/topic_info_iteration_{}.csv'.format(i),index=False)