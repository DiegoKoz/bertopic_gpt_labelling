from bertopic import BERTopic
from hdbscan import HDBSCAN
from sklearn.feature_extraction.text import CountVectorizer
from umap import UMAP
from sentence_transformers import SentenceTransformer
import copy
import numpy as np
from sklearn.decomposition import PCA
from nltk.corpus import stopwords
import pickle
import pandas as pd
from bertopic.vectorizers import ClassTfidfTransformer
import os
import time

import httpx

import openai

from bertopic.representation import KeyBERTInspired
from bertopic.representation import OpenAI as OpenAIRepresentation

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
main_representation = KeyBERTInspired()
### LLM

openai_api_key = os.getenv("OPENAI_API_KEY")

if not openai_api_key:
    raise EnvironmentError("OPENAI_API_KEY environment variable is required for temperature sweeps.")

openai_client = openai.OpenAI(api_key=openai_api_key)

long_name_prompt = """
I have a corpus of Biology with 100 topics. I have a topic that is described by the following keywords: [KEYWORDS]
Based on the information above, extract a short topic label between one and three words that can accurately represent the topic, in the following format:
topic: <topic label>
"""

# TEMPERATURES = [0.00, 0.25, 0.50, 0.75, 1.00]
TEMPERATURES = [0.00, 0.25, 0.50, 0.75, 1.00, 1.25, 1.50, 1.75, 2.00]
ITERATIONS_PER_TEMPERATURE = 20
OPENAI_MODEL = "gpt-4o-mini"
OUTPUT_DIR = '../results/topic_model/temperature_runs'
OPENAI_NR_DOCS = 50
OPENAI_DELAY_SECONDS = 5
MAX_RETRIES = 3
BASE_RETRY_DELAY_SECONDS = 30


def _format_temperature_suffix(value: float) -> str:
    return f"{value:.2f}".replace(".", "_")


def build_openai_representation(temperature: float) -> OpenAIRepresentation:
    generator_kwargs = {"temperature": temperature}
    return OpenAIRepresentation(
        openai_client,
        model=OPENAI_MODEL,
        prompt=long_name_prompt,
        chat=True,
        nr_docs=OPENAI_NR_DOCS,
        delay_in_seconds=OPENAI_DELAY_SECONDS,
        generator_kwargs=generator_kwargs,
    )


def build_openai_representation_bundle(temperature: float) -> dict:
    temp_suffix = _format_temperature_suffix(temperature)
    key = f"gpt4omini_lnp_t{temp_suffix}"
    return {key: build_openai_representation(temperature)}

## Stability
#base model
topic_model = BERTopic(verbose=True,embedding_model=sentence_model, low_memory=True, calculate_probabilities=False, 
                       vectorizer_model=vectorizer_model, 
                       hdbscan_model=hdbscan_model,
                       umap_model=umap_model,
                       ctfidf_model=ctfidf_model)

BASE_TOPIC_COLUMNS = ['Topic', 'Count', 'Name', 'Representative_Docs', 'Representation']


def _clean_label_cell(value):
    if isinstance(value, (list, tuple)):
        value = value[0] if value else ''
    if pd.isna(value):
        return ''
    return str(value).replace("'", "").replace('"', '')


def get_topic_info_clean(topic_model):
    topic_info = topic_model.get_topic_info()
    existing_base_columns = [col for col in BASE_TOPIC_COLUMNS if col in topic_info.columns]
    label_columns = [col for col in topic_info.columns if col not in existing_base_columns]
    for column in label_columns:
        topic_info[column] = topic_info[column].apply(_clean_label_cell)
    ordered_columns = existing_base_columns + label_columns
    return topic_info[ordered_columns]

# Add all models together to be run in a single `fit`
topics, probabilities = topic_model.fit_transform(text_df.text.values, embeddings)
base_topic_model = copy.deepcopy(topic_model)
os.makedirs(OUTPUT_DIR, exist_ok=True)

for temperature in TEMPERATURES:
    for iteration in range(1, ITERATIONS_PER_TEMPERATURE + 1):
        for attempt in range(1, MAX_RETRIES + 1):
            try:
                temp_topic_model = copy.deepcopy(base_topic_model)
                representation_models = {"Main": main_representation}
                representation_models.update(build_openai_representation_bundle(temperature))
                temp_topic_model.update_topics(text_df.text.values, representation_model=representation_models)
                ti = get_topic_info_clean(temp_topic_model)
                ti['temperature'] = temperature
                ti['iteration'] = iteration
                output_path = os.path.join(
                    OUTPUT_DIR,
                    'topic_info_temperature_{}_run_{:02d}.csv'.format(
                        _format_temperature_suffix(temperature),
                        iteration,
                    ),
                )
                ti.to_csv(output_path, index=False)
                break
            except (openai.APIConnectionError, httpx.HTTPError) as err:
                if attempt == MAX_RETRIES:
                    raise
                wait_seconds = BASE_RETRY_DELAY_SECONDS * attempt
                print(
                    f"[temperature={temperature} iteration={iteration}] OpenAI labelling failed (attempt {attempt}/{MAX_RETRIES}): {err}. "
                    f"Retrying in {wait_seconds}s...",
                    flush=True,
                )
                time.sleep(wait_seconds)