
from top2vec import Top2Vec


def read_top2vec(file_model):
  model = Top2Vec.load(file_model)
  return[model]



def get_top_number(model):
  return[model.get_num_topics()]



def py_get_topic_sizes(model):
  topic_sizes, topic_nums = model.get_topic_sizes()
  return[topic_sizes, topic_nums]




def py_get_topics(model):
  topic_words, word_scores, topic_nums = model.get_topics(model.get_num_topics())
  topic_words = topic_words.tolist()
  word_scores  = word_scores.tolist()
  return[topic_words, word_scores, topic_nums]




def py_get_similiar_words(model, word_selected, number_of_words):
  words, word_scores = model.similar_words(keywords=[word_selected], keywords_neg=[], num_words=number_of_words)
  return[words, word_scores]





def topics_per_document(model, document):
  topic_nums, topic_score, topics_words, word_scores = model.get_documents_topics(doc_ids = [document],
                          num_topics = model.get_num_topics())
  topic_score = topic_score.tolist()
  topic_nums = topic_nums.tolist() 
  return[topic_score, topic_nums]
              
  
def documents_by_topic(model, topic_number, number_of_documents):
  documents, document_scores, document_ids = model.search_documents_by_topic(topic_num=topic_number, num_docs=number_of_documents)
  return[documents, document_scores, document_ids]
  
  
  
  
  
def add_documents_to_top2vec(model, dataset):
  model_update = model.add_documents(dataset.text.tolist(),
 
  tokenizer=None, use_embedding_model_tokenizer=False,
  doc_ids = dataset.id_added_new.tolist()
  )  
  
  return model_update
  
  
  

  
  
  
  
  
  
  
