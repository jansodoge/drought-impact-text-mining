
import spacy
#import de_dep_news_trf
import pandas

from transformers import AutoTokenizer, AutoModelForSequenceClassification, pipeline



def process_ner(test):
  nlp = spacy.load("de_core_news_lg")
  
  
  #test  = pandas.read_csv(file) 
  
  
  
  docs = list(nlp.pipe(test.text))
    
  
  def extract_tokens_plus_meta(doc:spacy.tokens.doc.Doc):
      """Extract tokens and metadata from individual spaCy doc."""
      return [
          (i.text, i.i, i.lemma_, i.ent_type_, i.tag_, 
           i.dep_, i.pos_, i.is_stop, i.is_alpha, 
           i.is_digit, i.is_punct) for i in doc
      ]
      
  def tidy_tokens(docs):
      """Extract tokens and metadata from list of spaCy docs."""
      
      cols = [
          "doc_id", "token", "token_order", "lemma", 
          "ent_type", "tag", "dep", "pos", "is_stop", 
          "is_alpha", "is_digit", "is_punct"
      ]
      
      
      meta_df = []
      for ix, doc in enumerate(docs):
          meta = extract_tokens_plus_meta(doc)
          meta = pandas.DataFrame(meta)
          meta.columns = cols[1:]
          meta = meta.assign(doc_id = ix).loc[:, cols]
          meta_df.append(meta)
          
      return pandas.concat(meta_df)      
    
  
  annotations  = pandas.DataFrame(tidy_tokens(docs = docs))
  #annotations.to_csv("annotations_ner_python.csv", sep=',')
  return[annotations]










#https://huggingface.co/Sahajtomar/German_Zeroshot

#german zeroshot based on gbert from huggingface
def gbert_zeroshot_learning(sequence, candidate_labels):
  
  
  model = AutoModelForSequenceClassification.from_pretrained("Sahajtomar/German_Zeroshot")
  classifier = pipeline("zero-shot-classification",
                        model="Sahajtomar/German_Zeroshot")
  #sequence = "e christoph bonnie, landwirt aus aachen, treibt die trockenheit derzeit noch keine sorgenfalten ins gesicht. das liegt vor allem daran, dass der 41-jährige auf seinem 32 hektar großen hof, der seit drei generationen in familienhand ist, vorwiegend gemüse und obst anbaut. salat, kohl und erdbeeren können, anders als getreidefelder, relativ einfach bewässert werden. die wiesen, auf denen bonnies rinder grasen, benötigen wegen des nebenan fließenden bachs keine zusätzliche bewässerung. etwa 20 000 liter bringt der landwirt, mithilfe von wassersprengern täglich an die wurzeln seiner produkte. das wasser bezieht bonnie aus einer zisterne, die 400 000 liter fasst, inzwischen aber so gut wie leer ist. so langsam könnte es also schon mal regnen , sagt er. (ai) e dr. erwin kempf, hautarzt in aachen, sagt: es kommen zurzeit deutlich mehr patienten mit beschwerden in die praxis, die durch das wetter bedingt sind. vielfach kommt es durch die ungewöhnlich starke sonneneinstrahlung der letzten tage zu lichtallergien. die einen haben ein ekzem, die anderen kleine bläschen. auf alle fälle kommt es zu rötungen und juckreiz an stellen, die das licht erreicht, etwa gesicht, arm und dekolleté. die aggressivität der verstärkten uv-strahlung tut nicht gut, denn: normalerweise gewöhnt sich die haut ab jahresbegin langsam an mehr sonne, diesmal ist alles viel zu plötzlich. verhindern lassen sich schmerzhafter sonnenauswirkungen durch einen erhöhten sonnenschutzfaktor, der, so kempf, mindestens bei 20 liegen sollte. (sar) e rainer krüger, agrarmeteorologe, stellt fest, dass die diesjährigen messungen von temperatur und niederschlag stark von den langjährigen mittelwerten für aachen abweichen. in den monaten april und mai lag die durchschnittstemperatur, verglichen mit denen der jahre 1961 bis 1990, um 8,1 beziehungsweise 12,5 grad höher. zwei sommertage wurden für aachen gemessen, der heißeste war mit 27,8 grad sonntag, der 8. mai. die sonnenscheindauer im april sei von durchschnittlich 148 auf 232,9 stunden gestiegen. auch die niederschlagsmenge reicht nicht an die des normalzeitraums heran, im april waren es nur 51,1 prozent. die hohe konzentration von blütenstaub, vor allem der birkenpollen sei auch mit der seit februar anhaltenden trockenheit zu erklären. (pia) e derk buchsteiber beschäftigt die trockenheit schon seit wochen. der betriebsleiter der perlenbachtalsperre in monschau hat in den 50-jährigen aufzeichnungen des wasserverbandes noch keinen so trockenen april wie im jahr 2011 gefunden. leider kann ich keinen regentanz veranstalten , sagt buchsteiner, denn die perlenbachtalsperre lebt bereits von der reserve und der trinkwassersee ist schon um einen meter gefallen. noch sei zwar die versorgung nicht bedroht, und man solle sich auch nicht aus dem moment heraus leiten lassen, aber: es darf mit der trockenheit nicht noch drei monate lang so weitergehen. die schauertätigkeit am vergangenen dienstag ist für buchsteiner völlig irrelevant: davon ist nichts an der talsperre angekommen. (p. st.) e wolfram graf-rudolf, chef des aachener tierparks: die trockenheit und hitze haben überhaupt keinen einfluss auf unsere tiere. im gegenteil: die tiere sind grundsätzlich entspannter, wenn die sonne scheint. die einzige ausnahmeaktion ist für die schweine erforderlich: für die müssen wir in diesen tagen für eine künstliche suhle sorgen und von hand das wasser aufschütten, damit sie ihre haut pflegen können , so graf-rudolf. ansonsten herrsche alltag im tierpark: wir haben ja keine großen weideflächen, die austrocknen können. das bestehende kleine areal für die watussi-rinder bereite keine sorgen . anders verhalte sich dies mit den frisch angepflanzten bäumen und büschen: die müssen wir in der tat ständig künstlich bewässern , sagt der tierpark-direktor. (mku) e heijo plum, vorsitzender des stadtbundes aachener familiengärtner, gibt sich bisher recht gelassen, denn die trockenheit habe sich noch nicht auf die kleingärtnerei ausgewirkt. die gartenpächter gießen frühmorgens und abends. wir haben genügend regenwasser von den dächern in fässern gesammelt und noch gibt es reserven. wenn die wasserreserven aufgebraucht sind, müsse man auf kranwasser zurückgreifen und das sei erfahrungsgemäß nicht so gut wie regenwasser. die pflanzzeit beginnt aber erst anfang oder mitte mai und deshalb ist die trockenheit für uns noch nicht zum problem geworden. zum bepflanzen müsse man den boden außerdem nur leicht anfeuchten. die bauern wird es natürlich mehr schmerzen. bei uns dagegen ist es eher erst zehn vor zwölf. (pia"
  #candidate_labels = ["Landwirtschaft"]
  hypothesis_template = "In diesem Text geht es um {}."    ## Since monolingual model,its sensitive to hypothesis template. This can be experimented
  
  return[classifier(sequence, candidate_labels, hypothesis_template=hypothesis_template)]










