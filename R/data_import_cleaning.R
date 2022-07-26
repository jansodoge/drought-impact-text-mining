
#' Re-format html format of newspaper articles to R dataframe
#'
#' @param html_file File input (required in .html or .htm)
#'
#' @return Dataframe containing information about newspaper articles (text, meta, ID)
#' @export
#'
#' @examples transform_html_to_df("html_to_text_corpus/exportHtml.htm")
transform_html_to_df <- function(html_file){
  #scrape individual documents from html
  page <- read_html(html_file, encoding = "UTF-8")
  tpage <- htmlParse(page)
  doc <- xpathSApply(tpage, "//div[@class='singleDocument modulePageBreak']")
  last <- xpathSApply(tpage, "//div[@class='singleDocument modulePageBreak Last']")
  first <- xpathSApply(tpage, "//div[@class='singleDocument']")
  docs <- c(doc, last, first)
  if(length(docs) != 50){
    #print("Warning Message: Number of detected articles not 50")
  }
  #create some vectors storing information which we will extract below
  extracted_texts <- c()
  alternate_texts <- c() #sometimes text also stored within this very tag
  meta_raw <- c()
  headers <- c()
  permalinks <- c() # not Permafrost
  for(article in seq(1,length(docs))){
    #since are are multiple text tags sometimes, we select the tag which contains the longest string i.e. likely the article text
    potential_texts <-  xpathSApply(docs[[article]], ".//pre[@class='text']", xmlValue)
    if(length(potential_texts >0)){
      potential_texts <- potential_texts[nchar(potential_texts)==max(nchar(potential_texts))][1]
    }
    else{
      potential_texts <- NA
    }
    #alternative text stored in HTML tag with class textCompact
    potential_texts_alt <-  max(xpathSApply(docs[[article]], ".//pre[@class='textCompact']", xmlValue))
    extracted_texts <- append(extracted_texts, 
                              potential_texts)
    alternate_texts <- append(alternate_texts,
                              potential_texts_alt)
    #gather further attributes from HTML
    meta_raw <- append(meta_raw,
                       as.character(xpathSApply(docs[[article]], ".//pre[@class='gray']", xmlValue)[1]))
    headers <-  append(headers,
                       as.character(xpathSApply(docs[[article]], ".//pre[@class='boldLarge']", xmlValue)[1]))
    permalinks <- append(permalinks,
                         as.character(xpathSApply(docs[[article]], ".//pre[@class='text permaLink']//a", xmlAttrs)[1]))
    
  }
  retreived_tibble <- data.frame(text = extracted_texts, # main text string
                                 texts_alt  = alternate_texts, #alternative text if 'text' column is NA
                                 meta  = tolower(meta_raw), # contains meta information, will be splitted in pipe here
                                 header = headers, # article header
                                 permalink = permalinks) %>%  # permalink can serve as ID for article 
    mutate(date = stringr::str_extract(meta, "[0-9]{1,2}.[0-9]{1,2}.[0-9]{4}"), # transform date
           newspaper  = stringr::str_remove(stringr::str_extract(meta, ".* [0-9]{1,2}.[0-9]{1,2}.[0-9]{4}"), # transform newspaper name
                                            "[0-9]{1,2}.[0-9]{1,2}.[0-9]{4}")) %>% 
    mutate(newspaper  = stringr::str_remove(newspaper, 
                                            "vom")#,
           #date = lubridate::dmy(date)
    ) %>% #turn date to lubridate format
    dplyr::select(!meta)
  return(retreived_tibble)
}



read_wiso_data <- function(files_storage){
  
  batch <- lapply(files_storage,function(i){
    #print(i)
    try(transform_html_to_df(i))
  })
  return(as.data.frame(do.call(rbind, batch)))
}




clean_newspaper_articles <- function(full_corpus){
  #to lower
  full_corpus$text <- tolower(full_corpus$text)
  full_corpus$header <- tolower(full_corpus$header)
  
  na_text_observations <- full_corpus %>% 
    filter(is.na(text)) #these need to be added manually
  #and remove these temporally to insert later
  full_corpus <- full_corpus %>% 
    anti_join(na_text_observations, by = c("permalink" = "permalink"))
  #write.csv(na_text_observations, "na_text_observations.csv")
  
  
  #empty observations <-- I figured that these are truly empty thus we remove them
  empty_observations <- full_corpus %>% 
    filter(text == " ")
  full_corpus <- full_corpus %>% 
    anti_join(empty_observations, by = c("permalink" = "permalink"))
  
  
  #observations with very short character length, similiar with NA to remvove, check and insert later
  short_chars <- subset(full_corpus, nchar(as.character(text)) <= 200)
  #write.csv(short_chars, "short_chars_to_check.csv")
  full_corpus <- full_corpus %>% 
    anti_join(short_chars, by = c("permalink" = "permalink"))
  
  
  
  corrected_na_values_a <- read.csv("R/data/na_text_observations.csv") %>% 
    filter(!is.na(text))
  
  full_corpus <- full_corpus %>% 
    dplyr::bind_rows(corrected_na_values_a)
  
  
  token_corpus <- full_corpus %>% 
    tidytext::unnest_tokens(word, text)
  
  
  keyword_tokens <- full_corpus %>% 
    tidytext::unnest_tokens(word, text) %>% 
    pull(word) 
  
  
  
  duerre_terms <- grepl("dürre.*", keyword_tokens)
  keys <- keyword_tokens[duerre_terms]
  unique_keys <- unique(keys)
  
  
  trockenheit_terms <- grepl("trockenheit.*", keyword_tokens)
  keys_trockenheit <- keyword_tokens[trockenheit_terms]
  unique_keys_trockenheit <- unique(keys_trockenheit)
  
  #TBD: maybe add foreign countries
  
  
  wrong_tokens_a <- c("sexdürre", "dürrenstetten",
                      "dürrenstettenerin", "dürrenhofe",
                      "dürrenwaider", "dürrenrieder",
                      "dürrenzimmern", "dürrenbacher",
                      "dürrenebersdorf", "dürrenmungenau",
                      "dürrenbach", "dürrengleina",
                      "dürrenzimm", "dürrenbühl",
                      "dürresbachtal", "einigedürre",
                      "spindeldürres", "einigedürre",
                      "dürrenhofer", "dürrengerbisdorf",
                      "dürrengerbisdorfer", "dürrenzimmerner",
                      "dürrenberg", "dürrenhof", "dürrenebersdorfer",
                      "dürrenebernsdorf", "spindeldürre",
                      "dürrematt", "klapperdürren", "dürrestraße",
                      "dürrenwaldstetten", "dürrenwaldstetter",
                      "dürrenebersdorfern", "dürrenbachtal", "dürrenwadstetten",
                      "dürrenuhlsdorf", "besucherdürre", "dürreähnliche",
                      "spindeldürrer", "spindeldürrer", "dürrenriedern",
                      "dürrellenbacher", "dürrenstetter", "dürrenbaches",
                      "dürrenot", "dürrenbachbrücke", "dürresbach", "dürrerhof",
                      "dürrenhofstraße", "dürrenhofstraße.nn",
                      "dürreschule", "dürrengleinaern",
                      "dürrenwaiderhammer", "dürrenwaldstettener",
                      "dürrenmatt", "jedermanndürrenwaldstetten",
                      "dürrenmungenauer", "dürrenmettstetten", 
                      "dürrenmettstetter", "dürrenieder",
                      "dürrematts", "klapperdürre",
                      "dürrenbachquelle", "informationsdürre",
                      "dürrenmatts", "spindeldürren", "dürreckberg",
                      "dürrengleinaer", "dürrenhoftunnel", "dürrenebersdorf", 
                      "dürrenbachs", "zinsdürre", "dürrenstraße",
                      "dürrenbachsanierung", "dividendendürre", "dürrenwald",
                      "dürrenmettstettens", "dürrenstein", "rasseldürre",
                      "dürrekalamitäten", "dürre.der", "dürrenberger",
                      "blattdürre", "dürrenwaidener", "humordürre",
                      "dürrenwaldstettern", "kauferingdürre", "dürreatlanten",
                      "dürrekünstler", "dürresdürrefoto", "dürrenwaidertal",
                      "dürreberg", "dürreholz", "dürrenwaidern",
                      "konzertdürre", "dürrenbergs", "dürrenhofes")
  wrong_tokens_b <- c("trockenheitskrankheit", "mundtrockenheit",
                      "trockenheitalle", "trockenheitauch",
                      "zopftrockenheit", "scheidentrockenheit", "marienbergtrockenheit")
  
  correct_tokens <- c(setdiff(unique_keys_trockenheit, wrong_tokens_b), setdiff(unique_keys, wrong_tokens_a))
  
  
  #get IDs of texts where a 'wrong' token is found
  texts_w_wrong_tokens <- token_corpus %>% 
    filter(word %in% wrong_tokens_a  | word %in% wrong_tokens_b) %>% 
    distinct(permalink) %>% 
    pull(permalink)
  
  
  #then get those and check for articles where we do not find at least one correct token
  texts_w_wrong_tokens <- token_corpus %>% 
    filter(permalink %in% texts_w_wrong_tokens) %>% 
    filter(!(word %in% correct_tokens)) %>% 
    distinct(permalink) %>% 
    pull(permalink)
  
  
  #finally check for lacking accounting of keywords relevant mentioned in header
  texts_w_wrong_tokens <-  full_corpus %>% 
    filter(permalink %in% texts_w_wrong_tokens) %>% 
    filter(!str_detect(header, paste(correct_tokens, collapse="|"))) %>% 
    distinct(permalink) %>% 
    pull(permalink)
  
  #for validationn please check if all these articles are BS
  wrong_articles_check_req <- full_corpus %>% 
    filter(permalink %in% texts_w_wrong_tokens)
  
  
  corpus_w_corrected_tokens <- full_corpus %>% 
    filter(!permalink %in% texts_w_wrong_tokens)
  
  
  #both these IDs seem to be redudant, TBD:: check later
  full_corpus <-  corpus_w_corrected_tokens
  full_corpus <- tibble::rowid_to_column(full_corpus, "id")
  
  
  full_corpus <- full_corpus %>% 
    drop_na(date) %>% 
    rowid_to_column("match_py") %>% 
    mutate(match_py  = match_py -1)
  
  
  
  return(full_corpus)
  
}



hashing_duplicates <- function(clean_text_articles, cutoff_threshold = .9){
  
  full_corpus <- clean_text_articles
  
  
  full_corpus <- full_corpus %>% 
    mutate(year = lubridate::year(lubridate::dmy(date)))
  
  
  results <- purrr::map(unique(full_corpus$year), function(year_selected){
    
    full_corpus_tmp <- full_corpus %>% 
      dplyr::filter(year == year_selected)
    
    
    minhash <- minhash_generator(n = 480, seed = 3552)
    corpus <- TextReuseCorpus(text=full_corpus_tmp[,"text"], tokenizer = tokenize_ngrams, n = 5, #so we pass them all files and it does what? 
                              minhash_func = minhash, keep_tokens = TRUE,
                              progress = TRUE)
    
    buckets <- lsh(corpus, bands = 240, progress = TRUE)
    #see people who cite textreuse for advice
    #find candidates
    candidates <- lsh_candidates(buckets) #Locality sensitive hashing (LSH) discovers potential matches among a corpus of documents quickly, so that only likely pairs can be compared.
    lsh_compare(candidates, corpus, jaccard_similarity, progress = FALSE)
    
  })
  
  ##for many documents
  
  return(results)
}





remove_hashing_duplicates <- function(hashing_results, 
                                      full_corpus,
                                      cutoff_threshold = .95){
  
  results <-  hashing_results
  
  full_corpus <- full_corpus %>% 
    mutate(year = lubridate::year(lubridate::dmy(date)))
  
  results_2 <- purrr::map2_dfr(results, unique(full_corpus$year), function(hashing_data, year_selected){
    
    full_corpus_tmp <- full_corpus %>% 
      dplyr::filter(year == year_selected) %>% 
      rowid_to_column(var = "doc_id")
    
    results <- hashing_data %>% 
      dplyr::mutate(a = stringr::str_remove(string = a,pattern = "doc-"),
                    b = stringr::str_remove(string = b,pattern = "doc-"))
    
    to_remove <- results[which(results[,3]>=cutoff_threshold),2]     
    to_remove <- unique(to_remove)
    to_remove <- as.numeric(to_remove$b)
    to_remove <- sort(to_remove)
    remove <- as_tibble(to_remove)
    names(remove)[1] <- "doc_id"
    
    dt <- data.frame(full_corpus_tmp)
    dt$id <- as.numeric(dt$id)
    anti_join(dt, remove, by = "doc_id", copy = FALSE)
  })
  return(results_2)
}
                         







read_merge_nuts_data <- function(nut_shape_files){
  
  
  relevant_files <- nut_shape_files[stringr::str_detect(nut_shape_files, ".shp")]  
  all_nuts_shape <- data.frame()
  for(nuts_shape_file in relevant_files){
    
    tmp <-  sf::read_sf(nuts_shape_file)
    all_nuts_shape <- rbind(all_nuts_shape, tmp)
    
  }
  all_nuts_shape <- all_nuts_shape %>% 
    st_as_sf()
  
  
  all_nuts_shape
}








#remove non-relevant articles w/ top2vec approach

add_corpus_top2vec <- function(clean_text_articles_removed_duplicates,
                               hashing_results,
                               clean_text_articles){
  
  
  top2vec_model <- read_top2vec("R/data/saved_model_21_1_21")[1][[1]]
  

  topics_scores_articles_df <- purrr::map_dfr(unique(clean_text_articles_removed_duplicates$permalink),
                                              function(doc_id_tmp){
                                                topics_per_document(top2vec_model,doc_id_tmp ) %>% 
                                                  dplyr::bind_cols() %>% 
                                                  dplyr::rename(score = ...1,
                                                                topic = ...2) %>% 
                                                  mutate(document_id  = doc_id_tmp)
                                              })
  
  
  
 removed_articles_non_relevant <- topics_scores_articles_df %>% 
    group_by(document_id) %>% 
    slice_max(order_by = score) %>% 
   ungroup() %>% 
    dplyr::filter(topic %in% c(9,10,12,23,25,32,44,48,56,63,88,104))
 
 
 
 clean_text_articles_removed_duplicates <- clean_text_articles_removed_duplicates %>% 
   dplyr::anti_join(removed_articles_non_relevant,  by = c("permalink" = "document_id"))
   
 
 
  
  
  #essentially we return from the already reduced corpus only those that are relevant
 #removed_articles_non_relevant
 
 clean_text_articles <- clean_text_articles %>% 
   dplyr::semi_join(clean_text_articles_removed_duplicates, by = c("permalink" = "permalink"))
  
 
 
 clean_text_articles %>% 
   dplyr::select(!match_py) %>% 

   rowid_to_column("match_py") %>% 
   mutate(match_py  = match_py -1) %>% 
   dplyr::select(!id) %>% 
   rowid_to_column("id")


}







hashing_duplicates_2nd_iteration <- function(clean_text_articles, cutoff_threshold = .9){
  
  full_corpus <- clean_text_articles
  
  
  
    
    
    minhash <- minhash_generator(n = 480, seed = 3552)
    corpus <- TextReuseCorpus(text=full_corpus[,"text"], tokenizer = tokenize_ngrams, n = 5, #so we pass them all files and it does what? 
                              minhash_func = minhash, keep_tokens = TRUE,
                              progress = TRUE)
    
    buckets <- lsh(corpus, bands = 240, progress = TRUE)
    #see people who cite textreuse for advice
    #find candidates
    candidates <- lsh_candidates(buckets) #Locality sensitive hashing (LSH) discovers potential matches among a corpus of documents quickly, so that only likely pairs can be compared.
    lsh_compare(candidates, corpus, jaccard_similarity, progress = FALSE)
    
  
  
  
}











