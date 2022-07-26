








collect_single_impact_classes <- function(...){
  
  predictions <- list(...)
  
  
  merged_df_of_impacts <- data.frame()
  for(dataset in  predictions){
    
    
    merged_df_of_impacts <- dplyr::bind_rows(merged_df_of_impacts,
                                             dataset)
    
    
  }
  
  
  merged_df_of_impacts
  
  
  
}






pre_process_specific_training_data <- function(raw_data, training_variable, index = .75){
  raw_data$training_variable <- raw_data[, as.character(training_variable)] 
  
  data <-  raw_data %>% 
    filter(Relevant..1.yes. == 1) %>% 
    dplyr::select(id, text, training_variable) %>% 
    group_by(id) %>% 
    summarise("{training_variable}" := (ifelse(sum(training_variable)>0,1,0)))%>% 
    left_join(raw_data %>% dplyr::select(id, text), by = c("id" = "id")) %>% 
    mutate("{training_variable}" := as.character(.[[2]]))
  
  
  data$strata_needed <- pull(data, training_variable)
  
  
  
  
  articles_split <- initial_split(data, strata = strata_needed, prop = index)
  return(articles_split)
  
  
}



#' Title
#'
#' @param raw_data 
#' @param training_variable 
#'
#' @return
#' @export
#'
#' @examples
create_supervised_training_dataset <- function(articles_split, type_split,
                                               lemmatization = FALSE,
                                               ner_method = "rspacy"){
  
  #load stopwords 
  stopwords_german <- read_csv("R/data/stopwords_german.csv", 
                               col_names = FALSE, show_col_types = FALSE) 
  
  
  complaints_train <- training(articles_split)
  complaints_test <- testing(articles_split)
  
  if(type_split=="training"){
    
    
    if(lemmatization == FALSE){
      
     if(ner_method == "spacy"){
      complaints_train <- process_ner(complaints_train) %>% 
        as.data.frame() %>% 
        dplyr::select(doc_id, token, pos) %>% 
        dplyr::filter(!pos %in% c("PUNCT", "NUM", "SPACE")) %>% 
        mutate(token = tolower(token)) %>% 
        dplyr::anti_join(stopwords_german, by = c("token" = "X1")) %>% 
        dplyr::filter(token != "seite") %>% 
        group_by(doc_id) %>% 
        summarize(text = str_c(token, collapse = " ")) %>%
        mutate(doc_id = as.numeric(str_remove(doc_id, "doc"))) %>% 
        arrange(doc_id) %>% 
        ungroup() %>% 
        dplyr::bind_cols(complaints_train) %>% 
        dplyr::select(text...2, id, strata_needed) %>% 
        dplyr::rename(text = text...2)
     }
        
    
      if(ner_method == "rspacy"){
      complaints_train <- complaints_train %>%
        mutate(doc_id = paste0("doc", row_number())) %>%
        dplyr::select(doc_id, everything()) %>%
        spacy_parse() %>%
        dplyr::filter(!pos %in% c("PUNCT", "NUM", "SPACE")) %>%
        mutate(token = tolower(token)) %>%
        dplyr::anti_join(stopwords_german, by = c("token" = "X1")) %>%
        dplyr::filter(token != "seite") %>%
        group_by(doc_id) %>%
        summarize(text = str_c(token, collapse = " ")) %>%
        mutate(doc_id = as.numeric(str_remove(doc_id, "doc"))) %>%
        arrange(doc_id) %>%
        ungroup() %>%
        dplyr::bind_cols(complaints_train) %>%
        dplyr::select(text...2, id, strata_needed) %>%
        dplyr::rename(text = text...2)
      }
      
      
      
    }
    if(lemmatization == TRUE){
      
      
      if(ner_method == "rspacy"){
      complaints_train <- complaints_train %>%
        mutate(doc_id = paste0("doc", row_number())) %>%
        dplyr::select(doc_id, everything()) %>%
        spacy_parse() %>% 
        dplyr::filter(!pos %in% c("PUNCT", "NUM", "SPACE")) %>% 
        mutate(lemma = tolower(lemma)) %>% 
        dplyr::anti_join(stopwords_german, by = c("lemma" = "X1")) %>% 
        dplyr::filter(lemma != "seite") %>% 
        group_by(doc_id) %>% 
        summarize(text = str_c(lemma, collapse = " ")) %>%
        mutate(doc_id = as.numeric(str_remove(doc_id, "doc"))) %>% 
        arrange(doc_id) %>% 
        ungroup() %>% 
        dplyr::bind_cols(complaints_train) %>% 
        dplyr::select(text...2, id, strata_needed) %>% 
        dplyr::rename(text = text...2)
      }
      
      if(ner_method == "spacy"){
        complaints_train <- process_ner(complaints_train) %>% 
          as.data.frame() %>% 
          dplyr::select(doc_id, token, pos, lemma) %>% 
          dplyr::filter(!pos %in% c("PUNCT", "NUM", "SPACE")) %>% 
          mutate(lemma = tolower(lemma)) %>% 
          dplyr::anti_join(stopwords_german, by = c("lemma" = "X1")) %>% 
          dplyr::filter(lemma != "seite") %>% 
          group_by(doc_id) %>% 
          summarize(text = str_c(lemma, collapse = " ")) %>%
          mutate(doc_id = as.numeric(str_remove(doc_id, "doc"))) %>% 
          arrange(doc_id) %>% 
          ungroup() %>% 
          dplyr::bind_cols(complaints_train) %>% 
          dplyr::select(text...2, id, strata_needed) %>% 
          dplyr::rename(text = text...2)
      }
    }
    return(complaints_train)
  }
  
  
  
  
  
  
  
  
  if(type_split=="testing"){
    
    
    if(lemmatization == FALSE){
      
      
      if(ner_method == "rspacy"){
      complaints_test <- complaints_test %>%
        mutate(doc_id = paste0("doc", row_number())) %>%
        dplyr::select(doc_id, everything()) %>%
        spacy_parse() %>% 
        dplyr::filter(!pos %in% c("PUNCT", "NUM", "SPACE")) %>% 
        mutate(token = tolower(token)) %>% 
        dplyr::anti_join(stopwords_german, by = c("token" = "X1")) %>% 
        dplyr::filter(token != "seite") %>% 
        group_by(doc_id) %>% 
        summarize(text = str_c(token, collapse = " ")) %>%
        mutate(doc_id = as.numeric(str_remove(doc_id, "doc"))) %>% 
        arrange(doc_id) %>% 
        ungroup() %>% 
        dplyr::bind_cols(complaints_test) %>% 
        dplyr::select(text...2, id, strata_needed) %>% 
        dplyr::rename(text = text...2)
      }
      
      
      if(ner_method == "spacy"){
        complaints_test <-  process_ner(complaints_test) %>% 
          as.data.frame() %>% 
          dplyr::select(doc_id, token, pos) %>% 
          dplyr::filter(!pos %in% c("PUNCT", "NUM", "SPACE")) %>% 
          mutate(token = tolower(token)) %>% 
          dplyr::anti_join(stopwords_german, by = c("token" = "X1")) %>% 
          dplyr::filter(token != "seite") %>% 
          group_by(doc_id) %>% 
          summarize(text = str_c(token, collapse = " ")) %>%
          mutate(doc_id = as.numeric(str_remove(doc_id, "doc"))) %>% 
          arrange(doc_id) %>% 
          ungroup() %>% 
          dplyr::bind_cols(complaints_test) %>% 
          dplyr::select(text...2, id, strata_needed) %>% 
          dplyr::rename(text = text...2)
      }
      
      
      
      
      
      
      
      
      
      
      
    }
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    if(lemmatization == TRUE){
      
      if(ner_method == "rspacy"){
      complaints_test <- complaints_test %>%
        mutate(doc_id = paste0("doc", row_number())) %>%
        dplyr::select(doc_id, everything()) %>%
        spacy_parse() %>% 
        dplyr::filter(!pos %in% c("PUNCT", "NUM", "SPACE")) %>% 
        mutate(lemma = tolower(lemma)) %>% 
        dplyr::anti_join(stopwords_german, by = c("lemma", "X1")) %>% 
        dplyr::filter(lemma != "seite") %>% 
        group_by(doc_id) %>% 
        summarize(text = str_c(lemma, collapse = " ")) %>%
        mutate(doc_id = as.numeric(str_remove(doc_id, "doc"))) %>% 
        arrange(doc_id) %>% 
        ungroup() %>% 
        dplyr::bind_cols(complaints_test) %>% 
        dplyr::select(text...2, id, strata_needed) %>% 
        dplyr::rename(text = text...2)
      }
      
      
      if(ner_method == "rspacy"){
        complaints_test <- process_ner(complaints_test) %>% 
          as.data.frame() %>% 
          dplyr::select(doc_id, token, pos, lemma) %>% 
          dplyr::filter(!pos %in% c("PUNCT", "NUM", "SPACE")) %>% 
          mutate(lemma = tolower(lemma)) %>% 
          dplyr::anti_join(stopwords_german, by = c("lemma" = "X1")) %>% 
          dplyr::filter(lemma != "seite") %>% 
          group_by(doc_id) %>% 
          summarize(text = str_c(lemma, collapse = " ")) %>%
          mutate(doc_id = as.numeric(str_remove(doc_id, "doc"))) %>% 
          arrange(doc_id) %>% 
          ungroup() %>% 
          dplyr::bind_cols(complaints_test) %>% 
          dplyr::select(text...2, id, strata_needed) %>% 
          dplyr::rename(text = text...2)
        
      }
      
      
      
      
      
      
      
      
    }
    
    
    
    
    
    
    
    
    
    return(complaints_test)
  }
  
  
  
  
  
}







#' Title
#'
#' @param data 
#'
#' @return
#' @export
#'
#' @examples
train_fit_tidymodel <- function(complaints_test,
                                complaints_train,
                                lemmatization = FALSE){
  
  
  
  
  
  complaints_train <- complaints_train %>% 
    mutate(strata_needed = as.factor(strata_needed)) %>% 
    drop_na()
  
  
  #define the model w/tuning params
  tune_spec <- logistic_reg(penalty = tune(), mixture = 1) %>%
    set_mode("classification") %>%
    set_engine("glmnet")
  
  #grid with values for tuning
  lambda_grid <- grid_regular(penalty(), levels = 30)
  
  
  #recipe
  articles_rec <- recipe(strata_needed ~ text, 
                         data = complaints_train) %>% 
    themis::step_downsample(strata_needed) %>% 
    step_tokenize(text) %>%
    step_tokenfilter(text, max_tokens = 1e3) %>%
    step_tfidf(text)
  
  
  
  #create workflow with recipe and model w/ tuning param
  tune_wf <- workflow() %>%
    add_recipe(articles_rec) %>%
    add_model(tune_spec)
  
  
  #for cross-validation
  complaints_folds <- vfold_cv(complaints_train, strata = strata_needed)
  #tune the grid
  tune_rs <- tune::tune_grid(
    tune_wf,
    complaints_folds,
    grid = lambda_grid,
    control = control_resamples(save_pred = TRUE)
  )
  #select best performing model
  chosen_auc <- tune_rs %>%
    select_by_one_std_err(metric = "roc_auc", -penalty)
  final_lasso <- finalize_workflow(tune_wf, chosen_auc)
  fitted_lasso <- fit(final_lasso, complaints_train)
  return(fitted_lasso)
}





#' Title
#'
#' @param tidymodel_object 
#' @param prediction_data 
#'
#' @return
#' @export
#'
#' @examples
predict_tidymodels <- function(tidymodel_object, prediction_data, prediction_class){
  
  
  bind_cols(prediction_data,
            predict(tidymodel_object, prediction_data)) %>% 
    dplyr::select(id, text, .pred_class, match_py) %>% 
    mutate(type_of_class = prediction_class)
  
  
}


