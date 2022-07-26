library(targets)
library(tarchetypes)
library(jsonlite)
library(leaflet)
library(RColorBrewer)
library(htmltools)
library(httr)
library(rvest)
library(ggmap)
library(ggbeeswarm)
library(patchwork)
library(XML)
library(geosphere)
library(htmltools)
library(ggsci)
library(treemapify)
library(rlist)
library(stringr)
library(themis)
library(tidyverse)
library(textreuse)
library(lubridate)
library(RColorBrewer)
library(janitor)
library(sf)
library(ncdf4)
library(raster)
library(spacyr)
library(magrittr)
library(readr)
library(readtext)
library(tidytext)
library(raster)
library(ggside)
library(leaflet)
library(stringdist)
library(reticulate)
library(rgdal)
library(tidymodels)
library(discrim)
library(textrecipes)
library(gt)
library(ggpubr)
library(stargazer)
library(tidypredict)
library(readxl)
library(furrr)
library(future)
reticulate::use_python("C:\\Users\\sodoge\\Anaconda3\\envs\\pipeline_targets_reticulate/python.exe") # on windows UFZ


reticulate::source_python('~/GitHub/drouht_text_data_mining/R/top2vec_model.py')


source("R/data_import_cleaning.R")
source("R/classification_models.R")
source("R/validation_data_processing.R")
source("R/location_modelling.R")
source("R/location_impact_synthesis.R")
source("R/paper_figures.R")
source("R/paper_figures_validation.R")
source("R/keyword_based_scheme.R")




source("R/shiny_dashboard_functions_app.R")
reticulate::source_python('R/python_spacy_ner_pipe_module.py')
reticulate::source_python('R/top2vec_model.py')

spacyr::spacy_initialize(condaenv = "pipeline_targets_reticulate",
                         model = "de_core_news_lg")





path_to_data <- function(){
  list.files("R/data/raw_text_data", recursive = TRUE, full.names = TRUE)
}



# Set target-specific options such as packages.
tar_option_set(packages = "dplyr")



#experimenting with static branching
values <- tibble(
  impact_class = c("Agriculture", "Livestock", "Forestry",
                   "X5.3.fire", "X1.1.Crops",
                   "X2.3.food", "X4.4.transpo",
                   "X3.1.tree", "X3.2.timber",
                   "X3.3.pest", "X3.4.dead")
)





# End this file with a list of target objects.
list(
  
  
  tarchetypes::tar_map(
    values = values,
   
    
    
    
    
    tar_target(pre_process_training_dataset,
               pre_process_specific_training_data(supervised_training_test_data, impact_class)),
    
    tar_target(model_datasets_training,
               create_supervised_training_dataset(pre_process_training_dataset, "training")),
    
    tar_target(model_datasets_testing,
               create_supervised_training_dataset(pre_process_training_dataset, "testing")),
    
    tar_target(tidymodel_classifier, 
               train_fit_tidymodel(model_datasets_testing,
                                   model_datasets_training
                                   )),
    #predict the classes
    targets::tar_target(predict_class, 
                        predict_tidymodels(tidymodel_classifier, 
                                           non_relevant_topics, 
                                           prediction_class = impact_class))
    

  ),
  
  
  #target  raw newspaper articles
  targets::tar_target(raw_data_file, list.files("R/data/raw_text_data", recursive = TRUE, full.names = TRUE),
                      format = "file"),
  #import raw newspaper articles
  targets::tar_target(raw_data, read_wiso_data(raw_data_file)), #for test purposes use fewer articles here
  
  #clean imported_articles
  targets::tar_target(clean_text_articles, clean_newspaper_articles(raw_data)),
  
  
  #remove duplicates, hashing 
  targets::tar_target(hashing_results, hashing_duplicates(clean_text_articles)),
  
  
  #second round on reduced corpus
  targets:::tar_target(hashing_results_round2, hashing_duplicates_2nd_iteration(non_relevant_topics)),
  
  
  targets::tar_target(clean_text_articles_removed_duplicates,
                      remove_hashing_duplicates(hashing_results, 
                                                clean_text_articles)),
  
  
  
  
  
  
  
  
  #geonames target
  targets::tar_target(geonames_data_file, 
                      "R/data/geonames/geonames_data_w_cleaned_tokens.RData", 
                      format = "file"),
  targets::tar_target(geonames_data, process_geonames_data(geonames_data_file)),
  
  
  targets::tar_target(impact_locations_b, model_impact_locations_b(ner_annotations_spacy,
                                                                   geonames_data,
                                                                   non_relevant_topics,
                                                                   nuts_geo_data,
                                                                   predict_impact_classes)),
  
  
  
  targets::tar_target(cluster_impact_locations, cluster_article_locations(impact_locations_b)),
  targets::tar_target(clustered_impact_locations, set_cluster_cutoff_threshold(cluster_impact_locations,
                                                                               cutoff_threshold = 140000)),
  
  
  #predict the actual location from the cluster data
  targets::tar_target(predicted_cluster_locations, predict_cluster_location(clustered_impact_locations, level  = "NUTS3")),
  
  
  #target for new impact locations dataset
  
  targets::tar_target(impacts_locations_dataset_b, 
                      dplyr::bind_rows(
                      merge_locations_impacts_function_b(predict_impact_classes, 
                                                         predicted_cluster_locations , 
                                                         clustered_impact_locations,
                                                         non_relevant_topics,
                                                         nuts_geo_data,
                                                         beta_cutof_threshold = 0.8,
                                                         time_interval_length = 1,
                                                         geonames_data,
                                                         hashing_results_round2,
                                                         unit_output = "nuts3"),
                      merge_locations_impacts_function_b(predict_impact_classes, 
                                                         predicted_cluster_locations , 
                                                         clustered_impact_locations,
                                                         non_relevant_topics,
                                                         nuts_geo_data,
                                                         beta_cutof_threshold = 0.8,
                                                         time_interval_length = 1,
                                                         geonames_data,
                                                         hashing_results_round2,
                                                         unit_output = "nuts2"),
                      merge_locations_impacts_function_b(predict_impact_classes, 
                                                         predicted_cluster_locations , 
                                                         clustered_impact_locations,
                                                         non_relevant_topics,
                                                         nuts_geo_data,
                                                         beta_cutof_threshold = 0.8,
                                                         time_interval_length = 1,
                                                         geonames_data,
                                                         hashing_results_round2,
                                                         unit_output = "nuts1"))),
  
                      
                      
                      
  
  
  
  
  

  
  #perform NER on corpus
  targets::tar_target(ner_annotations_spacy, as.data.frame(process_ner(non_relevant_topics))),
  
  
  
  
  
  
  

  
  
  
  #model training data
  targets::tar_target(supervised_training_test_data_file, "R/data/supervised_training_test_data.csv",
                      format = "file"),
  
  #read training and testing data
  targets::tar_target(
    supervised_training_test_data,
    read.csv(supervised_training_test_data_file)
  ),
  
  
  


  
  #dataset target which collects the predicted impacts from all individual classifiers
  targets::tar_target(predict_impact_classes, 
                      collect_single_impact_classes(
                                                    predict_class_Livestock,
                                                    predict_class_Forestry,
                                                    predict_class_Agriculture,
                                                    predict_class_X3.4.dead,
                                                    predict_class_X4.4.transpo,
                                                    predict_class_X3.3.pest,
                                                    predict_class_X3.2.timber,
                                                    predict_class_X3.1.tree,
                                                    predict_class_X2.3.food,
                                                    predict_class_X5.3.fire,
                                                    predict_class_X1.1.Crops)),
  
  
  
  # 
  # #predict with mariana's keywords
  # targets::tar_target(keyword_based_classification_dataset, 
  #                     evaluate_de_brito_keyword_search(non_relevant_topics, raw_data)),
  # targets::tar_target(dataset_final_keywords, 
  #                     create_spatial_temporal_keywords_based_dataset(keyword_based_classification_dataset, 
  #                                                                                predicted_cluster_locations , 
  #                                                                                clustered_impact_locations,
  #                                                                                non_relevant_topics,
  #                                                                                nuts_geo_data,
  #                                                                                geonames_data,
  #                                                                                hashing_results_round2)),
  # 
  
  
  
  
  
  
  #nuts data target
  targets::tar_target(nut_shape_files, list.files("R/data/nuts", recursive = TRUE, full.names = TRUE)),
  
  #read and merge nuts data files
  targets::tar_target(nuts_geo_data, read_merge_nuts_data(nut_shape_files)),
  
  
  
  
  

  
  
  #shiny dashboard
  targets::tar_target(shiny_app_dashboard_overview, dashboard_shiny_app(impacts_locations_dataset_b, nuts_geo_data
                                                                        )),
  
  
  
  
  
  
  
  #targets for creating the validation DB table
  
  #targets for files first
  targets::tar_target(crop_yield_data_file, "R/data/crop_yields_genesis_nuts1.csv",
                      format = "file"),
  
  
  targets::tar_target(crop_yield_data_nuts2, "R/data/genis_yields_nuts2.csv",
                      format = "file"),
  
  
  targets::tar_target(forest_fire_data_file, "R/data/forest_fire_data.xlsx",
                      format = "file"),
  targets::tar_target(drought_awareness_files, list.files("R/data/durre_trockenheit_google_trends", 
                                                  recursive = TRUE, full.names = TRUE)),
  
  #create database
  targets::tar_target(validation_database,
                      create_validation_database(drought_awareness_files,
                                                 forest_fire_data_file,
                                                 crop_yield_data_file,
                                                 nuts_geo_data,
                                                 crop_yield_data_nuts2
                                                 )),
  
  
  
  
  
  #tarchetypes::tar_render(report_file, "empirical_validation_report.Rmd"),
  

  
  
 

  
  
  #drought monitor data for validation
  #evaluating the location extraction
  targets::tar_target(drought_monitor_validation_data, 
                      "R/data/drought_monitor/248981_SMI_SM_Lall_Gesamtboden_monatlich_1951-2020_inv.nc",
                      format = "file"),
  
  targets::tar_target(process_drought_monitor_data,
                      create_drought_monitor_data(drought_monitor_validation_data, nuts_geo_data)),
  
  
  #preicpitation data
  targets::tar_target(precipitation_validation_data_files, 
                      list.files("R/data/precipitation_data", full.names = TRUE),
                      format = "file"),
  
  
  targets::tar_target(process_precipitation_dataset, 
                      process_precipitation_data(precipitation_validation_data_files)),
  
  
  
  #find topics that are not relevant and return those
  targets::tar_target(non_relevant_topics, add_corpus_top2vec(clean_text_articles_removed_duplicates,
                                                              hashing_results,
                                                              clean_text_articles)),
  
  
  targets::tar_target(paper_figures_classifier_eval, 
                      create_figures_classifier_eval(supervised_training_test_data,
                                                     impacts_locations_dataset_b)),



targets::tar_target(paper_figures_dataset_descriptives, 
                    create_figures_dataset_descriptives(predict_impact_classes,
                                                        impacts_locations_dataset_b,
                                                        nuts_geo_data
                                                   )),


targets::tar_target(paper_validation_figures,
                    
                    get_empirical_validation_figures(validation_database, impacts_locations_dataset_b,
                                                     process_drought_monitor_data,
                                                     process_precipitation_dataset))
)
  
  
  
  
  
  






