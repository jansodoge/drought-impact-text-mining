library(ggbeeswarm)
library(patchwork)
library(ggpubr)
library(ggsci)


impact_class_titles <- data.frame(short_abbv  = c("class_x_1_1",
                                                  "class_x_1_2",
                                                  "class_x_1_3",
                                                  "class_x_1_4",
                                                  "class_x_1_5",
                                                  "class_x_2_1",
                                                  "class_x_2_2",
                                                  "class_x_2_3",
                                                  "class_x_2_4",
                                                  "class_x_2_5",
                                                  "class_x_3_1",
                                                  "class_x_3_2",
                                                  "class_x_3_3",
                                                  "class_x_3_4",
                                                  "class_x_3_5",
                                                  "class_x_4_1",
                                                  "class_x_4_2",
                                                  "class_x_4_3",
                                                  "class_x_4_4",
                                                  "class_x_4_5",
                                                  "class_x_5_1",
                                                  "class_x_5_2",
                                                  "class_x_5_3",
                                                  "class_x_5_4",
                                                  "class_x_5_5"),
                                  
                                  name_new = c("reduced_crop_productivity",
                                               "early_harvesting",
                                               "increased_irrigation_need",
                                               "economic_losses_agriculture",
                                               "need_economic_help",
                                               "reduced_livestock_productivity",
                                               "forced_livestock_reduction",
                                               "shortage_feed_livestock",
                                               "general_animal_impacts",
                                               "economic_losses_livestock",
                                               "reduced_tree_growth",
                                               "decrease_forestry_products",
                                               "increase_pests_diseases",
                                               "increase_tree_dieback",
                                               "economic_losses_forestry",
                                               "reduced_hydropower_production",
                                               "impaired_production_power_plant",
                                               "impaired_industrial_production",
                                               "impaired_stream_navigability",
                                               "increased_cost_energy_transport_infrastructure",
                                               "parks_tourism_recreation",
                                               "forest_and_wildfires",
                                               "positive_impacts",
                                               "conflicts",
                                               "other_environmental_health_problems"))
  
  






evaluate_de_brito_keyword_search <- function(clean_text_articles, raw_data){
  
  
  
  
  clean_text_articles <- clean_text_articles %>% 
    dplyr::left_join(raw_data, by = c("permalink" = "permalink")) %>% 
    dplyr::select(permalink, text.y, match_py, id) %>% 
    unnest_tokens(output = sentence, 
                  input = text.y,
                  token  ="sentences") %>% 
    mutate(text = sentence)
  
  
  #impact class 1
  x_1_1 <-  stringr::str_detect(clean_text_articles$text, "missernte|ernteausfälle|ertragseinbuß|ernteschäden|ernteverluste|ernteeinbußen|weniger ernte|schlechte ernte|ernte eingefahren|weniger geerntet|pflanzenkrankheit") | (
    stringr::str_detect(clean_text_articles$text, "ernte|getreide|raps|feld|geerntet|erntemenge") &
      stringr::str_detect(clean_text_articles$text, "einbußen|verluste|trockenstress|befall|schädlinge|krankheiten|schäden|gefährdet|stress|weniger|gering|prozent"))
  x_1_2 <- stringr::str_detect(clean_text_articles$text,
                               "früher geernte|frühere ernte") | (
                                 stringr::str_detect(clean_text_articles$text,
                                                     "ernte|getreide|raps|feld|geerntet|erntemenge") &
                                   stringr::str_detect(clean_text_articles$text,
                                                       "früh|biomasse") &
                                   stringr::str_detect(clean_text_articles$text,
                                                       "!frühjahr"))
  x_1_3 <- stringr::str_detect(clean_text_articles$text,
                               "landwirtschaft|ernte|getreide|raps|feld|ererntet") &
    stringr::str_detect(clean_text_articles$text,
                        "bewässer|wassermangel|wasserknappheit|wenig waasser|fehlt wasser|kein wasser|beregnung") 
  x_1_4 <- stringr::str_detect(clean_text_articles$text,
                               "landwirtschaft|ernte|getreide|raps|feld|geerntet|erntemenge|bauer") &
    stringr::str_detect(clean_text_articles$text,
                        "euro|wirtschaftlich|preisverfall")
  x_1_5 <- stringr::str_detect(clean_text_articles$text,
                               "staatliche hilfen|hilfen beantragen|entschädigung|dürrehilfe|dürre-hilfen|zuschüsse|unterstützung|finanzielle hilfe|finanzielle hilfen")
  
  
  
  
  
  
  x_2_1 <- stringr::str_detect(clean_text_articles$text,
                               "brut|züchten|züchtng|milch|käse|honig|milchkühe|vieh|kühe|kuh|rinde|schafe|schweine|schlachtkühe|schlachtrinder") &
    stringr::str_detect(clean_text_articles$text,
                        "verluste|befall|schädlinge|schäden|gefährdet|stress|weniger|gering")
  
  
  x_2_2 <- stringr::str_detect(clean_text_articles$text,
                               "notschlachtung") | (
                                 stringr::str_detect(clean_text_articles$text,
                                                     "vieh|kühe|kuh|rinder|schafe|schweine|schlachtkühe|schlachtrinder") &
                                   stringr::str_detect(clean_text_articles$text,
                                                       "schlachtung|früh|geschlacht|schlacht"))  
  
  
  x_2_3 <- stringr::str_detect(clean_text_articles$text,
                               "futterknappheit|futternot|futtergewinnung|futtermangel|kein futter|futter knapp|nicht genug futter|brauche futter|hunger|wenig nahrung|futter|futtermittel|nahrung|tierfutter|ernährung|futterreserven|nahrungsangebot")
  
  
  x_2_4 <- stringr::str_detect(clean_text_articles$text, 
                               "tierpark|eichhörnchen|insektensterben|vögel|störche|biene|amphibien|wespen|specht|elefanten|rehwild|frösche|kaninchen|igel") & 
    stringr::str_detect(clean_text_articles$text,
                        "einbußen|verluste|befall|schädliche|schäden|gefährdet|tot|stress|belast")
  
  
  x_2_5 <- stringr::str_detect(clean_text_articles$text,
                               "kühe|kuh|rinder|schafe|schweine|schlachtkühe|schlachtrinder|futter|futtermittel|nahrung|tierfutter|ernährung|futterreserven|nahrungsangebot")  &
    stringr::str_detect(clean_text_articles$text,
                        "euro|wirtschaftlich|kosten")
  
  
  
  
  x_3_1 <- stringr::str_detect(clean_text_articles$text,
                               "waldschäden|geringes wachstum|bäume vertrocknet|vertrocknete bäume|schädigt bäume|bäume geschädtigt|schwächen bäume") | (
                                 stringr::str_detect(clean_text_articles$text,
                                                     "bäume|sträucher|büsche|wäd|blätter|forst") & 
                                   stringr::str_detect(clean_text_articles$text,
                                                       "trockenstress|wachstum|braun|einbußen|verluste|gefährdet|vertrockne|geschädigt"))  
  
  x_3_2 <- stringr::str_detect(clean_text_articles$text,
                               "tannen|fichten|weihnachtsb|schadholz|pilze")
  
  x_3_3 <- stringr::str_detect(clean_text_articles$text,
                               "borkenkäfer|käfer|befallene bäume|bäume befallen|insektenbefall|schädlinge|befall|kranke bäume|plage|paralisten|rußrindenkrankheit")
  
  
  x_3_4 <- stringr::str_detect(clean_text_articles$text,
                               "waldsterben|toten bäume|tote bäume|baumsterben") | (
                                 stringr::str_detect(clean_text_articles$text,
                                                     "bäume|baum|sträucher|büsche|wäld|blätter|forst|saatgut") & 
                                   stringr::str_detect(clean_text_articles$text,
                                                       "sterben|stirbt|tot|absterbende|abgestorben|abgestorbener|absterben"))
  
  
  
  x_3_5 <- stringr::str_detect(clean_text_articles$text,
                               "bäume|baum|sträcuher|büsche|wäld|wald|blätter|forst|saatgut") &
    stringr::str_detect(clean_text_articles$text,
                        "euro|wirtschaftlich|kosten|überangebot|preisverfall")
  
  
  x_4_1 <- stringr::str_detect(clean_text_articles$text,
                               "kraftwerke|strom|energie")
  
  x_4_2 <- stringr::str_detect(clean_text_articles$text,
                               "atomkraftwerke|kraftwerke|kühlwasser")
  
  x_4_3 <- stringr::str_detect(clean_text_articles$text,
                               "industrie|fabrik")
  
  x_4_4 <- stringr::str_detect(clean_text_articles$text,
                               "transport|fährbetriebe|binnenschiffer|transportkosten|fähren|schiffe|ladekapazität|häfen|frachtschiffe")
  
  
  x_4_5 <- stringr::str_detect(clean_text_articles$text,
                               "industrie|fabrik|transport|kraftwerke|strom|energie|transportkosten|fähren|schiffe|häfen") &
    stringr::str_detect(clean_text_articles$text,
                        "euro|wirtschaftlich|kosten")
  
  
  x_5_1 <- stringr::str_detect(clean_text_articles$text,
                               "grillverbot|sport|schwimm|paddelverbot|kanuverbot|feuerwerk|erholungsfunktion|kleingarten|jäger|gondelfahrten|grillen|orgel|waldbesucher|kerzen|segel|tourist|besucher")
  
  
  
  
  x_5_2 <- stringr::str_detect(clean_text_articles$text,
                               "flammen|feuer|brand|brände|waldbrand|waldbrände|brannt|brennt|verbrannt|gebrannt") 
  
  #& 
   # stringr::str_detect(clean_text_articles$text,
    #                    "!waldbrandgefahr")
  
  
  x_5_3 <- stringr::str_detect(clean_text_articles$text,
                               "positiv")
  
  x_5_4 <- stringr::str_detect(clean_text_articles$text,
                               "konflikt")
  
  x_5_5 <- stringr::str_detect(clean_text_articles$text,
                               "staub|erosion|luftqualität|käranlage|allergi")
  
  
  
  impact_dataset <- tibble(permalink = clean_text_articles$permalink,
                           id = clean_text_articles$id,
                           text = clean_text_articles$text,
                           match_py = clean_text_articles$match_py,
                           class_x_1_1 = x_1_1,
                           class_x_1_2 = x_1_2,
                           class_x_1_3 = x_1_3,
                           class_x_1_4 = x_1_4,
                           class_x_1_5 = x_1_5,
                           class_x_2_1 = x_2_1,
                           class_x_2_2 = x_2_2,
                           class_x_2_3 = x_2_3,
                           class_x_2_4 = x_2_4,
                           class_x_2_5 = x_2_5,
                           class_x_3_1 = x_3_1,
                           class_x_3_2 = x_3_2,
                           class_x_3_3 = x_3_3,
                           class_x_3_4 = x_3_4,
                           class_x_3_5 = x_3_5,
                           class_x_4_1 = x_4_1,
                           class_x_4_2 = x_4_2,
                           class_x_4_3 = x_4_3,
                           class_x_4_4 = x_4_4,
                           class_x_4_5 = x_4_5,
                           class_x_5_1 = x_5_1,
                           class_x_5_2 = x_5_2,
                           class_x_5_3 = x_5_3,
                           class_x_5_4 = x_5_4,
                           class_x_5_5 = x_5_5,
  ) %>% 
    mutate(across(class_x_1_1:class_x_5_5, ~ifelse(. == TRUE,1,0))) %>% 
    pivot_longer(!c(permalink, id, text, match_py), names_to = "type_of_class", values_to = ".pred_class")
  
  
  
  return(impact_dataset)
  
}





#new method for clustering based location detection to create impacts locations dataset

merge_locations_impacts_function_keyword_impacts <- function(predict_impact_classes, 
                                               predicted_cluster_locations , 
                                               clustered_impact_locations,
                                               clean_text_articles,
                                               nuts_geo_data,
                                               beta_cutof_threshold = 0.8,
                                               time_interval_length = 1,
                                               geonames_data,
                                               hashing_results_round2,
                                               unit_output = "nuts3"){
  
  
  
  nuts3 <- nuts_geo_data %>% 
    filter(CNTR_CODE == "DE") %>% 
    filter(LEVL_CODE == 3)
  nuts2 <- nuts_geo_data %>% 
    filter(CNTR_CODE == "DE") %>% 
    filter(LEVL_CODE == 2)
  nuts1 <- nuts_geo_data %>% 
    filter(CNTR_CODE == "DE") %>% 
    filter(LEVL_CODE == 1)
  
  
  #something to remove coarser similiar articles per month+year
  
  #TBD: I need a second round of hashing on the clean corpus since
  #now IDs are all wrong
  
  
  
  removal_candidates <- hashing_results_round2 %>% 
    dplyr::mutate(a = stringr::str_remove(string = a,pattern = "doc-"),
                  b = as.numeric(stringr::str_remove(string = b,pattern = "doc-"))) %>% 
    dplyr::filter(score > beta_cutof_threshold) %>% 
    distinct(b) %>% 
    dplyr::rename(doc_id = b)
  
  
  
  
  
  
  
  #create a DF of articles w/ reference to modelled impact locations via nuts-id
  
  #predicted_cluster locations contains identified impact clusters 
  predicted_cluster_locations <- predicted_cluster_locations %>% 
    group_by(doc_id) %>% 
    count() %>% 
    dplyr::filter(n == 1) %>% #we only select those documents where one cluster could specifically be identified
    #here we need to continue implementing more advanced heuristics at some point
    ungroup() %>% 
    dplyr::left_join(predicted_cluster_locations %>% 
                       dplyr::select(doc_id, clusters), by = c("doc_id" = "doc_id")) %>% 
    dplyr::select(doc_id, clusters) %>% 
    dplyr::left_join(as_tibble(clustered_impact_locations) %>% dplyr::select(!geometry), 
                     by = c("doc_id" = "doc_id", 
                            "clusters" = "clusters")) %>%
    dplyr::left_join(geonames_data, by = c("object_id" = "object_id")) %>% 
    sf::st_as_sf(.) 
  
  
  
  #Intersect locations identified in each cluster with nuts data to see to which nuts the respective impacts identify
  #change here respectively for the different nuts
  
  if(unit_output == "nuts3"){
    predicted_locations_nuts <- sf::st_intersects(predicted_cluster_locations, nuts3)
    
    predicted_locations_nuts <- purrr::map_df(as.list(predicted_locations_nuts),
                                              ~as_tibble(.[1])) %>% 
      dplyr::bind_cols(predicted_cluster_locations) %>% 
      dplyr::select(doc_id, token, nuts_name, nuts_id, object_id, geometry)
    
  }
  
  
  if(unit_output == "nuts2"){
    predicted_locations_nuts <- sf::st_intersects(predicted_cluster_locations, nuts2)
    
    predicted_locations_nuts <- purrr::map_df(as.list(predicted_locations_nuts),
                                              ~as_tibble(.[1])) %>% 
      dplyr::bind_cols(predicted_cluster_locations) %>% 
      dplyr::left_join(nuts2 %>% rowid_to_column(var = "nuts_match") %>% dplyr::select(nuts_match, NUTS_ID) %>%
                         as_tibble() %>% 
                         dplyr::select(!geometry),
                       by = c("value" = "nuts_match")) %>% 
      mutate(nuts_id = NUTS_ID) %>% 
      dplyr::select(doc_id, token, nuts_name, nuts_id, object_id, geometry)   
  }
  
  
  if(unit_output == "nuts1"){
    predicted_locations_nuts <- sf::st_intersects(predicted_cluster_locations, nuts1)
    
    predicted_locations_nuts <- purrr::map_df(as.list(predicted_locations_nuts),
                                              ~as_tibble(.[1])) %>% 
      dplyr::bind_cols(predicted_cluster_locations) %>% 
      dplyr::left_join(nuts1 %>% rowid_to_column(var = "nuts_match") %>% dplyr::select(nuts_match, NUTS_ID) %>%
                         as_tibble() %>% 
                         dplyr::select(!geometry),
                       by = c("value" = "nuts_match")) %>% 
      mutate(nuts_id = NUTS_ID) %>% 
      dplyr::select(doc_id, token, nuts_name, nuts_id, object_id, geometry)   
  }
  
  
  
  
  
  
  #removing this here to save some place
  predicted_locations_nuts <- predicted_locations_nuts %>% 
    dplyr::select(-geometry)
  
  
  
  
  
  #merge locations, impacts and text corpora 
  impacs_locations_dataset <- predict_impact_classes %>% #the predicted impact classes
    dplyr::filter(.pred_class == 1) %>% #use only cases where an impact has been predicted
    dplyr::left_join(clean_text_articles %>% #join each impact to a text
                       dplyr::select(id, text, header, date, newspaper),
                     by = c("id" = "id")) %>% 
    dplyr::left_join(predicted_locations_nuts, by = c("id" = "doc_id")) %>% #now add the locations of an impact
    dplyr::rename(text  = text.y) %>% 
    dplyr::select(id, text, type_of_class, header, date, newspaper, token, nuts_id, 
                  nuts_name, object_id) %>% 
    drop_na(type_of_class) %>% 
    drop_na(nuts_id) %>% 
    mutate(lubridate_format_date = as.Date(date,
                                           tryFormats = c("%d.%m.%Y"))) %>% 
    mutate(month_extracted = lubridate::month(lubridate_format_date),
           year_extracted = lubridate::year(lubridate_format_date))
  
  
  
  
  #remove those articles from hashing (prelim here)
  impacs_locations_dataset <- impacs_locations_dataset %>% 
    dplyr::anti_join(removal_candidates, by = c("id" = "doc_id"))
  
  
  
  
  
  #create time intervals around each drought impact
  impacs_locations_dataset_time_intervals <- impacs_locations_dataset %>% 
    mutate(lubridate_format_date = as.Date(date,
                                           tryFormats = c("%d.%m.%Y")))
  
  
  
  
  impacs_locations_dataset_time_intervals <- impacs_locations_dataset_time_intervals %>% 
    mutate(doubled_occurence = NA) %>% 
    group_by(type_of_class, nuts_id) %>% 
    group_map( ~ {
      iterating_for_df <- .x %>% 
        as_tibble()
      index <- 1
      return_df <- data.frame()
      
      while(index < nrow(iterating_for_df)){
        
        if(is.na(iterating_for_df$doubled_occurence[index])){
          
          next_day <- iterating_for_df$lubridate_format_date[index] + 
            lubridate::days(1) # remove these that appear one day after
          current_day <- iterating_for_df$lubridate_format_date[index]
          
          
          iterating_for_df$doubled_occurence[iterating_for_df$lubridate_format_date == next_day | iterating_for_df$lubridate_format_date == current_day] <- "to_remove"
          
          return_df <- dplyr::bind_rows(return_df, 
                                        iterating_for_df[index,])
          
        }
        index <- index + 1
        
      }
      #iterating_for_df <- iterating_for_df %>% 
      #  dplyr::filter(is.na(doubled_occurence))
      
      return(return_df)
      #return(iterating_for_df)
    }, .keep = TRUE) %>% 
    bind_rows()
  
  
  
  
  impacs_locations_dataset_time_intervals_reduced <- impacs_locations_dataset_time_intervals %>% 
    dplyr::select(date, nuts_id, type_of_class, id) %>% 
    drop_na(nuts_id) %>% 
    drop_na(date) %>% 
    drop_na(type_of_class) %>% 
    mutate(lubridate_format_date = as.Date(date, tryFormats = c("%d.%m.%Y"))) %>% 
    mutate(month_date = lubridate::month(lubridate_format_date),
           year_date = lubridate::year(lubridate_format_date)) %>% 
    
    group_by(nuts_id, type_of_class, month_date, year_date) %>% 
    count(name = "MIS")  %>% 
    mutate(statistical_unit = unit_output)
  
  
  
  
  
  
  
  
  
  
  return(impacs_locations_dataset_time_intervals_reduced)
  
  
  
  
  
  
}




create_spatial_temporal_keywords_based_dataset <- function(keyword_based_classification_dataset, 
                                                           predicted_cluster_locations , 
                                                           clustered_impact_locations,
                                                           non_relevant_topics,
                                                           nuts_geo_data,
                                                           geonames_data,
                                                           hashing_results_round2){


nuts_3_level_data <- merge_locations_impacts_function_keyword_impacts(keyword_based_classification_dataset, 
                                   predicted_cluster_locations , 
                                   clustered_impact_locations,
                                   non_relevant_topics,
                                   nuts_geo_data,
                                   beta_cutof_threshold = 0.8,
                                   time_interval_length = 1,
                                   geonames_data,
                                   hashing_results_round2,
                                   unit_output = "nuts3")

nuts_2_level_data <- merge_locations_impacts_function_keyword_impacts(keyword_based_classification_dataset, 
                                                                      predicted_cluster_locations , 
                                                                      clustered_impact_locations,
                                                                      non_relevant_topics,
                                                                      nuts_geo_data,
                                                                      beta_cutof_threshold = 0.8,
                                                                      time_interval_length = 1,
                                                                      geonames_data,
                                                                      hashing_results_round2,
                                                                      unit_output = "nuts2")


nuts_1_level_data <- merge_locations_impacts_function_keyword_impacts(keyword_based_classification_dataset, 
                                                                      predicted_cluster_locations , 
                                                                      clustered_impact_locations,
                                                                      non_relevant_topics,
                                                                      nuts_geo_data,
                                                                      beta_cutof_threshold = 0.8,
                                                                      time_interval_length = 1,
                                                                      geonames_data,
                                                                      hashing_results_round2,
                                                                      unit_output = "nuts1")

dplyr::bind_rows(nuts_3_level_data, nuts_1_level_data, nuts_2_level_data )


}






####function for generating validation files of dataset with keywords


get_keywords_dataset_validation_results <- function(validation_database, dataset_final_keywords,
                                                    process_drought_monitor_data,
                                                    process_precipitation_dataset){
  
  
  impacts_locations_dataset_b <- dataset_final_keywords %>% 
    dplyr::left_join(impact_class_titles, by = c("type_of_class" = "short_abbv")) %>% 
    ungroup() %>% 
    dplyr::select(!type_of_class) %>% 
    dplyr::rename(type_of_class = name_new  )
  
  
  
  
  
  
  
  
  number_of_articles_wiso_db <<- data.frame(year_date = 
                                              seq(2000,2021,1),
                                            articles = 
                                              c(3727202, 3805202, 4046952, 5279895,
                                                6539592, 7031086, 7546534, 7769275,
                                                8381606, 8416353, 8652610, 9316843, 
                                                9447050, 9178527, 9121899, 9098091,
                                                10399161, 12321421, 13537762, 13428658,
                                                11360680, 11693705))
  
  nut_conversions <<- data.frame(
    nut_text = c(
      "Brandenburg/Berlin", "Brandenburg", "Baden-Wuerttemberg",
      "Bayern","Hessen","Mecklenburg-Vorpommern",
      "Niedersachsen","Niedersachsen/Hamburg/Bremen",
      "Nordrhein-Westfalen","Rheinland-Pfalz","Schleswig-Holstein",
      "Saarland","Sachsen","Sachsen-Anhalt","Thueringen/Sachsen-Anhalt", "Thueringen",
      "Deutschland"),
    nuts_short = c(
      NA,"DE4","DE1","DE2","DE7","DE8","DE9",NA,"DEA","DEB",
      "DEF","DEC","DED","DEE",NA,"DEG",NA))
  
  
  
  
  ##############################################
  #### Validating agricultural yields
  #############################################
  validate_agriculture_crop_specific <- function(validation_database, 
                                                 impacts_locations_dataset_b,
                                                 crop_selected = "Silomais",
                                                 nuts_level_selected = "nuts1"){
    
    if(nuts_level_selected == "nuts1"){
      validation_series <- validation_database %>% 
        dplyr::filter(sector == "agricultural_yields") %>% 
        dplyr::filter(unit == crop_selected)}
    if(nuts_level_selected == "nuts2"){
      validation_series <- validation_database %>% 
        dplyr::filter(sector == "agricultural_yields_nuts2") %>% 
        dplyr::mutate(NUTS_ID = nut) %>% 
        dplyr::filter(unit == crop_selected)}
    
    if(nuts_level_selected == "nuts3"){
      validation_series <- validation_database %>% 
        dplyr::filter(sector == "agricultural_yields_nuts3") %>% 
        dplyr::mutate(NUTS_ID = nut) %>% 
        dplyr::filter(unit == crop_selected)}
    
    validation_series <- validation_series %>% 
      group_by(NUTS_ID, date_year) %>% 
      summarise(total_yields = sum(value)) %>% 
      mutate(date_year = as.numeric(date_year)) %>% 
      ungroup() %>% 
      group_by(NUTS_ID) %>% 
      mutate(yields_compared_lag_1 = lag(total_yields, n = 1, order_by = date_year),
             yields_compared_lag_2 = lag(total_yields, n = 2, order_by = date_year),
             yields_compared_lag_3 = lag(total_yields, n = 3, order_by = date_year)) %>% 
      ungroup() %>% 
      mutate(yields_compared_lag = pmax(yields_compared_lag_1, 
                                        yields_compared_lag_2, 
                                        yields_compared_lag_3,
                                        na.rm = TRUE)) %>% 
      #Increase / Original Number × 100 (memo to me: learn math from second grade)
      mutate(yields_compared_lag = (yields_compared_lag - total_yields)/yields_compared_lag)
    
    impacts_series <- impacts_locations_dataset_b %>% 
      dplyr::filter(statistical_unit == nuts_level_selected) %>% 
      dplyr::filter(type_of_class == "reduced_crop_productivity") %>% 
      group_by(year_date, nuts_id) %>% 
      count() %>% 
      mutate(year_date = as.numeric(year_date)) 
    
    #let's start explaining the temporal differences
    #adjusted for number of articles
    spearman_correlations_by_nuts_adjusted <- expand_grid(nut = validation_series$NUTS_ID, 
                                                          date_year = seq(2000, 2020)) %>% 
      dplyr::left_join(validation_series,
                       by = c("nut" = "NUTS_ID", "date_year" = "date_year")) %>% 
      dplyr::left_join(impacts_series,
                       by = c("nut" = "nuts_id", "date_year" = "year_date")) %>% 
      dplyr::select(nut, n, yields_compared_lag, date_year) %>% 
      dplyr::left_join(number_of_articles_wiso_db, by= c
                       ("date_year" = "year_date")) %>% 
      dplyr::mutate(n_adjusted = (n / articles)) %>% 
      drop_na(n_adjusted,yields_compared_lag ) %>%
      
      drop_na(nut) %>% 
      group_by(nut) %>% 
      dplyr::select(n_adjusted, yields_compared_lag) %>% 
      dplyr::summarise(correlation = stats::cor(n_adjusted, yields_compared_lag,
                                                use = "complete.obs",
                                                method = "spearman")) 
    
    agricultural_losses_nuts_annual <- spearman_correlations_by_nuts_adjusted %>% 
      mutate(temoral_scope = "annual",
             explanation = "temporal",
             spatial_scope = nuts_level_selected)
    
    #let's explain spatial differences
    spearman_correlations_by_year <- expand_grid(nut = validation_series$NUTS_ID, 
                                                 date_year = seq(2000, 2020)) %>% 
      dplyr::left_join(validation_series,
                       by = c("nut" = "NUTS_ID", "date_year" = "date_year")) %>% 
      dplyr::left_join(impacts_series,
                       by = c("nut" = "nuts_id", "date_year" = "year_date")) %>% 
      dplyr::select(nut, n, yields_compared_lag, date_year) %>% 
      drop_na(nut) %>% 
      group_by(date_year) %>% 
      dplyr::select(n, yields_compared_lag) %>% 
      dplyr::summarise(correlation = stats::cor(n, yields_compared_lag,
                                                use = "complete.obs",
                                                method = "spearman")) 
    
    agricultural_losses_years_annual <- spearman_correlations_by_year %>% 
      mutate(temoral_scope = "annual",
             explanation = "spatial",
             spatial_scope = nuts_level_selected)
    
    nuts_agriculture_losses_validation <- dplyr::bind_rows(agricultural_losses_years_annual, 
                                                           agricultural_losses_nuts_annual)
    
    return(nuts_agriculture_losses_validation)
  }
  
  
  
  
  agriculture_validation_results <- dplyr::bind_rows(validate_agriculture_crop_specific(validation_database, 
                                                                                        impacts_locations_dataset_b,
                                                                                        nuts_level_selected = "nuts2"),
                                                     validate_agriculture_crop_specific(validation_database, 
                                                                                        impacts_locations_dataset_b,
                                                                                        nuts_level_selected = "nuts1"),
                                                     validate_agriculture_crop_specific(validation_database, 
                                                                                        impacts_locations_dataset_b,
                                                                                        nuts_level_selected = "nuts3")) %>% 
    mutate(indicator = "Agricultural yields - Agricultural DIS")
  
  
  
  
  
  
  ##############################################
  #### Validating google trends
  #############################################
  
  
  get_spatial_correlations_google_trends <- function(class_selected){
    validation_series_annual <- validation_database %>% 
      dplyr::filter(sector == "awareness") %>% 
      group_by(date_year, NUTS_ID) %>% 
      dplyr::summarise(total_searches = sum(value)) %>% 
      mutate(date_year = as.numeric(date_year)) 
    impacts_series_annual <- impacts_locations_dataset_b %>% 
      dplyr::filter(statistical_unit == "nuts1") %>% 
      dplyr::filter(type_of_class == class_selected) %>% 
      group_by(year_date, nuts_id) %>% 
      count() %>% 
      mutate(year_date = as.numeric(year_date)) 
    spearman_correlations_spatial_differneces <- validation_series_annual %>% 
      dplyr::left_join(impacts_series_annual,
                       by = c("NUTS_ID" = "nuts_id", "date_year" = "year_date")) %>% 
      dplyr::select(NUTS_ID, n, total_searches, date_year) %>% 
      group_by(date_year) %>% 
      dplyr::select(n, total_searches) %>% 
      drop_na(n, total_searches) %>% 
      dplyr::summarise(correlation = stats::cor(n, total_searches,
                                                use = "complete.obs",
                                                method = "spearman")) %>% 
      mutate(class = class_selected)
    return(spearman_correlations_spatial_differneces)
  }
  
  google_trends_spatial_corr_statistics <- get_spatial_correlations_google_trends("reduced_crop_productivity") %>% 
    dplyr::bind_rows(#get_spatial_correlations_google_trends("early_harvesting"),
                     get_spatial_correlations_google_trends("increased_irrigation_need"),
                     get_spatial_correlations_google_trends("economic_losses_agriculture"),
                     get_spatial_correlations_google_trends("need_economic_help"),
                     get_spatial_correlations_google_trends("reduced_livestock_productivity"),
                     get_spatial_correlations_google_trends("forced_livestock_reduction"),
                     get_spatial_correlations_google_trends("shortage_feed_livestock"),
                     get_spatial_correlations_google_trends("general_animal_impacts"),
                     get_spatial_correlations_google_trends("economic_losses_livestock"),
                     get_spatial_correlations_google_trends("reduced_tree_growth"),
                     get_spatial_correlations_google_trends("decrease_forestry_products"),
                     get_spatial_correlations_google_trends("increase_pests_diseases"),
                     get_spatial_correlations_google_trends("increase_tree_dieback"),
                     get_spatial_correlations_google_trends("economic_losses_forestry"),
                     get_spatial_correlations_google_trends("reduced_hydropower_production"),
                     get_spatial_correlations_google_trends("impaired_production_power_plant"),
                     get_spatial_correlations_google_trends("impaired_industrial_production"),
                     get_spatial_correlations_google_trends("impaired_stream_navigability"),
                     get_spatial_correlations_google_trends("increased_cost_energy_transport_infrastructure"),
                     get_spatial_correlations_google_trends("parks_tourism_recreation"),
                     #get_spatial_correlations_google_trends("forest_and_wildfires"),
                     get_spatial_correlations_google_trends("positive_impacts"),
                     get_spatial_correlations_google_trends("conflicts"),
                     get_spatial_correlations_google_trends("other_environmental_health_problems"),
                     get_spatial_correlations_google_trends("reduced_tree_growth")) %>% 
    mutate(temoral_scope = "annual",
           explanation = "spatial",
           spatial_scope = "nuts1")
  
  

  
  
  
  
  get_temporal_correlations_google_trends <- function(class_selected){
    validation_series_annual <- validation_database %>% 
      dplyr::filter(sector == "awareness") %>% 
      group_by(date_year, NUTS_ID) %>% 
      dplyr::summarise(total_searches = sum(value)) %>% 
      mutate(date_year = as.numeric(date_year)) 
    impacts_series_annual <- impacts_locations_dataset_b %>% 
      dplyr::filter(statistical_unit == "nuts1") %>% 
      dplyr::filter(type_of_class == class_selected) %>% 
      group_by(year_date, nuts_id) %>% 
      count() %>% 
      mutate(year_date = as.numeric(year_date)) 
    spearman_correlations_by_nuts <- validation_series_annual %>% 
      dplyr::left_join(impacts_series_annual,
                       by = c("NUTS_ID" = "nuts_id", "date_year" = "year_date")) %>% 
      dplyr::left_join(number_of_articles_wiso_db, by= c("date_year" = "year_date")) %>% 
      dplyr::mutate(n_adjusted = (n / articles) ) %>%
      dplyr::select(NUTS_ID, n_adjusted, total_searches, date_year) %>% 
      group_by(NUTS_ID) %>% 
      dplyr::select(n_adjusted, total_searches) %>% 
      drop_na(n_adjusted, total_searches) %>% 
      dplyr::summarise(correlation = stats::cor(n_adjusted, total_searches,
                                                use = "complete.obs",
                                                method = "spearman")) %>% 
      
      mutate(class = class_selected)
    return(spearman_correlations_by_nuts)
  }
  google_trends_temporal_corr_statistics <-  get_temporal_correlations_google_trends("reduced_crop_productivity") %>% 
    dplyr::bind_rows(#get_temporal_correlations_google_trends("early_harvesting"),
      get_temporal_correlations_google_trends("increased_irrigation_need"),
      get_temporal_correlations_google_trends("economic_losses_agriculture"),
      get_temporal_correlations_google_trends("need_economic_help"),
      get_temporal_correlations_google_trends("reduced_livestock_productivity"),
      get_temporal_correlations_google_trends("forced_livestock_reduction"),
      get_temporal_correlations_google_trends("shortage_feed_livestock"),
      get_temporal_correlations_google_trends("general_animal_impacts"),
      get_temporal_correlations_google_trends("economic_losses_livestock"),
      get_temporal_correlations_google_trends("reduced_tree_growth"),
      get_temporal_correlations_google_trends("decrease_forestry_products"),
      get_temporal_correlations_google_trends("increase_pests_diseases"),
      get_temporal_correlations_google_trends("increase_tree_dieback"),
      get_temporal_correlations_google_trends("economic_losses_forestry"),
      get_temporal_correlations_google_trends("reduced_hydropower_production"),
      get_temporal_correlations_google_trends("impaired_production_power_plant"),
      get_temporal_correlations_google_trends("impaired_industrial_production"),
      get_temporal_correlations_google_trends("impaired_stream_navigability"),
      get_temporal_correlations_google_trends("increased_cost_energy_transport_infrastructure"),
      get_temporal_correlations_google_trends("parks_tourism_recreation"),
      #get_temporal_correlations_google_trends("forest_and_wildfires"),
      get_temporal_correlations_google_trends("positive_impacts"),
      get_temporal_correlations_google_trends("conflicts"),
      get_temporal_correlations_google_trends("other_environmental_health_problems"),
      get_temporal_correlations_google_trends("reduced_tree_growth")) %>% 
    mutate(temoral_scope = "annual",
           explanation = "temporal",
           spatial_scope = "nuts1")
  
  
  
  
  ##############################################
  #### Forest fire
  #############################################
  
  
  validation_series_annual <- validation_database %>% 
    dplyr::filter(sector == "forest_fires") %>% 
    group_by(date_year, NUTS_ID) %>% 
    summarise(forest_fires = sum(value)) %>% 
    mutate(date_year = as.numeric(date_year)) 
  
  impacts_series_annual <- impacts_locations_dataset_b %>% 
    dplyr::filter(statistical_unit == "nuts1") %>% 
    dplyr::filter(type_of_class == "forest_and_wildfires") %>% 
    group_by(year_date, nuts_id) %>% 
    count() %>% 
    mutate(year_date = as.numeric(year_date))
  
  validation_series_monthly <- validation_database %>% 
    dplyr::filter(sector == "forest_fires") %>% 
    group_by(date_year, NUTS_ID, date_month) %>% 
    dplyr::summarise(forest_fires = sum(value)) %>% 
    mutate(date_year = as.numeric(date_year))
  
  impacts_series_monthly <- impacts_locations_dataset_b %>% 
    dplyr::filter(statistical_unit == "nuts1") %>% 
    dplyr::filter(type_of_class == "class_x_1_1") %>% 
    mutate(year_date = as.numeric(year_date))
  
  #let's start with temporal trends now
  spearman_correlations_by_nuts <- validation_series_annual %>% 
    dplyr::left_join(impacts_series_annual,
                     by = c("NUTS_ID" = "nuts_id", "date_year" = "year_date")) %>% 
    dplyr::left_join(number_of_articles_wiso_db, by= c("date_year" = "year_date")) %>% 
    dplyr::mutate(n_adjusted = (n / articles) ) %>%
    dplyr::select(NUTS_ID, n_adjusted, forest_fires, date_year) %>% 
    group_by(NUTS_ID) %>% 
    dplyr::select(n_adjusted, forest_fires) %>% 
    drop_na(n_adjusted, forest_fires) %>% 
    dplyr::summarise(correlation = stats::cor(n_adjusted, forest_fires,
                                              use = "complete.obs",
                                              method = "spearman"))
  
  
  spearman_correlations_by_nuts_forest_fires_annual <- spearman_correlations_by_nuts %>% 
    mutate(temoral_scope = "annual",
           explanation = "temporal",
           spatial_scope = "nuts1")
  
  #now even monthly
  spearman_correlations_temporal_differneces_monthly <- validation_series_monthly %>% 
    dplyr::left_join(impacts_series_monthly,
                     by = c("NUTS_ID" = "nuts_id", "date_year" = "year_date",
                            "date_month" = "month_date")) %>% 
    dplyr::left_join(number_of_articles_wiso_db, by= c("date_year" = "year_date")) %>% 
    dplyr::mutate(n_adjusted = (MIS / articles) ) %>%
    dplyr::select(NUTS_ID, MIS, forest_fires, date_year, date_month) %>% 
    group_by(NUTS_ID) %>% 
    dplyr::select(MIS, forest_fires) %>% 
    drop_na(MIS, forest_fires) %>% 
    dplyr::summarise(correlation = stats::cor(MIS, forest_fires,
                                              use = "complete.obs",
                                              method = "spearman")) 
  
  spearman_correlations_by_nuts_forest_fires_monthly <- spearman_correlations_temporal_differneces_monthly %>% 
    mutate(temoral_scope = "monthly",
           explanation = "temporal",
           spatial_scope = "nuts1")
  
  
  #let's do spatial trends
  
  
  spearman_correlations_by_year <- validation_series_annual %>% 
    dplyr::left_join(impacts_series_annual,
                     by = c("NUTS_ID" = "nuts_id", "date_year" = "year_date")) %>% 
    dplyr::select(NUTS_ID, n, forest_fires, date_year) %>% 
    group_by(date_year) %>% 
    dplyr::select(n, forest_fires) %>% 
    dplyr::summarise(correlation = stats::cor(n, forest_fires,
                                              use = "complete.obs",
                                              method = "spearman")) 
  
  spearman_correlations_by_year_spatial <- spearman_correlations_by_year %>% 
    mutate(temoral_scope = "annual",
           explanation = "spatial",
           spatial_scope = "nuts1")
  
  
  spearman_correlations_by_year_monthly <- validation_series_monthly %>% 
    dplyr::left_join(impacts_series_monthly,
                     by = c("NUTS_ID" = "nuts_id", "date_year" = "year_date",
                            "date_month" = "month_date")) %>% 
    dplyr::select(NUTS_ID, MIS, forest_fires, date_year, date_month) %>% 
    group_by(date_year, date_month) %>% 
    dplyr::select(MIS, forest_fires) %>% 
    drop_na(MIS, forest_fires) %>% 
    dplyr::summarise(correlation = stats::cor(MIS, forest_fires,
                                              use = "complete.obs",
                                              method = "spearman")) 
  
  spearman_correlations_by_momth_spatial <- spearman_correlations_by_year_monthly %>% 
    mutate(temoral_scope = "monthly",
           explanation = "spatial",
           spatial_scope = "nuts1")
  
  
  
  #assemble forest fire data
  forest_fire_correlation_stats <- dplyr::bind_rows(spearman_correlations_by_momth_spatial, 
                                                    spearman_correlations_by_year_spatial,
                                                    spearman_correlations_by_nuts_forest_fires_monthly,
                                                    spearman_correlations_by_nuts_forest_fires_annual) %>% 
    mutate(indicator = "Forest fire statistics - Forest fire DIS")
  
  
  
  
  
  
  

  
  ##############################################
  #### SMI validation
  #############################################
  get_smi_spatial_correlation <- function(class_selected){
    
    nut_mean_smi <- process_drought_monitor_data %>% 
      dplyr::select(date_year, V1, V2) %>% 
      dplyr::rename(nut = V1,
                    smi = V2) %>% 
      group_by(nut) %>% 
      summarise(mean_smi_total = median(smi, na.rm = TRUE))
    
    
    validation_series <- process_drought_monitor_data %>% 
      dplyr::select(date_year, V1, V2) %>% 
      dplyr::rename(nut = V1,
                    smi = V2) %>% 
      group_by(date_year, nut) %>% 
      summarise(mean_smi = median(smi, na.rm = TRUE)) %>% 
      dplyr::left_join(nut_mean_smi, by = c("nut" = "nut")) %>% 
      mutate(rel_smi =   mean_smi - mean_smi_total) %>% 
      dplyr::filter(nut != "DE30") #causes bugs
    
    
    impacts_series <- impacts_locations_dataset_b %>% 
      dplyr::filter(statistical_unit == "nuts2") %>% 
      dplyr::filter(type_of_class == class_selected) %>% 
      group_by(year_date, nuts_id) %>% 
      summarise(n  = sum(MIS, na.rm = TRUE)) %>% 
      mutate(year_date = as.numeric(year_date)) 
    
    spearmman_by_year <- validation_series %>% 
      dplyr::left_join(impacts_series,
                       by = c("nut" = "nuts_id", "date_year" = "year_date")) %>% 
      dplyr::select(date_year, nut, n, rel_smi) %>% 
      drop_na(date_year) %>% 
      group_by(date_year) %>% 
      dplyr::select(n, rel_smi) %>% 
      dplyr::summarise(correlation = stats::cor(n, rel_smi,
                                                use = "complete.obs",
                                                method = "spearman")) %>% 
      mutate(class = class_selected)
    return(spearmman_by_year)
  }
  
  smi_spatial <- get_smi_spatial_correlation("class_x_1_1") %>% 
    dplyr::bind_rows(get_smi_spatial_correlation("class_x_1_1"),
                     get_smi_spatial_correlation("class_x_1_1"),
                     get_smi_spatial_correlation("class_x_1_1"),
                     get_smi_spatial_correlation("class_x_1_1"),
                     get_smi_spatial_correlation("class_x_1_1"),
                     get_smi_spatial_correlation("class_x_1_1"),
                     get_smi_spatial_correlation("class_x_1_1"),
                     get_smi_spatial_correlation("class_x_1_1"),
                     get_smi_spatial_correlation("class_x_1_1"),
                     get_smi_spatial_correlation("class_x_1_1")) 
  
  
  get_smi_temporal_correlation <- function(class_selected){
    
    validation_series <- process_drought_monitor_data %>% 
      dplyr::select(date_year, V1, V2) %>% 
      dplyr::rename(nut = V1,
                    smi = V2) %>% 
      group_by(date_year, nut) %>% 
      summarise(mean_smi = median(smi, na.rm = TRUE))
    
    
    impacts_series <- impacts_locations_dataset_b %>% 
      dplyr::filter(statistical_unit == "nuts2") %>% 
      dplyr::filter(type_of_class == class_selected) %>% 
      group_by(year_date, nuts_id) %>% 
      summarise(n  = sum(MIS, na.rm = TRUE)) %>% 
      mutate(year_date = as.numeric(year_date)) 
    
    spearman_by_nut <- validation_series %>% 
      dplyr::left_join(impacts_series,
                       by = c("nut" = "nuts_id", "date_year" = "year_date")) %>% 
      dplyr::left_join(number_of_articles_wiso_db, by= c("date_year" = "year_date")) %>% 
      dplyr::mutate(n_adjusted = (n / articles) ) %>%
      dplyr::select(date_year, nut, n_adjusted, mean_smi) %>% 
      group_by(nut) %>% 
      dplyr::select(n_adjusted, mean_smi) %>% 
      drop_na(n_adjusted,mean_smi) %>% 
      dplyr::summarise(correlation = stats::cor(n_adjusted, mean_smi,
                                                use = "complete.obs",
                                                method = "spearman")) %>% 
      mutate(class = class_selected)
    return(spearman_by_nut)
  }
  
  smi_temporal <- get_smi_temporal_correlation("class_x_1_1") %>% 
    dplyr::bind_rows(get_smi_temporal_correlation("class_x_1_1"),
                     get_smi_temporal_correlation("class_x_1_1"),
                     get_smi_temporal_correlation("class_x_1_1"),
                     get_smi_temporal_correlation("class_x_1_1"),
                     get_smi_temporal_correlation("class_x_1_1"),
                     get_smi_temporal_correlation("class_x_1_1"),
                     get_smi_temporal_correlation("class_x_1_1"),
                     get_smi_temporal_correlation("class_x_1_1"),
                     get_smi_temporal_correlation("class_x_1_1"),
                     get_smi_temporal_correlation("class_x_1_1")) 
  
  
  
  
  ##############################################
  #### Precipitation
  #############################################
  
  
  
  get_precipitation_temporal_correlation <- function(class_selected){
    
    
    validation_series <- process_precipitation_dataset %>% 
      dplyr::left_join(nut_conversions, by = c("NUT" = "nut_text")) %>% 
      mutate(precipitation_avg = as.numeric(precipitation_avg)) %>% 
      mutate(Jahr = as.numeric(Jahr)) %>% 
      dplyr::filter(Jahr > 1999) %>% 
      dplyr::rename(date_year = Jahr,
                    date_month = Monat,
                    nut = nuts_short) %>% 
      dplyr::select(nut, date_year, date_month, precipitation_avg) %>% 
      group_by(date_year, nut) %>% 
      summarise(total_precipitation = sum(precipitation_avg, na.rm = TRUE))
    
    impacts_series <- impacts_locations_dataset_b %>% 
      dplyr::filter(statistical_unit == "nuts1") %>% 
      dplyr::filter(type_of_class == class_selected) %>% 
      group_by(year_date, nuts_id) %>% 
      summarise(n = sum(MIS, na.rm = TRUE)) %>% 
      mutate(year_date = as.numeric(year_date)) 
    
    spearman_by_nut <- validation_series %>% 
      dplyr::left_join(impacts_series,
                       by = c("nut" = "nuts_id", "date_year" = "year_date")) %>% 
      dplyr::left_join(number_of_articles_wiso_db, by= c("date_year" = "year_date")) %>% 
      dplyr::mutate(n_adjusted = (n / articles) ) %>%
      dplyr::select(date_year, nut, n_adjusted, total_precipitation) %>% 
      group_by(nut) %>% 
      dplyr::select(n_adjusted, total_precipitation) %>% 
      drop_na(n_adjusted,total_precipitation) %>% 
      dplyr::summarise(correlation = stats::cor(n_adjusted, total_precipitation,
                                                use = "complete.obs",
                                                method = "spearman")) %>% 
      mutate(class = class_selected)
    return(spearman_by_nut)
  }
  
  temporal_precipitation <- get_precipitation_temporal_correlation("class_x_1_1") %>% 
    dplyr::bind_rows(get_precipitation_temporal_correlation("class_x_1_1"),
                     get_precipitation_temporal_correlation("class_x_1_1"),
                     get_precipitation_temporal_correlation("class_x_1_1"),
                     get_precipitation_temporal_correlation("class_x_1_1"),
                     get_precipitation_temporal_correlation("class_x_1_1"),
                     get_precipitation_temporal_correlation("class_x_1_1"),
                     get_precipitation_temporal_correlation("class_x_1_1"),
                     get_precipitation_temporal_correlation("class_x_1_1"),
                     get_precipitation_temporal_correlation("class_x_1_1"),
                     get_precipitation_temporal_correlation("class_x_1_1")) 
  
  get_temporal_spatial_correlation <- function(class_selected){
    
    
    mean_preci <-  process_precipitation_dataset %>% 
      dplyr::left_join(nut_conversions, by = c("NUT" = "nut_text")) %>% 
      mutate(precipitation_avg = as.numeric(precipitation_avg)) %>% 
      mutate(Jahr = as.numeric(Jahr)) %>% 
      dplyr::filter(Jahr > 1999) %>% 
      dplyr::rename(date_year = Jahr,
                    date_month = Monat,
                    nut = nuts_short) %>% 
      dplyr::select(nut, date_year, date_month, precipitation_avg) %>% 
      group_by(nut) %>% 
      summarise(mean_precipitation = mean(precipitation_avg, na.rm = TRUE))
    
    
    
    validation_series <- process_precipitation_dataset %>% 
      dplyr::left_join(nut_conversions, by = c("NUT" = "nut_text")) %>% 
      mutate(precipitation_avg = as.numeric(precipitation_avg)) %>% 
      mutate(Jahr = as.numeric(Jahr)) %>% 
      dplyr::filter(Jahr > 1999) %>% 
      dplyr::rename(date_year = Jahr,
                    date_month = Monat,
                    nut = nuts_short) %>% 
      dplyr::select(nut, date_year, date_month, precipitation_avg) %>% 
      group_by(date_year, nut) %>% 
      summarise(total_precipitation = mean(precipitation_avg, na.rm = TRUE)) %>% 
      dplyr::left_join(mean_preci, by = c("nut" = "nut")) %>% 
      mutate(total_precipitation = total_precipitation - mean_precipitation)
    
    impacts_series <- impacts_locations_dataset_b %>% 
      dplyr::filter(statistical_unit == "nuts1") %>% 
      dplyr::filter(type_of_class == class_selected) %>% 
      group_by(year_date, nuts_id) %>% 
      summarise( n = sum(MIS, na.rm = TRUE)) %>% 
      mutate(year_date = as.numeric(year_date)) 
    
    spearmman_by_year <- validation_series %>% 
      dplyr::left_join(impacts_series,
                       by = c("nut" = "nuts_id", "date_year" = "year_date")) %>% 
      dplyr::select(date_year, nut, n, total_precipitation) %>% 
      drop_na(date_year) %>% 
      group_by(date_year) %>% 
      dplyr::select(n, total_precipitation) %>% 
      dplyr::summarise(correlation = stats::cor(n, total_precipitation,
                                                use = "complete.obs",
                                                method = "spearman")) %>% 
      mutate(class = class_selected)
    return(spearmman_by_year)
  }
  
  spatial_precipitation <- get_temporal_spatial_correlation("class_x_1_1") %>% 
    dplyr::bind_rows(get_temporal_spatial_correlation("class_x_1_1"),
                     get_temporal_spatial_correlation("class_x_1_1"),
                     get_temporal_spatial_correlation("class_x_1_1"),
                     get_temporal_spatial_correlation("class_x_1_1"),
                     get_temporal_spatial_correlation("class_x_1_1"),
                     get_temporal_spatial_correlation("class_x_1_1"),
                     get_temporal_spatial_correlation("class_x_1_1"),
                     get_temporal_spatial_correlation("class_x_1_1"),
                     get_temporal_spatial_correlation("class_x_1_1"),
                     get_temporal_spatial_correlation("class_x_1_1"))
  
  
  
  
  
  
  
  
  
  
  
  
  
  #######################################
  #### Assemble figure
  #######################################
  
  
  
  geophysical_correlations <- dplyr::bind_rows(
    spatial_precipitation %>%
      mutate(indicator = "precipitation",
             explanation = "spatial"),
    temporal_precipitation %>%
      mutate(indicator = "precipitation",
             explanation = "temporal"),
    smi_spatial %>%
      mutate(indicator = "SMI",
             explanation = "spatial"),
    smi_temporal %>%
      mutate(indicator = "SMI",
             explanation = "temporal")) %>% 
    dplyr::filter(class %in% c("class_x_1_1", "class_x_1_1", "class_x_1_1", "class_x_1_1"))
  
  
  

  p1_main <- dplyr::bind_rows(
    google_trends_temporal_corr_statistics,
    google_trends_spatial_corr_statistics) %>% 
    mutate(indicator = "Google Trends") %>% 
    dplyr::filter(class %in% c("class_x_1_1", "class_x_1_1",
                               "class_x_1_1")) %>% 
    mutate(indicator = paste(indicator, "-", class)) %>% 
    dplyr::filter(indicator != "Google Trends - X5.3.fire") %>% 
    
    dplyr::bind_rows(agriculture_validation_results, 
                     forest_fire_correlation_stats) %>% 
    dplyr::filter(spatial_scope == "nuts1") %>% 
    dplyr::filter(temoral_scope == "annual") %>% 
    mutate(highlight_year = ifelse(date_year %in% c(2003,2018,2019, 2015), date_year, "else")) %>% 
    ggplot(aes(x = correlation, y = indicator, color = highlight_year,  shape = explanation))+
    geom_boxplot(color = "black", outlier.shape = NA)+
    geom_beeswarm(groupOnX=FALSE, cex = 1.8)+
    
    
    geom_vline(xintercept = 0, color = "black", linetype = "dashed")+
    facet_wrap(~explanation)+
    theme_light()+
    labs(subtitle = "a")+
    xlab("Spearman's Rho")+
    ylab("")+
    theme(axis.text.y = element_text(size = 12),
          legend.position = "none",
          panel.grid.major.y = element_blank(),
          panel.spacing = unit(2, "lines"),
          strip.background = element_rect(
            color="white", fill="white", size=1.5, linetype="solid"
          ),
          
          strip.text = element_text(colour = "black", size = 12, vjust = 2,
                                    face= "italic"),
          panel.border = element_rect(colour = "black", fill=NA, size=.5))+
    scale_x_continuous(limits = c(-1,1), expand = c(0,0))+
    #scale_color_npg()
    scale_color_manual(values = c("#E64B35FF", "#4DBBD5FF", "#00A087FF",
                                  "#3C5488FF", "#9b9b9b"))+
    scale_y_discrete(breaks = c("Google Trends - Forestry", 
                                "Google Trends - Agriculture",
                                "Forest fire statistics - Forest fire DIS",
                                "Agricultural yields - Agricultural DIS"),
                     labels = c("Google Trends - Forestry DIS", 
                                "Google Trends - Agriculture DIS",
                                "Forest fires - Fire DIS",
                                "Crop yield losses - Agriculture DIS" ))
  
  
  
  
  
  p2_main <- geophysical_correlations %>% 
    mutate(highlight_year = ifelse(date_year %in% c(2003,2018,2019, 2015), date_year, "other year")) %>% 
    mutate(correlation_feature = paste(indicator, "-", class)) %>% 
    dplyr::filter(correlation_feature != "Livestock") %>% 
    dplyr::filter(!correlation_feature %in% c("precipitation - Livestck",
                                              "precipitation - X5.3.fire",
                                              "Google Trends - X5.3.fire",
                                              "SMI - X5.3.fire",
                                              "SMI - Livestock",
                                              "SMI - Forestry",
                                              "SMI - Agriculture"
    )) %>% 
    ggplot(aes(y = correlation_feature, x = correlation, color = highlight_year, shape = explanation))+
    geom_boxplot(color = "black", outlier.shape = NA)+
    geom_beeswarm(groupOnX=FALSE, cex = 1.8)+
    
    geom_vline(xintercept = 0, color = "black", linetype = "dashed")+
    facet_wrap(~explanation )+
    theme_light()+
    xlab("Spearman's Rho")+
    ylab("")+
    theme(axis.text.y = element_text(size = 12),
          legend.position = "bottom",
          legend.title = element_text(size = 12),
          legend.text = element_text(size = 10),
          panel.grid.major.y = element_blank(),
          panel.spacing = unit(2, "lines"),
          strip.background = element_rect(
            color="white", fill="white", size=1.5, linetype="solid"
          ),
          strip.text = element_text(colour = "black", size = 12, vjust = 2,
                                    face= "italic"),
          panel.border = element_rect(colour = "black", fill=NA, size=.5))+
    scale_color_manual(values = c("#E64B35FF", "#4DBBD5FF", "#00A087FF",
                                  "#3C5488FF", "#9b9b9b"))+
    scale_x_continuous(limits = c(-1,1), expand = c(0,0))+
    
    #scale_color_npg()+
    labs(color = "Year with severe drought",
         subtitle = "b")+
    guides(shape = FALSE)+
    scale_y_discrete(breaks = c("precipitation - Livestock", 
                                "precipitation - Forestry",
                                "precipitation - Agriculture"),
                     labels = c("Precipitation - Livestock DIS", 
                                "Precipitation - Forestry DIS",
                                "Precipitation - Agriculture DIS"))
  
  
  validation_main_plot <- p1_main / p2_main
  
  ggsave(filename = "figures/main_validation_keyword_based.svg", plot = validation_main_plot,
         device = "svg",
         width = 9, height = 6)
  
  
  
  
  
  ######
  # SMI and precipitation single regression plot
  #####
  
  ###smi part
  nut_mean_smi <- process_drought_monitor_data %>% 
    dplyr::select(date_year, V1, V2) %>% 
    dplyr::rename(nut = V1,
                  smi = V2) %>% 
    group_by(nut) %>% 
    summarise(mean_smi_total = mean(smi, na.rm = TRUE))
  
  validation_series <- process_drought_monitor_data %>% 
    dplyr::select(date_year, V1, V2) %>% 
    dplyr::rename(nut = V1,
                  smi = V2) %>% 
    group_by(date_year, nut) %>% 
    summarise(mean_smi = mean(smi, na.rm = TRUE)) %>% 
    dplyr::left_join(nut_mean_smi, by = c("nut" = "nut")) %>% 
    mutate(rel_smi =   mean_smi - mean_smi_total) %>% 
    dplyr::filter(nut != "DE30")
  
  impacts_series <- impacts_locations_dataset_b %>% 
    dplyr::filter(statistical_unit == "nuts2") %>% 
    dplyr::filter(type_of_class == "class_x_1_1") %>% 
    group_by(year_date, nuts_id) %>% 
    summarise(n = sum(MIS, na.rm = TRUE)) %>% 
    mutate(year_date = as.numeric(year_date)) 
  
  p2_smi <-  validation_series %>% 
    dplyr::left_join(impacts_series,
                     by = c("nut" = "nuts_id", "date_year" = "year_date")) %>% 
    dplyr::select(date_year, nut, n, rel_smi) %>%
    dplyr::filter(date_year %in% c(2003,2015,2018,2019)) %>% 
    ggplot(aes(x = rel_smi, y = n, color = as.factor(date_year)))+
    #geom_point()+
    geom_smooth(method = "lm", se = TRUE)+
    stat_cor(aes(label = ..r.label..), 
             method = "spearman", cor.coef.name = "rho", label.y = c(100,90,80,70),
             size = 3)+
    xlab("SMI - Historic Mean SMI [-]")+
    ylab("Agriculture DIS")+
    ggsci::scale_color_npg()+
    labs(color = "Year", title = "d")+
    theme_classic()
  
  
  
  
  nut_mean_smi <- process_drought_monitor_data %>% 
    dplyr::select(date_year, V1, V2) %>% 
    dplyr::rename(nut = V1,
                  smi = V2) %>% 
    group_by(nut) %>% 
    summarise(mean_smi_total = mean(smi, na.rm = TRUE))
  
  validation_series <- process_drought_monitor_data %>% 
    dplyr::select(date_year, V1, V2) %>% 
    dplyr::rename(nut = V1,
                  smi = V2) %>% 
    group_by(date_year, nut) %>% 
    summarise(mean_smi = mean(smi, na.rm = TRUE)) %>% 
    dplyr::left_join(nut_mean_smi, by = c("nut" = "nut")) %>% 
    mutate(rel_smi =   mean_smi - mean_smi_total) %>% 
    dplyr::filter(nut != "DE30")
  
  impacts_series <- impacts_locations_dataset_b %>% 
    dplyr::filter(statistical_unit == "nuts2") %>% 
    dplyr::filter(type_of_class == "class_x_1_1") %>% 
    group_by(year_date, nuts_id) %>% 
    summarise(n = sum(MIS, na.rm = TRUE)) %>% 
    mutate(year_date = as.numeric(year_date)) 
  
  p1_smi <-  validation_series %>% 
    dplyr::left_join(impacts_series,
                     by = c("nut" = "nuts_id", "date_year" = "year_date")) %>% 
    dplyr::select(date_year, nut, n, rel_smi) %>%
    dplyr::filter(date_year %in% c(2003,2015,2018,2019)) %>% 
    ggplot(aes(x = rel_smi, y = n, color = as.factor(date_year)))+
    #geom_point()+
    geom_smooth(method = "lm", se = TRUE)+
    stat_cor(aes(label = ..r.label..), 
             method = "spearman", cor.coef.name = "rho", label.y = c(100,90,80,70),
             size =3)+
    xlab("SMI - Historic Mean SMI [-]")+
    ylab("Forestry DIS")+
    ggsci::scale_color_npg()+
    labs(color = "Year", title = "c")+
    theme_classic()
  
  
  
  
  
  
  
  #precipitation part
  
  
  
  mean_preci <-  process_precipitation_dataset %>% 
    dplyr::left_join(nut_conversions, by = c("NUT" = "nut_text")) %>% 
    mutate(precipitation_avg = as.numeric(precipitation_avg)) %>% 
    mutate(Jahr = as.numeric(Jahr)) %>% 
    dplyr::filter(Jahr > 1999) %>% 
    dplyr::rename(date_year = Jahr,
                  date_month = Monat,
                  nut = nuts_short) %>% 
    dplyr::select(nut, date_year, date_month, precipitation_avg) %>% 
    group_by(nut) %>% 
    summarise(mean_precipitation = mean(precipitation_avg, na.rm = TRUE))
  
  validation_series <- process_precipitation_dataset %>% 
    dplyr::left_join(nut_conversions, by = c("NUT" = "nut_text")) %>% 
    mutate(precipitation_avg = as.numeric(precipitation_avg)) %>% 
    mutate(Jahr = as.numeric(Jahr)) %>% 
    dplyr::filter(Jahr > 1999) %>% 
    dplyr::rename(date_year = Jahr,
                  date_month = Monat,
                  nut = nuts_short) %>% 
    dplyr::select(nut, date_year, date_month, precipitation_avg) %>% 
    group_by(date_year, nut) %>% 
    summarise(total_precipitation = mean(precipitation_avg, na.rm = TRUE)) %>% 
    dplyr::left_join(mean_preci, by = c("nut" = "nut")) %>% 
    mutate(total_precipitation = total_precipitation - mean_precipitation)
  
  impacts_series <- impacts_locations_dataset_b %>% 
    dplyr::filter(statistical_unit == "nuts1") %>% 
    dplyr::filter(type_of_class == "class_x_1_1") %>% 
    group_by(year_date, nuts_id) %>% 
    count() %>% 
    mutate(year_date = as.numeric(year_date)) 
  
  p1 <-  validation_series %>% 
    dplyr::left_join(impacts_series,
                     by = c("nut" = "nuts_id", "date_year" = "year_date")) %>% 
    dplyr::select(date_year, nut, n, total_precipitation) %>% 
    dplyr::filter(date_year %in% c(2003,2015,2018,2019)) %>% 
    ggplot(aes(x = total_precipitation, y = n, color = as.factor(date_year)))+
    #geom_point()+
    geom_smooth(method = "lm", se = TRUE)+
    #facet_wrap(~date_year)+
    stat_cor(aes(label = ..r.label..), 
             method = "spearman", cor.coef.name = "rho", label.y = c(16,14.5,13,11.5),
             size =5)+
    xlab("Precipitation - Historic Mean [mm]")+
    ylab("Agriculture DIS")+
    ggsci::scale_color_npg()+
    labs(color = "Year", title = "a")+
    theme_classic()+
    scale_x_continuous(breaks = c(-20,-10,0,10), limits = c(-20,10))+
    theme(axis.title = element_text(size = 14),
          
          legend.text = element_text(size = 14),
          legend.title = element_text(size = 14))
  
  
  
  
  mean_preci <-  process_precipitation_dataset %>% 
    dplyr::left_join(nut_conversions, by = c("NUT" = "nut_text")) %>% 
    mutate(precipitation_avg = as.numeric(precipitation_avg)) %>% 
    mutate(Jahr = as.numeric(Jahr)) %>% 
    dplyr::filter(Jahr > 1999) %>% 
    dplyr::rename(date_year = Jahr,
                  date_month = Monat,
                  nut = nuts_short) %>% 
    dplyr::select(nut, date_year, date_month, precipitation_avg) %>% 
    group_by(nut) %>% 
    summarise(mean_precipitation = mean(precipitation_avg, na.rm = TRUE))
  
  validation_series <- process_precipitation_dataset %>% 
    dplyr::left_join(nut_conversions, by = c("NUT" = "nut_text")) %>% 
    mutate(precipitation_avg = as.numeric(precipitation_avg)) %>% 
    mutate(Jahr = as.numeric(Jahr)) %>% 
    dplyr::filter(Jahr > 1999) %>% 
    dplyr::rename(date_year = Jahr,
                  date_month = Monat,
                  nut = nuts_short) %>% 
    dplyr::select(nut, date_year, date_month, precipitation_avg) %>% 
    group_by(date_year, nut) %>% 
    summarise(total_precipitation = mean(precipitation_avg, na.rm = TRUE)) %>% 
    dplyr::left_join(mean_preci, by = c("nut" = "nut")) %>% 
    mutate(total_precipitation = total_precipitation - mean_precipitation)
  
  impacts_series <- impacts_locations_dataset_b %>% 
    dplyr::filter(statistical_unit == "nuts1") %>% 
    dplyr::filter(type_of_class == "class_x_1_1") %>% 
    group_by(year_date, nuts_id) %>% 
    count() %>% 
    mutate(year_date = as.numeric(year_date)) 
  
  p2 <-  validation_series %>% 
    dplyr::left_join(impacts_series,
                     by = c("nut" = "nuts_id", "date_year" = "year_date")) %>% 
    dplyr::select(date_year, nut, n, total_precipitation) %>% 
    dplyr::filter(date_year %in% c(2003,2015,2018,2019)) %>% 
    ggplot(aes(x = total_precipitation, y = n, color = as.factor(date_year)))+
    #geom_point()+
    geom_smooth(method = "lm", se = TRUE)+
    #facet_wrap(~date_year)+
    stat_cor(aes(label = ..r.label..), 
             method = "spearman", cor.coef.name = "rho", label.y = c(16,14.5,13,11.5),
             size = 5)+
    xlab("Precipitation - Historic Mean [mm]")+
    ylab("Forestry DIS")+
    ggsci::scale_color_npg()+
    labs(color = "Year", title = "b")+
    theme_classic()+
    scale_x_continuous(breaks = c(-20,-10,0,10), limits = c(-20,10))+
    theme(axis.title = element_text(size = 14),
          
          legend.text = element_text(size = 14),
          legend.title = element_text(size = 14))
  
  
  
  impacts_series <- impacts_locations_dataset_b %>% 
    dplyr::filter(statistical_unit == "nuts1") %>% 
    dplyr::filter(type_of_class == "class_x_1_1") %>% 
    group_by(year_date, nuts_id) %>% 
    count() %>% 
    mutate(year_date = as.numeric(year_date)) 
  
  p3 <-  validation_series %>% 
    dplyr::left_join(impacts_series,
                     by = c("nut" = "nuts_id", "date_year" = "year_date")) %>% 
    dplyr::select(date_year, nut, n, total_precipitation) %>% 
    dplyr::filter(date_year %in% c(2003,2015,2018,2019)) %>% 
    ggplot(aes(x = total_precipitation, y = n, color = as.factor(date_year)))+
    #geom_point()+
    geom_smooth(method = "lm", se = TRUE)+
    #facet_wrap(~date_year)+
    stat_cor(aes(label = ..r.label..), 
             method = "spearman", cor.coef.name = "rho", label.y = c(16,14.5,13,11.5),
             size = 5)+
    xlab("Precipitation - Historic Mean [mm]")+
    ylab("Livestock DIS")+
    ggsci::scale_color_npg()+
    labs(color = "Year", title = "c")+
    theme_classic()+
    theme(axis.title = element_text(size = 14),
          legend.text = element_text(size = 14),
          legend.title = element_text(size = 14))
  
  scale_x_continuous(breaks = c(-20,-10,0,10), limits = c(-20,10))
  
  
  #plot_multiple_smi_precipitation <- ggpubr::ggarrange(p1, p2,
  #                  p1_smi, p2_smi,
  #                  common.legend = TRUE)
  
  
  
  plot_multiple_smi_precipitation <- ggpubr::ggarrange(p1, p2, p3, nrow = 1,
                                                       
                                                       common.legend = TRUE)
  
  
  ggsave(filename = "figures/second_validation_keyword_based.svg", plot = plot_multiple_smi_precipitation,
         device = "svg",
         width = 12, height = 6)
  
  
  
  
  
  
  #######################
  ###appendix############
  
  
  ##### --> evaluating individual crops
  
  
  get_spatial_correlation_for_crop <- function(crop){
    validation_series <- validation_database %>% 
      dplyr::filter(sector == "agricultural_yields") %>% 
      dplyr::filter(unit == crop) %>% 
      group_by(NUTS_ID, date_year) %>% 
      summarise(total_yields = sum(value)) %>% 
      mutate(date_year = as.numeric(date_year)) %>% 
      ungroup() %>% 
      group_by(NUTS_ID) %>% 
      mutate(yields_compared_lag_1 = lag(total_yields, n = 1, order_by = date_year),
             yields_compared_lag_2 = lag(total_yields, n = 2, order_by = date_year),
             yields_compared_lag_3 = lag(total_yields, n = 3, order_by = date_year)) %>% 
      ungroup() %>% 
      mutate(yields_compared_lag = pmax(yields_compared_lag_1, 
                                        yields_compared_lag_2, 
                                        yields_compared_lag_3,
                                        na.rm = TRUE))
    impacts_series <- impacts_locations_dataset_b %>% 
      dplyr::filter(statistical_unit == "nuts1") %>% 
      dplyr::filter(type_of_class == "class_x_1_1") %>% 
      group_by(year_date, nuts_id) %>% 
      count() %>% 
      mutate(year_date = as.numeric(year_date)) 
    
    spearman_correlations_by_year <- expand_grid(nut = validation_series$NUTS_ID, 
                                                 date_year = seq(2000, 2020)) %>% 
      dplyr::left_join(validation_series,
                       by = c("nut" = "NUTS_ID", "date_year" = "date_year")) %>% 
      dplyr::left_join(impacts_series,
                       by = c("nut" = "nuts_id", "date_year" = "year_date")) %>% 
      dplyr::select(nut, n, yields_compared_lag, date_year) %>% 
      drop_na(yields_compared_lag, n, date_year, nut) %>% 
      group_by(date_year) %>% 
      dplyr::select(n, yields_compared_lag) %>% 
      dplyr::summarise(correlation = stats::cor(n, yields_compared_lag,
                                                use = "complete.obs",
                                                method = "spearman")) %>% 
      mutate(crop_covered = crop)
    
    return(spearman_correlations_by_year)
  }
  
  
  
  p1 <- get_spatial_correlation_for_crop("Winterweizen") %>% 
    dplyr::bind_rows(get_spatial_correlation_for_crop("Roggen und Wintermenggetreide")) %>%
    dplyr::bind_rows(get_spatial_correlation_for_crop("Wintergerste")) %>%
    dplyr::bind_rows(get_spatial_correlation_for_crop("Sommergerste")) %>%
    dplyr::bind_rows(get_spatial_correlation_for_crop("Hafer")) %>%
    dplyr::bind_rows(get_spatial_correlation_for_crop("Triticale")) %>%
    dplyr::bind_rows(get_spatial_correlation_for_crop("Kartoffeln")) %>%
    #dplyr::bind_rows(get_spatial_correlation_for_crop("Zuckerrüben")) %>%
    dplyr::bind_rows(get_spatial_correlation_for_crop("Winterraps")) %>%
    dplyr::bind_rows(get_spatial_correlation_for_crop("Silomais")) %>%
    ggplot(aes(x = as.factor(date_year), y  = crop_covered, fill = correlation))+
    geom_tile(color = "black")+
    theme_minimal()+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
          legend.position = "none",
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          plot.background = element_rect(fill = "white"))+
    ggsci::scale_fill_gsea(reverse = TRUE)+
    ylab("")+
    xlab("")+
    scale_y_discrete(breaks = c("Winterweizen", "Winterraps", "Wintergerste", "Triticale",
                                "Sommergerste", "Silomais", "Roggen und Wintermenggetreide",
                                "Kartoffeln", "Hafer"),
                     labels = c("Winter wheat", "Winter oilseed rape", "Winter barley", "Triticale",
                                "Spring barley", "Silage corn", "Rye and winter cereals",
                                "Potatoes", "Oats"))
  
  get_temporal_correlation_for_crop <- function(crop){
    validation_series <- validation_database %>% 
      dplyr::filter(sector == "agricultural_yields") %>% 
      dplyr::filter(unit == crop) %>% 
      group_by(NUTS_ID, date_year) %>% 
      summarise(total_yields = sum(value)) %>% 
      mutate(date_year = as.numeric(date_year)) %>% 
      ungroup() %>% 
      group_by(NUTS_ID) %>% 
      mutate(yields_compared_lag_1 = lag(total_yields, n = 1, order_by = date_year),
             yields_compared_lag_2 = lag(total_yields, n = 2, order_by = date_year),
             yields_compared_lag_3 = lag(total_yields, n = 3, order_by = date_year)) %>% 
      ungroup() %>% 
      mutate(yields_compared_lag = pmax(yields_compared_lag_1, 
                                        yields_compared_lag_2, 
                                        yields_compared_lag_3,
                                        na.rm = TRUE))
    impacts_series <- impacts_locations_dataset_b %>% 
      dplyr::filter(statistical_unit == "nuts1") %>% 
      dplyr::filter(type_of_class == "class_x_1_1") %>% 
      group_by(year_date, nuts_id) %>% 
      count() %>% 
      mutate(year_date = as.numeric(year_date)) 
    
    spearman_correlations_by_nuts_adjusted <- expand_grid(nut = validation_series$NUTS_ID, 
                                                          date_year = seq(2000, 2020)) %>% 
      dplyr::left_join(validation_series,
                       by = c("nut" = "NUTS_ID", "date_year" = "date_year")) %>% 
      dplyr::left_join(impacts_series,
                       by = c("nut" = "nuts_id", "date_year" = "year_date")) %>% 
      dplyr::select(nut, n, yields_compared_lag, date_year) %>% 
      dplyr::left_join(number_of_articles_wiso_db, by= c("date_year" = "year_date")) %>% 
      dplyr::mutate(n_adjusted = (n / articles)) %>% 
      drop_na(n_adjusted, yields_compared_lag) %>% 
      drop_na(nut) %>% 
      group_by(nut) %>% 
      dplyr::select(n_adjusted, yields_compared_lag) %>% 
      dplyr::summarise(correlation = stats::cor(n_adjusted, yields_compared_lag,
                                                use = "complete.obs",
                                                method = "spearman")) %>% 
      mutate(crop_covered = crop)
    return(spearman_correlations_by_nuts_adjusted)
  }
  
  
  
  
  p2 <- get_temporal_correlation_for_crop("Winterweizen") %>% 
    dplyr::bind_rows(get_temporal_correlation_for_crop("Roggen und Wintermenggetreide")) %>%
    dplyr::bind_rows(get_temporal_correlation_for_crop("Wintergerste")) %>%
    dplyr::bind_rows(get_temporal_correlation_for_crop("Sommergerste")) %>%
    dplyr::bind_rows(get_temporal_correlation_for_crop("Hafer")) %>%
    dplyr::bind_rows(get_temporal_correlation_for_crop("Triticale")) %>%
    dplyr::bind_rows(get_temporal_correlation_for_crop("Kartoffeln")) %>%
    #dplyr::bind_rows(get_temporal_correlation_for_crop("Zuckerrüben")) %>%
    dplyr::bind_rows(get_temporal_correlation_for_crop("Winterraps")) %>%
    dplyr::bind_rows(get_temporal_correlation_for_crop("Silomais")) %>%
    ggplot(aes(x = as.factor(nut), y  = crop_covered, fill = correlation))+
    geom_tile(color = "black")+
    theme_minimal()+
    scale_y_discrete(breaks = c("Winterweizen", "Winterraps", "Wintergerste", "Triticale",
                                "Sommergerste", "Silomais", "Roggen und Wintermenggetreide",
                                "Kartoffeln", "Hafer"),
                     labels = c("Winter wheat", "Winter oilseed rape", "Winter barley", "Triticale",
                                "Spring barley", "Silage corn", "Rye and winter cereals",
                                "Potatoes", "Oats"))+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
          axis.text.y = element_blank(),
          axis.title.y = element_blank(),
          axis.ticks.y = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          plot.background = element_rect(fill = "white"))+
    ggsci::scale_fill_gsea(reverse = TRUE)+
    xlab("")+
    ylab("")
  
  
  crop_individual <- p1 + p2
  
  
  ggsave(filename = "figures/validation_individual_crops_keyword_based.svg", plot = crop_individual,
         device = "svg",
         width = 10, height = 6)
  
  
  
  
  #### google trends detailed appendix
  
  
  p1 <-  ggplot(data = google_trends_spatial_corr_statistics  %>%  dplyr::filter(class !=  "X3.4.dead"), 
                aes(x = as.factor(date_year ), y  = class, fill = correlation))+
    geom_tile(color = "black")+
    theme_minimal()+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
          legend.position = "none",
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          plot.background = element_rect(fill = "white"))+
    ylab("")+
    scale_y_discrete(breaks = c("Agriculture", "Forestry", "Livestock",    "X1.1.Crops" , 
                                "X2.3.food", "X3.1.tree",   
                                "X3.2.timber", "X3.3.pest", 
                                "X4.4.transpo",
                                "X5.3.fire"),
                     labels = c("Agriculture", "Forestry", "Livestock",    "Crop yield losses" , 
                                "Reduced food supply for livestock", "Reduces tree growth",   
                                "Decrease in timber production", "Pest in forestry", 
                                "Transport infrastructure",
                                "Fires"))+
    xlab("")+
    scale_fill_gsea(reverse = TRUE)
  
  p2 <-   ggplot(data = google_trends_temporal_corr_statistics %>%  dplyr::filter(class !=  "X3.4.dead"), 
                 aes(x = as.factor(NUTS_ID ), y  = class, fill = correlation))+
    geom_tile(color = "black")+
    theme_minimal()+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
          axis.text.y = element_blank(),
          axis.title.y = element_blank(),
          axis.ticks.y = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          plot.background = element_rect(fill = "white") )+
    xlab("")+
    ylab("")+
    ggsci::scale_fill_gsea(reverse = TRUE)+
    scale_y_discrete(breaks = c("Agriculture", "Forestry", "Livestock",    "X1.1.Crops" , 
                                "X2.3.food", "X3.1.tree",   
                                "X3.2.timber", "X3.3.pest", 
                                "X4.4.transpo",
                                "X5.3.fire"),
                     labels = c("Agriculture", "Forestry", "Livestock",    "Crop yield losses" , 
                                "Reduced food supply for livestock", "Reduces tree growth",   
                                "Decrease in timber production", "Pest in forestry", 
                                "Transport infrastructure",
                                "Fires"))
  
  google_trends_detailed <- p1+p2
  
  
  ggsave(filename = "figures/google_trends_detailed_keyword_based.svg", plot = google_trends_detailed,
         device = "svg",
         width = 10, height = 6)
  
  
  
  
  agriculture_spatial_levels <- agriculture_validation_results %>% 
    ggplot(aes(x = correlation, y = spatial_scope))+
    geom_boxplot(color = "black")+
    geom_vline(xintercept = 0, color = "black", linetype = "dashed")+
    facet_wrap(~explanation )+
    theme_light()+
    xlab("Spearman's Rho")+
    ylab("")+
    theme(axis.text.y = element_text(size = 8),
          legend.position = "none",
          panel.grid.major.y = element_blank(),
          strip.background = element_rect(
            color="white", fill="white", size=1.5, linetype="solid"
          ),
          
          strip.text = element_text(colour = "black", size = 12, vjust = 2,
                                    face= "italic"),
          panel.border = element_rect(colour = "black", fill=NA, size=.5))+
    xlim(c(-1,1))
  
  
  
  
  
  
  
  
}







