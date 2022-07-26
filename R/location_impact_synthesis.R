

#new method for clustering based location detection to create impacts locations dataset

merge_locations_impacts_function_b <- function(predict_impact_classes, 
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
                       dplyr::select(id, text, permalink, header, date, newspaper),
                     by = c("id" = "id")) %>% 
    dplyr::left_join(predicted_locations_nuts, by = c("id" = "doc_id")) %>% #now add the locations of an impact
    dplyr::rename(text  = text.y) %>% 
    dplyr::select(id, text, type_of_class, permalink, header, date, newspaper, token, nuts_id, 
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

