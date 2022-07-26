

process_geonames_data <- function(pre_procssed_file){
  
  exclude_tokens <- 
    tibble(names = 
    c("usa",
      "Norwegen",
      "england",
      "wild",
      "alpen",
      "zwinger",
      "zwingen",
      "zuschlag",
      "zug",
      "zorn",
      "hirsch",
      "hipp",
      "zwilling"
    ))
  
  load(pre_procssed_file)
  geonames_data_excl_tokens %>% 
    dplyr::anti_join(exclude_tokens, by = c("name" = "names")) %>% 
    dplyr::filter(!object_id %in% c(2950158, 2950160,6471849,
                                    6547539, 9252208, 2852459,
                                    2911299,
                                    2838632#that's schleswi hol
                                    ))
    
  

}






model_impact_locations_b <- function(ner_annotations_spacy,
                                     geonames_data,
                                     clean_text_articles,
                                     nuts_geo_data,
                                     predict_impact_classes){
  
  
  
  
  
  
  #these are the classified articles that we are looking for anyway
  predict_impact_classes <- predict_impact_classes %>% 
    dplyr::filter(.pred_class == 1)
  impact_classified_articles <- unique(predict_impact_classes$match_py) %>% 
    as_tibble()
  
  
  spacy_annotations <-  ner_annotations_spacy %>% 
    filter(ent_type == "LOC") %>%   #check other ent_type later!!!
    mutate(text_opening_location  = ifelse(token_order < 3,  TRUE, FALSE)) %>%  
    dplyr::left_join(clean_text_articles, by = c("doc_id" = "match_py")) %>% 
    dplyr::select(doc_id, token, lemma, ent_type, text_opening_location, id, text, header, date, newspaper, permalink) %>% 
    dplyr::semi_join(impact_classified_articles, by = c("doc_id" = "value"))
  
  
  places <- geonames_data %>% 
    sf::st_as_sf() %>% 
    mutate(name = tolower(name)) 
  
  osm_dictionary <- places %>% 
    drop_na(name) 
  
  #directly merge found tokens w/osm dictionary
  spacy_annotations_osm <- spacy_annotations %>% 
    dplyr::semi_join(osm_dictionary, by = c("token" = "name")) %>% 
    dplyr::select(doc_id, token)
  
  
  
  #check two-component names
  spacy_annotations_two_components <-  ner_annotations_spacy %>% 
    filter(ent_type == "LOC") %>% 
    mutate(connected = ifelse(lag(token_order) == token_order -1, TRUE, FALSE)) %>% 
    mutate(new_places  = ifelse(connected == TRUE, paste(lag(token), token), NA)) %>% 
    filter(!is.na(new_places)) %>% 
    mutate(token = new_places) %>% 
    dplyr::select(doc_id, token)
  
  #check three-component names
  spacy_annotations_three_components <-  ner_annotations_spacy %>% 
    filter(ent_type == "LOC") %>% 
    mutate(connected = ifelse(lag(token_order) == token_order -1 &&
                                lag(token_order, n = 2) == token_order -2 
                              , TRUE, FALSE)) %>% 
    mutate(new_places  = ifelse(connected == TRUE, paste(lag(token, n =2),lag(token), token), NA)) %>% 
    filter(!is.na(new_places)) %>% 
    mutate(token = new_places) %>% 
    dplyr::select(doc_id, token)
  
  multi_component_locations <- as_tibble(rbind(spacy_annotations_three_components, spacy_annotations_two_components)) %>% 
    dplyr::semi_join(osm_dictionary, by = c("token" = "name"))
  
  #create dataframe with all identified locations
  locations_mapped_osm <- dplyr::bind_rows(multi_component_locations, spacy_annotations_osm)
  
  
  located_osm <- locations_mapped_osm %>% 
    dplyr::left_join(places, by = c("token" = "name")) %>% 
    dplyr::semi_join(impact_classified_articles, by = c("doc_id" = "value")) %>% 
    mutate(doc_id = doc_id +1)
  
  
  
  
  #maybe this can be removed, to keep multiple instances, too
  located_osm <- located_osm %>% 
    distinct(doc_id, token, nuts_id, .keep_all = TRUE)
  
  
  
  
  
  #now let's remove some trash
  non_relevant_locations_tokens <- read_csv("R/data/geonames/non_relevant_locations_tokens.csv", 
                                                 col_names = FALSE)
  
  
  
  located_osm <- located_osm %>% 
    dplyr::anti_join(non_relevant_locations_tokens, 
                     by = c("token" = "X1"))
  
  
  
  
  
  return(located_osm)
  
}



cluster_article_locations <- function(located_osm){ 
  
  clusters <-  purrr::map(unique(located_osm$doc_id), 
                          ~try(create_location_clusters_b(.x, 
                                                          located_osm)))
  
  return(clusters)
  
}

set_cluster_cutoff_threshold <- function(cluster_impact_locations, cutoff_threshold = 10000){
  
  
  
  cutoff <- purrr::map_df(cluster_impact_locations, ~create_cutoff(.x,
                                                                   cutoff_threshold))
  
  return(cutoff)
  
}



create_cutoff <- function(.x, cutoff_threshold){
  
  if(is.null(.x)){
    return(tibble())
  }
  
  .x[[2]]$clusters <- cutree(.x[[1]], h=cutoff_threshold)
  
  return_df <- .x[[2]] %>% 
    dplyr::select(doc_id, clusters, object_id, token) %>% 
    as_tibble()
  
  return(return_df)
}



create_location_clusters <- function(doc, located_osm){
  
  locations_article <- located_osm %>% 
    dplyr::filter(doc_id == doc) %>% 
    sf::st_as_sf(.)
  
  
  if(nrow(locations_article) <  2){
    return(locations_article)
  }
  
  xy <- SpatialPointsDataFrame(
    matrix(st_coordinates(locations_article$geometry), ncol=2), data.frame(ID=seq(1:nrow(locations_article))),
    proj4string=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))
  
  distances <- distm(xy)
  hc <- hclust(as.dist(distances), method="complete")
  locations_article$clusters <- cutree(hc, h=100)
  return(locations_article)
}


create_location_clusters_b <- function(doc, located_osm){
  
  
  
  locations_article <- located_osm %>% 
    dplyr::filter(doc_id == doc) %>% 
    sf::st_as_sf(.)
  
  
  if(nrow(locations_article) <  2){
    return(NULL)
  }
  
  xy <- SpatialPointsDataFrame(
    matrix(st_coordinates(locations_article$geometry), ncol=2), data.frame(ID=seq(1:nrow(locations_article))),
    proj4string=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))
  
  distances <- distm(xy)
  hc <- hclust(as.dist(distances), method="complete")
  locations_article$clusters <- cutree(hc, h=100)
  return(list(hc, locations_article))
}







predict_cluster_location <- function(clustered_impact_locations, level  = "NUTS3"){
  
  clustered_impact_locations <- clustered_impact_locations %>% 
    dplyr::select(-geometry) %>% 
    as_tibble() 
  
  
  
  article_locations <- purrr::map_df(unique(clustered_impact_locations$doc_id), 
                                     ~singl_predict_cluster_location(dplyr::filter(clustered_impact_locations, doc_id == .x), .x))
  
  
  
  
  
  
}


singl_predict_cluster_location <- function(clustered_impact_locations, doc_id_selected){
  
  clustered_impact_locations_tmp <- clustered_impact_locations 
  
  clustered_impact_locations_tmp <- clustered_impact_locations_tmp %>% 
    dplyr::left_join( clustered_impact_locations_tmp %>%
                        as_tibble() %>% 
                        count(token, name = "frequency_of_occurece"),
                      by = c("token" = "token")) %>% 
    filter(frequency_of_occurece < 5)
  
  if(nrow(clustered_impact_locations_tmp) == 0){
    return(data.frame())
  }
  
  
  impact_locations_b_filt_prediction <- clustered_impact_locations_tmp %>% 
    group_by(clusters) %>% 
    count(sort = TRUE) %>%  
    ungroup() %>% 
    slice_max(n = 1, order_by =   n) %>% 
    mutate(doc_id = doc_id_selected)# %>% 
  #dplyr::left_join(as_tibble(clustered_impact_locations_tmp), by = c("clusters" = "clusters"))
  
  return(impact_locations_b_filt_prediction)
  
}







