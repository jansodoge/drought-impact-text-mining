

###setting up some glbal variables
get_empirical_validation_figures <- function(validation_database, impacts_locations_dataset_b,
                                             process_drought_monitor_data,
                                             process_precipitation_dataset){
  
  
  

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
    dplyr::filter(type_of_class == "Agriculture") %>% 
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

google_trends_spatial_corr_statistics <- get_spatial_correlations_google_trends("Agriculture") %>% 
  dplyr::bind_rows(get_spatial_correlations_google_trends("Forestry"),
                   get_spatial_correlations_google_trends("Livestock"),
                   get_spatial_correlations_google_trends("X1.1.Crops"),
                   get_spatial_correlations_google_trends("X2.3.food"),
                   get_spatial_correlations_google_trends("X3.1.tree"),
                   get_spatial_correlations_google_trends("X3.2.timber"),
                   get_spatial_correlations_google_trends("X3.3.pest"),
                   get_spatial_correlations_google_trends("X3.4.dead"),
                   get_spatial_correlations_google_trends("X4.4.transpo"),
                   get_spatial_correlations_google_trends("X5.3.fire")) %>% 
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
google_trends_temporal_corr_statistics <- get_temporal_correlations_google_trends("Agriculture") %>% 
  dplyr::bind_rows(get_temporal_correlations_google_trends("Forestry"),
                   get_temporal_correlations_google_trends("Livestock"),
                   get_temporal_correlations_google_trends("X1.1.Crops"),
                   get_temporal_correlations_google_trends("X2.3.food"),
                   get_temporal_correlations_google_trends("X3.1.tree"),
                   get_temporal_correlations_google_trends("X3.2.timber"),
                   get_temporal_correlations_google_trends("X3.3.pest"),
                   get_temporal_correlations_google_trends("X3.4.dead"),
                   get_temporal_correlations_google_trends("X4.4.transpo"),
                   get_temporal_correlations_google_trends("X5.3.fire")) %>% 
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
  dplyr::filter(type_of_class == "X5.3.fire") %>% 
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
  dplyr::filter(type_of_class == "X5.3.fire") %>% 
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

smi_spatial <- get_smi_spatial_correlation("Agriculture") %>% 
  dplyr::bind_rows(get_smi_spatial_correlation("Forestry"),
                   get_smi_spatial_correlation("Livestock"),
                   get_smi_spatial_correlation("X1.1.Crops"),
                   get_smi_spatial_correlation("X2.3.food"),
                   get_smi_spatial_correlation("X3.1.tree"),
                   get_smi_spatial_correlation("X3.2.timber"),
                   get_smi_spatial_correlation("X3.3.pest"),
                   get_smi_spatial_correlation("X3.4.dead"),
                   get_smi_spatial_correlation("X4.4.transpo"),
                   get_smi_spatial_correlation("X5.3.fire")) 


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

smi_temporal <- get_smi_temporal_correlation("Agriculture") %>% 
  dplyr::bind_rows(get_smi_temporal_correlation("Forestry"),
                   get_smi_temporal_correlation("Livestock"),
                   get_smi_temporal_correlation("X1.1.Crops"),
                   get_smi_temporal_correlation("X2.3.food"),
                   get_smi_temporal_correlation("X3.1.tree"),
                   get_smi_temporal_correlation("X3.2.timber"),
                   get_smi_temporal_correlation("X3.3.pest"),
                   get_smi_temporal_correlation("X3.4.dead"),
                   get_smi_temporal_correlation("X4.4.transpo"),
                   get_smi_temporal_correlation("X5.3.fire")) 




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

temporal_precipitation <- get_precipitation_temporal_correlation("Agriculture") %>% 
  dplyr::bind_rows(get_precipitation_temporal_correlation("Forestry"),
                   get_precipitation_temporal_correlation("Livestock"),
                   get_precipitation_temporal_correlation("X1.1.Crops"),
                   get_precipitation_temporal_correlation("X2.3.food"),
                   get_precipitation_temporal_correlation("X3.1.tree"),
                   get_precipitation_temporal_correlation("X3.2.timber"),
                   get_precipitation_temporal_correlation("X3.3.pest"),
                   get_precipitation_temporal_correlation("X3.4.dead"),
                   get_precipitation_temporal_correlation("X4.4.transpo"),
                   get_precipitation_temporal_correlation("X5.3.fire")) 

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

spatial_precipitation <- get_temporal_spatial_correlation("Agriculture") %>% 
  dplyr::bind_rows(get_temporal_spatial_correlation("Forestry"),
                   get_temporal_spatial_correlation("Livestock"),
                   get_temporal_spatial_correlation("X1.1.Crops"),
                   get_temporal_spatial_correlation("X2.3.food"),
                   get_temporal_spatial_correlation("X3.1.tree"),
                   get_temporal_spatial_correlation("X3.2.timber"),
                   get_temporal_spatial_correlation("X3.3.pest"),
                   get_temporal_spatial_correlation("X3.4.dead"),
                   get_temporal_spatial_correlation("X4.4.transpo"),
                   get_temporal_spatial_correlation("X5.3.fire"))













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
  dplyr::filter(class %in% c("Agriculture", "Forestry", "Livestock", "X5.3.fire"))



p1_main <- dplyr::bind_rows(
  google_trends_temporal_corr_statistics,
  google_trends_spatial_corr_statistics) %>% 
  mutate(indicator = "Google Trends") %>% 
  dplyr::filter(class %in% c("Agriculture", "Forestry",
                             "X5.3.fire")) %>% 
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
  facet_wrap(~explanation )+
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

ggsave(filename = "figures/main_validation.svg", plot = validation_main_plot,
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
  dplyr::filter(type_of_class == "Agriculture") %>% 
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
  dplyr::filter(type_of_class == "Forestry") %>% 
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
  dplyr::filter(type_of_class == "Agriculture") %>% 
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
  dplyr::filter(type_of_class == "Forestry") %>% 
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
  dplyr::filter(type_of_class == "Livestock") %>% 
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


ggsave(filename = "figures/second_validation.svg", plot = plot_multiple_smi_precipitation,
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
    dplyr::filter(type_of_class == "Agriculture") %>% 
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
    dplyr::filter(type_of_class == "Agriculture") %>% 
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


ggsave(filename = "figures/validation_individual_crops.svg", plot = crop_individual,
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


ggsave(filename = "figures/google_trends_detailed.svg", plot = google_trends_detailed,
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
