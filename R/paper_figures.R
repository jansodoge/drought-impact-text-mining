#new impact class names







create_figures_classifier_eval <-function(supervised_training_test_data,
                               impacts_locations_dataset_b){
  
  
  library(ggsci)
  library(patchwork)
  #load required/trained models
  targets::tar_load(tidymodel_classifier_Forestry)
  targets::tar_load(model_datasets_testing_Forestry)
  #Livestock
  targets::tar_load(tidymodel_classifier_Livestock)
  targets::tar_load(model_datasets_testing_Livestock)
  #Agriculture
  targets::tar_load(tidymodel_classifier_Agriculture)
  targets::tar_load(model_datasets_testing_Agriculture)
  #Forest fires
  targets::tar_load(tidymodel_classifier_X5.3.fire)
  targets::tar_load(model_datasets_testing_X5.3.fire)
  #Transport
  targets::tar_load(tidymodel_classifier_X4.4.transpo)
  targets::tar_load(model_datasets_testing_X4.4.transpo)
  #Dead trees
  targets::tar_load(tidymodel_classifier_X3.4.dead)
  targets::tar_load(model_datasets_testing_X3.4.dead)
  #Pest trees
  targets::tar_load(tidymodel_classifier_X3.3.pest)
  targets::tar_load(model_datasets_testing_X3.3.pest)
  #timber
  targets::tar_load(tidymodel_classifier_X3.2.timber)
  targets::tar_load(model_datasets_testing_X3.2.timber)
  #timber
  targets::tar_load(tidymodel_classifier_X3.1.tree)
  targets::tar_load(model_datasets_testing_X3.1.tree)
  #food
  targets::tar_load(tidymodel_classifier_X2.3.food)
  targets::tar_load(model_datasets_testing_X2.3.food)
  #crops
  targets::tar_load(tidymodel_classifier_X1.1.Crops)
  targets::tar_load(model_datasets_testing_X1.1.Crops)
  
  #create figure...
  return_model_metrics <- function(tidymodel, testing_dataset, 
                                   graphic_title = ""){
    tidymodel %>% 
      predict(new_data = testing_dataset) %>% 
      bind_cols(testing_dataset %>%  dplyr::select(strata_needed)) %>% 
      conf_mat(strata_needed, .pred_class) %>% 
      summary() %>% 
      dplyr::filter(.metric %in% c("accuracy", "precision", "recall", "f_meas")) %>% 
      mutate(class = graphic_title) 
    
  }
  
  
  data <- dplyr::bind_rows(return_model_metrics(tidymodel_classifier_X1.1.Crops, 
                                                model_datasets_testing_X1.1.Crops,
                                                "X1.1.Crops"),
                           return_model_metrics(tidymodel_classifier_X2.3.food, 
                                                model_datasets_testing_X2.3.food,
                                                "X2.3.Food"),
                           return_model_metrics(tidymodel_classifier_X3.1.tree, 
                                                model_datasets_testing_X3.1.tree,
                                                "x3.1.tree"),
                           return_model_metrics(tidymodel_classifier_X3.1.tree, 
                                                model_datasets_testing_X3.1.tree,
                                                "x3.1.tree"),
                           return_model_metrics(tidymodel_classifier_X3.2.timber, 
                                                model_datasets_testing_X3.2.timber,
                                                "X3.2.timber"),
                           return_model_metrics(tidymodel_classifier_X3.3.pest, 
                                                model_datasets_testing_X3.3.pest,
                                                "X3.3.pest"),
                           return_model_metrics(tidymodel_classifier_X3.4.dead, 
                                                model_datasets_testing_X3.4.dead,
                                                "X3.4.dead"),
                           return_model_metrics(tidymodel_classifier_X4.4.transpo, 
                                                model_datasets_testing_X4.4.transpo,
                                                "X4.4.transpo"),
                           return_model_metrics(tidymodel_classifier_X5.3.fire, 
                                                model_datasets_testing_X5.3.fire,
                                                "X5.3.fire"),
                           return_model_metrics(tidymodel_classifier_Agriculture, 
                                                model_datasets_testing_Agriculture,
                                                "Agriculture"),
                           return_model_metrics(tidymodel_classifier_Livestock, 
                                                model_datasets_testing_Livestock,
                                                "Livestock"),
                           return_model_metrics(tidymodel_classifier_Forestry, 
                                                model_datasets_testing_Forestry,
                                                "Forestry"))
  
  
  
  metrics_plot <-   data %>% 
    dplyr::filter(class != "X3.4.dead") %>% 
    mutate(class = replace(class, class == "X1.1.Crops", "Crop yield losses (n = 802)"),
           class = replace(class, class == "X3.2.timber", "Decrease in timber production (n = 419)"),
           class = replace(class, class == "X3.3.pest", "Pest in forestry (n  = 482)"),
           class = replace(class, class == "X5.3.fire", "Fires (n  = 448)"),
           class = replace(class, class == "X4.4.transpo", 
                                  "Transport Infrastructure (n = 117)"),
           class = replace(class, class == "X2.3.Food", "Reduced food supply for livestock (n = 591)"),
           class = replace(class, class == "x3.1.tree", "Reduced tree growth (n  = 554)"),
           class = replace(class, class == "X3.4.dead", "Forestry - Dead trees (n = 303)"),
           class = replace(class, class == "Agriculture", "Agriculture (n = 1227)"),
           class = replace(class, class == "Livestock", "Livestock (n  = 762)"),
           class = replace(class, class == "Forestry", "Forestry (n = 873)")
    ) %>% 
    dplyr::filter(.metric != "f_meas") %>% 
    
    group_by(.metric) %>% 
    arrange(desc(.estimate)) %>% 
    ggplot(aes(x = .estimate, y  = reorder(class, .estimate), color = .metric, group = .metric))+
    geom_point( size = 4)+
    scale_y_discrete(position = "right")+
    theme(panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.major.y = element_line(color ="black"),
          legend.position = "bottom",
          panel.background = element_rect(fill = "white", color = "grey"),
          axis.text.y = element_text(size = 14),
          axis.text.x = element_text(size = 10),
          legend.title = element_text(size = 14),
          axis.title.x = element_text(size = 14), 
          plot.margin = margin(1,1,1,1, "cm"),
          panel.spacing.y = unit(.5, "cm"),
          strip.text.y = element_blank(),
          legend.text = element_text(size = 12),
          axis.title.y = element_blank())+
    ylab("Class")+
    xlab("Estimate")+
    scale_color_npg()+
    labs(color = "Metric")+
      scale_x_continuous(limits = c(0.8,1), expand = c(0,0))
  
  x <- rev(c("Forestry",
             "Forestry- Pest",
             "Forestry - Timber",
             "Livestock",
             "Forest fires",
             "Transport Infrastructure",
             "Livestock - Food Supply",
             "Agriculture",
             #"Forestry - Dead trees",
             "Forestry - Tree growth",
             "Agriculture - Crops"))
  
  relevant_classes <- c("Agriculture", "Forestry", "Livestock", 
                        "X1.1.Crops", "X2.3.food", "X3.1.tree",
                        "X3.2.timber", "X3.3.pest", "X3.4.dead",
                        "X4.4.transpo", "X5.3.fire")
  
  sub_classes <- supervised_training_test_data %>% 
    dplyr::select(id, X1.1.Crops:X5.6.others) %>% 
    pivot_longer(!id, names_to = "impact_class", values_to = "impact") %>% 
    group_by(impact_class, impact) %>% 
    count() %>% 
    drop_na(impact) %>% 
    tidyr::pivot_wider(impact_class, names_from = impact, values_from = n) %>%
    dplyr::rename(negatives = `0`,
                  positives  = `1`) %>% 
    ungroup()
  
  sample_df <- supervised_training_test_data %>% 
    dplyr::select(id, Agriculture:Energy) %>% 
    pivot_longer(!id, names_to = "impact_class", values_to = "impact") %>% 
    group_by(impact_class, impact) %>% 
    count() %>% 
    drop_na(impact) %>% 
    tidyr::pivot_wider(impact_class, names_from = impact, values_from = n) %>%
    dplyr::rename(negatives = `0`) %>% 
    mutate(positives = sum(`1`, `2`, `3`, `4`, `5`, na.rm = TRUE)) %>% 
    dplyr::select(impact_class, negatives, positives) %>% 
    ungroup() %>% 
    bind_rows(sub_classes) %>% 
    dplyr::filter(impact_class %in% relevant_classes) %>%
    dplyr::filter(impact_class != "X3.4.dead") %>% 
    
    mutate(impact_class = replace(impact_class, impact_class == "X1.1.Crops", "Crop yield losses"),
           impact_class = replace(impact_class, impact_class == "X3.2.timber", "Decrease in timber production"),
           impact_class = replace(impact_class, impact_class == "X3.3.pest", "Pest in forestry"),
           impact_class = replace(impact_class, impact_class == "X5.3.fire", "Fires"),
           impact_class = replace(impact_class, impact_class == "X4.4.transpo", 
                                  "Transport Infrastructure"),
           impact_class = replace(impact_class, impact_class == "X2.3.food", "Reduced food supply for livestock"),
           impact_class = replace(impact_class, impact_class == "X3.1.tree", "Reduced tree growth"),
           impact_class = replace(impact_class, impact_class == "X3.4.dead", "Forestry - Dead trees")
    ) %>% 
    tidyr::pivot_longer(!impact_class, names_to = "type", values_to = "count")

  samples <- sample_df %>% 
    mutate(impact_class =  factor(impact_class, levels = x)) %>%
    mutate(type = ifelse(type == "negatives", "without impact", 
                         "with impact")) %>% 
    
    ggplot(aes(y = impact_class, x = count, fill = type))+
    geom_bar(position="stack", stat="identity")+
    
    ggsci::scale_fill_npg()+
    theme_light()+
    #geom_text(aes(label=count), position=position_dodge(width=0.9))+
    xlab("Number of articles")+
    ylab("")+
    theme(panel.grid.major.y = element_blank(),
          axis.text.y = element_blank(),
          legend.position = "bottom",
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_rect(fill = "white", color = "grey"))+
    labs(fill = "")
  
  model_eval_plot <- metrics_plot #+ samples
  ggsave(filename = "figures/classifier_eval_figure.svg", plot = model_eval_plot,
         device = "svg",
                width = 11, height = 6)
  

  ###### confusion matrix plot
  return_confusion_matrix <- function(tidymodel, testing_dataset, 
                                      graphic_title = ""){
    tidymodel %>% 
      predict(new_data = testing_dataset) %>% 
      bind_cols(testing_dataset %>%  dplyr::select(strata_needed)) %>% 
      conf_mat(strata_needed, .pred_class) %>% 
      pluck(1) %>% 
      as_tibble() %>% 
      ggplot(aes(Prediction, Truth, alpha= n))+
      geom_tile(show.legend = FALSE)+
      geom_text(aes(label = n), colour = "white", alpha = 1, size = 3)+
      labs(title = graphic_title)+
      theme(plot.title = element_text(size=8),
            axis.title = element_text(size =7),
            axis.text = element_text(size = 6))
  }
  
  x1.1.Crops <- return_confusion_matrix(tidymodel_classifier_X1.1.Crops, 
                                        model_datasets_testing_X1.1.Crops,
                                        "Crop yield losses")
  x2.3.Food <- return_confusion_matrix(tidymodel_classifier_X2.3.food, 
                                       model_datasets_testing_X2.3.food,
                                       "Reduced food supply for livestock")
  x3.1.tree <- return_confusion_matrix(tidymodel_classifier_X3.1.tree, 
                                       model_datasets_testing_X3.1.tree,
                                       "Reduced tree growth")
  x3.2.timber <- return_confusion_matrix(tidymodel_classifier_X3.2.timber, 
                                         model_datasets_testing_X3.2.timber,
                                         "Decrease in timber production")
  x3.3.pest <- return_confusion_matrix(tidymodel_classifier_X3.3.pest, 
                                       model_datasets_testing_X3.3.pest,
                                       "Pest in forestry")
  
  x4.4.transpo <- return_confusion_matrix(tidymodel_classifier_X4.4.transpo, 
                                          model_datasets_testing_X4.4.transpo,
                                          "Transport infrastructure")
  x5.3.fire <- return_confusion_matrix(tidymodel_classifier_X5.3.fire, 
                                       model_datasets_testing_X5.3.fire,
                                       "Fires")
  agriculture <- return_confusion_matrix(tidymodel_classifier_Agriculture, 
                                         model_datasets_testing_Agriculture,
                                         "Agriculture")
  livestock <- return_confusion_matrix(tidymodel_classifier_Livestock, 
                                       model_datasets_testing_Livestock,
                                       "Livestock")
  forestry <- return_confusion_matrix(tidymodel_classifier_Forestry, 
                                      model_datasets_testing_Forestry,
                                      "Forestry")
  
  
confusion_matrix_plot <- (forestry | livestock | agriculture  | x4.4.transpo | x1.1.Crops) /
    ( x3.3.pest | x3.2.timber | x2.3.Food | x3.1.tree |  x5.3.fire) 
  
  
ggsave(filename = "figures/supplemantary_confusion_matrix.svg", plot = confusion_matrix_plot,
       device = "svg",
       width = 10, height = 6)
  


###keywords for qualitative eval
get_keywords_classification <- function(model, caption_gt){
  model %>%
    pull_workflow_fit() %>%
    tidy() %>%
    arrange(-estimate) %>% 
    head(10) %>% 
    mutate(term  = stringr::str_remove(term, "tfidf_text_")) %>% 
    gt(caption = caption_gt) %>% 
    tab_style(
      style = list(
        cell_fill(color = "#f2f2f2")
      ),
      locations = cells_body(
        rows = seq(1,9,2)
      )) %>% 
    data_color(
      columns = c(estimate),
      colors = scales::col_numeric(
        palette = c("#ffffd9","#7fcdbb", "#225ea8"),
        domain = NULL))
}
keywords_models <- list(
  get_keywords_classification(tidymodel_classifier_X1.1.Crops, "X1.1.crops"),
  get_keywords_classification(tidymodel_classifier_X2.3.food, "X2.3.food"),
  get_keywords_classification(tidymodel_classifier_X3.1.tree, "X3.1.tree"),
  get_keywords_classification(tidymodel_classifier_X3.2.timber, "X3.2.timber"),
  get_keywords_classification(tidymodel_classifier_X3.3.pest, "X3.3.pest"),
  get_keywords_classification(tidymodel_classifier_X3.4.dead, "X3.4.dead"),
  get_keywords_classification(tidymodel_classifier_X4.4.transpo, "X4.4.transpo"),
  get_keywords_classification(tidymodel_classifier_X5.3.fire, "X5.3.fire"),
  get_keywords_classification(tidymodel_classifier_Agriculture, "Agriculture"),
  get_keywords_classification(tidymodel_classifier_Livestock, "Livestock"),
  get_keywords_classification(tidymodel_classifier_Forestry, "Forestry"))
  
}





create_figures_dataset_descriptives <- function(predict_impact_classes,
                                                impacts_locations_dataset_b,
                                                nuts_geo_data){
  
  
  
  
  
  treemap <- predict_impact_classes %>% 
    dplyr::filter(.pred_class == 1) %>% 
    group_by(type_of_class) %>% 
    dplyr::filter(type_of_class != "X3.4.dead") %>% 
    count() %>% 
    mutate(type_of_class = replace(type_of_class, type_of_class == "X1.1.Crops", "Crop yield losses"),
           type_of_class = replace(type_of_class, type_of_class == "X3.2.timber", "Decrease in timber production"),
           type_of_class = replace(type_of_class, type_of_class == "X3.3.pest", "Pest in forestry"),
           type_of_class = replace(type_of_class, type_of_class == "X5.3.fire", "Fires"),
           type_of_class = replace(type_of_class, type_of_class == "X4.4.transpo", "Transport \n Infrastructure"),
           type_of_class = replace(type_of_class, type_of_class == "X2.3.food", "Reduced food supply \n for livestock"),
           type_of_class = replace(type_of_class, type_of_class == "X3.1.tree", "Reduced tree growth"),
           type_of_class = replace(type_of_class, type_of_class == "X3.4.dead", "Forestry - Dead trees")
    ) %>% 
    ggplot(aes(area = n, fill = type_of_class, 
               label = paste0(type_of_class, "\n", "(n=",n, ")"))) +
    geom_treemap()+
    scale_fill_discrete(type = c("Agriculture" = "#cf422e",
                                 "Forestry" = "#01a087",
                                 "Livestock"  = "#3c5388",
                                 "Crop yield losses"  = "#f05f46",
                                 "Reduced food supply for livestock"  = "#8093cd",
                                 "Reduced tree growth"  = "#004532",
                                 "Decrease in timber production"  = "#00624d",
                                 "Pest in forestry"  = "#008169",
                                 "Transport \n Infrastructure"  = "#f39b7f",
                                 "Fires"  = "4dbbd5"))+
    geom_treemap_text(colour = "white",
                      place = "centre",
                      size = 14)+
    theme(legend.position = "none")
  
  
  ggsave(filename = "figures/dataset_treemap.svg", plot = treemap,
         device = "svg",
         width = 10, height = 6)
  
  
  
  
  
  targets::tar_load(impacts_locations_dataset_b)
  targets::tar_load(nuts_geo_data)
  nuts1 <-  nuts_geo_data %>% 
    as_tibble() %>% 
    dplyr::select(-geometry) %>% 
    filter(CNTR_CODE == "DE") %>% 
    filter(LEVL_CODE == 1) 
  nuts2 <- nuts_geo_data %>% 
    as_tibble() %>% 
    dplyr::select(-geometry) %>% 
    filter(CNTR_CODE == "DE") %>% 
    filter(LEVL_CODE == 2)
  nuts3 <- nuts_geo_data %>% 
    as_tibble() %>% 
    dplyr::select(-geometry) %>% 
    filter(CNTR_CODE == "DE") %>% 
    filter(LEVL_CODE == 3)
  
  
  
  
  number_of_articles_wiso_db <- data.frame(year_date = 
                                             seq(2000,2021,1),
                                           articles = 
                                             c(3727202, 3805202, 4046952, 5279895,
                                               6539592, 7031086, 7546534, 7769275,
                                               8381606, 8416353, 8652610, 9316843, 
                                               9447050, 9178527, 9121899, 9098091,
                                               10399161, 12321421, 13537762, 13428658,
                                               11360680, 11693705))
  
  
  
  
  
  annual_data_overview <- impacts_locations_dataset_b %>%
    dplyr::filter(statistical_unit == "nuts3") %>% 
    dplyr::filter(type_of_class %in% c("Agriculture", "Forestry", "Livestock", 
                                       "X5.3.fire")) %>% 
    mutate(type_of_class = ifelse(type_of_class == "X5.3.fire", "Fires", type_of_class)) %>% 
    group_by(year_date, type_of_class) %>% 
    count() %>% 
    mutate(n = ifelse(is.na(n),0,n)) %>% 
    dplyr::left_join(number_of_articles_wiso_db, by = c("year_date" = "year_date")) %>% 
    mutate(corrected_n = (n/articles)*100) 
  annual_data_overview$corrected_n <- rescale(annual_data_overview$corrected_n)
  
  
  main_body <-  ggplot(data = annual_data_overview,
                       aes(x = year_date, y = corrected_n, color  = type_of_class))+
    geom_line(alpha = .2, size = 2)+
    geom_point()+
    labs(color = "Drought impact class", x  = "", y = "Normalized DIS",
         title = "b")+
    #theme_minimal()+
    theme(legend.position = "bottom",
          plot.title = element_text(hjust = -0, vjust = 3),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          axis.text.y = element_text(size = 12),
          axis.text.x = element_text(size = 12),
          legend.title = element_text(size = 14),
          legend.text = element_text(size = 12),
          axis.title.y = element_text(size = 14), 
          
          panel.border = element_rect(colour = "black", fill=NA, size=.5))+
    scale_color_npg()+
    scale_y_continuous(limits = c(0,1) ,expand=c(0,0), breaks = c(0,0.25,0.5,0.75,1))+
    scale_x_continuous(breaks = c(2000,2003,2006,2009,2012,2015,2018,2021), limits = c(2000,2021),
                       expand=c(0,0))
  
  
  
  
  
  subplot <- impacts_locations_dataset_b %>%
    dplyr::filter(statistical_unit == "nuts3") %>% 
    dplyr::filter(type_of_class %in% c("Agriculture", "Forestry", "Livestock", 
                                       "X5.3.fire")) %>% 
    mutate(type_of_class = ifelse(type_of_class == "X5.3.fire", "Fires", type_of_class)) %>% 
    group_by(year_date, type_of_class) %>% 
    count() %>% 
    mutate(n = ifelse(is.na(n),0,n)) %>% 
    dplyr::left_join(number_of_articles_wiso_db, by = c("year_date" = "year_date")) 
  
  subplot <- ggplot(data = subplot,
                    aes(x = year_date, y = n, color  = type_of_class))+
    geom_line(alpha = .2, size = 2)+
    geom_point()+
    labs(color = "Drought impact class", x  = "", y = "DIS",
         title = "a")+
    scale_y_continuous(breaks = c(0,500,1000, 1500), labels = c(0,500,1000, 1500), limits = c(0,1500),
                       expand=c(0,0))+
    #theme_minimal()+
    theme(legend.position = "none",
          plot.title = element_text(hjust = -0, vjust = 3),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          axis.text.y = element_text(size = 12),
          axis.text.x = element_text(size = 12),
          legend.title = element_text(size = 14),
          legend.text = element_text(size = 12),
          axis.title.y = element_text(size = 14), 
          
          
          panel.border = element_rect(colour = "black", fill=NA, size=.5),
          plot.background = element_rect(colour = "gray", fill="white", 
                                         margin(0,0,0,0)))+
    scale_color_npg()+
    scale_x_continuous(breaks = c(2000,2003,2006,2009,2012,2015,2018,2021), limits = c(2000,2021),
                       expand=c(0,0))
  
  
  
  
  
  temporal_descriptive_plot <- subplot / main_body
    
  
  #inset_element(subplot, 
   #               left = .21, right = .75,
  #                bottom = .55, top = .99)
  
  
  ggsave(filename = "figures/temporal_dataset_descriptive.svg", plot = temporal_descriptive_plot,
         device = "svg",
         width = 10, height = 6)
  
  
  
  #spatial map for four years
  
  nuts3_mapping <- nuts_geo_data %>% 
    as_tibble() %>% 
    filter(CNTR_CODE == "DE") %>% 
    filter(LEVL_CODE == 3)
  map_df <- expand.grid(nuts_id = nuts3_mapping$NUTS_ID, year_date = seq(2000,2020)) %>% 
    dplyr::left_join(impacts_locations_dataset_b %>% 
                       dplyr::filter(type_of_class %in% c("Agriculture", "Forestry", "Livestock", 
                                                     "X5.3.fire")),
                     by = c("year_date" = "year_date",
                            "nuts_id" = "nuts_id")) %>% 
    group_by(nuts_id, year_date) %>% 
    summarise(n = sum(MIS, na.rm = TRUE)) %>% 
    dplyr::filter(year_date %in% c(2003,2015,2018,2019)) %>% 
    mutate(n = ifelse(is.na(n),0,n)) %>% 
    dplyr::left_join(number_of_articles_wiso_db, by = c("year_date" = "year_date")) %>% 
    mutate(n = (n/articles)*100) %>%  
    dplyr::right_join(nuts3_mapping, by = c("nuts_id" = "NUTS_ID")) %>%
    dplyr::rename(DIS = n) %>% 
    sf::st_as_sf()
  map_df$DIS <- rescale(map_df$DIS)
  
  spatial_descriptive_plot <- ggplot(map_df) + 
    geom_sf(aes(fill=(DIS)), color = NA)+
    facet_wrap(~year_date)+
    #scale_fill_gradient2(low = "white",mid = "yellow", high = "red",
    #                     breaks = c(0, .5, 1))+
    scale_fill_distiller(palette = "YlOrRd", direction = 1)+
    geom_sf(fill = "transparent", color = "gray20", size = .2, 
            data = . %>%   group_by(CNTR_CODE) %>% summarise())+
    theme_minimal()+
    theme(axis.title = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          panel.grid = element_blank(),
          legend.title = element_text(size = 8),
          legend.text = element_text(size = 6),
          legend.position = "right")+
    labs(fill = "Normalized DIS")
  
  ggsave(filename = "figures/spatial_dataset_descriptive.svg", plot = spatial_descriptive_plot,
         device = "svg",
         width = 5, height = 3)
  
  
  
  
  
  
  
  #spatio-temporal appendix dataset figure
  
  
  spatio_temporal_dataset_appendix <- expand.grid(nuts_id = nuts1$NUTS_ID, 
              year_date = seq(2000,2020,1),
              month_date = seq(1,12),
              type_of_class = c("Agriculture", "Forestry", "Livestock", "X5.3.fire")) %>% 
    dplyr::left_join(impacts_locations_dataset_b %>%
                       dplyr::filter(statistical_unit == "nuts3") %>% 
                       mutate(nuts_id = substr(nuts_id, 1, 3)) %>% 
                       group_by(year_date, nuts_id, type_of_class, month_date) %>% 
                       summarise(MIS_per_year = median(MIS, na.rm = TRUE)), 
                     by = c("nuts_id"  = "nuts_id", "type_of_class" = "type_of_class","year_date" = "year_date", "month_date" = "month_date")) %>% 
    mutate(MIS_per_year = ifelse(is.na(MIS_per_year),0,MIS_per_year)) %>% 
    dplyr::left_join(nuts1, by = c("nuts_id" = "NUTS_ID")) %>% 
    dplyr::left_join(number_of_articles_wiso_db, by = c("year_date" = "year_date")) %>% 
    mutate(corrected_n = (MIS_per_year/articles)*100) %>%  
    ungroup() %>% 
    mutate(corrected_n = scales::rescale(corrected_n)) %>% 
    mutate(date_fluid = lubridate::ym(paste(year_date, "-", month_date))) %>% 
    dplyr::filter(!NAME_LATN %in% c("Berlin", "Hamburg", "Bremen")) %>% 
    mutate(type_of_class = ifelse(type_of_class == "X5.3.fire", "Fire", type_of_class)) %>% 
    ggplot(aes(x = (date_fluid), y = corrected_n, fill = type_of_class))+
    geom_area()+
    facet_wrap(~NAME_LATN, ncol = 1, strip.position = "left")+
    theme_minimal()+
    ggsci::scale_fill_npg()+
    theme(legend.position = "bottom",
          #axis.text.x = element_blank(),
          #axis.ticks.x = element_blank(),
          axis.title.x = element_blank(),
          strip.text.y = element_text(angle = 0),
          strip.text.x = element_text(size = 6),
          axis.text.y = element_blank(),
          legend.text = element_text(size = 6),
          legend.title = element_text(size = 6),
          #axis.title.y = element_blank(),
          panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_rect(fill = "white"),
          panel.border = element_rect(color = "lightgrey",
                                      fill = NA,
                                      size = 0.1))+
    labs(fill='Drought impact class') +
    theme(strip.text.y.left = element_text(angle = 0, size = 6),
          panel.spacing = unit(0, "lines"))+
    scale_x_date(date_breaks = "2 year", date_labels = "%Y",
                 limits = c(lubridate::date("2000-01-01"), NA),
                 expand = c(0, 0))+
    scale_y_continuous(limits = c(0,1), expand = c(0,0), position = "right")+
    ylab("Normalized DIS")
  
  
  
  ggsave(filename = "figures/spatio_temporal_dataset_descriptive_appendix.svg", 
         plot = spatio_temporal_dataset_appendix,
         device = "svg",
         width = 5, height = 4)
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
}






