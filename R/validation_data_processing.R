











create_validation_database <- function(drought_awareness_files,
                                       forest_fire_data_file,
                                       crop_yield_data_file,
                                       nuts_geo_data,
                                       crop_yield_data_nuts2){
  
  
  
  nuts1 <-  nuts_geo_data %>% 
    filter(CNTR_CODE == "DE") %>% 
    filter(LEVL_CODE == 1)
  
  
  ######drought awareness
  files <- drought_awareness_files
  
  awareness_df <- data.frame()
  for(file in files){
    
    tmp <- read_csv(file, 
                    skip = 1) %>% 
      pivot_longer(!Monat, names_to = "unit", values_to = "awareness") %>% 
      mutate(nut = stringr::str_remove(
        stringr::str_remove(stringr::str_extract(unit, "\\(.*\\)"),
                            "\\("),
        "\\)")) %>% #I just want to say I'm sorry for that mess, coffee supply in the basement is difficult
      mutate(keyword = stringr::str_extract(unit, 
                                            "dürre|trockenheit")) %>% 
      mutate(date_year = str_extract(Monat, 
                                     "[0-9]*"),
             date_month = str_extract(Monat,
                                      "[0-9]*$")) %>% 
      dplyr::select(date_year, date_month, keyword, nut, awareness)
    
    awareness_df <- dplyr::bind_rows(awareness_df, 
                                     tmp)
  }
  
  
  
  #######forest fires
  years <- as.character(seq(2010, 2020,1))
  forest_fire_df <- data.frame()
  
  for(year in years){
    
    
    
    forest_fire_data <- read_excel(forest_fire_data_file, 
                                   sheet = year) %>% 
      mutate(nut = ...1) %>%
      dplyr::select(-...1) %>% 
      pivot_longer(!nut, values_to = "forest_fires", names_to = "month") %>% 
      mutate(date_month = case_when(
        month == "Jan" ~ 01,
        month == "Feb" ~ 02,
        month == "Mar" ~ 03,
        month == "Apr" ~ 04,
        month == "May" ~ 05,
        month == "Jun" ~ 06,
        month == "Jul" ~ 07,
        month == "Aug" ~ 08,
        month == "Sep" ~ 09,
        month == "Oct" ~ 10,
        month == "Nov" ~ 11,
        month == "Dec" ~ 12,
      )) %>% 
      mutate(date_year = year)
    
    forest_fire_df <- dplyr::bind_rows(forest_fire_df,
                                       forest_fire_data) %>% 
      mutate(indicator = "forest_fire_events")
    
    
    
  }
  
  #####crop yields nuts-1
  
  crop_yields_genesis <- read_delim(crop_yield_data_file, 
                                    delim = ";", escape_double = FALSE, trim_ws = TRUE, 
                                    skip = 5,
                                    locale = locale(encoding = "ISO-8859-1"))
  
  crop_yields_genesis$...1[1] <- "date_year" # defining columns right here, ugly though
  crop_yields_genesis$...2[1] <- "misc"
  crop_yields_genesis$...3[1] <- "nut_raw"
  
  
  crop_yields_genesis <- crop_yields_genesis %>% 
    row_to_names(row_number = 1) %>% 
    dplyr::select(-misc) %>% 
    pivot_longer(!c(nut_raw, date_year), values_to = "yield", names_to = "crop_type") %>% 
    dplyr::filter(yield != "/" & yield != "-" & yield != ".") %>% 
    
    mutate(nut = nut_raw) %>% 
    dplyr::select(nut, date_year, yield, crop_type) %>% 
    mutate(date_month = NA)
  
  
  
  ##########crop yields nuts-2
  crop_yields_genesis_nuts2 <- read_delim(crop_yield_data_nuts2, 
                                          delim = ";", escape_double = FALSE, trim_ws = TRUE, 
                                          skip = 5,
                                          locale = locale(encoding = "ISO-8859-1"))
  crop_yields_genesis_nuts2$...1[1] <- "date_year" # defining columns right here, ugly though
  crop_yields_genesis_nuts2$...2[1] <- "misc"
  crop_yields_genesis_nuts2$...3[1] <- "nut_raw"
  
  crop_yields_genesis_nuts2 <- crop_yields_genesis_nuts2 %>% 
    row_to_names(row_number = 1) %>% 
    dplyr::select(-misc) %>% 
    pivot_longer(!c(nut_raw, date_year), values_to = "yield", names_to = "crop_type") %>% 
    dplyr::filter(yield != "/" & yield != "-" & yield != ".") %>% 
    
    mutate(nut = nut_raw) %>% 
    dplyr::select(nut, date_year, yield, crop_type) %>% 
    mutate(date_month = NA)
  
  #linking nuts2 to ids
  nuts_2_links_df <- data.frame(text_name = c(
                                  "Schleswig-Holstein", "Braunschweig, Stat. Region",     
                                "Hannover, Stat. Region", "Lüneburg, Stat. Region",         
                                "Weser-Ems, Stat. Region", "Düsseldorf, Regierungsbezirk",   
                                "Köln, Regierungsbezirk", "Münster, Regierungsbezirk",      
                                "Detmold, Regierungsbezirk","Arnsberg, Regierungsbezirk",     
                                "Darmstadt, Regierungsbezirk", "Gießen, Regierungsbezirk",       
                                "Kassel, Regierungsbezirk", "Koblenz, Stat. Region",          
                                "Trier, Stat. Region", "Rheinhessen-Pfalz, Stat. Region",
                                "Stuttgart, Regierungsbezirk","Karlsruhe, Regierungsbezirk",    
                                "Freiburg, Regierungsbezirk", "Tübingen, Regierungsbezirk",     
                                "Oberbayern, Regierungsbezirk", "Niederbayern, Regierungsbezirk", 
                                "Oberpfalz, Regierungsbezirk","Oberfranken, Regierungsbezirk",  
                                "Mittelfranken, Regierungsbezirk","Unterfranken, Regierungsbezirk", 
                                "Schwaben, Regierungsbezirk", "Saarland","Brandenburg" ,                   
                                "Mecklenburg-Vorpommern", "Chemnitz, Stat. Region",         
                                "Dresden, Stat. Region","Leipzig, Stat. Region",          
                                "Sachsen-Anhalt", "Thüringen" ,                     
                                "Chemnitz, Regierungsbezirk", "Dresden, Regierungsbezirk",      
                                "Leipzig, Regierungsbezirk","Dessau, Stat. Region" ,          
                                "Halle, Stat. Region", "Magdeburg, Stat. Region"), 
                                nuts_code = c(
                                  "DEF0", "DE91", "DE92",         
                                  "DE93", "DE94", "DEA1",   
                                  "DEA2", "DEA3", "DEA4",      
                                  "DEA5", "DE71", "DE72",       
                                  "DE73", "DEB1", "DEB2",            
                                  "DEB3", "DE11", "DE12",    
                                  "DE13", "DE14", "DE21",   
                                  "DE22", "DE23", "DE24",  
                                  "DE25", "DE26", "DE27",     
                                  "DEC0", "DE40", "DE80",         
                                  "DED4", "DED2", "DED5",          
                                  "DEE0", "DEG0", "DED4",     
                                  "DED2", "DED5",      
                                  NA, NA, NA)) %>% 
    dplyr::filter(!text_name %in% c("Dessau, Stat. Region" ,          
                                    "Halle, Stat. Region", "Magdeburg, Stat. Region",
                                    "Chemnitz, Stat. Region",         
                                    "Dresden, Stat. Region","Leipzig, Stat. Region"))
  crop_yields_genesis_nuts2 <- crop_yields_genesis_nuts2 %>% 
    dplyr::left_join(nuts_2_links_df, 
                     by = c("nut" = "text_name")) %>% 
    dplyr::mutate(nut = nuts_code)
  
  
  
  
  #nuts3
  #########first file
  
  files_nuts3 <- list.files("R/data/nuts3_yields", full.names = TRUE)
  
  crop_yields_genesis_nuts3_a <- read_delim(files_nuts3[1], 
                                          delim = ";", escape_double = FALSE, trim_ws = TRUE, 
                                          skip = 5,
                                          locale = locale(encoding = "ISO-8859-1"))
  crop_yields_genesis_nuts3_a$...1[1] <- "date_year" # defining columns right here, ugly though
  crop_yields_genesis_nuts3_a$...2[1] <- "misc"
  crop_yields_genesis_nuts3_a$...3[1] <- "nut_raw"
  crop_yields_genesis_nuts3_a <- crop_yields_genesis_nuts3_a %>% row_to_names(row_number = 1) %>% 
    dplyr::select(-misc) %>% 
    pivot_longer(!c(nut_raw, date_year), values_to = "yield", names_to = "crop_type") %>% 
    dplyr::filter(yield != "/" & yield != "-" & yield != ".") %>% mutate(nut = nut_raw) %>% 
    dplyr::select(nut, date_year, yield, crop_type) %>% mutate(date_month = NA)
  
  crop_yields_genesis_nuts3_b <- read_delim(files_nuts3[2], 
                                            delim = ";", escape_double = FALSE, trim_ws = TRUE, 
                                            skip = 5,
                                            locale = locale(encoding = "ISO-8859-1"))
  crop_yields_genesis_nuts3_b$...1[1] <- "date_year" # defining columns right here, ugly though
  crop_yields_genesis_nuts3_b$...2[1] <- "misc"
  crop_yields_genesis_nuts3_b$...3[1] <- "nut_raw"
  crop_yields_genesis_nuts3_b <- crop_yields_genesis_nuts3_b %>% row_to_names(row_number = 1) %>% 
    dplyr::select(-misc) %>% 
    pivot_longer(!c(nut_raw, date_year), values_to = "yield", names_to = "crop_type") %>% 
    dplyr::filter(yield != "/" & yield != "-" & yield != ".") %>% mutate(nut = nut_raw) %>% 
    dplyr::select(nut, date_year, yield, crop_type) %>% mutate(date_month = NA)
  
  crop_yields_genesis_nuts3_c <- read_delim(files_nuts3[3], 
                                            delim = ";", escape_double = FALSE, trim_ws = TRUE, 
                                            skip = 5,
                                            locale = locale(encoding = "ISO-8859-1"))
  crop_yields_genesis_nuts3_c$...1[1] <- "date_year" # defining columns right here, ugly though
  crop_yields_genesis_nuts3_c$...2[1] <- "misc"
  crop_yields_genesis_nuts3_c$...3[1] <- "nut_raw"
  crop_yields_genesis_nuts3_c <- crop_yields_genesis_nuts3_c %>% row_to_names(row_number = 1) %>% 
    dplyr::select(-misc) %>% 
    pivot_longer(!c(nut_raw, date_year), values_to = "yield", names_to = "crop_type") %>% 
    dplyr::filter(yield != "/" & yield != "-" & yield != ".") %>% mutate(nut = nut_raw) %>% 
    dplyr::select(nut, date_year, yield, crop_type) %>% mutate(date_month = NA)
  
  
  crop_yields_genesis_nuts3 <- dplyr::bind_rows(crop_yields_genesis_nuts3_c,
                                                crop_yields_genesis_nuts3_b,
                                                crop_yields_genesis_nuts3_a)
  
  
  library(readxl)
  nuts_id_mergers <- read_excel("R/data/nuts_id_mergers.xlsx")
  
  crop_yields_genesis_nuts3 <- crop_yields_genesis_nuts3 %>% 
    dplyr::left_join(nuts_id_mergers, by = c("nut" = "NUTS_NAME")) %>% 
    dplyr::mutate(nut = ID_nuts3_daten)
  
  
  
  
  ######merge 
  awareness_df <- awareness_df %>% 
    mutate(date_month = as.numeric(date_month),
           value = awareness,
           unit = keyword,
           sector = "awareness") %>% 
    dplyr:::select(nut, date_month, date_year, sector, unit, value)
  
  
  forest_fire_df <- forest_fire_df %>% 
    mutate(value = forest_fires,
           unit = "number_of_firest",
           sector = "forest_fires") %>% 
    dplyr::select(nut, date_month, date_year, sector, unit, value)
  
  
  crop_yields_genesis <- crop_yields_genesis %>% 
    mutate(yield = str_replace(yield, ",", replacement = ".")) %>% 
    mutate(unit = crop_type,
           sector = "agricultural_yields",
           value  = as.numeric(yield)) %>% 
    dplyr::select(nut, date_year, unit, sector, value)
  
  
  crop_yields_genesis_nuts2 <- crop_yields_genesis_nuts2 %>% 
    mutate(yield = str_replace(yield, ",", replacement = ".")) %>% 
    mutate(unit = crop_type,
           sector = "agricultural_yields_nuts2",
           value  = as.numeric(yield)) %>% 
    dplyr::select(nut, date_year, unit, sector, value)
  
  crop_yields_genesis_nuts3 <- crop_yields_genesis_nuts3 %>% 
    mutate(yield = str_replace(yield, ",", replacement = ".")) %>% 
    mutate(unit = crop_type,
           sector = "agricultural_yields_nuts3",
           value  = as.numeric(yield)) %>% 
    dplyr::select(nut, date_year, unit, sector, value)
    
  
  
  
  
  
  validation_db <- dplyr::bind_rows(crop_yields_genesis,
                                    forest_fire_df,
                                    awareness_df,
                                    crop_yields_genesis_nuts2,
                                    crop_yields_genesis_nuts3)
  
  
  #get nuts names right
  validation_db <- validation_db %>% 
    mutate(nut = ifelse(nut == "Baden-Württemberg, Land", "Baden-Württemberg", nut)) %>% 
    dplyr::left_join(nuts1, by = c("nut" = "NAME_LATN")) %>% 
    dplyr::select(-geometry)
  
  
  return(validation_db)
  
  
  
}



create_drought_monitor_data <- function(drought_monitor_validation_data, nuts_geo_data){
  
  nuts1 <-  nuts_geo_data %>% 
    filter(CNTR_CODE == "DE") %>% 
    filter(LEVL_CODE == 1) 
  nuts2 <- nuts_geo_data %>% 
    filter(CNTR_CODE == "DE") %>% 
    filter(LEVL_CODE == 2)
  nuts3 <- nuts_geo_data %>% 
    filter(CNTR_CODE == "DE") %>% 
    filter(LEVL_CODE == 3)
  
  nc_data <-nc_open(drought_monitor_validation_data)
  
  
  
  
  lon <- ncvar_get(nc_data, "lon")
  lat <- ncvar_get(nc_data, "lat", verbose = F)
  t <- ncvar_get(nc_data, "time")
  
  
  t_days <- ymd_hms(stringr::str_remove(nc_data[["dim"]][["time"]][["units"]], "days since ")) + days(x = t)
  
  
  index_date_time_matches <- tibble::tibble(t, t_days) %>% 
    mutate(t = as.numeric(t))
  ndvi.array <- ncvar_get(nc_data, "SMI")
  fillvalue <- ncatt_get(nc_data, "SMI", "_FillValue")
  nc_close(nc_data) 
  ndvi.array[ndvi.array == fillvalue$value] <- NA
  
  
  
  smi_spatio_temporal_df <- data.frame(NUT = NA, SMI = NA, date = as.Date(NA))
  for(elem in seq(1,nrow(index_date_time_matches))){
    if(index_date_time_matches$t[elem] > 17866){ 
      ndvi.slice <- ndvi.array[, , elem] 
      r <- raster(t(ndvi.slice), xmn=min(lon), xmx=max(lon), ymn=min(lat), 
                  ymx=max(lat), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
      tmp <- sf::st_as_sf(rasterToPolygons(r))
      
      tmp_2 <- sf::st_intersects(nuts2, tmp)
      
      for(nut_unit in seq(1,length(tmp_2))){
        
        nut_specific <- tmp %>% 
          slice(tmp_2[[nut_unit]]) %>% 
          pull(layer) %>% 
          base::mean(., na.rm = TRUE) #we calculate the mean value of the soil moisture index
        
        nut_concerned <- nuts2$NUTS_ID[nut_unit]
        
        extracted_values <- c(nut_concerned, nut_specific, (index_date_time_matches$t[elem]))
        
        smi_spatio_temporal_df <- dplyr::bind_rows(smi_spatio_temporal_df, as.data.frame(t(extracted_values)))
      }
    }
  }
  
  
  smi_spatio_temporal_df <- smi_spatio_temporal_df %>% 
    dplyr::select(V1, V2, V3) %>% 
    mutate(V2 = as.numeric(V2),
           V3 = as.numeric(V3)) %>% 
    dplyr::left_join(index_date_time_matches, by = c("V3" = "t")) %>% 
    dplyr::mutate(date_year  = year(t_days))
  
  
  
  return(smi_spatio_temporal_df)
  
  
}


process_precipitation_data <- function(precipitation_validation_data_files){
  
  
  precipitation_data <- data.frame()
  for(file in precipitation_validation_data_files){
    
    tmp <- read.table(file,
                      skip = 1,
                      sep =";") %>% 
      dplyr::select(-V20) %>% 
      row_to_names(row_number = 1) %>% 
      tidyr::pivot_longer(!c(Jahr, Monat), names_to = "NUT", values_to = "precipitation_avg") 
    
    
    precipitation_data <- dplyr::bind_rows(precipitation_data, tmp)
    
  }
  
  return(precipitation_data)
  
  
}


