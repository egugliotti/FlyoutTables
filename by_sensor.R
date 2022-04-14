# Functions to make flyout table

# This contains 3 different functions that each arrange the data a bit differently 
# by_sensor: arranged by Orbit_Type2, Platform, `Date End of Life (EOL)`
# by_sensor_sort2: arranged by Orbit Type, `Date Launched`, Platform
# by_sensor_sort3: arranged by Orbit Type, Platform, `Date Launched`
# by_sensor_names: arranged by Platform, Orbit Type, `Date Launched`

# The first part of this code looks for the CGMS in the network acronym connected to each system.
# Next, in platform deployment plans, the code looks for the string "CGMS Information - " and the ".*" bit deletes everything before that (such as IIAD information)
# Separate out the info in deployment plans into different columns using the | as the delimeter and dame the columns "dep_1-4", there should only be 4 bits of info
# 

# IF YOU SOMETHING BREAKS and you need to test this, just copy the dat1 and below until the } and replace dat with flyout_clean and sensor with either sensors[1,1]
# (you must have already run that code in the CGMS_pngs.RMD file for that to work) OR whatever sensor from the list in quotations "Microwave Sounder"
by_sensor<- function(dat, sensor) {
  dat1<- dat %>% 
    filter(network_acronym == "CGMS") %>%
    filter(grepl("CGMS", platform_deployment_plans)) %>%
    mutate(platform_deployment_plans = sub(".*CGMS Information - ","",platform_deployment_plans)) %>%
    separate(platform_deployment_plans, sep = "[\\|]", c("dep_one","dep_two","dep_three","dep_four")) %>%
    pivot_longer(cols = starts_with("dep_"), names_to = "extra", values_to = "deployment_plans") %>% 
    select(-extra) %>%
    separate(deployment_plans, sep = ":", c("type", "value")) %>% 
    filter(type == "Sensors") %>% 
    mutate(value = trimws(value, "l")) %>% 
    pivot_wider(names_from = "type", values_from = "value") %>% 
    separate_rows(Sensors, sep = "; ") %>% 
    filter(Sensors == sensor) %>%
    mutate(orbit_crossing_time = as.POSIXct(orbit_crossing_time, format = "%H:%M")) %>% #must format times
    mutate(orbit_crossing_time_simple = hour(orbit_crossing_time)) %>% #more formatting times to just get the hour
    #Create time categories based on how I saw CGMS splitting them up into Early AM, Mid AM, and PM
    mutate(orbit_crossing_category = ifelse(is.na(orbit_crossing_time_simple), orbit_crossing_time_simple, 
                                            ifelse(orbit_crossing_time_simple<9, "Early AM", 
                                                   ifelse(orbit_crossing_time_simple>=9 & orbit_crossing_time_simple<12, "Mid AM", "PM")))) %>%
    #CGMS wanted low inclination orbits identified so renamed that
    mutate(orbit_type = ifelse(orbit_type == "Drifting" & orbit_inclination_deg < 30, "SS Drift Low Incl", orbit_type)) %>%
    mutate(orbit = orbit_type) %>% 
    mutate(satellite_longitude_deg = as.numeric(satellite_longitude_deg)) %>%
    # Needed to categorize the orbits as East and West for the names in the table, want it to say GEO ## W if neg number and GEO ## E if pos number
    mutate(orbit = ifelse(orbit == "Geostationary" & satellite_longitude_deg < 0, paste0("GEO ",-satellite_longitude_deg," W"),
                          ifelse(orbit == "Geostationary" &satellite_longitude_deg > 0, paste0("GEO ",satellite_longitude_deg," E"),
                                 ifelse(orbit == "Geostationary" & satellite_longitude_deg == 0, paste0("GEO ", satellite_longitude_deg), orbit)))) %>%
    # Need SS orbits to have the crossing category attached
    mutate(orbit = ifelse(orbit == "Sun-synchronous", paste0("Sun-synchronous ", orbit_crossing_category), orbit)) %>%
    mutate(orbit = ifelse(orbit == "SS Drift Low Incl", "Low Inclination", orbit)) %>%
    # The orbit_type is different, it's what will be used within the table cells to color code
    mutate(orbit_type = ifelse(orbit_type == "Geostationary", satellite_longitude_deg, orbit_type)) %>%
    mutate(orbit_type = ifelse(orbit_type == "Sun-synchronous", paste0("SS ",orbit_crossing_category), orbit_type)) %>%
    mutate(orbit_type = ifelse(is.na(orbit_type), "TBD", orbit_type)) %>%
    mutate(orbit_type = ifelse(orbit_type == "Drifting", "SS Drifting", orbit_type)) %>%
    select(platform_acronym, date_type, date,  orbit_type, orbit, orbit_crossing_category, satellite_longitude_deg, orbit_inclination_deg) %>% 
    distinct(platform_acronym, date_type,  .keep_all = TRUE) %>% 
    pivot_wider(names_from = date_type, values_from = date) %>%
    filter(!is.na(`Date Launched`)) %>%
    filter(!is.na(`Date End of Life (EOL)`)) %>%
    filter(!is.na(platform_acronym))
  
  dat_eol<- dat1 %>%
    select(platform_acronym, `Date End of Life (EOL)`) %>%
    dplyr::rename(Platform = platform_acronym)
  
  # Create sequence of years from date launched to end of life date
  M <- Map(seq, dat1$`Date Launched`, dat1$`Date End of Life (EOL)`, by = "year")
  
  # Create new data frame that repeats all the important info for each of the years from launch until end of life
  dat2<-data.frame(
    Platform = rep.int(dat1$platform_acronym, vapply(M, length, 1L)),
    Orbit_Type = rep.int(dat1$orbit_type, vapply(M, length, 1L)),
    Orbit = rep.int(dat1$orbit, vapply(M, length, 1L)),
    Year = as.Date(do.call(c, M), format = "%m-%d-%Y"))
  
  # Constrict years to those that CGMS wants and pivot wider to get years as columns
  dat_wide<- dat2 %>%
    filter(Year >= "2022-01-01" & Year <= "2032-12-31") %>%
    arrange(Year) %>%
    mutate(Year = year(Year)) %>%
    mutate(Orbit_Type2 = Orbit_Type) %>%
    pivot_wider(names_from = Year, values_from = Orbit_Type) %>% 
    left_join(dat_eol, by = "Platform") %>%
    mutate(Orbit = as.factor(Orbit)) %>%
    mutate(Orbit_Type2 = as.factor(Orbit_Type2)) %>%
    mutate(Platform = as.factor(Platform)) %>%
    arrange(Orbit_Type2, Platform, `Date End of Life (EOL)`) %>% 
    select(-`Date End of Life (EOL)`, -Orbit_Type2, -Orbit) %>%
    as.data.frame()
  
  
  dat_wide
}


# Same function as above but arranged by Orbit Type, `Date Launched`, Platform
by_sensor_sort2<- function(dat, sensor) {
  dat1<- dat %>% 
    filter(network_acronym == "CGMS") %>%
    filter(grepl("CGMS", platform_deployment_plans)) %>%
    mutate(platform_deployment_plans = sub(".*CGMS Information - ","",platform_deployment_plans)) %>%
    separate(platform_deployment_plans, sep = "[\\|]", c("dep_one","dep_two","dep_three","dep_four")) %>%
    pivot_longer(cols = starts_with("dep_"), names_to = "extra", values_to = "deployment_plans") %>% 
    select(-extra) %>%
    separate(deployment_plans, sep = ":", c("type", "value")) %>% 
    filter(type == "Sensors") %>% 
    mutate(value = trimws(value, "l")) %>% 
    pivot_wider(names_from = "type", values_from = "value") %>% 
    separate_rows(Sensors, sep = "; ") %>%  
    filter(Sensors == sensor) %>% 
    mutate(orbit_crossing_time = as.POSIXct(orbit_crossing_time, format = "%H:%M")) %>%
    mutate(orbit_crossing_time_simple = hour(orbit_crossing_time)) %>%
    mutate(orbit_crossing_category = ifelse(is.na(orbit_crossing_time_simple), orbit_crossing_time_simple, 
                                            ifelse(orbit_crossing_time_simple<9, "Early AM", 
                                                   ifelse(orbit_crossing_time_simple>=9 & orbit_crossing_time_simple<12, "Mid AM", "PM")))) %>%
    mutate(orbit_type = ifelse(orbit_type == "Drifting" & orbit_inclination_deg < 30, "SS Drift Low Incl", orbit_type)) %>%
    mutate(orbit = orbit_type) %>% 
    mutate(satellite_longitude_deg = as.numeric(satellite_longitude_deg)) %>%
    mutate(orbit = ifelse(orbit == "Geostationary" & satellite_longitude_deg < 0, paste0("GEO ",-satellite_longitude_deg," W"),
                          ifelse(orbit == "Geostationary" &satellite_longitude_deg > 0, paste0("GEO ",satellite_longitude_deg," E"),
                                 ifelse(orbit == "Geostationary" & satellite_longitude_deg == 0, paste0("GEO ", satellite_longitude_deg), orbit)))) %>%
    mutate(orbit = ifelse(orbit == "Sun-synchronous", paste0("Sun-synchronous ", orbit_crossing_category), orbit)) %>%
    mutate(orbit = ifelse(orbit == "SS Drift Low Incl", "Low Inclination", orbit)) %>%
    # mutate(orbit = ifelse(is.na(orbit) ~ "TBD")) %>%
    mutate(orbit_type = ifelse(orbit_type == "Geostationary", satellite_longitude_deg, orbit_type)) %>%
    mutate(orbit_type = ifelse(orbit_type == "Sun-synchronous", paste0("SS ",orbit_crossing_category), orbit_type)) %>%
    mutate(orbit_type = ifelse(is.na(orbit_type), "TBD", orbit_type)) %>%
    mutate(orbit_type = ifelse(orbit_type == "Drifting", "SS Drifting", orbit_type)) %>%
    select(platform_acronym, date_type, date,  orbit_type, orbit, orbit_crossing_category, satellite_longitude_deg, orbit_inclination_deg) %>% 
    distinct(platform_acronym, date_type,  .keep_all = TRUE) %>% 
    pivot_wider(names_from = date_type, values_from = date) %>%
    filter(!is.na(`Date Launched`)) %>%
    filter(!is.na(`Date End of Life (EOL)`)) %>%
    filter(!is.na(platform_acronym))
  
  dat_launch<- dat1 %>%
    select(platform_acronym, `Date Launched`) %>%
    dplyr::rename(Platform = platform_acronym)
  
  M <- Map(seq, dat1$`Date Launched`, dat1$`Date End of Life (EOL)`, by = "year")
  
  dat2<-data.frame(
    Platform = rep.int(dat1$platform_acronym, vapply(M, length, 1L)),
    Orbit_Type = rep.int(dat1$orbit_type, vapply(M, length, 1L)),
    Orbit = rep.int(dat1$orbit, vapply(M, length, 1L)),
    Year = as.Date(do.call(c, M), format = "%m-%d-%Y"))
  
  dat_wide<- dat2 %>%
    filter(Year >= "2022-01-01" & Year <= "2032-12-31") %>%
    arrange(Year) %>%
    mutate(Year = year(Year)) %>%
    mutate(Orbit_Type2 = Orbit_Type) %>%
    pivot_wider(names_from = Year, values_from = Orbit_Type) %>% 
    left_join(dat_launch, by = "Platform") %>%
    mutate(Orbit = as.factor(Orbit)) %>%
    mutate(Orbit_Type2 = as.factor(Orbit_Type2)) %>%
    mutate(Platform = as.factor(Platform)) %>%
    arrange(Orbit_Type2, `Date Launched`, Platform) %>% 
    select(-`Date Launched`, -Orbit_Type2, -Orbit) %>%
    as.data.frame()
  
  
  dat_wide
}

# Same function as above but arranged by Orbit Type, Platform, `Date Launched`
by_sensor_sort3<- function(dat, sensor) {
  dat1<- dat %>% 
    filter(network_acronym == "CGMS") %>%
    filter(grepl("CGMS", platform_deployment_plans)) %>%
    mutate(platform_deployment_plans = sub(".*CGMS Information - ","",platform_deployment_plans)) %>%
    separate(platform_deployment_plans, sep = "[\\|]", c("dep_one","dep_two","dep_three","dep_four")) %>%
    pivot_longer(cols = starts_with("dep_"), names_to = "extra", values_to = "deployment_plans") %>% 
    select(-extra) %>%
    separate(deployment_plans, sep = ":", c("type", "value")) %>% 
    filter(type == "Sensors") %>% 
    mutate(value = trimws(value, "l")) %>% 
    pivot_wider(names_from = "type", values_from = "value") %>% 
    separate_rows(Sensors, sep = "; ") %>% 
    filter(Sensors == sensor) %>%
    mutate(orbit_crossing_time = as.POSIXct(orbit_crossing_time, format = "%H:%M")) %>%
    mutate(orbit_crossing_time_simple = hour(orbit_crossing_time)) %>%
    mutate(orbit_crossing_category = ifelse(is.na(orbit_crossing_time_simple), orbit_crossing_time_simple, 
                                            ifelse(orbit_crossing_time_simple<9, "Early AM", 
                                                   ifelse(orbit_crossing_time_simple>=9 & orbit_crossing_time_simple<12, "Mid AM", "PM")))) %>%
    mutate(orbit_type = ifelse(orbit_type == "Drifting" & orbit_inclination_deg < 30, "SS Drift Low Incl", orbit_type)) %>%
    mutate(orbit = orbit_type) %>% 
    mutate(satellite_longitude_deg = as.numeric(satellite_longitude_deg)) %>%
    mutate(orbit = ifelse(orbit == "Geostationary" & satellite_longitude_deg < 0, paste0("GEO ",-satellite_longitude_deg," W"),
                          ifelse(orbit == "Geostationary" &satellite_longitude_deg > 0, paste0("GEO ",satellite_longitude_deg," E"),
                                 ifelse(orbit == "Geostationary" & satellite_longitude_deg == 0, paste0("GEO ", satellite_longitude_deg), orbit)))) %>%
    mutate(orbit = ifelse(orbit == "Sun-synchronous", paste0("Sun-synchronous ", orbit_crossing_category), orbit)) %>%
    mutate(orbit = ifelse(orbit == "SS Drift Low Incl", "Low Inclination", orbit)) %>%
    # mutate(orbit = ifelse(is.na(orbit) ~ "TBD")) %>%
    mutate(orbit_type = ifelse(orbit_type == "Geostationary", satellite_longitude_deg, orbit_type)) %>%
    mutate(orbit_type = ifelse(orbit_type == "Sun-synchronous", paste0("SS ",orbit_crossing_category), orbit_type)) %>%
    mutate(orbit_type = ifelse(is.na(orbit_type), "TBD", orbit_type)) %>%
    mutate(orbit_type = ifelse(orbit_type == "Drifting", "SS Drifting", orbit_type)) %>%
    select(platform_acronym, date_type, date,  orbit_type, orbit, orbit_crossing_category, satellite_longitude_deg, orbit_inclination_deg) %>% 
    distinct(platform_acronym, date_type,  .keep_all = TRUE) %>% 
    pivot_wider(names_from = date_type, values_from = date) %>%
    filter(!is.na(`Date Launched`)) %>%
    filter(!is.na(`Date End of Life (EOL)`)) %>%
    filter(!is.na(platform_acronym))
  
  dat_launch<- dat1 %>%
    select(platform_acronym, `Date Launched`) %>%
    dplyr::rename(Platform = platform_acronym)
  
  M <- Map(seq, dat1$`Date Launched`, dat1$`Date End of Life (EOL)`, by = "year")
  
  dat2<-data.frame(
    Platform = rep.int(dat1$platform_acronym, vapply(M, length, 1L)),
    Orbit_Type = rep.int(dat1$orbit_type, vapply(M, length, 1L)),
    Orbit = rep.int(dat1$orbit, vapply(M, length, 1L)),
    Year = as.Date(do.call(c, M), format = "%m-%d-%Y"))
  
  dat_wide<- dat2 %>%
    filter(Year >= "2022-01-01" & Year <= "2032-12-31") %>%
    arrange(Year) %>%
    mutate(Year = year(Year)) %>%
    mutate(Orbit_Type2 = Orbit_Type) %>%
    pivot_wider(names_from = Year, values_from = Orbit_Type) %>% 
    left_join(dat_launch, by = "Platform") %>%
    mutate(Orbit = as.factor(Orbit)) %>%
    mutate(Orbit_Type2 = as.factor(Orbit_Type2)) %>%
    mutate(Platform = as.factor(Platform)) %>%
    arrange(Orbit_Type2, Platform, `Date Launched`) %>% 
    select(-`Date Launched`, -Orbit_Type2, -Orbit) %>%
    as.data.frame()
  
  
  dat_wide
}

# Same function as above but arranged by Platform, Orbit Type, `Date Launched`
by_sensor_names<- function(dat, sensor) {
  dat1<- dat %>% 
    filter(network_acronym == "CGMS") %>%
    filter(grepl("CGMS", platform_deployment_plans)) %>%
    mutate(platform_deployment_plans = sub(".*CGMS Information - ","",platform_deployment_plans)) %>%
    separate(platform_deployment_plans, sep = "[\\|]", c("dep_one","dep_two","dep_three","dep_four")) %>%
    pivot_longer(cols = starts_with("dep_"), names_to = "extra", values_to = "deployment_plans") %>% 
    select(-extra) %>%
    separate(deployment_plans, sep = ":", c("type", "value")) %>% 
    filter(type == "Sensors") %>% 
    mutate(value = trimws(value, "l")) %>% 
    pivot_wider(names_from = "type", values_from = "value") %>% 
    separate_rows(Sensors, sep = "; ") %>% 
    filter(Sensors == sensor) %>%
    mutate(orbit_crossing_time = as.POSIXct(orbit_crossing_time, format = "%H:%M")) %>%
    mutate(orbit_crossing_time_simple = hour(orbit_crossing_time)) %>%
    mutate(orbit_crossing_category = ifelse(is.na(orbit_crossing_time_simple), orbit_crossing_time_simple, 
                                            ifelse(orbit_crossing_time_simple<9, "Early AM", 
                                                   ifelse(orbit_crossing_time_simple>=9 & orbit_crossing_time_simple<12, "Mid AM", "PM")))) %>%
    mutate(orbit_type = ifelse(orbit_type == "Drifting" & orbit_inclination_deg < 30, "SS Drift Low Incl", orbit_type)) %>%
    mutate(orbit = orbit_type) %>% 
    mutate(satellite_longitude_deg = as.numeric(satellite_longitude_deg)) %>%
    mutate(orbit = ifelse(orbit == "Geostationary" & satellite_longitude_deg < 0, paste0("GEO ",-satellite_longitude_deg," W"),
                          ifelse(orbit == "Geostationary" &satellite_longitude_deg > 0, paste0("GEO ",satellite_longitude_deg," E"),
                                 ifelse(orbit == "Geostationary" & satellite_longitude_deg == 0, paste0("GEO ", satellite_longitude_deg), orbit)))) %>%
    mutate(orbit = ifelse(orbit == "Sun-synchronous", paste0("Sun-synchronous ", orbit_crossing_category), orbit)) %>%
    mutate(orbit = ifelse(orbit == "SS Drift Low Incl", "Low Inclination", orbit)) %>%
    # mutate(orbit = ifelse(is.na(orbit) ~ "TBD")) %>%
    mutate(orbit_type = ifelse(orbit_type == "Geostationary", satellite_longitude_deg, orbit_type)) %>%
    mutate(orbit_type = ifelse(orbit_type == "Sun-synchronous", paste0("SS ",orbit_crossing_category), orbit_type)) %>%
    mutate(orbit_type = ifelse(is.na(orbit_type), "TBD", orbit_type)) %>%
    mutate(orbit_type = ifelse(orbit_type == "Drifting", "SS Drifting", orbit_type)) %>%
    select(platform_acronym, date_type, date,  orbit_type, orbit, orbit_crossing_category, satellite_longitude_deg, orbit_inclination_deg) %>% 
    distinct(platform_acronym, date_type,  .keep_all = TRUE) %>% 
    pivot_wider(names_from = date_type, values_from = date) %>%
    filter(!is.na(`Date Launched`)) %>%
    filter(!is.na(`Date End of Life (EOL)`)) %>%
    filter(!is.na(platform_acronym))
  
  dat_launch<- dat1 %>%
    select(platform_acronym, `Date Launched`) %>%
    dplyr::rename(Platform = platform_acronym)
  
  M <- Map(seq, dat1$`Date Launched`, dat1$`Date End of Life (EOL)`, by = "year")
  
  dat2<-data.frame(
    Platform = rep.int(dat1$platform_acronym, vapply(M, length, 1L)),
    Orbit_Type = rep.int(dat1$orbit_type, vapply(M, length, 1L)),
    Orbit = rep.int(dat1$orbit, vapply(M, length, 1L)),
    Year = as.Date(do.call(c, M), format = "%m-%d-%Y"))
  

  dat_wide<- dat2 %>%
    filter(Year >= "2022-01-01" & Year <= "2032-12-31") %>%
    arrange(Year) %>%
    mutate(Year = year(Year)) %>%
    mutate(Orbit_Type2 = Orbit_Type) %>%
    pivot_wider(names_from = Year, values_from = Orbit_Type) %>% 
    left_join(dat_launch, by = "Platform") %>%
    mutate(Orbit = as.factor(Orbit)) %>%
    mutate(Orbit_Type2 = as.factor(Orbit_Type2)) %>%
    mutate(Platform = as.factor(Platform)) %>%
    arrange(Platform, Orbit_Type2, `Date Launched`) %>% 
    select(-`Date Launched`, -Orbit_Type2, -Orbit) %>%
    as.data.frame()
  
  
  dat_wide
}

by_sensor_ss<- function(dat, sensor) {
  dat1<- dat %>% 
    filter(network_acronym == "CGMS") %>%
    filter(grepl("CGMS", platform_deployment_plans)) %>%
    mutate(platform_deployment_plans = sub(".*CGMS Information - ","",platform_deployment_plans)) %>%
    separate(platform_deployment_plans, sep = "[\\|]", c("dep_one","dep_two","dep_three","dep_four")) %>%
    pivot_longer(cols = starts_with("dep_"), names_to = "extra", values_to = "deployment_plans") %>% 
    select(-extra) %>%
    separate(deployment_plans, sep = ":", c("type", "value")) %>% 
    filter(type == "Sensors") %>% 
    mutate(value = trimws(value, "l")) %>% 
    pivot_wider(names_from = "type", values_from = "value") %>% 
    separate_rows(Sensors, sep = "; ") %>% 
    filter(Sensors == sensor) %>%
    mutate(orbit_crossing_time = as.POSIXct(orbit_crossing_time, format = "%H:%M")) %>%
    mutate(orbit_crossing_time_simple = hour(orbit_crossing_time)) %>%
    mutate(orbit_crossing_category = ifelse(is.na(orbit_crossing_time_simple), orbit_crossing_time_simple, 
                                            ifelse(orbit_crossing_time_simple<9, "Early AM", 
                                                   ifelse(orbit_crossing_time_simple>=9 & orbit_crossing_time_simple<12, "Mid AM", "PM")))) %>%
    mutate(orbit_type = ifelse(orbit_type == "Drifting" & orbit_inclination_deg < 30, "SS Drift Low Incl", orbit_type)) %>%
    mutate(orbit = orbit_type) %>% 
    mutate(satellite_longitude_deg = as.numeric(satellite_longitude_deg)) %>%
    mutate(orbit = ifelse(orbit == "Geostationary" & satellite_longitude_deg < 0, paste0("GEO ",-satellite_longitude_deg," W"),
                          ifelse(orbit == "Geostationary" &satellite_longitude_deg > 0, paste0("GEO ",satellite_longitude_deg," E"),
                                 ifelse(orbit == "Geostationary" & satellite_longitude_deg == 0, paste0("GEO ", satellite_longitude_deg), orbit)))) %>%
    mutate(orbit = ifelse(orbit == "SS Drift Low Incl", "Low Inclination", orbit)) %>%
    # mutate(orbit = ifelse(is.na(orbit) ~ "TBD")) %>%
    mutate(orbit_type = ifelse(orbit_type == "Geostationary", satellite_longitude_deg, orbit_type)) %>%
    mutate(orbit_type = ifelse(is.na(orbit_type), "TBD", orbit_type)) %>%
    mutate(orbit_type = ifelse(orbit_type == "Drifting", "SS Drifting", orbit_type)) %>%
    select(platform_acronym, date_type, date,  orbit_type, orbit, orbit_crossing_category, satellite_longitude_deg, orbit_inclination_deg) %>% 
    distinct(platform_acronym, date_type,  .keep_all = TRUE) %>% 
    pivot_wider(names_from = date_type, values_from = date) %>%
    filter(!is.na(`Date Launched`)) %>%
    filter(!is.na(`Date End of Life (EOL)`)) %>%
    filter(!is.na(platform_acronym))
  
  dat_launch<- dat1 %>%
    select(platform_acronym, `Date Launched`) %>%
    dplyr::rename(Platform = platform_acronym)
  
  M <- Map(seq, dat1$`Date Launched`, dat1$`Date End of Life (EOL)`, by = "year")
  
  dat2<-data.frame(
    Platform = rep.int(dat1$platform_acronym, vapply(M, length, 1L)),
    Orbit_Type = rep.int(dat1$orbit_type, vapply(M, length, 1L)),
    Orbit = rep.int(dat1$orbit, vapply(M, length, 1L)),
    Year = as.Date(do.call(c, M), format = "%m-%d-%Y"))
  
  
  dat_wide<- dat2 %>%
    filter(Year >= "2022-01-01" & Year <= "2032-12-31") %>%
    arrange(Year) %>%
    mutate(Year = year(Year)) %>%
    mutate(Orbit_Type2 = Orbit_Type) %>%
    pivot_wider(names_from = Year, values_from = Orbit_Type) %>% 
    left_join(dat_launch, by = "Platform") %>%
    mutate(Orbit = as.factor(Orbit)) %>%
    mutate(Orbit_Type2 = as.factor(Orbit_Type2)) %>%
    mutate(Platform = as.factor(Platform)) %>%
    arrange(Orbit_Type2, Platform, `Date Launched`) %>% 
    select(-`Date Launched`, -Orbit_Type2, -Orbit) %>%
    as.data.frame()
  
  
  dat_wide
}