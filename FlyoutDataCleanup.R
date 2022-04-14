# Flyout Tables Cleanup

# Load packages and web services
library(httr)
library(rlist)
library(dplyr)
library(devtools)
library(jsonlite)
library(tidyverse)
library(lubridate)
#install_github("jeroenooms/jsonlite")
#eores<-"https://eorestest.nesdis-hq.noaa.gov/ws/ws/"
#eores<-https://eoreswstest2.nesdis-hq.noaa.gov/ws/ws/"
#eores<-"https://eores.nesdis-hq.noaa.gov/ws/ws/"
eores<-"https://eores.nesdis-hq.noaa.gov/ws/ws/"

#treename<-"EOA2016"
treename<-"NOSIA-II"
#treename<-"NOSIA-2-1"

#networks
r<-GET(paste0(eores,"get_reports_network_basic_information",sep=""))
json_text<-content(r, as = "text")
networks_json<-fromJSON(json_text)
networks<-networks_json[['rptntwrkbasic']]

#networks2systems
r<-GET(paste0(eores,"get_reports_network_observing_system_associations",sep=""))
json_text<-content(r, as = "text")
networks2systems_json<-fromJSON(json_text)
networks2systems<-networks2systems_json[['rptntwrk2system']]

#systems
r<-GET(paste0(eores,"get_reports_observing_system_basic_information",sep=""))
json_text<-content(r, as = "text")
systems_json<-fromJSON(json_text)
systems<-systems_json[['rptsystembasic']]

#gcmd master
r<-GET(paste0(eores,"get_reports_gcmd_master",sep=""))
json_text<-content(r, as = "text")
gcmd_json<-fromJSON(json_text)
gcmd<-gcmd_json[['rptgcmdmaster']]

#platforms
r<-GET(paste0(eores,"get_reports_platform_basic_information",sep=""))
json_text<-content(r, as = "text")
platforms_json<-fromJSON(json_text)
platforms<-platforms_json[['rptpltfrmbasic']]

#platforms2system
r<-GET(paste0(eores,"get_reports_observing_system_platform_associations",sep=""))
json_text<-content(r, as = "text")
platform2system_json<-fromJSON(json_text)
platform2system<-platform2system_json[['rptsystem2pltfrm']]

#platform POH
r<-GET(paste0(eores,"get_reports_platform_organizational_affiliations",sep=""))
json_text<-content(r, as = "text")
platform2poh_json<-fromJSON(json_text)
platform2poh<-platform2poh_json[['rptpltfrmorgs']]

#platform dates
r<-GET(paste0(eores,"get_reports_platform_key_dates",sep=""))
json_text<-content(r, as = "text")
platform2dates_json<-fromJSON(json_text)
platform2dates<-platform2dates_json[['rptpltfrmdates']]

#Sensing Elements
r<-GET(paste0(eores,"get_reports_sensing_element_basic_information",sep=""))
json_text<-content(r, as = "text")
se_json<-fromJSON(json_text)
se<-se_json[['rptsebasic']]

#se2platform
r<-GET(paste0(eores,"get_reports_platform_sensing_element_associations",sep=""))
json_text<-content(r, as = "text")
se2platform_json<-fromJSON(json_text)
se2platform<-se2platform_json[['rptpltfrm2se']]

#environmental parameters
r<-GET(paste0(eores,"get_reports_environmental_parameter_basic_information",sep=""))
json_text<-content(r, as = "text")
ep_json<-fromJSON(json_text)
ep<-ep_json[['rptenvparambasic']]

#se2ep
r<-GET(paste0(eores,"get_reports_sensing_element_environmental_parameter_associations",sep=""))
json_text<-content(r, as = "text")
ep2se_json<-fromJSON(json_text)
ep2se<-ep2se_json[['rptse2envparam']]


# The final data frame needs to be named flyout_clean for the CGMS_pngs.RMD code to work
# This joins the data and selects/filters along the way. The most complicated thing here is formatting the platform country names because they have some 
# extraneous brackets and quotation marks
flyout_clean<- networks %>%
  full_join(networks2systems, by = "network_id") %>%
  full_join(systems, by = "system_id") %>% 
  filter(grepl("Satellite", system_type)) %>%
  select(network_acronym, system_id, system_name, system_acronym, system_type, system_intended_use, osc_baseline_observing_system_category) %>%
  left_join(platform2system, by = "system_id") %>%
  left_join(platforms, by = "platform_id") %>% 
  dplyr::rename(platform_country_names = country_names) %>% 
  select(-system_id, -platform_description, -system_platform_id, -orbit_altitude_km, -orbit_period_min, -nadir_repeat, -nadir_repeat_units, -orbit_eccentricity, -perigee_altitude_km, -apogee_altitude_km, -country_code, -date_last_updated) %>%
  left_join(platform2poh, by = "platform_id") %>% 
  left_join(platform2dates, by = "platform_id") %>%
  select(-platform_poh_id, -level_1_name, -level_1_short_name, -level_4_name, -level_4_short_name, -level_5_name, -level_5_short_name, -level_6_name, -level_6_short_name, -level_7_name, -level_7_short_name, -level_8_name, -level_8_short_name, -poh_master_id, -platform_date_id) %>% 
  filter(!date_type %in% c("Date Full Operating Capability (FOC)", "OSC Comment")) %>% 
  filter(poh_affiliation_type != "PLATFORM OPERATOR") %>%
  mutate(level_3_name = ifelse(level_2_short_name != "NASA", level_3_name, level_2_name)) %>%
  mutate(level_3_short_name = ifelse(level_2_short_name != "NASA", level_3_short_name, level_2_short_name)) %>%
  mutate(owner_name = paste0(level_3_name," (", level_3_short_name,")")) %>%
  select(-level_2_name, -level_2_short_name, -level_3_name, -level_3_short_name) %>%
  left_join(se2platform, by = "platform_id") %>%
  select(-platform_sensing_element_id, -platform_id) %>%
  left_join(se, by = "sensing_element_id") %>% 
  select(network_acronym:owner_name, sensing_element_name:sensing_element_type, sensing_element_life_cycle_phase) %>%
  filter(!sensing_element_life_cycle_phase %in% c("Failed", "Retired","Degraded")) %>%
  mutate(date = as.POSIXct(date, format = "%m-%d-%Y")) %>%
  separate(platform_country_names, c("junk","platform_country_names"), sep = "([{}])") %>%
  select(-junk) %>%
  mutate(platform_country_names = gsub('\"','', platform_country_names)) %>% 
  mutate(platform_country_names = gsub(',',', ', platform_country_names))  %>%
  mutate(platform_country_names = ifelse(platform_country_names == "", NA, platform_country_names)) %>%
  mutate(owner_name = ifelse(owner_name == "NONE (NA)", NA, owner_name)) 