

# Run econ-753-project.R first


library(tidycensus)
library(tidyverse)
library(ggplot2)
library(sf)
library(dplyr)


setwd("/Users/emilypisano/Documents/JJ Grad School/Economics for New York/Group Project")



occupied_households <- readRDS("2017 hvs occupied.rds")


# Should I select only the columns I want? Maybe I should select by different graphs and tables I want to make instead
occupied_households_sv <- occupied_households %>%
   select(,c(150,155,156,97,29,30,32,31,112,133,153,154,85,25,24,56,58,62,63,68,80,176,82,83,84,85,116,118,127,128,129,130,131,24,2,1,188,188, 189,190,191,192,193,194,195,196,197,198,199,200,201,202,203,204,205,206,207,208,209,210,211,212,213,214,215,216,217,218,219,220,221,222,223,224,225,226,227,228,229,230,231,232,233,234,235,236,237,238,239,240,241,242,243,244,245,246,247,248,249,250,251,252,253,254,255,256,257,258,259,260,261,262,263,264,265,266,267,101,100,90,102))




occupied_households_sv <- occupied_households_sv %>%
  mutate(
    geo.join.id = case_when(
      (Borough == "3") ~ "36041", # 3 manhattan in hvs, 1 in acs
      (Borough == "1") ~ "36042", # 1 bronx in hvs, 2 is bronx in acs
      (Borough == "2") ~ "36043", # 2 brooklyn in hvs, 3 in acs 
      (Borough == "5") ~ "36045", # 5 staten island in hvs
      (Borough == "4") ~ "36044"  # 4 queens in hvs
    )
  )


occupied_households_sv <- occupied_households_sv %>%   
  mutate(
    geo.join.id = case_when(
      Sub.borough.area == "01" ~ paste0(as.character(geo.join.id), "01"),
      Sub.borough.area == "02" ~ paste0(as.character(geo.join.id), "02"),
      Sub.borough.area == "03" ~ paste0(as.character(geo.join.id), "03"),
      Sub.borough.area == "04" ~ paste0(as.character(geo.join.id), "04"),
      Sub.borough.area == "05" ~ paste0(as.character(geo.join.id), "05"),
      Sub.borough.area == "06" ~ paste0(as.character(geo.join.id), "06"),
      Sub.borough.area == "07" ~ paste0(as.character(geo.join.id), "07"),
      Sub.borough.area == "08" ~ paste0(as.character(geo.join.id), "08"),
      Sub.borough.area == "09" ~ paste0(as.character(geo.join.id), "09"),
      Sub.borough.area == "10" ~ paste0(as.character(geo.join.id), "10"),
      Sub.borough.area == "11" ~ paste0(as.character(geo.join.id), "11"),
      Sub.borough.area == "12" ~ paste0(as.character(geo.join.id), "12"),
      Sub.borough.area == "13" ~ paste0(as.character(geo.join.id), "13"),
      Sub.borough.area == "14" ~ paste0(as.character(geo.join.id), "14"),
      Sub.borough.area == "15" ~ paste0(as.character(geo.join.id), "15"),
      Sub.borough.area == "16" ~ paste0(as.character(geo.join.id), "16"),
      Sub.borough.area == "17" ~ paste0(as.character(geo.join.id), "17"),
      Sub.borough.area == "18" ~ paste0(as.character(geo.join.id), "18"),
      TRUE ~ NA_character_
    )
  )
    

occupied_households_sv <- occupied_households_sv %>%   
  mutate(
    geo.join.id = as.character(geo.join.id),
    geo.join.id = case_when(
      geo.join.id %in% c("3604203","3604206") ~ "3604263",
      geo.join.id %in% c("3604201","3604202") ~ "3604221",
      geo.join.id %in% c("3604101","3604102") ~ "3604121",
      geo.join.id %in% c("3604105","3604106") ~ "3604165",
      TRUE ~ geo.join.id 
    ))

occupied_households_sv <- occupied_households_sv %>%
  mutate(geo.join.id = as.numeric(geo.join.id))

# need to combine with geospatial


geo_id <- aq_acs %>%
  select("Geo.Join.ID","Geo.Place.Name","name","geometry", "pct_children","total_population","med_hh_income","pop_under_18") %>%
  distinct() %>%
  mutate(Geo.Join.ID = as.numeric(Geo.Join.ID))



occupied_households_sv <- occupied_households_sv %>%
  left_join(geo_id, by = c("geo.join.id" = "Geo.Join.ID"))




# convert relevant columns to numeric

occupied_households_sv[1:117] <- lapply(occupied_households_sv[1:117], as.numeric)

column_classes <- sapply(occupied_households_sv, class)




occupied_households_sv <- st_sf(geometry = st_sfc(occupied_households_sv$geometry), data = occupied_househo