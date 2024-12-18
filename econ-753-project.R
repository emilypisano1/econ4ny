
library(tidycensus)

setwd("/Users/emilypisano/Documents/JJ Grad School/Economics for New York/Group Project")

preprocess <- function(path) {
  aq <- read.csv(path) %>%
    rename(`Start.Date` = `Start_Date`) %>%
    tibble(.name_repair = 'universal') %>%  # working with spaces in names is fucking impossible
    mutate(`Start.Date` = as.Date(Start.Date, format = "%m/%d/%Y")) %>%  # convert to datetime
    filter(
      !str_detect(Time.Period, 'Average') &
        Geo.Type.Name == 'CD' 
    ) %>%  # drop annual average data, select only community districts
    pivot_wider(names_from = c("Name", "Measure.Info"), values_from = Data.Value) %>%
    # pivot over the measurements
    rename_with(fix_names) %>%
    mutate(Geo.Join.ID = as.character(.$Geo.Join.ID + 3604000))
  
  merge_cds <- function(data, cd_ids, replacement_geo_id, replacement_name) {
    cd_subset = data %>% filter(.$Geo.Join.ID %in% cd_ids) %>%
      group_by(Start.Date) %>%
      summarise_if(is.numeric, mean, na.rm = TRUE)
    cd_subset$Geo.Join.ID = replacement_geo_id
    cd_subset$Geo.Place.Name = replacement_name
    return(bind_rows(data %>% filter(!(.$Geo.Join.ID %in% cd_ids)), cd_subset))
  }

  aq = merge_cds(aq, c("3604203", "3604206"), "3604263", "Bronx Community Districts 3 & 6")
  aq = merge_cds(aq, c("3604201", "3604202"), "3604221", "Bronx Community Districts 1 & 2")
  aq = merge_cds(aq, c("3604101", "3604102"), "3604121", "Manhattan Community Districts 1 & 2")
  aq = merge_cds(aq, c("3604105", "3604106"), "3604165", "Manhattan Community Districts 5 & 6")

  return(aq %>% arrange(Start.Date, Geo.Place.Name))
}

fix_names <- function(names) {
  return(case_match(
    names,
    "Ozone (O3)_ppb" ~ "O3_ppb",
    "Nitrogen dioxide (NO2)_ppb" ~ "NO2_ppb",
    "Fine particles (PM 2.5)_mcg/m3" ~ "PM2.5_mcg/m3",
    "Outdoor Air Toxics - Formaldehyde_Âµg/m3" ~ "Formaldehyde_ug/m3",
    "Outdoor Air Toxics - Benzene_Âµg/m3" ~ "Benzene_ug/m3",
    .default = as.character(names)
  ))
} 

# ACS variables: 
# B19001_001: median household income
# B01003_001: total population
# B01001_003E-6E: male population under 18
# B01001_027E-31E: female population under 18
# B01002_001E: median age by sex
# B02001_001E: race (?)
# B02001_002E: race, white


fix_cd_name <- function(name) {
  return(
    name %>% str_replace("NYC-", "") %>% 
      str_replace("Community District", "CD") %>%
      str_replace(" PUMA; New York", "")
    )
}

get_census_data <- function() {
  
  acs_data = get_acs(
    variables = c(
      "B01003_001", "B19001_001", "B01001_003E", "B01001_004E", "B01001_005E", "B01001_006E", "B01001_027E", 
      "B01001_028E", "B01001_029E", "B01001_030E", "B01001_031E", "B02001_001E", "B02001_002E"
    ),
    geography = "puma",
    state = "New York", 
    year = 2022, 
    survey = "acs5",
    geometry = FALSE) %>%
      filter(str_detect(.$NAME, "Bronx|Brookly|Manhattan|Queens|Staten Island")) %>%
      pivot_wider(id_cols = all_of(c("GEOID", "NAME")), names_from = "variable", values_from = "estimate") %>%
      mutate("pop_under_18" = rowSums(across(c(
        "B01001_003", "B01001_004", "B01001_005", "B01001_006", "B01001_027",
        "B01001_028", "B01001_029", "B01001_030", "B01001_031"
      )))) %>%
      rename(c(
        "total_population" = "B01003_001",
        "med_hh_income" = "B19001_001",
        "name" = "NAME"
        )) %>%
      mutate("pct_children" = .$pop_under_18 / .$total_population * 100, "name" = fix_cd_name(name)) %>%
      select(!matches("B0"))
  
  # get one series just to get the geometries
  acs_geoms = get_acs(
    variables = c("B01003_001"),
    geography = "puma",
    state = "New York", 
    year = 2017, 
    survey = "acs5",
    geometry = TRUE
  ) %>% 
    filter(str_detect(.$NAME, "Bronx|Brookly|Manhattan|Queens|Staten Island")) %>%
    select(all_of(c("GEOID", "geometry"))) 
  
  return(left_join(acs_data, acs_geoms, by = "GEOID"))
}

merge_aq_acs_data <- function(aq_data_path) {
  aq_data <- preprocess(aq_data_path)
  census_data <- get_census_data()
  merged <- left_join(aq_data, census_data, by = c("Geo.Join.ID" = "GEOID"))
  return(merged)
}

aq_acs <-merge_aq_acs_data("/Users/emilypisano/Documents/JJ Grad School/Economics for New York/Group Project/Air_Quality_20241120.csv")

