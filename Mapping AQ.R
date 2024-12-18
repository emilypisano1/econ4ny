# Subsetting and Mapping

# install.packages("ggmap")
# install.packages("remotes")
# remotes::install_github("UrbanInstitute/urbnthemes", build_vignettes = TRUE)
# install.packages("RColorBrewer")
# install.packages("viridis")
library(viridis)
library(tidyverse)
library(urbnthemes)
library(ggplot2)
library(RColorBrewer)
library(scales)

setwd("/Users/emilypisano/Documents/JJ Grad School/Economics for New York/Group Project")


# # Run econ-753-project.R and Cleaning hvs first


# set up open streets mapping 


# Clean Data Sets For Mapping
aq_17_mapping <- aq_acs %>%
  filter(Start.Date >= "2016-12-01" & Start.Date <= "2017-12-01")



aq_17_mapping <- st_sf(geometry = st_sfc(aq_17_mapping$geometry), data = aq_17_mapping)


# asthma <- aq_health %>%
#   filter(Measure == "Estimated annual rate (under age 18)")
# 
# 
# aq_health <- st_sf(geometry = st_sfc(aq_health$geometry), data = aq_health)
# 
# 
# asthma <- st_sf(geometry = st_sfc(asthma$geometry), data = asthma)
# aq_health$`data.Asthma emergency department visits due to PM2.5_per 100,000 adults`[is.na(aq_health$`data.Asthma emergency department visits due to PM2.5_per 100,000 adults`)] <- 0



## Air Quality

# Mapping PM 2.5 Concentration



pm2.5_map <- ggplot(aq_17_mapping) +
  geom_sf(aes(fill = `data.PM2.5_mcg/m3`)) +
  scale_size_continuous(range = c(1, 10)) +
  labs(title = "Heatmap of PM 2.5 Concentration in 2017", fill = "PM 2.5") +
  theme(
    axis.text = element_blank(),    # Remove axis text (lat/long)
    axis.ticks = element_blank(),   # Remove axis ticks
    axis.title = element_blank(),
    panel.grid = element_blank(),   # Remove gridlines
    panel.background = element_blank()# Remove axis titles
  ) + 
  scale_fill_viridis_c(option = "D", direction = -1, na.value = 'transparent')




# Mapping NO2


no2_map <- ggplot(aq_17_mapping) +
  geom_sf(aes(fill = `data.NO2_ppb`)) + 
  scale_size_continuous(range = c(1, 10)) +
  labs(title = "Heatmap of NO2 Concentration", fill = "NO2") +
  theme(
    axis.text = element_blank(),    # Remove axis text (lat/long)
    axis.ticks = element_blank(),   # Remove axis ticks
    axis.title = element_blank(),
    panel.grid = element_blank(),   # Remove gridlines
    panel.background = element_blank()# Remove axis titles
  ) + 
  scale_fill_viridis_c(option = "D", direction = -1, na.value = 'transparent')


# Mapping Ozone

ozone_map <- ggplot(aq_17_mapping) +
  geom_sf(aes(fill = `data.O3_ppb`)) + 
  scale_size_continuous(range = c(1, 10)) +
  labs(title = "Heatmap of Ozone Concentration", fill = "O3")+
  theme(
    axis.text = element_blank(),    # Remove axis text (lat/long)
    axis.ticks = element_blank(),   # Remove axis ticks
    axis.title = element_blank(),
    panel.grid = element_blank(),   # Remove gridlines
    panel.background = element_blank()# Remove axis titles
  ) + 
  scale_fill_viridis_c(option = "D", direction = -1, na.value = 'transparent')

# Demographic Data 

pop_under_18 <- aq_17_mapping %>%
  group_by(geometry) %>%
  summarise(pop = median(data.pop_under_18))



pop_under_18_map <- ggplot(pop_under_18) +
  geom_sf(aes(fill = `pop`)) + 
  scale_size_continuous(range = c(1, 10)) +
  labs(title = "Population Under 18", fill = "Count", labels = label_comma()) +
  theme(
    axis.text = element_blank(),    # Remove axis text (lat/long)
    axis.ticks = element_blank(),   # Remove axis ticks
    axis.title = element_blank(),
    panel.grid = element_blank(),   # Remove gridlines
    panel.background = element_blank()# Remove axis titles
  ) + 
  scale_fill_viridis_c(option = "D", 
                       direction = -1, 
                       na.value = 'transparent'
                       )
          


med_hh_inc <- aq_17_mapping %>%
  group_by(geometry) %>%
  summarise(median_hh_income = median(data.med_hh_income))


med_hh_inc_map <- ggplot(med_hh_inc) +
  geom_sf(aes(fill = `median_hh_income`)) + 
  scale_size_continuous(range = c(1, 10)) +
  labs(title = "Median Household Income", fill = "$ Amount") +
  theme(
    axis.text = element_blank(),    # Remove axis text (lat/long)
    axis.ticks = element_blank(),   # Remove axis ticks
    axis.title = element_blank(),
    panel.grid = element_blank(),   # Remove gridlines
    panel.background = element_blank()# Remove axis titles
  ) + 
  scale_fill_viridis_c(option = "D", na.value = 'transparent', labels = label_comma())

tot_pop <- aq_17_mapping %>%
  group_by(geometry) %>%
  summarise(pop = median(data.total_population))


tot_pop_map <- ggplot(tot_pop) +
  geom_sf(aes(fill = `pop`)) + 
  scale_size_continuous(range = c(1, 10)) +
  labs(title = "Total Population", fill = "Count") +
  theme(
    axis.text = element_blank(),    # Remove axis text (lat/long)
    axis.ticks = element_blank(),   # Remove axis ticks
    axis.title = element_blank(),
    panel.grid = element_blank(),   # Remove gridlines
    panel.background = element_blank()# Remove axis titles
  ) + 
  scale_fill_viridis_c(option = "D", direction = -1, na.value = 'transparent')



# Mapping Health Effects

# asthma_children <- aq_17_mapping %>%
#   group_by(geometry) %>%
#   summarise(ratio = median(`data.Asthma emergency department visits due to PM2.5_per 100,000 children`))
# 
#  asthma_child_map <- ggplot(aq_17_mapping) +
#    geom_sf(aes(fill = `ratio`)) +
#    scale_size_continuous(range = c(1, 10)) +
#    labs(title = "Heatmap of Asthma emergency visits", fill = "Visits Per 100,000 children") +
#    theme(
#      axis.text = element_blank(),    # Remove axis text (lat/long)
#      axis.ticks = element_blank(),   # Remove axis ticks
#      axis.title = element_blank(),
#      panel.grid = element_blank(),   # Remove gridlines
#      panel.background = element_blank()# Remove axis titles
#    ) +
#    scale_fill_viridis_c(option = "D", direction = -1, na.value = 'transparent')
# 
# resp_adults <- aq_17_mapping %>%
#    group_by(geometry) %>%
#    summarise(ratio = median(`data.Respiratory hospitalizations due to PM2.5 (age 20+)_per 100,000 adults`))
#  
#  
#  
#  resp_map <- ggplot(aq_17_mapping) +
#    geom_sf(aes(fill = `resp_adults`)) +
#    scale_size_continuous(range = c(1, 10)) +
#    labs(title = "Heatmap of Respiratory hospitalizations due to PM2.5", fill = "Visits per 100,000 adults") +
#    theme(
#      axis.text = element_blank(),    # Remove axis text (lat/long)
#      axis.ticks = element_blank(),   # Remove axis ticks
#      axis.title = element_blank(),
#      panel.grid = element_blank(),   # Remove gridlines
#      panel.background = element_blank()# Remove axis titles
#    ) +
#    scale_fill_viridis_c(option = "D", direction = -1, na.value = 'transparent')
# 
#  card_adults <- aq_17_mapping %>%
#    group_by(geometry) %>%
#    summarise(ratio = median(`data.Cardiovascular hospitalizations due to PM2.5 (age 40+)_per 100,000 adults`))
#  
#  
#  
#  
#  cardiac_map <- ggplot(aq_17_mapping) +
#    geom_sf(aes(fill = `card_adults`)) +
#    scale_size_continuous(range = c(1, 10)) +
#    labs(title = "Heatmap of Cardiovascular hospitalizations due to PM2.5", fill = "Visits per 100,000 adults") +
#    theme(
#      axis.text = element_blank(),    # Remove axis text (lat/long)
#      axis.ticks = element_blank(),   # Remove axis ticks
#      axis.title = element_blank(),
#      panel.grid = element_blank(),   # Remove gridlines
#      panel.background = element_blank()# Remove axis titles
#    ) +
#    scale_fill_viridis_c(option = "D", direction = -1, na.value = 'transparent')
# 
#  card_adults <- aq_17_mapping %>%
#    group_by(geometry) %>%
#    summarise(ratio = median(`data.Cardiovascular hospitalizations due to PM2.5 (age 40+)_per 100,000 adults`))
#  
#  
#  
# 
#  deaths_map <- ggplot(aq_17_mapping) +
#    geom_sf(aes(fill = `data.Deaths due to PM2.5_per 100,000 adults`)) +
#    scale_size_continuous(range = c(1, 10)) +
#    labs(title = "Heatmap of Deaths due to PM2.5", fill = "Deaths per 100,000 adults") +
#    theme(
#      axis.text = element_blank(),    # Remove axis text (lat/long)
#      axis.ticks = element_blank(),   # Remove axis ticks
#      axis.title = element_blank(),
#      panel.grid = element_blank(),   # Remove gridlines
#      panel.background = element_blank()# Remove axis titles
#    ) +
#    scale_fill_viridis_c(option = "D", direction = -1, na.value = 'transparent')






# From HVS

## Health Variables

bad_health <- occupied_households_sv %>%
  group_by(geometry) %>%
  summarise(
    ratio = sum(
      case_when(
        data.General.Health..respondent %in% c(4,5) ~ data.Final.household.weight,
        TRUE ~ 0  # Set other cases to 0
      )
    ) / sum(
      case_when(
        data.Mental.health %in% c(1,2,3,4,5,6) ~ data.Final.household.weight,
        TRUE ~ 0  # Set other cases to 0
      )
    ))


bad_health_map <- ggplot(bad_health) +
  geom_sf(aes(fill = `ratio`)) +  
  labs(title = "Ratio of population in fair or poor health", fill = "Ratio") +
  theme(
    axis.text = element_blank(),    # Remove axis text (lat/long)
    axis.ticks = element_blank(),   # Remove axis ticks
    axis.title = element_blank(),
    panel.grid = element_blank(),   # Remove gridlines
    panel.background = element_blank()# Remove axis titles
  ) + 
  scale_fill_viridis_c(option = "D", direction = -1, na.value = 'transparent')


mental_health_ratio <- occupied_households_sv %>%
  group_by(geometry) %>%
  summarise(
    ratio = sum(
      case_when(
        data.Mental.health %in% 1 ~ data.Final.household.weight,
        TRUE ~ 0  # Set other cases to 0
      )
    ) / sum(
      case_when(
        data.Mental.health %in% c(1,2) ~ data.Final.household.weight,
        TRUE ~ 0  # Set other cases to 0
      )
    ),
    no.obs = sum(
      case_when(
        data.Mental.health %in% c(8) ~ data.Final.household.weight,
        TRUE ~ 0  # Set other cases to 0
      )
    ) / sum(
      case_when(
        data.Mental.health %in% c(1,2,8) ~ data.Final.household.weight,
        TRUE ~ 0  # Set other cases to 0
      )
    ))


mental_health_map <- ggplot(mental_health_ratio) +
  geom_sf(aes(fill = `ratio`)) +  
  labs(title = "Avoided Mental Health Care", fill = "Ratio") +
  theme(
    axis.text = element_blank(),    # Remove axis text (lat/long)
    axis.ticks = element_blank(),   # Remove axis ticks
    axis.title = element_blank(),
    panel.grid = element_blank(),   # Remove gridlines
    panel.background = element_blank()# Remove axis titles
  ) + 
  scale_fill_viridis_c(option = "D", direction = -1, na.value = 'transparent')

mental_health_no_obs_map <- ggplot(mental_health_ratio) +
  geom_sf(aes(fill = `no.obs`)) +  
  labs(title = "Avoided Mental Health Care,Unreported", fill = "Ratio") +
  theme(
    axis.text = element_blank(),    # Remove axis text (lat/long)
    axis.ticks = element_blank(),   # Remove axis ticks
    axis.title = element_blank(),
    panel.grid = element_blank(),   # Remove gridlines
    panel.background = element_blank()# Remove axis titles
  ) + 
  scale_fill_viridis_c(option = "D", direction = -1, na.value = 'transparent')




#Building Condition
poor_building_condition_ratio <- occupied_households_sv %>%
  group_by(geometry) %>%
  summarise(ratio = (sum(data.Condition.of.building %in%  c(1,3) * data.Final.household.weight)) / (sum(data.Condition.of.building %in% c(1,2,3)* data.Final.household.weight))) 



poor_building_condition_map <- ggplot(poor_building_condition_ratio) +
  geom_sf(aes(fill = `ratio`)) +  # Use fill color based on Median Rent
  labs(title = "Heatmap of Poor Building Condition", fill = "Ratio of Observed Building Condition") +
  theme(
    axis.text = element_blank(),    # Remove axis text (lat/long)
    axis.ticks = element_blank(),   # Remove axis ticks
    axis.title = element_blank(),
    panel.grid = element_blank(),   # Remove gridlines
    panel.background = element_blank()# Remove axis titles
  ) + 
  scale_fill_viridis_c(option = "D", direction = -1, na.value = 'transparent')





# Ratio of Maintenance Deficiencies

occupied_households_sv <- occupied_households_sv %>%
  mutate(deficiency.recode = case_when(
      data.Number.of.2017.maintenance.deficiencies == 1 ~ "No Maintenance Deficiencies",
      data.Number.of.2017.maintenance.deficiencies == 2 ~ "Maintenance Deficiency",
      data.Number.of.2017.maintenance.deficiencies == 3 ~ "Repeated Maintenance Deficiency",
      data.Number.of.2017.maintenance.deficiencies == 4 ~ "Repeated Maintenance Deficiency",
      data.Number.of.2017.maintenance.deficiencies == 5 ~ "Repeated Maintenance Deficiency",
      data.Number.of.2017.maintenance.deficiencies == 6 ~ "Serious Repeated Maintenance Deficiency",
      data.Number.of.2017.maintenance.deficiencies == 7 ~ "Serious Repeated Maintenance Deficiency",
      data.Number.of.2017.maintenance.deficiencies == 8 ~ "Serious Repeated Maintenance Deficiency",
      data.Number.of.2017.maintenance.deficiencies == 9 ~ "Unreported",
      is.na(data.Number.of.2017.maintenance.deficiencies) ~ "Missing",  # handle NAs
      TRUE ~ NA_character_  # assign NA for all other unmatched cases
  ))

deficiency_ratio <- occupied_households_sv %>%
  group_by(geometry) %>%
  summarise(
    ratio = sum(
      case_when(
        deficiency.recode %in% c("Maintenance Deficiency", "Serious Repeated Maintenance Deficiency") ~ data.Final.household.weight,
        TRUE ~ 0  # Set other cases to 0
      )
    ) / sum(
      case_when(
        deficiency.recode %in% c("No Maintenance Deficiencies", "Maintenance Deficiency", "Serious Repeated Maintenance Deficiency") ~ data.Final.household.weight,
        TRUE ~ 0  # Set other cases to 0
      )
    ),
    high.ratio = sum(
      case_when(
        deficiency.recode %in% c("Serious Repeated Maintenance Deficiency") ~ data.Final.household.weight,
        TRUE ~ 0  # Set other cases to 0
      )
    ) / sum(
      case_when(
        deficiency.recode %in% c("No Maintenance Deficiencies", "Maintenance Deficiency", "Serious Repeated Maintenance Deficiency") ~ data.Final.household.weight,
        TRUE ~ 0  # Set other cases to 0
      )
      ),
    no.obs = sum(
      case_when(
        deficiency.recode %in% c("Unreported") ~ data.Final.household.weight,
        TRUE ~ 0  # Set other cases to 0
      )
    ) / sum(
      case_when(
        deficiency.recode %in% c("Unreported","No Maintenance Deficiencies", "Maintenance Deficiency", "Serious Repeated Maintenance Deficiency") ~ data.Final.household.weight,
        TRUE ~ 0  # Set other cases to 0
      )
    ))




maintenance_map<- ggplot(deficiency_ratio) +
  geom_sf(aes(fill = `high.ratio`)) + 
  labs(title = "Heatmap of Serious Repeated Maintenance Deficiencies", fill = "Ratio of Buildings with Reported Maintenance Deficiencies") +
  theme(
    axis.text = element_blank(),    # Remove axis text (lat/long)
    axis.ticks = element_blank(),   # Remove axis ticks
    axis.title = element_blank(),
    panel.grid = element_blank(),   # Remove gridlines
    panel.background = element_blank()# Remove axis titles
  ) + 
  scale_fill_viridis_c(option = "D", direction = -1, na.value = 'transparent')


bad_maintenance_map <- ggplot(deficiency_ratio) +
  geom_sf(aes(fill = `no.obs`)) +  
  labs(title = "Heatmap of Serious Repeated Maintenance Deficiencies", fill = "Ratio of Buildings with No-Record on Maintenance Deficiencies")+
  theme(
    axis.text = element_blank(),    # Remove axis text (lat/long)
    axis.ticks = element_blank(),   # Remove axis ticks
    axis.title = element_blank(),
    panel.grid = element_blank(),   # Remove gridlines
    panel.background = element_blank()# Remove axis titles
  ) + 
  scale_fill_viridis_c(option = "D", direction = -1, na.value = 'transparent')

# Broken Windows

broken_windows_ratio <- occupied_households_sv %>%
  group_by(geometry) %>%
  summarise(
    ratio = (sum(data.Broken.Boarded.Windows..observation %in%  1 * data.Final.household.weight)) / (sum(data.Condition.of.building %in% c(1,2)* data.Final.household.weight)),
    ratio.no.obs = (sum(data.Broken.Boarded.Windows..observation %in%  8 * data.Final.household.weight)) / (sum(data.Condition.of.building %in% c(1,2,8)* data.Final.household.weight)
    )
    ) 



broken_windows_map <- ggplot(broken_windows_ratio) +
  geom_sf(aes(fill = `ratio`)) +  # Use fill color based on Median Rent
  labs(title = "Heatmap of Broken Windows", fill = "Ratio of Observed Broken Windows") +
  theme(
    axis.text = element_blank(),    # Remove axis text (lat/long)
    axis.ticks = element_blank(),   # Remove axis ticks
    axis.title = element_blank(),
    panel.grid = element_blank(),   # Remove gridlines
    panel.background = element_blank()# Remove axis titles
  ) + 
  scale_fill_viridis_c(option = "D", direction = -1, na.value = 'transparent')

underreported_windows_map <- ggplot(broken_windows_ratio) +
  geom_sf(aes(fill = `ratio.no.obs`)) +  # Use fill color based on Median Rent
  labs(title = "Heatmap of Broken Windows", fill = "Ratio of Observed Broken Windows") +
  theme(
    axis.text = element_blank(),    # Remove axis text (lat/long)
    axis.ticks = element_blank(),   # Remove axis ticks
    axis.title = element_blank(),
    panel.grid = element_blank(),   # Remove gridlines
    panel.background = element_blank()# Remove axis titles
) + 
  scale_fill_viridis_c(option = "D", direction = -1, na.value = 'transparent')

## Under-reported observations in certain areas




## Poor Heating
# [85] "Additional.sources.of.heat"

# Nitrogen Dioxide

fuel_oil <- occupied_households_sv %>%
  group_by(geometry) %>%
  summarise(
    ratio = sum(
      case_when(
        data.Type.of.heating.fuel %in% 1 ~ data.Final.household.weight,
        TRUE ~ 0  # Set other cases to 0
      )
    ) / sum(
      case_when(
        data.Type.of.heating.fuel %in% c(1,2,3,4) ~ data.Final.household.weight,
        TRUE ~ 0  # Set other cases to 0
      ))
  )

fuel_map <- ggplot(fuel_oil) +
  geom_sf(aes(fill = `ratio`)) +  # Use fill color based on Median Rent
  labs(title = "Heatmap of Housing with Fuel Oil", fill = "Ratio") +
  theme(
    axis.text = element_blank(),    # Remove axis text (lat/long)
    axis.ticks = element_blank(),   # Remove axis ticks
    axis.title = element_blank(),
    panel.grid = element_blank(),   # Remove gridlines
    panel.background = element_blank()# Remove axis titles
  ) + 
  scale_fill_viridis_c(option = "D", direction = -1, na.value = 'transparent')

additional_heating_ratio <- occupied_households_sv %>%
  group_by(geometry) %>%
  summarise(
    ratio = sum(
      case_when(
        data.Additional.sources.of.heat %in% 1 ~ data.Final.household.weight,
        TRUE ~ 0  # Set other cases to 0
      )
    ) / sum(
      case_when(
        data.Additional.sources.of.heat %in% c(1,2) ~ data.Final.household.weight,
        TRUE ~ 0  # Set other cases to 0
      )
    ),
    no.obs = sum(
      case_when(
        data.Additional.sources.of.heat %in% c(8) ~ data.Final.household.weight,
        TRUE ~ 0  # Set other cases to 0
      )
    ) / sum(
      case_when(
        data.Additional.sources.of.heat %in% c(1,2,8) ~ data.Final.household.weight,
        TRUE ~ 0  # Set other cases to 0
      )
    ))


additional_heating_map <- ggplot(additional_heating_ratio) +
  geom_sf(aes(fill = `ratio`)) +  
  labs(title = "Heatmap of Additional Heating Units", fill = "Ratio of Buildings with Additional Heating") +
  theme(
    axis.text = element_blank(),    # Remove axis text (lat/long)
    axis.ticks = element_blank(),   # Remove axis ticks
    axis.title = element_blank(),
    panel.grid = element_blank(),   # Remove gridlines
    panel.background = element_blank()# Remove axis titles
  ) + 
  scale_fill_viridis_c(option = "D", direction = -1, na.value = 'transparent')


underreported_heating_map <- ggplot(additional_heating_ratio) +
  geom_sf(aes(fill = `no.obs`)) +  
  labs(title = "Heatmap of Under-Reporting, Additional Heating Units", fill = "Ratio of Buildings with No-Record") +
  theme(
    axis.text = element_blank(),    # Remove axis text (lat/long)
    axis.ticks = element_blank(),   # Remove axis ticks
    axis.title = element_blank(),
    panel.grid = element_blank(),   # Remove gridlines
    panel.background = element_blank()# Remove axis titles
  ) + 
  scale_fill_viridis_c(option = "D", direction = -1, na.value = 'transparent')

# [82] "Heating.equipment.breakdown" 


heating_breakdown_ratio <- occupied_households_sv %>%
  group_by(geometry) %>%
  summarise(
    ratio = sum(
      case_when(
        data.Number.of.heating.breakdowns %in% c(2,3,4,5) ~ data.Final.household.weight,
        TRUE ~ 0  # Set other cases to 0
      )
    ) / sum(
      case_when(
        data.Number.of.heating.breakdowns %in% c(2,3,4,5,9) ~ data.Final.household.weight,
        TRUE ~ 0  # Set other cases to 0
      )
    ),
    no.obs = sum(
      case_when(
        data.Number.of.heating.breakdowns %in% c(8) ~ data.Final.household.weight,
        TRUE ~ 0  # Set other cases to 0
      )
    ) / sum(
      case_when(
        data.Number.of.heating.breakdowns %in% c(2,3,4,5,9,8) ~ data.Final.household.weight,
        TRUE ~ 0  # Set other cases to 0
      )
    ))

heating_breakdown_map<- ggplot(heating_breakdown_ratio) +
  geom_sf(aes(fill = `ratio`)) +  
  labs(title = "Heatmap of Heating Breakdown", fill = "Ratio of Heating Breakdown") +
  theme(
    axis.text = element_blank(),    # Remove axis text (lat/long)
    axis.ticks = element_blank(),   # Remove axis ticks
    axis.title = element_blank(),
    panel.grid = element_blank(),   # Remove gridlines
    panel.background = element_blank()# Remove axis titles
  ) + 
  scale_fill_viridis_c(option = "D", direction = -1, na.value = 'transparent')


underreported_breakdowns <- ggplot(heating_breakdown_ratio) +
  geom_sf(aes(fill = `no.obs`)) +  
  labs(title = "Heatmap of Under-Reporting, Heating Breakdown", fill = "Ratio of Buildings with No-Record") +
  theme(
    axis.text = element_blank(),    # Remove axis text (lat/long)
    axis.ticks = element_blank(),   # Remove axis ticks
    axis.title = element_blank(),
    panel.grid = element_blank(),   # Remove gridlines
    panel.background = element_blank()# Remove axis titles
  ) + 
  scale_fill_viridis_c(option = "D", direction = -1, na.value = 'transparent')


#  [84] "Functioning.Air.Conditioning" 


air_condition_ratio <- occupied_households_sv %>%
  group_by(geometry) %>%
  summarise(
    ratio = sum(
      case_when(
        data.Functioning.Air.Conditioning %in% c(1,2) ~ data.Final.household.weight,
        TRUE ~ 0  # Set other cases to 0
      )
    ) / sum(
      case_when(
        data.Functioning.Air.Conditioning %in% c(1,2,3) ~ data.Final.household.weight,
        TRUE ~ 0  # Set other cases to 0
      )
    ),
    no.obs = sum(
      case_when(
        data.Functioning.Air.Conditioning %in% c(4,8) ~ data.Final.household.weight,
        TRUE ~ 0  # Set other cases to 0
      )
    ) / sum(
      case_when(
        data.Functioning.Air.Conditioning %in% c(1,2,3,4,8) ~ data.Final.household.weight,
        TRUE ~ 0  # Set other cases to 0
      )
    ))

air_conditioning_map <- ggplot(air_condition_ratio) +
  geom_sf(aes(fill = `ratio`)) +  
  labs(title = "Heatmap of Functioning Air Condition", fill = "Ratio") +
  theme(
    axis.text = element_blank(),    # Remove axis text (lat/long)
    axis.ticks = element_blank(),   # Remove axis ticks
    axis.title = element_blank(),
    panel.grid = element_blank(),   # Remove gridlines
    panel.background = element_blank()# Remove axis titles
  ) + 
  scale_fill_viridis_c(option = "D", direction = 1, na.value = 'transparent')




# Density - Persons Per Room


density_map <- ggplot(occupied_households_sv) +
  geom_sf(aes(fill = `data.Persons.per.room`)) +  
  labs(title = "Heatmap of Persons per Room", fill = "Ratio ") +
  theme(
    axis.text = element_blank(),    # Remove axis text (lat/long)
    axis.ticks = element_blank(),   # Remove axis ticks
    axis.title = element_blank(),
    panel.grid = element_blank(),   # Remove gridlines
    panel.background = element_blank()# Remove axis titles
  ) + 
  scale_fill_viridis_c(option = "D", direction = -1, na.value = 'transparent')


# Economic 


# rental_assistance_ratio <- occupied_households_sv %>%
#   group_by(geometry) %>%
#   summarise(
#     rental.assistance.ratio = (sum(
#       case_when(
#         data.Rental.assistance.flag %in% c(1) ~ data.Final.household.weight,
#         TRUE ~ 0
#       ),
#       na.rm = TRUE
#     )) / (sum(
#       case_when(
#         data.Rental.assistance.flag %in% c(1, 2) ~ data.Final.household.weight,
#         TRUE ~ 0
#       ),
#       na.rm = TRUE
#     )
#   )
#   )
# 
# rental_assistance_map <- ggplot(rental_assistance_ratio) +
#   geom_sf(aes(fill = `rental.assistance.ratio`)) +  
#   labs(title = "Heatmap of Rental Assistance", fill = "Ratio") +
#   theme(
#     axis.text = element_blank(),    # Remove axis text (lat/long)
#     axis.ticks = element_blank(),   # Remove axis ticks
#     axis.title = element_blank(),
#     panel.grid = element_blank(),   # Remove gridlines
#     panel.background = element_blank()# Remove axis titles
#   ) + 
#   scale_fill_viridis_c(option = "D", direction = -1, na.value = 'transparent')


section_8_ratio <- occupied_households_sv %>%
  group_by(geometry) %>%
  summarise(
    section.8.ratio = sum(
      case_when(
        data.Federal.section.8 %in% c(1) ~ data.Final.household.weight,
        TRUE ~ 0
      ),
      na.rm = TRUE
    ) / sum(
      case_when(
        data.Federal.section.8 %in% c(1, 2) ~ data.Final.household.weight,
        TRUE ~ 0
      ),
      na.rm = TRUE
    ),
    owner = sum(
      case_when(
        data.Federal.section.8 %in% c(3) ~ data.Final.household.weight,
        TRUE ~ 0
      ),
      na.rm = TRUE
    ) / sum(
      case_when(
        data.Federal.section.8 %in% c(1, 2, 3) ~ data.Final.household.weight,
        TRUE ~ 0
      ),
      na.rm = TRUE
    )
  )

section_8_map <- ggplot(section_8_ratio) +
  geom_sf(aes(fill = `section.8.ratio`)) +  
  labs(title = "Heatmap of Federal Section 8 Housing", fill = "Ratio of Section 8") +
  theme(
    axis.text = element_blank(),    # Remove axis text (lat/long)
    axis.ticks = element_blank(),   # Remove axis ticks
    axis.title = element_blank(),
    panel.grid = element_blank(),   # Remove gridlines
    panel.background = element_blank()# Remove axis titles
  ) + 
  scale_fill_viridis_c(option = "D", direction = -1, na.value = 'transparent')


# Save all maps


getwd()
directory_path <- "/Users/emilypisano/Documents/JJ Grad School/Economics for New York/Group Project/" # Change for your directory
png(file = paste0(directory_path, "pm2.5_map.png"))
print(pm2.5_map)
dev.off()
png(file = paste0(directory_path, "no2_map.png"))
print(no2_map)
dev.off()
png(file = paste0(directory_path, "ozone_map.png"))
print(ozone_map)
dev.off()
png(file = paste0(directory_path, "pop_under_18_map.png"))
print(pop_under_18_map)
dev.off()
png(file = paste0(directory_path, "med_hh_inc_map.png"))
print(med_hh_inc_map)
dev.off()
png(file = paste0(directory_path, "tot_pop_map.png"))
print(tot_pop_map)
dev.off()
png(file = paste0(directory_path, "mental_health_map.png"))
print(mental_health_map)
dev.off()
png(file = paste0(directory_path, "mental_health_no_obs_map.png"))
print(mental_health_no_obs_map)
dev.off()
png(file = paste0(directory_path, "poor_building_condition_map.png"))
print(poor_building_condition_map)
dev.off()
png(file = paste0(directory_path, "maintenance_map.png"))
print(maintenance_map)
dev.off()
png(file = paste0(directory_path, "bad_maintenance_map.png"))
print(bad_maintenance_map)
dev.off()
png(file = paste0(directory_path, "broken_windows_map.png"))
print(broken_windows_map)
dev.off()
png(file = paste0(directory_path, "additional_heating_map.png"))
print(additional_heating_map)
dev.off()
png(file = paste0(directory_path, "underreported_heating_map.png"))
print(underreported_heating_map)
dev.off()
png(file = paste0(directory_path, "heating_breakdown_map.png"))
print(heating_breakdown_map)
dev.off()
png(file = paste0(directory_path, "air_conditioning_map.png"))
print(air_conditioning_map)
dev.off()
png(file = paste0(directory_path, "density_map.png"))
print(density_map)
dev.off()
png(file = paste0(directory_path, "rental_assistance_map.png"))
print(rental_assistance_map)
dev.off()
png(file = paste0(directory_path, "section_8_map.png"))
print(section_8_map)
dev.off()
png(file = paste0(directory_path, "bad_health.png"))
print(bad_health_map)
dev.off()






