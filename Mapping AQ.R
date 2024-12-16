# Subsetting and Mapping? 

# List of Maps

### From Air Quality
# PM 2.5
# NO2
# OZone


## From aq_health
# Health Effects (several)

### From HVS (consider # Number of houses with children living in these conditions)
# [25] "Broken.Boarded.Windows..observation")
## Quality of Infrastructure
#  [85] "Additional.sources.of.heat"
# [8] "Condition.of.Windows..Broken.missing"                  
# [9] "Condition.of.Windows..Rotten.loose"                    
# [10] "Condition.of.Windows..Boarded.up"                      
# [11] 
# "Number.of.2017.maintenance.deficiencies" 


## Poor Heating
# [85] "Additional.sources.of.heat"
# [82] "Heating.equipment.breakdown" 
#  [83] "Number.of.heating.breakdowns"
#  [84] "Functioning.Air.Conditioning" 




## Density 
# [129] "Persons.per.room

## Economic 
# Rental Assistance
# Federal Section 8
# [130] "Monthly.gross.rent" 
# [131] "Monthly.gross.rent.per.room.recode" 


setwd("/Users/emilypisano/Documents/JJ Grad School/Economics for New York/Group Project")


# # Run econ-753-project.R and Cleaning hvs first


# Clean Data Sets For Mapping
aq_17_mapping <- aq_acs %>%
  filter(Start.Date >= "2016-12-01" & Start.Date <= "2017-12-01")



aq_17_mapping <- st_sf(geometry = st_sfc(aq_17_mapping$geometry), data = aq_17_mapping)




aq_health <- st_sf(geometry = st_sfc(aq_health$geometry), data = aq_health)


asthma <- st_sf(geometry = st_sfc(asthma$geometry), data = asthma)
# aq_health$`data.Asthma emergency department visits due to PM2.5_per 100,000 adults`[is.na(aq_health$`data.Asthma emergency department visits due to PM2.5_per 100,000 adults`)] <- 0



## Air Quality

# Mapping PM 2.5 Concentration



ggplot(aq_17_mapping) +
  geom_sf(aes(fill = `data.PM2.5_mcg/m3`)) + 
  scale_fill_distiller(palette = "RdPu", 
                       direction = 1,
                       na.value='transparent') +
  theme_minimal() +  # Minimal theme for the plot
  scale_size_continuous(range = c(1, 10)) +
  labs(title = "Heatmap of PM 2.5 Concentration in 2017", fill = "PM 2.5")

# Mapping NO2

ggplot(aq_17_mapping) +
  geom_sf(aes(fill = `data.NO2_ppb`)) + 
  scale_fill_distiller(palette = "RdPu", 
                       direction = 1,
                       na.value='transparent') +
  theme_minimal() +  # Minimal theme for the plot
  scale_size_continuous(range = c(1, 10)) +
  labs(title = "Heatmap of NO2 Concentration", fill = "NO2")


# Mapping Ozone

ggplot(aq_17_mapping) +
  geom_sf(aes(fill = `data.O3_ppb`)) + 
  scale_fill_distiller(palette = "RdPu", 
                       direction = 1,
                       na.value='transparent') +
  theme_minimal() +  # Minimal theme for the plot
  scale_size_continuous(range = c(1, 10)) +
  labs(title = "Heatmap of Ozone Concentration", fill = "O3")



# Mapping Health Effects

## COME BACK
ggplot(aq_health) +
  geom_sf(aes(fill = `data.Asthma emergency department visits due to PM2.5_per 100,000 adults`)) + 
  scale_fill_distiller(palette = "RdPu", 
                       direction = 1,
                       na.value='transparent') +
  theme_minimal() +  # Minimal theme for the plot
  scale_size_continuous(range = c(1, 10)) +
  labs(title = "Heatmap of Asthma emergency visits", fill = "Calculated Visits")


ggplot(asthma) +
  geom_sf(aes(fill = `data.data.Asthma emergency department visits due to PM2.5_per 100,000 children`)) + 
  scale_fill_distiller(palette = "RdPu", 
                       direction = 1,
                       na.value='transparent') +
  theme_minimal() +  # Minimal theme for the plot
  scale_size_continuous(range = c(1, 10)) +
  labs(title = "Heatmap of Asthma emergency visits", fill = "Calculated Visits")

ggplot(aq_health) +
  geom_sf(aes(fill = `data.Asthma emergency department visits due to PM2.5_per 100,000 children`)) + 
  scale_fill_distiller(palette = "RdPu", 
                       direction = 1,
                       na.value='transparent') +
  theme_minimal() +  # Minimal theme for the plot
  scale_size_continuous(range = c(1, 10)) +
  labs(title = "Heatmap of Asthma emergency visits", fill = "Calculated Visits")

ggplot(aq_health) +
  geom_sf(aes(fill = `data.Respiratory hospitalizations due to PM2.5 (age 20+)_per 100,000 adults`)) + 
  scale_fill_distiller(palette = "RdPu", 
                       direction = 1,
                       na.value='transparent') +
  theme_minimal() +  # Minimal theme for the plot
  scale_size_continuous(range = c(1, 10)) +
  labs(title = "Heatmap of Respiratory hospitalizations due to PM2.5", fill = "Calculated Visits")

ggplot(aq_health) +
  geom_sf(aes(fill = `data.Cardiovascular hospitalizations due to PM2.5 (age 40+)_per 100,000 adults`)) + 
  scale_fill_distiller(palette = "RdPu", 
                       direction = 1,
                       na.value='transparent') +
  theme_minimal() +  # Minimal theme for the plot
  scale_size_continuous(range = c(1, 10)) +
  labs(title = "Heatmap of Cardiovascular hospitalizations due to PM2.5", fill = "Calculated Visits")


ggplot(aq_health) +
  geom_sf(aes(fill = `data.Deaths due to PM2.5_per 100,000 adults`)) + 
  scale_fill_distiller(palette = "RdPu", 
                       direction = 1,
                       na.value='transparent') +
  theme_minimal() +  # Minimal theme for the plot
  scale_size_continuous(range = c(1, 10)) +
  labs(title = "Heatmap of Cardiovascular hospitalizations due to PM2.5", fill = "Calculated Visits")




# From HVS



dilapidated_buildings <- occupied_households_sv %>%
  group_by(geometry) %>%
  filter(data.Condition.of.building == 2) %>%
  summarise(dilapidated_buildings = (sum(data.Condition.of.building))/2)





# How to deal with household weight?

dilapidated_buildings <- occupied_households_sv %>%
  group_by(geometry) %>%
  filter(data.Condition.of.building == 1) %>%
  summarise(count_A = sum(data.Final.household.weight))


ggplot(dilapidated_buildings) +
  geom_sf(aes(fill = `count_A`)) +  # Use fill color based on Median Rent
  scale_fill_distiller(palette = "RdPu", 
                       direction = 1,
                       na.value='transparent') +  # Minimal theme for the plot
  labs(title = "Heatmap of Persons Per Room", fill = "Persons Per Room")



# Persons Per room "General.Health..respondent"  

occupied_households_sv$data.Persons.per.room[is.na(occupied_households_sv$data.Persons.per.room)] <- 0

ggplot(occupied_households_sv) +
  geom_sf(aes(fill = `data.Persons.per.room`)) +  # Use fill color based on Median Rent
  # Add a black border around all geometries (including ones without count_A) 
  scale_fill_distiller(palette = "RdPu", 
                       direction = 1,
                       na.value='transparent') +
  theme_minimal() +  # Minimal theme for the plot
  labs(title = "Heatmap of Persons Per Room", fill = "Persons Per Room")




