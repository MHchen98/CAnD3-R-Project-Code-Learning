
# Canadian census data
# install.packages("cancensus")
library(cancensus)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggthemes)
library(stringr)
library(scales)
library(sf)

set_cancensus_api_key('CensusMapper_61afa8929f72dca7bb6bdbf96d9b133f', install = TRUE)

# To view available Census datasets
list_census_datasets()

# To view available named regions at different levels of Census hierarchy for the 2016 Census (for example)
list_census_regions("CA16")

# find data

explore_census_vectors("CA16")

census_data <- get_census(dataset='CA16', regions=list(CMA=c("59933","35535")),
                          vectors=a$vector,
                          level='CSD', use_cache = FALSE, quiet = TRUE)
dataset='CA16'
level="CSD"
  
#### census metropolitan areas

van <- list_census_regions(dataset = "CA16") |> 
  filter(name == "Vancouver") |>  
  slice(1) |> 
  pull(1)

mtl <- list_census_regions(dataset = "CA16") |>
  filter(name == "Montréal") |> 
  slice(1) |>
  pull(1)

tnt <- list_census_regions(dataset = "CA16") |>
  filter(name == "Toronto") |> 
  slice(1) |>
  pull(1)

orig_ids   <- c("v_CA16_4891")
orig_names <- c("owner_households_mortgage_pct")



van_data <- get_census(
  dataset    = "CA16",
  regions    = list(CMA = van),
  vectors    = c(orig_ids),   # original first, then appended age vars
  level      = "CSD",
  geo_format = "sf",
  labels     = "short",
  use_cache  = FALSE,
  quiet      = TRUE
)

colnames(van_data)[14] <-
  c(orig_names)

mtl_data <- get_census(
  dataset    = "CA16",
  regions    = list(CMA = mtl),
  vectors    = c(orig_ids),   # original first, then appended age vars
  level      = "CSD",
  geo_format = "sf",
  labels     = "short",
  use_cache  = FALSE,
  quiet      = TRUE
)

colnames(mtl_data)[14] <-
  c(orig_names)

tnt_data <- get_census(
  dataset    = "CA16",
  regions    = list(CMA = tnt),
  vectors    = c(orig_ids),   # original first, then appended age vars
  level      = "CSD",
  geo_format = "sf",
  labels     = "short",
  use_cache  = FALSE,
  quiet      = TRUE
)

colnames(tnt_data)[14] <-
  c(orig_names)

van_map_mortgage <- van_data |> 
  drop_na() |>
  ggplot() +
  geom_sf(mapping = aes(fill = owner_households_mortgage_pct),
          colour = "white",
          linewidth = 0.01) +
  theme_map(base_family = "Arial") + 
  scale_fill_viridis_c(option = "mako", 
                       limits = c(30, 70),
                       breaks = seq(30, 70, by = 20), 
                       name = NULL,
                       labels = function(x) paste0(x, "%"),
                       guide = guide_colourbar(title.position = "bottom")) +
  labs(title = "Vancouver") +
  theme(legend.position = "top",
        plot.title = element_text(face = "bold"))

mtl_map_mortgage <- mtl_data |> 
  drop_na() |>
  ggplot() +
  geom_sf(mapping = aes(fill = owner_households_mortgage_pct),
          colour = "white",
          linewidth = 0.01) +
  theme_map(base_family = "Arial") + 
  scale_fill_viridis_c(option = "inferno", 
                       limits = c(30, 90),
                       breaks = seq(30, 90, by = 20), 
                       name = NULL,
                       labels = function(x) paste0(x, "%"),
                       guide = guide_colourbar(title.position = "bottom")) +
  labs(title = "Montreal") +
  theme(legend.position = "top",
        plot.title = element_text(face = "bold"))

tnt_map_mortgage <- tnt_data |> 
  drop_na() |>
  ggplot() +
  geom_sf(mapping = aes(fill = owner_households_mortgage_pct),
          colour = "white",
          linewidth = 0.01) +
  theme_map(base_family = "Arial") + 
  scale_fill_viridis_c(option = "viridis",
                       limits = c(30, 80),
                       breaks = seq(30, 90, by = 20), 
                       name = NULL,
                       labels = function(x) paste0(x, "%"),
                       guide = guide_colourbar(title.position = "bottom")) +
  labs(title = "Toronto") +
  theme(legend.position = "top",
        plot.title = element_text(face = "bold"))

# van_map_age <- van_data |> 
#   drop_na() |>
#   ggplot() +
#   geom_sf(mapping = aes(fill = avg_age),
#           colour = "white",
#           linewidth = 0.01) +
#   theme_map(base_family = "Arial") + 
#   scale_fill_viridis_c(option = "mako",
#                        labels = function(x) sprintf("%.1f", x),
#                        na.value = "white",
#                        guide = guide_colourbar(title.position = "bottom")) +
#   labs(title = "Grand Vancouver Average Age",
#        subtitle = "Data from the 2016 Canadian Census",
#        fill = "Average Age") +
#   theme(legend.position = "bottom",
#         plot.title = element_text(face = "bold"))

# mtl_map_age <- mtl_data |> 
#   drop_na() |>
#   ggplot() +
#   geom_sf(mapping = aes(fill = avg_age),
#           colour = "white",
#           linewidth = 0.01) +
#   theme_map(base_family = "Arial") + 
#   scale_fill_viridis_c(option = "mako",
#                        labels = function(x) sprintf("%.1f", x),
#                        guide = guide_colourbar(title.position = "bottom")) +
#   labs(title = "Grand Montréal Average Age",
#        subtitle = "Data from the 2016 Canadian Census",
#        fill = "Average Age") +
#   theme(legend.position = "bottom",
#         plot.title = element_text(face = "bold"))

library(patchwork)

combined_maps <- (van_map_mortgage | mtl_map_mortgage | tnt_map_mortgage) +
  plot_annotation(
    title = "Comparing Vancouver, Montreal and Toronto: Percentage of Mortgage, 2016",
    subtitle = "Regions with missing data are dropped, Data from 2016 Canada Census",
    theme = theme(plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
                  plot.subtitle = element_text(size = 12, hjust = 0.5))
  )

ggsave(
  filename = "combined_maps.jpg",   # or "combined_maps.jpeg"
  plot = combined_maps,             # your patchwork object
  width = 12,                       # width in inches
  height = 6,                       # height in inches
  units = "in",                     # can also use "cm"
  dpi = 300,                        # resolution: 300 is print quality
  device = "jpeg"                   # explicitly set JPEG format
)

######## Age-sex pct of mortgage using PUMF 2016

setwd(getwd())

housing <- read.csv("/Users/mhc/CAnD3-R-Project-Code-Learning/RRWM/data/Census 2016/pumf-98M0001-E-2016-individuals_F1.csv")

library(dplyr)

var <- c("AGEGRP", "MarStH", "Sex", "Wages", "PresMortG", "Tenur", "HDGREE", "PR", "CMA")
df <- housing[var]

df <- df %>%
  mutate(agegrp_5 = ifelse(AGEGRP == 88, NA, AGEGRP)) %>%
  mutate(agegrp_5 = case_when(
    agegrp_5 %in% c(6, 7) ~ 1,   # 15–20
    agegrp_5 == 8        ~ 2,   # 21–25
    agegrp_5 == 9        ~ 3,   # 26–30
    agegrp_5 == 10       ~ 4,   # 31–35
    agegrp_5 == 11       ~ 5,   # 36–40
    agegrp_5 == 12       ~ 6,   # 41–45
    agegrp_5 == 13       ~ 7,   # 46–50
    agegrp_5 == 14       ~ 8,   # 51–55
    agegrp_5 == 15       ~ 9,   # 56–60
    agegrp_5 == 16       ~ 10,  # 61–65
    agegrp_5 == 17       ~ 11,  # 66–70
    agegrp_5 == 18       ~ 12,  # 71–75
    agegrp_5 == 19       ~ 13,  # 76–80
    agegrp_5 == 20       ~ 14,  # 81–85
    agegrp_5 >= 21       ~ 15   # 85+
  )) %>%                      # label variable values
  mutate(agegrp_5 = factor(
    agegrp_5,
    levels = 1:15,
    labels = c(
      "15–20", "21–25", "26–30", "31–35", "36–40",
      "41–45", "46–50", "51–55", "56–60", "61–65",
      "66–70", "71–75", "76–80", "81–85", "85+"
    )
  ))

df <- df %>%
  mutate(tenure = ifelse(Tenur %in% c(8,9), NA, Tenur)) %>%
  mutate(tenure = case_when(
    tenure == 1 ~ 1, # ownership
    tenure == 2 ~ 0
  )) 
  
df<- df %>%
  filter(CMA %in% c(933, 535, 462)) %>%
  mutate(
    city = case_when(
      CMA == 933 ~ "Vancouver",
      CMA == 535 ~ "Toronto",
      CMA == 462 ~ "Montreal",
      TRUE ~ NA_character_
    )
  )

# 0) Make sure city order is Vancouver, Montreal, Toronto
selected_cities <- df %>%
  mutate(
    city = factor(city, levels = c("Vancouver", "Montreal", "Toronto")),
    sex = case_when(
      is.numeric(Sex) & Sex == 1 ~ "Male",
      is.numeric(Sex) & Sex == 2 ~ "Female",
      as.character(Sex) %in% c("M","Male","male") ~ "Male",
      as.character(Sex) %in% c("F","Female","female") ~ "Female",
      TRUE ~ NA_character_
    )
  )


# 1. Compute the tenure rate (PresMortG = 1)
tenure_rates <- selected_cities %>%  
  filter(agegrp_5 != "15–20") %>%
  filter(!is.na(tenure), !is.na(agegrp_5), !is.na(sex), !is.na(city)) %>%
  group_by(city, sex, agegrp_5) %>%
  summarise(
    n = n(),
    tenure_rate = mean(tenure == 1, na.rm = TRUE),
    .groups = "drop"
  )

# 2. Plot: one facet per city, red = female, blue = male
combined_lines <- ggplot(tenure_rates,
       aes(x = agegrp_5, y = tenure_rate, color = sex, group = sex)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  scale_color_manual(values = c(Female = "red", Male = "blue"), name = NULL) +
  scale_y_continuous(labels = label_percent(accuracy = 0.1)) +
  labs(
    title = "Age-Group-Specific Housing Tenure Rate by Sex, 2016 Public Use Micro Files Data ",
    subtitle = "Three major CMAs: Vancouver, Montreal, Toronto",
    x = "Age group",
    y = "Proportion of Tenure"
  ) +
  facet_wrap(~ city, ncol = 3) +
  theme_minimal(base_family = "Arial") +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "top"
  )

ggsave(
  filename = "combined_lines.jpg",   # or "combined_maps.jpeg"
  plot = combined_lines,             # your patchwork object
  width = 12,                       # width in inches
  height = 6,                       # height in inches
  units = "in",                     # can also use "cm"
  dpi = 300,                        # resolution: 300 is print quality
  device = "jpeg"                   # explicitly set JPEG format
)



###### US census data

library(tidycensus)
library(tidyverse)
library(dplyr)
library(tigris)    # for state shapefiles
library(sf)        # spatial data support


census_api_key("27a5677e4cdce04a03d1ecc5f788caf27362f559", install = TRUE)

# variables codebook: https://api.census.gov/data.html

pums_vars_2023 <- pums_variables %>% 
  filter(year == 2023, survey == "acs5")
pums_vars_2023 %>% 
  distinct(var_code, var_label, data_type, level)

hearing_pums <- get_pums(
  variables = c("PUMA", "STATE", "SEX", "AGEP", "SCHL", "DEAR"),
  state = "all", 
  survey = "acs5",
  year = 2023
)

write.csv(hearing_pums, "hearing_pums.csv", row.names = FALSE)


# Recode hearing difficulty into numeric 1/0
hearing_pums <- hearing_pums %>%
  mutate(
    hearing_difficulty = ifelse(DEAR == "1", 1, 0)  # 1 = Yes, 0 = No
  )

# Calculate percentage by state
state_pct <- hearing_pums %>%
  group_by(STATE) %>%
  summarise(
    pct_with_difficulty = mean(hearing_difficulty, na.rm = TRUE) * 100
  )

head(state_pct)

states_sf <- states(cb = TRUE, year = 2023) %>%
  filter(!STUSPS %in% c("PR", "VI", "GU", "MP", "AS")) # drop territories

# Step 4: Merge summary data with shapefile
# STATE in PUMS is numeric FIPS code; GEOID in shapefile is also FIPS
states_merged <- states_sf %>%
  left_join(hearing_summary, by = c("STATEFP" = "STATE"))

# Get US states shapefile and shift Alaska & Hawaii
states <- states(cb = TRUE)
states_shifted <- shift_geometry(states)

# Merge with your percentage data
states_merged <- states_shifted %>%
  left_join(state_pct, by = c("GEOID" = "STATE"))

# Plot
ggplot(states_merged) +
  geom_sf(aes(fill = pct_with_difficulty), color = "white") +
  scale_fill_viridis_c(
    name = "Hearing Difficulty",
    option = "plasma",
    labels = scales::percent_format(scale = 1)
  ) +
  labs(
    title = "Percentage of People with Hearing Difficulty by State (ACS 2023)",
    caption = "Source: IPUMS / Census ACS 2023"
  ) +
  coord_sf(
    xlim = c(-125, -66),  # continental U.S. longitudes
    ylim = c(24, 50),     # continental U.S. latitudes
    expand = FALSE
  ) +
  theme_void() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    plot.caption = element_text(hjust = 0.5, size = 10)
  )


