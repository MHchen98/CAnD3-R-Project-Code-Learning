
# Canadian census data
# install.packages("cancensus")
library(cancensus)

set_cancensus_api_key('CensusMapper_61afa8929f72dca7bb6bdbf96d9b133f', install = TRUE)

# To view available Census datasets
list_census_datasets()

# To view available named regions at different levels of Census hierarchy for the 2016 Census (for example)
list_census_regions("CA21")

# To view available Census variables for the 2016 Census
list_census_vectors("CA16")

# find data

explore_census_vectors("CA21")

census_data <- get_census(dataset='CA16', regions=list(CMA=c("59933","35535")),
                          vectors=a$vector,
                          level='CSD', use_cache = FALSE, quiet = TRUE)
dataset='CA16'
level="CSD"
  
#### census metropolitan areas

van <- list_census_regions(dataset = "CA21") |> 
  filter(name == "Vancouver") |>  
  slice(1) |> 
  pull(1)

mtl <- list_census_regions(dataset = "CA21") |>
  filter(name == "Montréal") |> 
  slice(1) |>
  pull(1)

van_data <- get_census(dataset='CA21', 
                       regions=list(CMA=van),
                       vectors=c("v_CA21_4306", "v_CA21_4256", "v_CA21_4257", "v_CA21_386", "v_CA21_251"),
                       level='CSD', 
                       geo_format="sf", 
                       labels="short", 
                       use_cache = FALSE, 
                       quiet = TRUE)

mtl_data <- get_census(dataset='CA21', 
                       regions=list(CMA=mtl),
                       vectors=c("v_CA21_4306", "v_CA21_4256", "v_CA21_4257", "v_CA21_386", "v_CA21_251"),
                       level='CSD', 
                       geo_format="sf", 
                       labels="short", 
                       use_cache = FALSE, 
                       quiet = TRUE)

colnames(van_data)[17:21] <- c(
  "owner_households_mortgage_pct",
  "avg_rooms_per_dwelling",
  "households_by_persons_per_room",
  "avg_age",
  "year_65"
)

colnames(mtl_data)[17:21] <- c(
  "owner_households_mortgage_pct",
  "avg_rooms_per_dwelling",
  "households_by_persons_per_room",
  "avg_age",
  "year_65"
)

van_data %>%
  select(GeoUID, avg_age, owner_households_mortgage_pct) %>%
  filter(is.na(owner_households_mortgage_pct))


mtl_map_mortgage <- mtl_data |> 
  drop_na() |>
  ggplot() +
  geom_sf(mapping = aes(fill = owner_households_mortgage_pct),
          colour = "white",
          linewidth = 0.01) +
  theme_map(base_family = "Arial") + 
  scale_fill_viridis_c(option = "inferno",
                       labels = function(x) paste0(x, "%"),
                       guide = guide_colourbar(title.position = "bottom")) +
  labs(title = "Grand vancouver: Percentage of Mortgage",
       subtitle = "Data from the 2021 Canadian Census",
       fill = "% of Owner Households with a Mortgage") +
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
                       labels = function(x) paste0(x, "%"),
                       guide = guide_colourbar(title.position = "bottom")) +
  labs(title = "Grand Montréal: Percentage of Mortgage",
       subtitle = "Data from the 2021 Canadian Census",
       fill = "% of Owner Households with a Mortgage") +
  theme(legend.position = "top",
        plot.title = element_text(face = "bold"))

van_data |> 
  drop_na() |>
  select(GeoUID, owner_households_mortgage_pct) |>
  arrange(owner_households_mortgage_pct) 
# |> filter(is.na(avg_age))

van_map_age <- van_data |> 
  drop_na() |>
  ggplot() +
  geom_sf(mapping = aes(fill = avg_age),
          colour = "white",
          linewidth = 0.01) +
  theme_map(base_family = "Arial") + 
  scale_fill_viridis_c(option = "mako",
                       labels = function(x) sprintf("%.1f", x),
                       na.value = "white",
                       guide = guide_colourbar(title.position = "bottom")) +
  labs(title = "Grand Vancouver Average Age",
       subtitle = "Data from the 2021 Canadian Census",
       fill = "Average Age") +
  theme(legend.position = "bottom",
        plot.title = element_text(face = "bold"))

mtl_map_age <- mtl_data |> 
  drop_na() |>
  ggplot() +
  geom_sf(mapping = aes(fill = avg_age),
          colour = "white",
          linewidth = 0.01) +
  theme_map(base_family = "Arial") + 
  scale_fill_viridis_c(option = "mako",
                       labels = function(x) sprintf("%.1f", x),
                       guide = guide_colourbar(title.position = "bottom")) +
  labs(title = "Grand Montréal Average Age",
       subtitle = "Data from the 2021 Canadian Census",
       fill = "Average Age") +
  theme(legend.position = "bottom",
        plot.title = element_text(face = "bold"))

library(patchwork)

combined_maps <- (van_map_mortgage | van_map_age) / (mtl_map_mortgage | mtl_map_age) +
  plot_annotation(
    title = "Comparing Metro Vancouver and Metro Montreal: Housing and Aging, 2021",
    subtitle = "Regions with missing data are dropped, Data from 2021 Canada Census",
    theme = theme(plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
                  plot.subtitle = element_text(size = 12, hjust = 0.5))
  )

combined_maps


############################## Revised codes

library(ggplot2)
library(patchwork)
library(viridis)
library(ggthemes) # for theme_map
library(grid)

# Vancouver Mortgage Map
van_map_mortgage <- van_data |> 
  ggplot() +
  geom_sf(aes(fill = owner_households_mortgage_pct),
          colour = "white", linewidth = 0.01) +
  theme_map(base_family = "Arial") +
  scale_fill_viridis_c(
    option = "inferno",
    limits = c(40, 70),
    breaks = seq(40, 70, by = 5), 
    labels = function(x) paste0(x, "%"),
    na.value = "white",
    guide = guide_colourbar(title.position = "bottom")
  ) +
  labs(fill = "% of Owner Households with a Mortgage") +
  theme(legend.position = "bottom")

# Vancouver Age Map
van_map_age <- van_data |> 
  ggplot() +
  geom_sf(aes(fill = avg_age),
          colour = "white", linewidth = 0.01) +
  theme_map(base_family = "Arial") +
  scale_fill_viridis_c(
    option = "mako",
    limits = c(30, 60),
    breaks = seq(30, 60, by = 5), 
    labels = function(x) sprintf("%.0f", x),
    na.value = "white",
    guide = guide_colourbar(title.position = "bottom")
  ) +
  labs(fill = "Average Age") +
  theme(legend.position = "bottom")

# Montreal Mortgage Map
mtl_map_mortgage <- mtl_data |> 
  ggplot() +
  geom_sf(aes(fill = owner_households_mortgage_pct),
          colour = "white", linewidth = 0.01) +
  theme_map(base_family = "Arial") +
  scale_fill_viridis_c(
    option = "inferno",
    limits = c(40, 70),
    breaks = seq(40, 70, by = 5), 
    labels = function(x) paste0(x, "%"),
    na.value = "white",
    guide = guide_colourbar(title.position = "bottom")
  ) +
  labs(fill = "% of Owner Households with a Mortgage") +
  theme(legend.position = "bottom")

# Montreal Age Map
mtl_map_age <- mtl_data |> 
  ggplot() +
  geom_sf(aes(fill = avg_age),
          colour = "white", linewidth = 0.01) +
  theme_map(base_family = "Arial") +
  scale_fill_viridis_c(
    option = "mako",
    limits = c(30, 60),
    breaks = seq(30, 60, by = 5), 
    labels = function(x) sprintf("%.0f", x),
    na.value = "white",
    guide = guide_colourbar(title.position = "bottom")
  ) +
  labs(fill = "Average Age") +
  theme(legend.position = "bottom")

# Empty plots with just titles for column labels
van_label <- ggplot() + 
  theme_void() +
  labs(title = "Vancouver") +
  theme(plot.title = element_text(face = "bold", hjust = 0.5))

mtl_label <- ggplot() + 
  theme_void() +
  labs(title = "Montréal") +
  theme(plot.title = element_text(face = "bold", hjust = 0.5))

# Your 4 maps (van_map_mortgage, van_map_age, mtl_map_mortgage, mtl_map_age)
# make sure na.value = "white" is included in scale_fill_viridis_c

# Combine the column labels + maps
combined_maps <- (van_label | mtl_label) / 
  (van_map_mortgage | mtl_map_mortgage) /
  (van_map_age | mtl_map_age) +
  plot_layout(
    heights = c(0.05, 1, 1),
    guides = "collect"   # combine legends of same type
  ) +
  plot_annotation(
    title = "Comparing Metro Vancouver and Metro Montreal: Housing and Aging, 2021",
    caption = "Regions with missing data are shown in white; Data from 2021 Canada Census",
    theme = theme(
      plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
      plot.caption = element_text(size = 10, hjust = 0.5)
    )
  ) &
  theme(
    legend.position = "right",   # move legend to the right
    legend.direction = "vertical", # stack vertically
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 12) 
  )

ggsave(
  filename = "combined_maps.png",  # file name
  plot = combined_maps,            # your plot object
  width = 12,                      # width of the image
  height = 8,                      # height of the image
  units = "in",                    # units: "in", "cm", "mm"
  dpi = 300                        # resolution in dots per inch
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


