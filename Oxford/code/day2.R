# ------------------------------------------------------------------------------

# Day 2: Maps and Interactivity
# PopAging DataViz—UK
# Companion Script File
# SPDX-License-Identifier: MIT
# Copyright (c) 2025 Sakeef M. Karim

# PRELIMINARIES ----------------------------------------------------------------

library(systemfonts)
library(tidyverse)

# Mapping in ggplot2

library(sf)
library(sp)
library(terra)
library(ggspatial)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggthemes)
library(tidygeocoder)
library(geojsonsf)
library(tidycensus)
library(cancensus)
library(mapcan)
library(mapview)
library(leaflet)
library(leafpop)

# Animations

library(plotly)
library(gganimate)
# A helpful library for animations—but not used in this workshop!
#library(ggiraph)

# Misc

library(colorspace)
library(patchwork)
library(see)
library(scales)
library(ggrepel)
library(gt)

# LOADING THE DATA -------------------------------------------------------------

load("./data/day2.RData")

# BASIC MAPS -------------------------------------------------------------------

# A Map of the World -----------------------------------------------------------

# Using rnaturalearth to generate a map of the world in seconds:

ne_countries(scale = "medium",
             returnclass = "sf") |> 
# Removing Antarctica from the map ... 
filter(!name == "Antarctica") |>  
ggplot() + 
# The workhorse geom (via sf) to produce maps -- but not our only option:
geom_sf() +
theme_map(base_family = "IBM Plex Sans") +
labs(title = "A Map of the World")

# We can easily adjust or experiment with different map projections:

ne_countries(scale = "medium",
             returnclass = "sf") |>  
filter(!name == "Antarctica") |> 
ggplot() + 
geom_sf() +
# Changing the coordinate system
# Here, the popular Robinson projection:
coord_sf(crs = st_crs("ESRI:53030")) +
# More projections can be found here:
# https://en.wikipedia.org/wiki/List_of_map_projections
# Search for details here: https://epsg.io/?q=
# Example: orthographic, north pole
#  coord_sf(crs = st_crs("ESRI:102035")) +
theme_map(base_family = "IBM Plex Sans") +
labs(title = "A Map of the World")

# INTRODUCING STATS ------------------------------------------------------------

# Like any other geom, we can fill in our maps based on some variables 
# embedded in our input data frame!

map_data |> 
filter(!name == "Antarctica",
       # For simplicity, isolating 2015:
       year == 2015) |>  
ggplot() + 
geom_sf(colour = "white",
        linewidth = 0.1,
        # Filling in data as a function of TFR (percentiles):
        mapping = aes(fill = ntile(fertility_rate, 100))) +
# Creating our own gradient scale:
scale_fill_gradient2(low = muted("pink"), 
                     high = muted("red")) +
coord_sf(crs = st_crs("ESRI:53030")) +
theme_map(base_family = "IBM Plex Sans") +
labs(title = "A Map of the World",
     fill = "Fertility Rate in 2015 (Percentile)") +
theme(legend.position = "bottom",
      legend.justification = "center") +
guides(fill = guide_colourbar(title.position = "top"))
                                
# Quick Exercise ---------------------------------------------------------------

# Play around with some of the indicators in the data. These include, but are
# not limited to ...:

# year, age_dependency (old age dependency), fertility_rate, death_rate,
# net_migration, foreign_share

# Create some new maps using some of the skills you picked up in Day 1:

# Scroll down for an example ...


map_data |> 
  filter(!name == "Antarctica",
         # For simplicity, isolating 2015:
         year == 2015) |>  
  ggplot() + 
  geom_sf(colour = "white",
          linewidth = 0.1,
          # Filling in data as a function of TFR (percentiles):
          mapping = aes(fill = ntile(fertility_rate, 100))) +
  # Creating our own gradient scale:
  scale_fill_gradient2(low = muted("pink"), 
                       high = muted("red")) +
  coord_sf(crs = st_crs("ESRI:53030")) +
  theme_map(base_family = "IBM Plex Sans") +
  labs(title = "A Map of the World",
       fill = "Fertility Rate in 2015 (Percentile)") +
  theme(legend.position = "bottom",
        legend.justification = "center") +
  guides(fill = guide_colourbar(title.position = "top"))






map_data |> 
filter(!name == "Antarctica", 
       !income_grp == "2. High income: nonOECD", 
       year == 2015) |>  
ggplot() + 
geom_sf(colour = "white",
        linewidth = 0.05,
        mapping = aes(fill = age_dependency)) +
scale_fill_viridis_c(option = "magma", direction = -1, na.value = "grey") +
facet_wrap(~income_grp, ncol = 2) +
coord_sf(crs = st_crs("ESRI:53030")) +
labs(title = "Fertility Rate by Income Around the World",
     subtitle = "Year 2015",
     fill = "fertility Rate") +
theme(plot.title = element_text(size = 15,
                                face = "bold", hjust=0.5),
      plot.subtitle = element_text(hjust = 0.5),
      strip.text = element_text(size = 10),
      legend.position = "bottom",
      legend.justification = "right") +
guides(fill = guide_colourbar(title.position = "top"))


map_dup <- map_data %>%
  filter(!name == "Antarctica", 
         !income_grp == "2. High income: nonOECD",
         year == 2015) %>%
  tidyr::crossing(facet_group = unique(map_data$income_grp)) %>%
  mutate(
    fill_value = ifelse(income_grp == facet_group, age_dependency, NA),
    outside = income_grp != facet_group
  ) %>%
  st_as_sf()   # <-- convert back to sf

# Split data
outside_data <- map_dup %>% filter(outside)
inside_data  <- map_dup %>% filter(!outside)

# Plot
ggplot() +
  geom_sf(data = outside_data, fill = "grey", colour = "white", linewidth = 0.05) +
  geom_sf(data = inside_data, aes(fill = fill_value), colour = "white", linewidth = 0.05) +
  scale_fill_viridis_c(option = "magma", direction = -1, na.value = NA) +
  facet_wrap(~facet_group, ncol = 2) +
  coord_sf(crs = st_crs("ESRI:53030")) +
  labs(title = "Fertility Rate by Income Around the World",
       subtitle = "Year 2015",
       fill = "Fertility Rate") +
  theme(
    plot.title = element_text(size = 15, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    strip.text = element_text(size = 10),
    legend.position = "bottom",
    legend.justification = "right"
  ) +
  guides(fill = guide_colourbar(title.position = "top"))



map_data %>%
  filter(is.na(age_dependency)) %>%
  select(year, age_dependency, name)


# A Brief Detour ----------------------------------------------------------

# Patchwork ---------------------------------------------------------------

# Here's a fun way to sneak in a new package --- patchwork. Thanks to patchwork
# (and packages like ggpubr), we can easily combine plots.

# Below, we use the brilliant patchwork library to bring two distinct plots
# together in a single graphic. 

# In the spirit of simplicity, this exercise draws on map_data to
# (once again) analyse the uneven distribution of mortality around the world.

mortality_2015_robinson <-  map_data |> 
                            # Isolating 2015, removing Antarctica:
                            filter(year == 2015, 
                                   !name == "Antarctica") |> 
                            ggplot() + 
                            geom_sf(colour = "white",
                                    linewidth = 0.05,
                                    mapping = aes(fill = death_rate)) +
                            scale_fill_viridis_c(option = "magma", 
                                                 direction = -1) +
                            # Robinson Projection
                            coord_sf(crs = st_crs("ESRI:53030")) +
                            theme_map(base_family = "IBM Plex Sans") +
                            labs(fill = "Death Rate (per 1000)") +
                            guides(fill = guide_colourbar(title.position = "top"))

mortality_2015_orthographic <- mortality_2015_robinson + 
                               # Orthographic projection
                               coord_sf(crs = st_crs("ESRI:102035"))

two_maps <- mortality_2015_robinson + 
            # Adding the two maps together:
            mortality_2015_orthographic + 
            # Adding common titles, subtitles:
            plot_annotation(title = "Mortality Around the World in 2015",
                            subtitle = "Per World Bank Estimates") +
            # "Collecting" legends to avoid duplication:
            plot_layout(guides = "collect") &
            # Adjusting base themes:
            theme(legend.position = "bottom",
                  text = element_text(family = "IBM Plex Sans"),
                  plot.title = element_text(face = "bold"))

# If plots are stored as objects, we need to explicitly call 
# them to generate a preview:

two_maps

# Adding a Table ----------------------------------------------------------

# Tables can be artfully woven into our data visualisations to tell a more
# holistic story. Consider a (relatively nonsensical) example where we:
  
# 1. Generate a quick table using the wonderful gt package.

# 2. Use patchwork to add this table to our combined plot from above!

# The table

mortality_table <- map_data |> 
                   filter(year == 2015) |> 
                   as_tibble() |> 
                   select(Country = admin, death_rate) |>
                   # Ten lowest death rates in 2015
                   slice_min(n = 10, death_rate) |> 
                   gt() |> 
                   # Changing label for second column
                   cols_label(2 ~ "Crude Mortality Rate") |> 
                   # Retaining two decimals
                   fmt_number(decimals = 2) |>  
                   # Column alignment
                   cols_align(align = "center",
                              columns = everything()) |> 
                   # Font
                   opt_table_font(font = "IBM Plex Sans") |> 
                   # Title
                   tab_header(title = "Lowest Death Rates in the World (2015)") |>       
                   # Reducing font size
                   tab_options(table.font.size = px(10))

# Stitching maps, table together:

two_maps +
wrap_table(mortality_table, 
           # Matches dimensions
           space = "fixed")  

# Zooming in on Canada ---------------------------------------------------------

# Data comes from the mapcan package:

canada_map <- province_territories_2017 |> 
ggplot(aes(x = long, y = lat, group = group)) +
# Notice something?
geom_polygon(mapping = aes(fill = population),
             colour = "white",
             linewidth = 0.2) +
# Why'd we switch things up?
coord_sf() +
theme_map(base_family = "IBM Plex Sans") +
theme(legend.position = "top") +
scale_fill_viridis_c(labels = function(x) paste((x)/1000000, "mil"),
                     direction = -1) +
labs(fill = "Canadian Population in 2017") +
guides(fill = guide_colorbar(title.position = "bottom")) +
theme(legend.key.width = unit(0.85, "cm"))

terr_table <- province_territories_2017 %>%
  distinct(pr_english, .keep_all = TRUE) |> 
  as_tibble() |> 
  select(group, pr_english, population) |>
  # Ten lowest death rates in 2015
  slice_min(n = 5, population) |> 
  gt() |> 
  # Changing label for second column
  cols_label(2 ~ "Population") |> 
  # Retaining two decimals
  fmt_number(decimals = 0) |>  
  # Column alignment
  cols_align(align = "center",
             columns = everything()) |> 
  # Font
  opt_table_font(font = "IBM Plex Sans") |> 
  # Title
  tab_header(title = "Lowest Population in Canada (2017)") |>       
  # Reducing font size
  tab_options(table.font.size = px(10))


canada_map +
  wrap_table(terr_table, 
             # Matches dimensions
             panel = "rows",
             space = "free")  


                       
# Zooming in on the UK ---------------------------------------------------------

# Data on ethnicity (non-white share) in London at the Lower Super Output Area

# See london_sf

# A custom, gradient colour palette:

custom_pal <- c("#edf8fb", "#bfd3e6", "#9ebcda",
                "#8c96c6", "#8c6bb1", "#88419d", "#6e016b")

london_sf |> 
# For LSOAs with missing data, essentially ...
ggplot() +
geom_sf(aes(fill = non_white), colour = "white",
        linewidth = 0.009) +
facet_wrap(~year) +
scale_fill_gradientn(# Note the inclusion of a custom palette here:
                     colours = custom_pal,
                     # Fill colour for LSOAs with missing data:
                     na.value = "grey80",
                     labels =  function(x) paste0(x * 100, "%")) +
theme_map(base_family = "IBM Plex Sans") +
labs(title = "Spatial Distribution of Non-White Londoners, 2011-2021",
     subtitle = "Data from the 2011 and 2021 UK Census",
     fill = "Non-White Share") +
theme(plot.title = element_text(face = "bold", size = 15),
      plot.subtitle = element_text(colour = "grey50", size = 12),
      strip.text = element_text(size = 11),
      legend.position = "bottom",
      legend.title.position = "top",
      legend.key.width = unit(1, "cm"),
      legend.justification = "centre",
      legend.title = element_text(size = 12, hjust = 0.5),
      legend.text = element_text(size = 11)) 


# Zooming in even more/geocoding -----------------------------------------------

# Creating some data:

locations <- tibble(site = c("St Hilda's College",
                             "McGill University"),
                    address = c("Cowley Pl, Oxford OX4 1DY",
                                "McGill University")) |> 
             geocode(address = address, 
                     method = "arcgis")

locations

# Oxford (Point) -----------------------------------------------------------

# Here, we mark St Hilda's College using geom_point()

oxfordshire_sf |>  
ggplot() +
geom_sf() +
# Notice us isolating the first row via the slice function:
geom_point(data = locations |> slice(1),
           mapping = aes(x = long, y = lat)) +
geom_label_repel(data = locations |> slice(1),
                 mapping = aes(x = long,
                               y = lat,
                               label = site))

# Adding some Oxford-specific colours:

oxfordshire_sf |>  
ggplot() +
# Adding Oxford Blue:
geom_sf(fill = "#002147", colour = "white") +
# And "Oxford Green"
geom_point(colour = "#426A5A",
           size = 3.5,
           data = locations |> slice(1),
           mapping = aes(x = long, y = lat)) +
geom_label_repel(family = "Inconsolata",
                 fill = "#426A5A",
                 colour = "white",
                 size = 4.5,
                 nudge_y = 0.02,
                 data = locations |>  slice(1),
                 mapping = aes(x = long, 
                               y = lat,
                               label = site)) +
theme_void(base_family = "Inconsolata") +
labs(title = "Oxfordshire, England")  

# Montréal (Exercise) ----------------------------------------------------------

# canada_cd from Monica Alexander ...

mtl_sf <- canada_cd |>  
       ggplot() +
       geom_sf(colour = "grey")  + 
       theme_void() +
       coord_sf(xlim = c(-75, -73),
                ylim = c(45, 47))

mtl_sf +  
  geom_point(data = locations |> slice(2),
             mapping = aes(x = long, y = lat)) +
  geom_label_repel(data = locations |> slice(2),
                   mapping = aes(x = long,
                                 y = lat,
                                 label = site))


# How can we highlight McGill University (see above for some guidance ---
# or scroll down for the answer!)

mtl +
geom_point(colour = "#ed1b2f",
           size = 3.5,
           data = locations |> slice(2),
           mapping = aes(x = long, y = lat)) +
geom_label_repel(family = "IBM Plex Sans",
                 fill = "#ed1b2f",
                 colour = "white",
                 nudge_y = 0.15,
                 data = locations |> slice(2),
                 mapping = aes(x = long, 
                               y = lat,
                               label = site))






# TIDYCENSUS -------------------------------------------------------------------

census_api_key("YOUR CENSUS KEY")

# Fetching data from the American Community Survey

# Variables in 2023:

acs_2023 <- load_variables(2023, "acs5")

# Searching for ... non-white share:

acs_2023 |> filter(concept == "Race",
                   str_detect(label, "Whit|Tota"))


# Leon/Duval County  -----------------------------------------------------------

leon_county <- get_acs(state = "FL",
                       county = "Leon County",
                       geography = "tract",
                       variables = "B02001_002",
                       summary_var = "B02001_001",
                       geometry = TRUE,
                       year = 2023)  |> mutate(non_white_share = 1 - 
                                               estimate/summary_est)

duval_county <- get_acs(state = "FL",
                        county = "Duval County",
                        geography = "tract",
                        variables = "B02001_002",
                        summary_var = "B02001_001",
                        geometry = TRUE,
                        year = 2023)  |> mutate(non_white_share = 1 - 
                                                estimate/summary_est)

# The maps: Leon

# This is a map that indicating tracts where the non-white share 
# is lower than 25% or higher than 75% 
library(ggrepel)

# Project to planar CRS
leon_proj <- st_transform(leon_county, crs = 3857)

# Compute a point on surface for each tract
leon_labels <- leon_proj
st_geometry(leon_labels) <- st_point_on_surface(leon_proj$geometry)

# Only keep labels <25% or >75%
leon_labels <- leon_labels |>
  filter(non_white_share < 0.25 | non_white_share > 0.75)

# Create label column
leon_labels$label <- paste0(round(leon_labels$non_white_share * 100), "%")

# Extract coordinates for labels
coords <- st_coordinates(leon_labels)

# Plot
ggplot(leon_proj) +
  geom_sf(aes(fill = non_white_share), colour = "white", linewidth = 0.2) +
  geom_label_repel(
    data = leon_labels,
    aes(x = coords[,1], y = coords[,2], label = label),
    colour = ifelse(leon_labels$non_white_share < 0.25, "red", "white"),
    fill = "black",
    segment.color = "grey50",
    size = 3,
    min.segment.length = 0,
    max.overlaps = Inf   # allow all labels to be attempted
  ) +
  scale_fill_viridis_c(labels = function(x) paste0(x * 100, "%"), limits = c(0, 1)) +
  theme_minimal() +
  labs(fill = "Non-White Share", title = "Leon County")



leon_county |> 
ggplot() +
geom_sf(mapping = aes(fill = non_white_share), 
        colour = "white") +
theme_map(base_family = "IBM Plex Sans") +
scale_fill_viridis_c(guide = guide_legend(),
                     labels = function(x) paste0(x * 100, "%")) +
labs(fill = "Non-White Share", 
     title = "Leon County") +   
theme(legend.position = "bottom",
      plot.title = element_text(size = 14),
      legend.title = element_text(size = 12),
      legend.text = element_text(size = 11))

# The maps: Duval

duval_county |> ggplot() +
geom_sf(mapping = aes(fill = non_white_share), 
        colour = "white") +
theme_map(base_family = "Inconsolata") +
scale_fill_viridis_c(option = "magma",
                     labels = function(x) paste0(x * 100, "%")) +
labs(title = "Duval County",
     fill = "Non-White Share") +
theme(legend.position = "bottom",
      plot.title = element_text(size = 14),
      legend.title = element_text(size = 12),
      legend.text = element_text(size = 11))


# Can you adjust/beautify these plots? Spend 5-10 minutes exploring different
# possibilities. 

# CANCENSUS --------------------------------------------------------------------

# Montreal  --------------------------------------------------------------------

set_cancensus_api_key('YOUR KEY', install = TRUE)

# Interactively find variable(s) of interest:

explore_census_vectors(dataset = "CA21")

# Want to explore total non-white share in MTL ...

# Extracting MTL's census code:

mtl <- list_census_regions(dataset = "CA21") |> 
       filter(name == "Montréal") |>  
       slice(1) |> 
       pull(1)

mtl_data <- get_census(dataset = "CA21",
                       regions = list(CMA = mtl),
                       vectors = "v_CA21_4875",
                       level = "CT",
                       geo_format = "sf",
                       labels = "short")

# The map

mtl_data |> 
ggplot() +
geom_sf(mapping = aes(fill = vm_share),
        colour = "white",
        linewidth = 0.01) +
theme_map(base_family = "Inconsolata") + 
scale_fill_viridis_c(option = "inferno",
                     labels = function(x) paste0(x * 100, "%"),
                     guide = guide_colourbar(title.position = "bottom")) +
labs(title = "Grand Montréal",
     subtitle = "Data from the 2021 Canadian Census",
     fill = "Visible Minority Share") +
theme(legend.position = "top",
      plot.title = element_text(face = "bold"))

## Vancouver

van <- list_census_regions(dataset = "CA21") |> 
  filter(name == "Vancouver") |>  
  slice(1) |> 
  pull(1)

van_data <- get_census(dataset='CA21', 
                       regions=list(CMA=van),
                       vectors=c("v_CA21_4306", "v_CA21_4256", "v_CA21_4257"),
                       level='CSD', 
                       geo_format="sf", 
                       labels="short", 
                       use_cache = FALSE, 
                       quiet = TRUE)

colnames(van_data)[17:19] <- c(
  "owner_households_mortgage_pct",
  "avg_rooms_per_dwelling",
  "households_by_persons_per_room"
)


van_data |> 
  drop_na() |>
  ggplot() +
  geom_sf(mapping = aes(fill = owner_households_mortgage_pct),
          colour = "white",
          linewidth = 0.01) +
  theme_map(base_family = "Inconsolata") + 
  scale_fill_viridis_c(option = "inferno",
                       labels = function(x) paste0(x, "%"),
                       guide = guide_colourbar(title.position = "bottom")) +
  labs(title = "Grand Vancouver",
       subtitle = "Data from the 2021 Canadian Census",
       fill = "% of Owner Households with a Mortgage") +
  theme(legend.position = "top",
        plot.title = element_text(face = "bold"))

# Feel free to adjust the plot to your liking --- and find other indicators to
# visualise using cancensus OR tidycensus.

# INTERACTIVE MAPS -------------------------------------------------------------

# leaflet ----------------------------------------------------------------------

# Where workshop participants/Fellows were born---------------------------------

born_data

continent_palette <- colorFactor(palette = c("dodgerblue", "red", 
                                             "orange", "black", "purple"),
                                 # Corresponding to levels of the 
                                 # continent variable:
                                 domain = born_data$continent)

leaflet(born_data) |>
addProviderTiles(providers$CartoDB.DarkMatter) |> 
addProviderTiles(providers$CartoDB.Voyager) |> 
addCircleMarkers(lng = born_data$long, 
                 lat = born_data$lat,
                 fillOpacity = 0.5,
                 weight = 10,
                 radius = ~ sqrt(n) * 8,
                 color = ~continent_palette(continent),
                 stroke = FALSE) |> 
setView(lng= 90.4152, lat=23.8041, zoom = 5)

# mapview ----------------------------------------------------------------------

mapView(oxfordshire_sf, color = "white", 
        col.regions = "#446f85",
        layer.name = "Oxfordshire")

# Some interactivity

# Generating coordinates for Oxford:

uoxford <- tibble(site = "University of Oxford",
                  address = "Wellington Square, Oxford OX1 2JD") |>  
            geocode(address = address)

# Ensuring that our data is in a "simple features" format:

uoxford_sf <- st_as_sf(uoxford, coords = c("long", "lat"),
                       # WGS84 projection:
                       crs = 4326)

# Here's the popup:

mapView(uoxford_sf, 
        color = "white", 
        col.regions = "#002147",
        layer.name = "University of Oxford",
        # Size of point:
        cex = 25,
        popup = popupIframe("https://www.youtube.com/embed/lLC75gPigTQ?si=oRqbTPhEuPMZmKQ3",
                            width = 550, height = 300))


# popupIframe function comes from
# https://rdrr.io/github/r-spatial/leafpop/src/R/graph.R


# MORE INTERACTIVITY -----------------------------------------------------------


# plotly -----------------------------------------------------------------------

old_age_dependency  <- ggplot(data = select_countries,
                              aes(x = year, 
                                  y = age_dependency, 
                                 colour = country)) +
                       geom_line() +
                       scale_colour_colorblind() +
                       theme_bw(base_family = "Inconsolata") +
                       labs(colour = "", 
                            x = "", 
                            y = "Old Age Dependency")
           
select_countries_modified <- select_countries |> 
                             # Creating a text "tooltip" for our "hover label"
                             mutate(tooltip = paste(" Country:", 
                                                    country, "<br>", 
                                                    "Year:", year, "<br>",
                                                    "Old Age Depndency:",
                                                     round(age_dependency, 2)))

old_age_dependency_new  <- ggplot(data = select_countries_modified,
                                  aes(x = year, y = age_dependency, 
                                      text = tooltip,
                                      group = country,
                                      colour = country)) +
                           geom_line() +
                           scale_colour_colorblind() +
                           theme_bw(base_family = "Inconsolata") +
                           labs(colour = "", 
                                x = "", 
                                y = "Old Age Dependency") 

# Adjusting the "hover label" and legend:

ggplotly(old_age_dependency_new,
         tooltip = "text") |> 

# Adjusting the "hover label" and legend:
        # Changes font of tooltip:
layout(hoverlabel = list(font = list(family = "Inconsolata")),
       # Changes legend location
       legend = list(x = 0,
                     y = 1,
                     traceorder = "normal"))

# htmltools::save_html(STORED_OBJECT_HERE, "interactive_plot.html")

# Use the select_countries data frame + plotly to easily produce some 
# interactive plots of your own (in the next 10-15 mintutes or so).

old_age_dependency  <- ggplot(data = select_countries,
                              aes(x = year, 
                                  y = age_dependency, 
                                  colour = country)) +
                      geom_point() +
                      geom_line() +
                      scale_colour_colorblind() +
                      theme_bw(base_family = "Inconsolata") +
                      labs(colour = "",
                           x = "", 
                           y = "Old Age Dependency")

old_age_dependency + transition_reveal(year)
