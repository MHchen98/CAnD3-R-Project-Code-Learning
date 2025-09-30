# ------------------------------------------------------------------------------

# Day 3: Visualising Statistical Quantities
# PopAging DataViz—UK
# Companion Script File
# SPDX-License-Identifier: MIT
# Copyright (c) 2025 Sakeef M. Karim

# PRELIMINARIES ----------------------------------------------------------------

library(tidyverse)
library(systemfonts)

# Packages for estimating models, visualizing results:

library(estimatr)
library(ggeffects)
library(marginaleffects)
library(modelsummary)
library(effects)

# Misc:

library(gtsummary)
library(gt)
library(ggthemes)
library(hrbrthemes)
library(panelr)
library(skimr)
library(GGally)

# LOADING THE DATA -------------------------------------------------------------

load("./data/day3.RData")

# EXPLORING THE DATA ------------------------------------------------------

ggpairs(gapminder_2007 |> 
        select(2:5),
        mapping = aes(colour = continent))

# FITTING MODELS, VISUALIZING RESULTS ------------------------------------------

# Basic Linear Model -----------------------------------------------------------

mod_gapminder_1 <- lm(lifeExp ~ log_pop + log_gdpPercap + continent,
                      data = gapminder_2007)

# Quick Tables

tbl_regression(mod_gapminder_1)

modelsummary(mod_gapminder_1)

# Tidying Results --------------------------------------------------------------

mod_gapminder_1 |> tidy()

# Quick, Manual Coefficient Plot -----------------------------------------------

mod_gapminder_1 |> 
  tidy(conf.int = TRUE) |>  
  filter(str_detect(term, "log")) |>  
  ggplot(aes(x = estimate,
             y = term,
             xmin = conf.low,
             xmax = conf.high)) +
  geom_pointrange(color = "#2c7fb8", linewidth = 0.8, fatten = 2) + 
  geom_vline(xintercept = 0, linetype = "dashed", color = "red", linewidth = 0.6) +
  labs(
    title = "Effect of Log-Transformed Predictors",
    subtitle = "Estimates with 95% Confidence Intervals",
    x = "Coefficient Estimate",
    y = "Predictor"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12, color = "gray30"),
    axis.title.y = element_blank(),
    axis.text.y = element_text(face = "bold"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank()
  )


# Spend 10 minutes beautifying this basic visualization

# modelplot (via modelsummary) -------------------------------------------------

modelplot(mod_gapminder_1) 

# Customizing the plot

modelplot(mod_gapminder_1,
          # Omitting specific variables (e.g., controls, fixed effects etc.)
          coef_omit = "Interc|contin",
          # 99% confidence intervals:
          conf_level = 0.99) +
geom_vline(xintercept = 0,
           linetype = "dotted") +
    # Shading in "pointranges" based on significance level:
aes(color = ifelse(p.value < 0.01, "Significant", "Not significant")) +
scale_colour_manual(values = c("grey", "red"))

# A more "complicated" model --- but not necessarily a good one:
    
# Uses wage_panelr:

# wage_panelr |>  select(where(is.numeric)) |>  
#                 ggpairs()

mod_wages_1 <- lm_robust(lwage ~ years_worked + female +
                         black + blue_collar + south + union + 
                         manufacturing + wave,
                         data = wage_panelr,
                         clusters = id,
                         # Produces robust clustered SEs (via the Delta method, 
                         # as in Stata):
                         se_type = "stata") 
  
# Labels for coefficients:

wages_labels <- c("manufacturing1" = "Manufacturing",
                  "union1" = 'Union Member', 
                  "south1" = "Southerner",
                  "black1" = "Black",
                  "female1" = "Female",
                  "years_worked" = "Years in Workforce",
                  "blue_collar1" = "Blue Collar Worker")
    
mod_wages_1 |>  
modelplot(conf_level = 0.99,
            #String search:
            coef_omit = 'Interc|wave',
            colour = "red",
            # Renaming coefficients:
            coef_map = wages_labels) +
theme_bw(base_family = "Inconsolata") +
# Adding reference point for "non-significant" finding:
geom_vline(xintercept = 0, 
           linetype = "dashed") +
labs(x = "Coefficient Estimates",
     title = "Predicting the Log of Wages",
     subtitle = "With 99% Confidence Intervals") 

# Additional control:

mod_wages_2 <- lm_robust(lwage ~ years_worked + years_education + female + black +
                         blue_collar + south + union + manufacturing + wave,
                         data = wage_panelr,
                         clusters = id,
                         se_type = "stata") 

# Adjusting our labels:

wages_labels <- c(wages_labels, "years_education" = "Education (Years)")

mod_wages_2 |>  
modelplot(conf_level = 0.99,
          coef_omit = 'Interc|wave',
          colour = "red",
          coef_map = wages_labels) +
theme_bw(base_family = "Inconsolata") +
geom_vline(xintercept = 0, 
           linetype = "dashed") +
labs(x = "Coefficient Estimates",
     title = "Predicting the Log of Wages",
     subtitle = "With 99% Confidence Intervals") 

# Comparing models

wage_models <- list("Base Model" = mod_wages_1,
                    "Full Model" = mod_wages_2)

wage_models |>  
modelplot(coef_omit = 'Interc|wave|edu',
          size = 1,
          linewidth = 1,
          coef_map = wages_labels) +
# Adjusting colour scale:
scale_colour_brewer(palette = "Set2") +
# From hrbrthemes:
theme_modern_rc(base_family = "Inconsolata") +
theme(legend.title = element_blank()) +
guides(colour = guide_legend(override.aes = list(size = 0.5))) +
geom_vline(xintercept = 0,
           colour = "white",
           linetype = "dashed")

# Using facets

rev(wage_models) |>  
modelplot(coef_omit = 'Interc|wave|edu',
          # Matches arguments for geom_pointrange()
          size = 1,
          linewidth = 1,
          coef_map = wages_labels,
          colour = "skyblue",
          facet = TRUE) +
theme_bw(base_family = "Inconsolata") +
theme(legend.title = element_blank(),
      strip.text.y = element_text(angle = 0)) +
geom_vline(xintercept = 0,
           linetype = "dashed")


# ADJUSTED PREDICTIONS ---------------------------------------------------------

# ggeffects --------------------------------------------------------------------

# Fitting new model with interaction term:

mod_gapminder_2 <- lm(lifeExp ~ log_pop + continent*log_gdpPercap,
                      data = gapminder_2007)

# Using the ggeffect function:

ggeffect(mod_gapminder_2, 
         # Predicted life expectancy at logged GDP per capita of 6 to 11
         # for countries in Europe and Africa:
         terms = c("log_gdpPercap [6:11]", "continent [Europe, Africa]")) |>  
plot() +
labs(title = "Predicted Values of Life Expectancy", 
     x = "Log of Per Capita GDP",
     y = "Life Expectancy (Years)",
     fill = "",
     colour = "") +
theme_ggeffects(base_family = "IBM Plex Sans") +
scale_colour_economist() +
scale_fill_economist() +
theme(legend.position = "bottom") +
# Overriding default alpha of confidence intervals:
guides(fill = guide_legend(override.aes = list(alpha = 0.35)))

# The longer route:

mod_gapminder_2 |> 
ggeffect(terms = c("log_gdpPercap [6:11]",
                   "continent [Europe, Africa]")) |> 
as_tibble() |> 
ggplot(mapping = aes(x = x, 
                     y = predicted, 
                     ymin = conf.low,
                     ymax = conf.high,
                     # group corresponds to discrete term:
                     colour = group,
                     fill = group)) +
geom_line(size=1) +
# For confidence intervals:
  geom_ribbon(aes(group = group), 
              inherit.aes = TRUE,
              colour = NA, 
              fill = "grey70", 
              alpha = 0.4) + 
  labs(title = "Predicted Values of Life Expectancy", 
       x = "Log of Per Capita GDP",
       y = "Life Expectancy (Years)",
       fill = "",
       colour = "") +
  theme_ggeffects(base_family = "IBM Plex Sans") +
  scale_colour_economist() +
  scale_fill_economist() +
  theme(legend.position = "bottom")

# Can you adjust this plot so that it (more or less) matches the previous one?

# via {marginaleffects} --------------------------------------------------------

plot_predictions(mod_gapminder_2, 
                 condition = list("log_gdpPercap" = 6:11,
                                  "continent" = c("Africa", "Europe"))) +
theme_ggeffects(base_family = "IBM Plex Sans") +
labs(fill = "", 
     colour = "",
     x = "Log of GDP per Capita",
     y = "Life Expectancy")
                   
# The longer route ...
                 
avg_predictions(mod_gapminder_2,
                variables = list(log_gdpPercap = c(6:11),
                                 continent = c("Africa", "Europe"))) |>  
ggplot(mapping = aes(x = log_gdpPercap, 
                     y = estimate, 
                     ymin = conf.low,
                     ymax = conf.high,
                     colour = continent,
                     fill = continent)) +
geom_line() +
geom_ribbon(alpha = 0.5)  


# AVERAGE MARGINAL EFFECTS -----------------------------------------------------

avg_slopes(mod_gapminder_2,
           # Focal variable:
           variables = "log_gdpPercap",
           # Across levels of ...
           by = "continent")

plot_slopes(mod_gapminder_2,
            variables = "log_gdpPercap",
            condition = "continent") + 
theme_ggeffects(base_family = "IBM Plex Sans")

# AVERAGE COMPARISONS -----------------------------------------------------

 avg_comparisons(mod_gapminder_2,
                variables = list("continent" = "pairwise"),
                by = "log_gdpPercap")

plot_comparisons(mod_gapminder_2, 
                 # Unlocks pairwise comparisons:
                 variables = list("continent" = "pairwise"), 
                 # Key axis of comparison:
                 condition = "log_gdpPercap")  +
geom_hline(yintercept = 0, lty = "dashed") +
labs(x = "Log of GDP per Capita",
     y = "Life Expectancy") +
# Adding mapping aesthetics:
aes(fill = contrast, 
    colour = contrast) +
scale_colour_brewer(palette = "Dark2") +
scale_fill_brewer(palette = "Dark2") +
theme_ggeffects(base_family = "IBM Plex Sans") +
# Removing gratuitous legend:
theme(legend.position = "none") 
