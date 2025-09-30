############################################################
# Project: CAnD3 RRWM
# Script: rrpm_housing.R
# Author: Minheng

# Created: 2025-09-28
# Last updated: 2025-09-29
# Version: 1.0
#
# Description:
#   - This script cleans and prepares data extracted from Public Use Micro File 
#   - Data citation: Statistics Canada. 2019. Census of Population, 2016 [Canada] Public Use Microdata File (PUMF): Individuals File. Statistics Canada [producer and distributor], accessed September 10, 2021. ID: pumf-98M0001-E-2016-individuals.
#   - Handles missing values and recode vairables
#   - Outputs a descriptive table and ANOVA


################################################################## Data Cleaning

# read data
setwd(getwd())
housing <- read.csv("../RRWM/data/Census 2016/pumf-98M0001-E-2016-individuals_F1.csv")

# select variables and rename
var <- c("AGEGRP", "MarStH", "Sex", "Wages", "PresMortG", "Tenur", "HDGREE", "PR")
df <- housing[var]
df <- df[df$AGEGRP>=6, ]

# summary of the dataset
summary(df)

# recode and rename variables for analysis
# recode age groups to keep 15+ records with a 5-year interval
library(dplyr)

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

table_agegrp_5 <- prop.table(table(df$agegrp_5)) * 100   
table_agegrp_5

# marital status
df <- df %>%
  mutate(marr = case_when(
    MarStH %in% c(1, 4, 5, 6) ~ 0,  # single living
    MarStH %in% c(2, 3) ~ 1 # married or common law
  )) %>%
  mutate(marr = factor(
    marr,
    levels = c(0, 1),
    labels = c("single", "married or common law")
  ))

table_marr <- prop.table(table(df$marr)) * 100   
table_marr

# Sex
df <- df %>%
  mutate(female = case_when(
    Sex == 2 ~ 1,  # female
    Sex == 1 ~ 0  # male
  )) %>%
  mutate(female = factor(
    female,
    levels = c(0, 1),
    labels = c("male", "female")
  ))

table_female <- prop.table(table(df$female)) * 100   
table_female

# income
sort(unique(df$Wages), decreasing = TRUE)[1:5]  # search for N/A coding
df <- df %>%
  mutate(income_q = ifelse(Wages == 88888888 | Wages == 99999999, NA, Wages)) %>%
  mutate(income_q = case_when(
    ntile(income_q, 4) == 1 ~ 1,             # bottom 25%
    ntile(income_q, 4) %in% 2:3 ~ 2,         # middle 50%
    ntile(income_q, 4) == 4 ~ 3              # top 25%
  )) %>%
  mutate(income_q = factor(
    income_q,
    levels = c(1, 2, 3),
    labels = c("bottom 25%", "middle 50%", "top 25%")
  ))

table_income_q <- prop.table(table(df$income_q)) * 100   
table_income_q

# mortgage
df <- df %>%
  mutate(mortgage = ifelse(PresMortG == 8 | PresMortG == 9, NA, PresMortG)) %>%
  mutate(mortgage = factor(
    mortgage,
    levels = c(0,1),
    labels = c("without mortgage", "with mortgage")
  ))
# 1=With mortgage 0=without mortgage
table_mortgage <- prop.table(table(df$mortgage)) * 100   
table_mortgage

# tenure
df <- df %>%
  mutate(tenure = ifelse(Tenur %in% c(8,9), NA, Tenur)) %>%
  mutate(tenure = case_when(
    tenure == 1 ~ 1, # ownership
    tenure == 2 ~ 0
  )) %>%
  mutate(tenure = factor(
    tenure,
    levels = c(0,1),
    labels = c("Non-ownership", "Ownership")
  ))


# 1=Ownership 0=Non-ownership
table_tenure <- prop.table(table(df$tenure)) * 100   
table_tenure

# education

df <- df %>%
  mutate(edu_college = ifelse(HDGREE == 88 | HDGREE == 99, NA, HDGREE)) %>%
  mutate(edu_college = case_when(
    edu_college %in% c(1, 2, 3) ~ 0, # below college
    edu_college %in% c(seq(4, 13)) ~ 1 # college degree and above
  )) %>%
  mutate(edu_college = factor(
    edu_college,
    levels = c(0, 1),
    labels = c("below college", "college or above")
  ))
  
table_edu <- prop.table(table(df$edu_college)) * 100   
table_edu

# province
df <- df %>%
  mutate(province = case_when(
    PR %in% c(10, 11, 12, 13) ~ 1, # Atlantic provinces
    PR %in% c(24, 35) ~ 2, # central Canada
    PR %in% c(46, 47, 48) ~ 3, # Prairies
    PR == 59 ~ 4, # British Columbia
    PR == 70 ~ 5 # Northern Canada
  )) %>%
  mutate(province = factor(
    province,
    levels = 1:5,
    labels = c(
      "Atlantic provinces",
      "Central Canada",
      "Prairies",
      "British Columbia",
      "Northern Canada"
    )
  ))

table_province <- prop.table(table(df$province)) * 100   
table_province

# separate into two separate datasets: anyone with mortgage are entitled with tenure
df_mortgage <- df %>% filter(tenure==1)

# list-wise delete obs with missing values
df_tenure <- df[complete.cases(df[, c("tenure", "agegrp_5", "marr", "female", "income_q", "edu_college", "province")]), ]
df_mortgage <- df[complete.cases(df[, c("mortgage", "agegrp_5", "marr", "female", "income_q", "edu_college", "province")]),]


######################################################### Descriptive, including missing values
library(summarytools)

df_summary <- df |>
  select(tail(names(df), 8)) |>
  dfSummary()

view(df_summary)


######################################################## ANOVA (Analysis of Variance)

model_tenure <- glm(tenure ~ agegrp_5 + marr + female + income_q + edu_college + province, family=binomial, data=df_tenure)
model_mortgage <- glm(mortgage ~ agegrp_5 + marr + female + income_q + edu_college + province, family=binomial, data=df_mortgage)


# Visualization
library(broom)
library(ggplot2)
library(stringr)
library(forcats)

# 1. Fit models
model_mortgage <- glm(
  mortgage ~ female + marr + agegrp_5 + income_q + edu_college + province,
  data = df,
  family = binomial
)

model_tenure <- glm(
  tenure ~ female + marr + agegrp_5 + income_q + edu_college + province,
  data = df,
  family = binomial
)

# 2. Tidy output with confidence intervals
coef_mortgage <- tidy(model_mortgage, conf.int = TRUE) |>
  mutate(model = "Mortgage")

coef_tenure <- tidy(model_tenure, conf.int = TRUE) |>
  mutate(model = "Tenure")

# Load packages
library(dplyr)
library(ggplot2)
library(broom)
library(forcats)
library(stringr)

# 1. Fit models
model_mortgage <- glm(
  mortgage ~ female + marr + agegrp_5 + income_q + edu_college + province,
  data = df,
  family = binomial
)

model_tenure <- glm(
  tenure ~ female + marr + agegrp_5 + income_q + edu_college + province,
  data = df,
  family = binomial
)

# 2. Tidy output
coef_mortgage <- tidy(model_mortgage, conf.int = TRUE) |>
  mutate(model = "Mortgage")

coef_tenure <- tidy(model_tenure, conf.int = TRUE) |>
  mutate(model = "Tenure")

# 3. Combine and clean

group_levels <- list(
  "Gender" = c("female"),
  "Marital Status" = c("married or common law"),
  "Age Group" = c(
    "15–20", "21–25", "26–30", "31–35", "36–40", "41–45",
    "46–50", "51–55", "56–60", "61–65", "66–70", "71–75",
    "76–80", "81–85", "85+"
  ),
  "Income" = c("middle 50%", "top 25%"),
  "Education" = c("college or above"),
  "Province" = c("Central Canada", "Prairies", "British Columbia", "Northern Canada")
)

ordered_labels <- unlist(group_levels)

# 4. Order labels within group, and groups in desired order
all_coefs <- all_coefs |>
  filter(label %in% ordered_labels) |>
  mutate(label = factor(label, levels = rev(ordered_labels)))

# 5. Plot: no facets, just grouped label order
ggplot(all_coefs, aes(x = estimate, y = label, color = model)) +
  geom_point(position = position_dodge(width = 0.5)) +
  geom_errorbarh(
    aes(xmin = conf.low, xmax = conf.high),
    height = 0.2,
    position = position_dodge(width = 0.5)
  ) +
  theme_minimal(base_size = 12) +
  labs(
    title = "Grouped Coefficient Plot (Log Odds)",
    x = "Estimate (log-odds)",
    y = NULL,
    color = "Model"
  )
