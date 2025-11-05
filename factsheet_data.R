# Title: Country factsheets for Tobacco Atlas
# Date: 11/5/2025

#---- Import libraries ----
library(tidyverse)
library(janitor)
library(readxl)
library(writexl)
library(countrycode)

#---- Import data ----
df_fs <- read_xlsx("data/TA7_country_profiles_final_04102025.xlsx")

df <- df_fs |> 
  select(
    country,
    Country_TA6,
    ISO3,
    Comparo1,
    Countrynameforthepage
  )

ctry <- df |> 
  select(iso3 = ISO3)

# 1. Introduction section ----
intro <- df_fs |> 
  select(
    ISO3, deaths_prompt, cost_prompt, cost_currency
  )

# number of death
## Data Source: GBD 2023
## Link: https://vizhub.healthdata.org/gbd-results?params=gbd-api-2023-permalink/97f8e172c471a853e3fe88f0be6693b1
df_num_dth <- read_csv("data/tbc_dth_num_ihme_20251105.csv")

num_dth <- df_num_dth |> 
  clean_names() |> 
  remove_constant() |> 
  remove_empty() |> 
  mutate(
    iso3 = countrycode(location_name, "country.name.en", "iso3c"),
    iso3 = case_when(
      location_name == "Lebanese Republic" ~ "LBN",
      location_name == "Portuguese Republic" ~ "PRT",
      location_name == "Republic of Guyana" ~ "GUY",
      TRUE ~ iso3
    )
  ) |> 
  select(iso3, num_death = val)

# economic cost
## Data Source: CoRRE 2025
## Link: https://tobaccoatlas.org/corre
df_corre_red1 <- read_excel("data/cost_corre_20251105.xlsx",
                       sheet = "-1",
                       range = "A6:AW201") |> 
  clean_names() |> 
  remove_empty() |> 
  remove_constant()

cost_lcu <- df_corre_red1 |> 
  select(
    ctry = country,
    cost = annual_smoking_attributable_cost_of_illness_million_local_currency_unit
  ) |> 
  mutate(
    iso3 = countrycode(ctry, "country.name.en", "iso3c"),
    cost = format(as.numeric(cost) * 1e06, scientific = FALSE, big.mark = ",")
  ) |> 
  select(iso3, cost)

# intro dataset
fn_intro <- intro |>
  left_join(num_dth, join_by(ISO3 == iso3)) |> 
  left_join(cost_lcu, join_by(ISO3 == iso3)) |> 
  relocate(num_death, .after = deaths_prompt) |> 
  relocate(cost, .after = cost_prompt)
  

# 2. Current rates of smk and tbc use ----
prev <- df_fs |> 
  select(
    ISO3,
    prev_header,
    rates_paragraph,
    adult_prev_title,
    prev_text,
    num_title,
    youth_title,
    smokeless_title,
    death_title,
    death_label,
    link_to_chapters1,
    link_prev,
    link_youth,
    link_death
  )

# smoking prevalence
## Data Source: WHO GHO 2024
df_smk_prev <- read_csv("data/smk_prev_who_20250902.csv") |> 
  select(
    ind = IndicatorCode,
    ISO3 = SpatialDimValueCode,
    year = Period,
    sex = Dim1ValueCode,
    val = FactValueNumericLow
  ) |> 
  filter(
    ind == "M_Est_smk_curr_std" &
    year == 2025
  )

smk_prev <- df |> select(ISO3, ctry = Countrynameforthepage) |> 
  left_join(df_smk_prev, join_by(ISO3)) |> 
  mutate(val = paste0(val,"%")) |> 
  pivot_wider(
    id_cols = c(ISO3, ctry),
    names_from = sex,
    values_from = val
  ) |> 
  rename(
    men = SEX_MLE,
    women = SEX_FMLE,
  ) |> 
  mutate(
    prev_text = paste0("Adult smoking prevalence in ",ctry," is ",SEX_BTSX,"."),
    across(everything(),\(x) if_else(is.na(x), "NA", x))
  ) |> 
  select(ISO3, men, women, prev_text)

# smoker number
## Data Source: WHO trend report, 2025
## Data table is converted into Excel


# 3. Negative effect of tbc use ----
# 4. Impact of the tbc supply chain ----
# 5. Ending the tbc epidemic
