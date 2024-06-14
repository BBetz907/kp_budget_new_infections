# AUTHOR:   B. Betz | USAID
# PURPOSE:  Munge 2023 BUDGET DATA export from Tableau - out of date
# REF ID:   e7d6aa67 
# LICENSE:  MIT
# DATE:     2024-05-07
# UPDATED: 

# DEPENDENCIES ------------------------------------------------------------

library(gagglr)
library(tidyverse)

# IMPORT ------------------------------------------------------------------
budget_summary<- read_csv(file = "Data/budgets by ou, country, fy_data 2024_05_07.csv") |> 
  janitor::clean_names() |>   pivot_wider(names_from = "measure_names", values_from = "measure_values") |> 
  janitor::clean_names() |> 
  mutate(no_kp = case_when(is.na(kp_budget) ~ TRUE)) |> 
  group_by(fiscal_year) |> 
  mutate(kp_budget_global = sum(kp_budget, na.rm = TRUE),
         total_planned_funding_global = sum(total_planned_funding, na.rm = TRUE),
         kp_budget_percent_of_total_global = kp_budget_global/total_planned_funding_global) |> 
  ungroup() |> 
  group_by(fiscal_year, no_kp) |> 
  mutate(kp_budget_global_w = case_when(is.na(no_kp) ~ sum(kp_budget, na.rm = TRUE)),
         total_planned_funding_global_w = case_when(is.na(no_kp) ~ sum(total_planned_funding, na.rm = TRUE)),
         kp_budget_percent_of_total_global_w = kp_budget_global_w/total_planned_funding_global_w) |> 
  ungroup() |> 
  print(n=15)


countries <- budget_summary|> select(1:6) |>   pivot_longer(cols = 4:6, names_to = "measures", values_to = "values") |> print(n=30)

global <- budget_summary|> select(3, 8:10) |> slice(1:5) |> mutate(operating_unit = "global", country = "global") |> 
  pivot_longer(cols = 2:4, names_to = "measures", values_to = "values") |> 
  mutate(measures = str_remove_all(measures, "_global")) |> print()

global_countries_with_any_kp <- budget_summary|> select(3, 11:13) |> 
  slice(11:15) |>
  mutate(operating_unit = "global with any KP", country = "global with any KP") |> 
  pivot_longer(cols = 2:4, names_to = "measures", values_to = "values") |> 
  mutate(measures = str_remove_all(measures, "_global_w")) |> print()

budgets <- rbind(countries, global, global_countries_with_any_kp)

budgets |> pivot_wider(names_from = measures, values_from = values) 


budget_summary|> glimpse()

budget_summary|> filter(fiscal_year >= 2020,
                 country == "Burma") |> 
  ggplot(aes(x=fiscal_year)) +
  geom_col(aes(y=kp_budget_percent_of_total), fill ="blue", position = position_dodge(width = 0.8), width = 0.4) 
  # geom_col(aes(y=total_planned_funding_global), fill ="yellow", position = position_dodge(width = 0.8), width = 0.4)
  
budget_summary|> filter(fiscal_year >= 2020,
                 country == "Burma") |> 
  ggplot(aes(x=fiscal_year)) +
  geom_col(aes(y=kp_budget_global ), fill ="blue", position = position_dodge(width = 0.8), width = 0.4) 
# geom_col(aes(y=total_planned_funding_global), fill ="yellow", position = position_dodge(width = 0.8), width = 0.4)

budget_summary|> select(fiscal_year)




pepfar_budget_summary<- budget_summary|> 
  filter(fiscal_year >= 2020,
            country == "Burma") |> 
  select(fiscal_year, kp_budget_global, total_planned_funding_global, kp_budget_percent_of_total_global) |> 
  print(n=15)

write_csv(pepfar_budget, "Dataout/pepfar_budget_by_fy.csv")



budget24 <- budget_summary|> filter(fiscal_year == 2024, !is.na(total_planned_funding)) |> select(operating_unit, country, kp_budget_percent_of_total)

