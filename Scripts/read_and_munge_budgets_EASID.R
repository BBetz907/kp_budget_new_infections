# AUTHOR:   B. Betz | USAID
# PURPOSE:  munge cop ea structured integrated data set
# REF ID:   aa5590af 
# LICENSE:  MIT
# DATE:     2024-06-11
# UPDATED: 

# DEPENDENCIES ------------------------------------------------------------
  
library(gagglr)
library(tidyverse)

# GLOBAL VARIABLES --------------------------------------------------------
  
  
  ref_id <- "aa5590af"

# IMPORT ------------------------------------------------------------------
  easid <- read_csv("Data/COP EA Structured Integrated Dataset.csv") |> janitor::clean_names() |> glimpse()

glimpse(easid)
# MUNGE -------------------------------------------------------------------
allocated_kp_beneficiaries <- c("People Who Inject Drugs (Allocated)", "Transgender (Allocated)", "Sex Workers (Allocated)", "Men Having Sex with Men (Allocated)", "Key Populations (Allocated)")

  df <- easid |> 
    filter(fiscal_year %in% c("2024", "2025"),
      data_stream %in% c("FSD", "Initiative"),
            data_stream != "MER",
           program_area %in% c("PREV", "HTS")) |> 
  mutate(country = if_else(is.na(country) & !str_detect(ou, "Region"), ou, country),
          targeted_kp_funding = case_when(sub_beneficiary == "People in Prisons (Allocated)" ~ "Other",
                                         beneficiary=="Key Populations (Targeted)"  ~ "KP",
                                         .default = "Other"),
         allocated_kp_funding = case_when(sub_beneficiary %in% allocated_kp_beneficiaries ~ "KP",
                                          .default = "Other")) |> 
    group_by(fiscal_year, data_stream, program_area, beneficiary, sub_beneficiary, targeted_kp_funding, allocated_kp_funding, funding_agency, ou, country) |> 
    summarize(across(c("total_planned_funding"), ~sum(.,na.rm = TRUE)), .groups = "drop") 
  
  class(df)
  # df |> count(targeted_kp_funding, allocated_kp_funding
              # , beneficiary, sub_beneficiary
              # ) |> gt::gt()
  df |> count(ou, country) |> gt::gt()
  
  
  write_csv(df, "Dataout/pepfar_budget_expenditure_plans_by_fy.csv")
  
 programmed <- df |>  filter(data_stream == "FSD") |>    group_by(fiscal_year, data_stream, program_area, targeted_kp_funding, ou, country) |> 
    summarize(across(c("total_planned_funding"), ~sum(.,na.rm = TRUE)), .groups = "drop") |> 
   pivot_wider(names_from = "targeted_kp_funding", values_from = "total_planned_funding", names_glue = "{targeted_kp_funding}_programmed") |>
   janitor::clean_names() |> 
   mutate(across(c("kp_programmed", "other_programmed"), ~replace_na(.,0)),
          total_programmed_funding = kp_programmed + other_programmed,
          percent_kp_programmed = round(kp_programmed / total_programmed_funding ,3)) |> 
   glimpse()
  
 allocated <- df |>  filter(data_stream == "FSD") |>    group_by(fiscal_year, data_stream, program_area, allocated_kp_funding, ou, country) |> 
   summarize(across(c("total_planned_funding"), ~sum(.,na.rm = TRUE)), .groups = "drop") |> 
   pivot_wider(names_from = "allocated_kp_funding", values_from = "total_planned_funding", names_glue = "{allocated_kp_funding}_allocated") |>
   janitor::clean_names() |> 
   mutate(across(c("kp_allocated", "other_allocated"), ~replace_na(.,0)),
          total_allocated_funding = kp_allocated + other_allocated,
          percent_kp_allocated = round(kp_allocated / total_allocated_funding ,3),
         ) |> 
   glimpse()
 
 
 
janitor::compare_df_cols(allocated, programmed)

not_ssa <- c("Ukraine", "Haiti", "Dominican Republic", "Vietnam")

ea <- programmed |> inner_join(allocated) |> select(-contains("other")) |> 
  mutate(geo = case_when(country %in% not_ssa ~ "Outside Sub-Saharan Africa",
                         str_detect(ou, "Region") & !str_detect(ou, "West ")  ~ "Outside Sub-Saharan Africa",
                         .default = "Sub-Saharan Africa")) |> 
  glimpse()
  
ea |> count(geo, ou) |> gt::gt() 

write_csv(ea, "Dataout/pepfar_budget_expenditure.csv")

program_area_combined <- ea |> count(program_area)  |> pull(program_area) |> str_c(collapse = ", ")

ea_hts_prev <- ea |> mutate(program_area = program_area_combined) |> group_by(across(-c("kp_programmed":"percent_kp_allocated"))) |> 
  summarize(across(c("kp_programmed":"percent_kp_allocated"), ~ sum(., na.rm = TRUE)), .groups = "drop") |> 
  mutate(percent_kp_allocated  = round(kp_allocated / total_allocated_funding ,3),
         percent_kp_programmed = round(kp_programmed / total_programmed_funding ,3),
         total_funding = total_programmed_funding) |> select(-total_programmed_funding, -total_allocated_funding)

ea |> mutate(program_area = program_area_combined) |> group_by(across(-c("kp_programmed":"percent_kp_allocated"))) |> 
  summarize(across(c("kp_programmed":"percent_kp_allocated"), ~ sum(., na.rm = TRUE)), .groups = "drop") |> 
  filter(total_programmed_funding != total_allocated_funding)
  