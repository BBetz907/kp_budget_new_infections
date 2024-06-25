# AUTHOR:   B. Betz | USAID
# PURPOSE:  Munge 2023 UNAIDS New Infections data
# REF ID:   e7d6aa67 
# LICENSE:  MIT
# DATE:     2024-05-07
# UPDATED: 

# DEPENDENCIES ------------------------------------------------------------
  
  library(gagglr)
  library(tidyverse)

# Grab Headers ------------------------------------------------------------------
year <- readxl::read_excel(path = "Data/Korenromp JAIDS 2024 Country level data.xlsx", sheet = "2022",  
                           range = "B1", col_names = FALSE) |> pull()

  header_country_codes <- readxl::read_excel(path = "Data/Korenromp JAIDS 2024 Country level data.xlsx", sheet = "2022",  
                                     range = "A2:D2", col_names = TRUE) |> 
    select(1,3,4) |> janitor::clean_names() |> colnames() |> glimpse()
  
  header_pop_codes <-     readxl::read_excel(path = "Data/Korenromp JAIDS 2024 Country level data.xlsx", sheet = "2022",  
                                         range = "A1:CX1", col_names = TRUE) |> 
    select(8:10, 23,25,27,29, 31,33,36, 38, 40, 42, 44, 46,48:52) |> janitor::clean_names() |> 
    colnames()  |> str_remove_all("ni_") |> str_replace_all("k_ps", "kp") |> 
    print()
  

  #pull headers from data source and add fiscal year as the last
  count_ni_headers <- c(header_country_codes, str_c("count_ni_", header_pop_codes[1:16]), "fiscal_year")
  
  #return to do the same for PSEs, prev, incidence rates  by KP in later populations
  
 
  # IMPORT ------------------------------------------------------------------
  # perc_ni_headers <- c(header_country_codes, str_c("perc_ni_", header_pop_codes[4:15]), str_c("perc_ni_", header_pop_codes[17:20]))
  # 
  # unaids_percent_ni <- readxl::read_excel(path = "Data/Korenromp JAIDS 2024 Country level data.xlsx", sheet = "2022", 
  #                              range = "A3:CX177", 
  #                              col_names = FALSE) |> 
  #   select(1,3,4,23,25,27,29, 31,33,36, 38, 40, 42, 44, 46,49:52) |> 
  #   slice(1:172,175)
  # 
  # colnames(unaids_percent_ni) <- perc_ni_headers
  # 
  # glimpse(unaids_percent_ni)
  
  ########
  sheet_names <- c("2022", "2010")
  file_path <- "Data/Korenromp JAIDS 2024 Country level data.xlsx"
  
  keep_select <- function(df) {
    df |> 
      select(1,3,4,8:10, 24,26,28,30, 32,34,37, 39, 41, 43, 45, 47, 48) |> 
      slice(1:172,175)
  }
  
  
  unaids_ni <-  map_dfr(sheet_names, ~ {
    readxl::read_excel(file_path, 
                       sheet = .x, 
                       range = "A3:CX177", 
                       col_names = FALSE) |> 
      keep_select() |> 
      mutate(fiscal_year = .x)}) |> 
    set_names(count_ni_headers) |> 
    filter(!is.na(country)) |> 
    mutate(
           count_ni_kp = count_ni_sw + count_ni_msm_tgw + count_ni_tgw + count_ni_pwid,
           count_ni_kp_partners = count_ni_all_non_client_partners_of_kp_all_countries_incl_extrapolation - count_ni_wives_of_fsw_clients,
           count_ni_kp_and_partners = count_ni_kp + count_ni_kp_partners,
           count_ni_kp_clients_partners = count_ni_kp + count_ni_all_non_client_partners_of_kp_all_countries_incl_extrapolation + count_ni_clients_of_fsw,
           # count_ni_partners = count_ni_all_non_client_partners_of_kp_all_countries_incl_extrapolation,
           percent_ni_kp = count_ni_kp / count_ni_m_f_15_49y,
           percent_ni_kp_and_partners =  (count_ni_kp_partners + count_ni_kp) / count_ni_m_f_15_49y,
           percent_ni_kp_clients_partners =  count_ni_kp_clients_partners / count_ni_m_f_15_49y,
           ) |> 
    select(1:3, fiscal_year, percent_ni_kp, percent_ni_kp_and_partners, percent_ni_kp_clients_partners, count_ni_kp, count_ni_kp_partners, count_ni_kp_and_partners, count_ni_kp_clients_partners, count_ni_m_f_15_49y,
           count_ni_wives_of_fsw_clients, count_ni_clients_of_fsw, count_ni_sw, count_ni_msm_tgw, count_ni_tgw, count_ni_pwid, count_ni_msm_tgw, count_ni_m_15_49y, count_ni_f_15_49y) |> 
    mutate(
            country = case_when(country == "United Republic of Tanzania" ~ "Tanzania",
                               country == "Viet Nam" ~ "Vietnam",
                               country == "Myanmar" ~ "Burma",
                               country == "Lao People Democratic Republic" ~ "Laos",
                               country == "Cote dIvoire" ~ "Cote d'Ivoire",
                               
                               TRUE ~ country
    ))
  

  
  
  #define dummy data for Tableau Histograms
  dummy_data_2024 <- tibble(
    c = letters[1:11],
    kp_programmed = 0:10,
    kp_allocated = 0:10,
    total_funding = rep(10, 1),  # Value of 10 for each row
    count_ni_kp = 0:10,
    count_ni_kp_partners = 0:10,
    count_ni_kp_and_partners = 0:10,
    count_ni_kp_clients_partners = 0:10,
    count_ni_msm_tgw = 0:10,
    count_ni_m_f_15_49y = rep(10, 1),  # Value of 10 for each row
    count_ni_m_15_49y = rep(10, 1),     # Value of 10 for each row
    count_ni_f_15_49y = rep(10, 1)      # Value of 10 for each row
  ) |> mutate(fiscal_year = "2024", val = 0)
  
  dummy_data <- dummy_data_2024 |> mutate(fiscal_year = "2025") |> 
    bind_rows(dummy_data_2024) 
  