# AUTHOR:   B. Betz | USAID
# PURPOSE:  Munge 2023 UNAIDS New Infections data
# REF ID:   e7d6aa67 
# LICENSE:  MIT
# DATE:     2024-05-07
# UPDATED: 

# DEPENDENCIES ------------------------------------------------------------

library(gagglr)
library(tidyverse)

# merge 1, UNAIDS New Infections and PEPFAR budget data -------------
unaids_ni_2022 <- unaids_ni |> filter(fiscal_year == 2022) |> 
  select(-fiscal_year, -count_ni_wives_of_fsw_clients, -count_ni_clients_of_fsw, -count_ni_sw, 
         -count_ni_msm_tgw, -count_ni_tgw, -count_ni_pwid, -count_ni_msm_tgw, -count_ni_m_15_49y, -count_ni_f_15_49y)

#now bring together to expand data set
unaids_ni_pepfar_ea <- ea_hts_prev |> 
  left_join(unaids_ni_2022, by = "country") |> 
  # bind_rows(dummy_data) |> 
  # mutate(across(c(percent_ni_kp:count_ni_f_15_49y, ~if_else(is.na(country), NA, .x)))) |> 
  glimpse()

funding_below <- unaids_ni_pepfar_ea |> filter(fiscal_year ==2024, percent_kp_programmed < 0.95*percent_ni_kp_and_partners) |> select(country, percent_kp_programmed, percent_ni_kp_and_partners)
write_csv(funding_below, "Dataout/fundingbelow.csv")

write_csv(unaids_ni_pepfar_ea, "Dataout/unaids_ni_ea.csv")


unaids_ni_pepfar_ea |> count(ou, country, count_ni_kp) |> print(n=53)

unaids_ni_pepfar_ea |> filter(country == "Angola") |> glimpse()



# merge 2, 2010 and 2022 -----------------
ni_pepfar_country <- ea_hts_prev |> select(country, ou, geo) |> 
  inner_join(unaids_ni) |> 
  select(-contains("percent")) |> 
  group_by(geo, 
           country,
           fiscal_year
  ) |> 
  summarise(across(c(starts_with("count_ni_")), ~ sum(.x, na.rm = TRUE)), .groups = "drop") |> 
  pivot_longer(cols = c(starts_with("count_ni_")), 
               names_to = "pop", values_to = "count", 
               names_prefix = "count_ni_",
               names_transform = 
              ) |> 
  mutate(pop = str_replace_all(pop, "kp", "KP"),
         pop = case_when(
                         pop == "m_15_49y" ~ "total males (15-49)",
                         pop == "f_15_49y" ~ "total females (15-49)",
                         pop == "msm_tgw"  ~ "MSM & TGW",
                         pop == "pwid"     ~ "PWID",
                         pop == "sw"       ~ "Sex workers",
                         pop == "KP_partners" ~ "Partners of KP",
                         .default = str_replace_all(pop, "_", " ")
                         ),
         ) |> 
  filter(!pop %in% c("KP", "KP clients partners", "msm tg", "KP and partners", "tgw", "m f 15 49y")) |> 
  mutate(pop2=case_when(str_detect(pop, "total") ~ "Total",
                        pop %in% c("clients of fsw", "wives of fsw clients") ~ "Clients of SW & their partners",
                        pop %in% c("MSM & TGW", "PWID", "Sex workers") ~ "KP",
                        .default = pop
                        ),
         pop3 = case_when(pop2 %in% c("KP", "Partners of KP") ~ "KP & their partners",
                          pop2 == "Total" ~ "Total")) |> 
  # count(pop2, pop) |> 
  # mutate(geo = factor(geo, levels = c("Sub-Saharan Africa", "Outside Sub-Saharan Africa")),
  # fiscal_year = as.character(fiscal_year)
  # ) |> 
  # pivot_longer(cols = 2:6) |> 
  print()

write_csv(ni_pepfar_country, "Dataout/unaids_ni_pepfar_country.csv")

