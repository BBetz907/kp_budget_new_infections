
unaids_ni_2022 <- unaids_ni |> filter(fiscal_year == 2022) |> select(-fiscal_year)

#now bring together to expand data set
unaids_ni_pepfar_ea <- ea_hts_prev |> 
  left_join(unaids_ni_2022, by = "country") |> 
  # bind_rows(dummy_data) |> 
  # mutate(across(c(percent_ni_kp:count_ni_f_15_49y, ~if_else(is.na(country), NA, .x)))) |> 
  glimpse()



colnames(unaids_ni_pepfar_ea)

write_csv(unaids_ni_pepfar_ea, "Dataout/unaids_ni_ea.csv")

unaids_ni_pepfar_ea |> count(ou, country, count_ni_kp) |> print(n=53)

unaids_ni_pepfar_ea |> filter(country == "Angola") |> glimpse()
