# AUTHOR:   B. Betz | USAID
# PURPOSE:  
# REF ID:   2fce3ea2 
# LICENSE:  MIT
# DATE:     2024-06-13
# UPDATED: 

# DEPENDENCIES ------------------------------------------------------------
  
  library(tidyverse)
  library(gagglr)
  library(stringr)
  library(janitor)
  library(extrafont)
  library(scales)
  library(tidytext)
  library(patchwork)
  library(ggtext)
  library(glue)
  library(readxl)
  

# GLOBAL VARIABLES --------------------------------------------------------
  
  ref_id <- "2fce3ea2"

# Munge ------------------------------------------------------------------
  
  ni_pepfar <- ea_hts_prev |> select(country, ou, geo) |> 
    inner_join(unaids_ni) |> 
    select(-contains("percent")) |> 
    group_by(geo, fiscal_year) |> 
    summarise(across(c(count_ni_kp:count_ni_msm_tgw), ~ sum(.x, na.rm = TRUE)), .groups = "drop") |> 
    mutate(geo = factor(geo, levels = c("Sub-Saharan Africa", "Outside Sub-Saharan Africa")),
           fiscal_year = as.character(fiscal_year)) |> 
    # pivot_longer(cols = 2:6) |> 
    glimpse()

# visualize  -------------------------------------------------------------------
  ni_pepfar |> 
    ggplot(aes(x=fiscal_year)) + facet_grid(cols = vars(geo)) +
    # geom_col(aes( y=count_ni_kp_clients_partners /count_ni_m_f_15_49y), fill = grey40k) +
    #   geom_text(aes(y=count_ni_kp_clients_partners /count_ni_m_f_15_49y,
    #               label = scales::percent(count_ni_kp_clients_partners /count_ni_m_f_15_49y, accuracy = 1)), 
    #           position = position_nudge(y=0.04), fontface = "bold") +
    # geom_text(aes(y=count_ni_kp_clients_partners /count_ni_m_f_15_49y,
    #               label = scales::percent((count_ni_kp_clients_partners-count_ni_kp_and_partners) /count_ni_m_f_15_49y, accuracy = 1)), 
    #           position = position_nudge(y=-0.01)) +
    # geom_col(aes(y=count_ni_kp_and_partners/count_ni_m_f_15_49y), fill = denim_light) +
      # geom_text(aes(y=count_ni_kp_and_partners /count_ni_m_f_15_49y,
      #             label = scales::percent(count_ni_kp_partners /count_ni_m_f_15_49y, accuracy = 1)), 
      #         position = position_nudge(y=-0.007)) +
    geom_col(aes(y=count_ni_kp/count_ni_m_f_15_49y), fill = grey20k) +
      geom_text(aes(y=count_ni_kp /count_ni_m_f_15_49y,
                  label = scales::percent(count_ni_kp /count_ni_m_f_15_49y, accuracy = 1)), 
              position = position_nudge(y=-0.05), 
              # color = "white", 
              fontface = "bold") +
    geom_col(aes(y=count_ni_msm_tgw/count_ni_m_f_15_49y), fill = "#f28965", width = 0.7) +
      geom_text(aes(y=count_ni_msm_tgw /count_ni_m_f_15_49y,
                  label = scales::percent(count_ni_msm_tgw /count_ni_m_f_15_49y, accuracy = 1)), 
              position = position_nudge(y=-0.015)) +
    # geom_text(aes(y=count_ni_msm_tgw /count_ni_m_f_15_49y,
    #               label = scales::comma(count_ni_msm_tgw, scale = 10^-3, accuracy = 1, suffix = "k") ), 
    #           position = position_nudge(y=-0.15)) +
    # si_style_ygrid() +
    si_style_xyline()+
    scale_y_continuous(labels = percent_format(), breaks = seq(0, 1, by = 0.1), limits = c(0, .6)) +
    theme(axis.text.x = element_text(size=12),
          strip.text = element_text(size = 12, hjust = 0.5),
          axis.title.x = element_blank(),
          axis.text.y = element_blank()) +
    labs(
      title = glue::glue("UNAIDS estimates of new infections in PEPFAR OUs, 2010 and 2022"),
      subtitle = "Bars represent the percentage of total new infections.",
         # subtitle = glue::glue("Outside of SSA, HIV infections among MSM and TG infections increased from 2010 to 2022.
                               # Estimates of new infections among MSM and TG in SSA may be suppressed by low PSEs that result in part due to
                               # hostile legislation, stigma, and violence"),
        y = "Percent of total new infections",
      caption = "cite source")
  
  si_save("Images/UNAIDS new infections.png", width = 6, height = 5)
  
  