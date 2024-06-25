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

  #identify burden
  near95 <- c("Botswana", "Eswatini", "Kenya", "Lesotho", "Malawi", "Namibia", "Rwanda", "Zambia", "Zimbabwe")
  near90 <- c("Burundi", "Cameroon", "Ethiopia", "South Africa", "Uganda", "Vietnam")
  below90 <- c("Angola", "Cote d'Ivoire", "Democratic Republic of the Congo", "DRC", "Dominican Republic", "Haiti", "Philippines", "South Sudan", "Ukraine")
  unknown90 <- c("Nigeria", "Tanzania", "Mozambique")
  high_burden <- c("Botswana", "Eswatini", "Kenya", "Lesotho", "Malawi", "Namibia", "Rwanda", "Zambia", "Zimbabwe", "Burundi", "Cameroon", "Ethiopia", "South Africa", "Uganda", "Vietnam", "Angola", "Cote d'Ivoire", "Democratic Republic of the Congo", "DRC", "Dominican Republic", "Haiti", "Philippines", "South Sudan", "Ukraine", "Nigeria", "Tanzania", "Mozambique")
      
# Munge ------------------------------------------------------------------
  
  ni_pepfar <- ea_hts_prev |> select(country, ou, geo) |> 
    # filter(country %in% high_burden) |> 
    inner_join(unaids_ni) |> 
    select(-contains("percent")) |> 
    group_by(geo, 
             fiscal_year
             ) |> 
    summarise(across(c(count_ni_kp:count_ni_msm_tgw), ~ sum(.x, na.rm = TRUE)), .groups = "drop") |> 
    # mutate(geo = factor(geo, levels = c("Sub-Saharan Africa", "Outside Sub-Saharan Africa")),
           # fiscal_year = as.character(fiscal_year)
           # ) |> 
    # pivot_longer(cols = 2:6) |> 
    glimpse()

# visualize  -------------------------------------------------------------------
  ni_pepfar |> 
    mutate(label_mt = case_when(geo == "Sub-Saharan Africa" ~ scales::percent(count_ni_msm_tgw /count_ni_m_f_15_49y, accuracy = 0.1),
                                geo == "Outside Sub-Saharan Africa"  ~ scales::percent(count_ni_msm_tgw /count_ni_m_f_15_49y, accuracy = 1))
           ) |> 
    ggplot(aes(
      x=fiscal_year)) + facet_grid(cols = vars(
        geo)) +
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
              fontface = "bold",
              size = 6) +
    geom_col(aes(y=count_ni_msm_tgw/count_ni_m_f_15_49y), fill = "#f28965", width = 0.7) +
      geom_text(aes(y=count_ni_msm_tgw /count_ni_m_f_15_49y,
                  label = label_mt), 
              position = position_nudge(y=-0.012),
              size = 5,
              color = "#ffffff",
              fontface = "bold") +
    # geom_text(aes(y=count_ni_msm_tgw /count_ni_m_f_15_49y,
    #               label = scales::comma(count_ni_msm_tgw, scale = 10^-3, accuracy = 1, suffix = "k") ), 
    #           position = position_nudge(y=-0.15)) +
    # si_style_ygrid() +
    si_style_xyline()+
    scale_y_continuous(labels = percent_format(), breaks = seq(0, 1, by = 0.1), limits = c(0, .70),
                       expand = c(0, 0)) +
    theme(axis.text.x = element_text(size=12),
          axis.title.x = element_blank(),
          axis.title.y = element_text(size=12),
          axis.text.y = element_blank(),
          
          panel.spacing = unit(8, "lines"),  # Increase space between facets
          strip.text = element_text(size = 12, hjust = 0.5, face = "bold"),
          
          plot.caption = element_markdown(size = 11),
          plot.title = element_markdown(size=18),
          plot.subtitle = element_markdown(size = 12),
          ) +
    labs(
      title = glue::glue("UNAIDS estimates of new infections in PEPFAR OUs, 2010 & 2022"),
      subtitle = "Bars represent the % of total new infections who were KP.<span style='color:#f28965;'> Inner bars shows % who were MSM or TG.</span>",
         # subtitle = glue::glue("Outside of SSA, HIV infections among MSM and TG infections increased from 2010 to 2022.
                               # Estimates of new infections among MSM and TG in SSA may be suppressed by low PSEs that result in part due to
                               # hostile legislation, stigma, and violence"),
        y = "Percent of total new infections",
      caption = glue::glue("Data source:  Korenromp, Sabin, et al. 2023.  |  Ref ID: {ref_id}<br>New HIV infections among key populations & their partners in 2010 and 2022, a multisource estimation."))
  
  si_save("Images/UNAIDS new infections.png", width = 8.5, height = 6)

  