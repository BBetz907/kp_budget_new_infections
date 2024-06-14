library(readxl)
library(gagglr)
library(tidyverse)

region_order <- c("Latin America",
                  "Eastern and Southern Africa",
                  "Western and Central Africa",
                  "Eastern Europe and Central Asia",
                  "Caribbean",
                  "Middle East and North Africa",
                  "Asia and the Pacific",
                  "Low- and middle-income countries"
)

spend_and_need <- read_excel("C:/Users/bbetz/Downloads/UNAIDS Financial estimates and projections.xlsx") |> 
  janitor::clean_names() |>  arrange(desc(indicator)) |> 
  mutate(indicator = fct_relevel(indicator, c("HIV spending on KP", "Estimate total share needed for key populations")),
        indicator_full = str_c(indicator, " (", as.character(year), ")"),
        region = fct_relevel(comparison, region_order),
        # region_2 = str_wrap(levels(region), 12)
        )

levels(spend_and_need$region) <- str_wrap(levels(spend_and_need$region), 10)
custom_colors <- c(denim, golden_sand)

spend_and_need |>  ggplot(aes(x=indicator, y=percent, fill = indicator_full)) +
  geom_col(width = 1) + 
  geom_text(aes(label=scales::percent(percent, accuracy = 1)), vjust = -0.5) +
  facet_grid(col = vars(region), switch = "x") + 
  scale_y_continuous(labels = scales::percent_format(), limits = c(0, 0.5)) +
  scale_fill_manual(values = custom_colors, 
                    breaks = levels(fct_relevel(spend_and_need$indicator_full,
                                    c("HIV spending on KP (2022)", "Estimate total share needed for key populations (2025)")))
                    ) +
  si_style_yline() +
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.title = element_blank(),
        legend.position = "bottom",
        panel.spacing = unit(0.5, "lines"),
        strip.text.x = element_text(size = 9, angle = 0, hjust = 0.5, vjust = 01),  # Align text
        )

ggsave(plot = last_plot(), filename = "Images/kp_by_region.png", width = 8, height = 4.5)
