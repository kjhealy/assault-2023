## OECD Assault Deaths

library(tidyverse)
library(here)

library(showtext)
showtext_opts(dpi = 300)
showtext_auto()

library(myriad)
import_myriad_semi()
import_myriad_condensed()
theme_set(theme_myriad_semi())


compare <- c("AUT", "AUS", "BEL", "CAN", "DEU", "DNK", "ESP", "FIN", "FRA",
             "GBR", "GRC", "IRL", "ITA", "JPN", "NLD", "NOR", "NZL",
             "SWE", "USA")

my_colors <-  c("grey50", "firebrick")

df <- read_csv(here("data", "HEALTH_STAT_29032023020109792.csv")) |>
  janitor::clean_names()

out <- df |>
  arrange(country, year) |>
  group_by(country) |>
  mutate(us_flag = ifelse(cou == "USA", "United States", "Eighteen other OECD Countries"),
         avg_rate = slider::slide_dbl(value, mean, .before = 3)) |>
  filter(cou %in% compare) |>
  ggplot(aes(x = year, y = value, group = country, color = us_flag)) +
  geom_line() +
  scale_color_manual(values = my_colors) +
  labs(color = NULL,
       title = "Assault Death Rates in the OECD, 1960-2020",
       x = "Year",
       y = "Assault deaths per 100,000 population (standardized rates)",
       caption = "Data: OECD. Graph: @kjhealy")

ggsave(here::here("figures", "oecd-assault.pdf"), out, height = 8, width = 10)
ggsave(here::here("figures", "oecd-assault.png"), out, height = 8, width = 10, bg = "white")
