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

cou_string <- paste0(paste(compare[1:11], collapse = ", "), ",", "\n", paste(compare[12:18], collapse = ", "))
cou_string <- paste0(cou_string, ".")

my_colors <-  c("grey50", "firebrick")

df <- read_csv(here("data", "HEALTH_STAT_29032023020109792.csv")) |>
  janitor::clean_names()

out <- df |>
  arrange(country, year) |>
  group_by(country) |>
  mutate(us_flag = ifelse(cou == "USA", "United States", "Eighteen other OECD Countries"),
         us_flag = factor(us_flag, levels = c("United States", "Eighteen other OECD Countries"),
                          ordered = TRUE),
         avg_rate = slider::slide_dbl(value, mean, .before = 3)) |>
  filter(cou %in% compare) |>
  ggplot(aes(x = year, y = value, group = country, color = us_flag)) +
  geom_line() +
  scale_color_manual(values = rev(my_colors)) +
  labs(color = NULL,
       title = "Assault Death Rates in the OECD, 1960-2020",
       x = "Year",
       y = "Assault deaths per 100,000 population (standardized rates)",
       caption = paste("Countries: ", cou_string, "Data: OECD. Graph: @kjhealy"))

ggsave(here::here("figures", "oecd-assault.pdf"), out, height = 8, width = 10)
ggsave(here::here("figures", "oecd-assault.png"), out, height = 8, width = 10, bg = "white")
