library(tidyverse)
library(rvest)

url <- "https://nssdc.gsfc.nasa.gov/planetary/factsheet/index.html"

planet_data <- xml2::read_html(url) %>%
  html_node("table") %>%
  html_table() %>%
  janitor::row_to_names(1) %>%
  janitor::clean_names() %>%
  filter(x != "") %>%
  separate(x, into = c("stat", "unit"), sep = "\\(") %>%
  mutate(unit = str_remove(unit, "\\)")) %>%
  select(-unit) %>%
  pivot_longer(-stat, names_to = "planet", values_to = "value") %>%
  pivot_wider(names_from = stat, values_from = value) %>%
  janitor::clean_names() %>%
  mutate(
    ring_system = case_when(
      ring_system == "Yes" ~ TRUE,
      ring_system == "No" ~ FALSE,
      TRUE ~ NA),
    global_magnetic_field = case_when(
      global_magnetic_field == "Yes" ~ TRUE,
      global_magnetic_field == "No" ~ FALSE,
      TRUE ~ NA),
    planet = str_to_sentence(planet)) %>%
  mutate_at(vars(-planet, -ring_system, -global_magnetic_field), 
            ~as.numeric(str_remove_all(., "\\*|\\,")))

planet_data_2 <- planet_data %>%
filter(planet != "Moon") %>%
  mutate(au = distance_from_sun / planet_data[planet_data$planet == "Earth", "distance_from_sun"][[1]],
         radius = diameter / 2,
         orbit_in_hours = orbital_period * 24)

write_excel_csv(planet_data, "data/planet_data.csv")
write_rds(planet_data_2, "data/planet_data.RDS")
