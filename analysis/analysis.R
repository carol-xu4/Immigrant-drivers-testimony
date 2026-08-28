## Preliminaries -----------------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggthemes, readxl, data.table, gdata, ipumsr)

# Set working directory 
setwd("C:/Users/CarolXu/OneDrive - Cato Institute/Desktop/Drivers")

# ACS data -----------------------------------------------------------------
acs = fread("data/output/acs_drivers.csv")

# ANALYSIS -----------------------------------------------------------------

# ggplot color keys
colors_4 = c(
  "Native-born citizens" = "#3043B4",
  "Legal immigrants"     = "#7C756D",
  "Illegal immigrants"   = "#C97703",
  "All immigrants"       = "#0D0E51")

linetypes_4 = c(
  "Native-born citizens" = "solid",
  "Legal immigrants"     = "solid",
  "Illegal immigrants"   = "solid",
  "All immigrants"       = "dotted")

# total population (driving age 16+)
population_year = acs %>%
    group_by(year, immig_status) %>%
    summarise(
        n = n(),
        population = sum(perwt, na.rm = TRUE)) %>% ungroup()

print(population_year, n = Inf)

write_csv(population_year, "results/driving_pop_by_year")

# number of vehicles per household (household head's immigration status)
# [VEHICLES] reports the number of cars, vans, and trucks of one-ton capacity or less kept at home for use by household members
acs = acs %>%
  mutate(vehicles_n = case_when(
    vehicles == 0 ~ NA_real_,   # N/A - vacant unit or GQ
    vehicles == 9 ~ 0,          # "No vehicles available" -> 0
    TRUE ~ as.numeric(vehicles)))

vehicles_by_year = acs %>%
  filter(relate == 1) %>%
  group_by(year, immig_status) %>%
  summarise(
    n_hh = n(),
    households = sum(hhwt, na.rm = TRUE),
    avg_cars = weighted.mean(vehicles_n, w = hhwt, na.rm = TRUE),
    .groups = "drop")

vehicles_by_year_allimm = acs %>%
  filter(relate == 1, immig_status %in% c("Legal immigrants", "Illegal immigrants")) %>%
  group_by(year) %>%
  summarise(
    immig_status = "All immigrants",
    avg_cars = weighted.mean(vehicles_n, w = hhwt, na.rm = TRUE),
    households = sum(hhwt, na.rm = TRUE),
    .groups = "drop")

vehicles_by_year = bind_rows(vehicles_by_year, vehicles_by_year_allimm) %>%
  mutate(immig_status = factor(immig_status, levels = c(
    "Native-born citizens", "Legal immigrants", "Illegal immigrants", "All immigrants")))

print(vehicles_by_year, n = Inf)

write_csv(vehicles_by_year, "results/vehicles_by_year.csv")

ggplot(vehicles_by_year, aes(x = as.numeric(year), y = avg_cars, color = immig_status, linetype = immig_status)) +
  geom_line(linewidth = 1.8) +
  geom_point(size = 2) +
  scale_color_manual(values = colors_4) +
  scale_linetype_manual(values = linetypes_4) +
  scale_x_continuous(breaks = seq(2004, 2024, by = 2), expand = c(0.02, 0)) +
  scale_y_continuous(expand = c(0.02, 0), limits = c(1.25, 2)) +
  labs(
    title = "Average Household Vehicles by Immigration Status (2010-2024)",
    subtitle = "ACS; households only, non-GQ, driving age 16+; household head's status",
    x = NULL,
    y = NULL,
    color = NULL,
    linetype = NULL,
    caption = "Source: ACS via IPUMS") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 30, face = "bold", hjust = 0, color = "black"),
    plot.subtitle = element_text(size = 20, color = "gray40", hjust = 0, margin = margin(b = 12)),
    legend.position = "top",
    legend.justification = "left",
    legend.text = element_text(size = 20),
    legend.key.width = unit(1.5, "cm"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(color = "gray90", linewidth = 0.5),
    panel.grid.minor.y = element_blank(),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    axis.text.x = element_text(size = 25, color = "gray40"),
    axis.text.y = element_text(size = 25, color = "gray40"),
    plot.caption = element_text(size = 12, color = "gray40", hjust = 0),
    plot.caption.position = "plot",
    plot.title.position = "plot",
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA))

ggsave("results/vehicles_by_year.png", width = 15, height = 10)

# share of households with zero vehicles, by immigration status, 
zero_veh_by_year = acs %>%
  filter(relate == 1) %>%
  mutate(no_vehicle = vehicles_n == 0) %>%
  group_by(year, immig_status) %>%
  summarise(
    pct = sum(hhwt[no_vehicle], na.rm = TRUE) / sum(hhwt, na.rm = TRUE) * 100,
    .groups = "drop")

zero_veh_by_year_allimm = acs %>%
  filter(relate == 1, immig_status %in% c("Legal immigrants", "Illegal immigrants")) %>%
  mutate(no_vehicle = vehicles_n == 0) %>%
  group_by(year) %>%
  summarise(
    immig_status = "All immigrants",
    pct = sum(hhwt[no_vehicle], na.rm = TRUE) / sum(hhwt, na.rm = TRUE) * 100,
    .groups = "drop")

zero_veh_by_year = bind_rows(zero_veh_by_year, zero_veh_by_year_allimm) %>%
  mutate(immig_status = factor(immig_status, levels = c(
    "Native-born citizens", "Legal immigrants", "Illegal immigrants", "All immigrants")))

print(zero_veh_by_year, n = Inf)

write_csv(zero_veh_by_year, "results/zero_vehicle_households_by_year.csv")

ggplot(zero_veh_by_year, aes(x = as.numeric(year), y = pct, color = immig_status, linetype = immig_status)) +
  geom_line(linewidth = 1.8) +
  geom_point(size = 2) +
  scale_color_manual(values = colors_4) +
  scale_linetype_manual(values = linetypes_4) +
  scale_x_continuous(breaks = seq(2004, 2024, by = 2), expand = c(0.02, 0)) +
  scale_y_continuous(labels = function(x) paste0(x, "%"), expand = c(0.02, 0), limits = c(7, 18)) +
  labs(
    title = "Share of Households With No Vehicle, by Immigration Status (2004-2024)",
    subtitle = "ACS; households only, non-GQ, driving age 16+; reference person's status",
    x = NULL,
    y = NULL,
    color = NULL,
    linetype = NULL,
    caption = "Source: ACS via IPUMS") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 30, face = "bold", hjust = 0, color = "black"),
    plot.subtitle = element_text(size = 20, color = "gray40", hjust = 0, margin = margin(b = 12)),
    legend.position = "top",
    legend.justification = "left",
    legend.text = element_text(size = 20),
    legend.key.width = unit(1.5, "cm"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(color = "gray90", linewidth = 0.5),
    panel.grid.minor.y = element_blank(),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    axis.text.x = element_text(size = 25, color = "gray40"),
    axis.text.y = element_text(size = 25, color = "gray40"),
    plot.caption = element_text(size = 12, color = "gray40", hjust = 0),
    plot.caption.position = "plot",
    plot.title.position = "plot",
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA))

ggsave("results/zero_vehicle_households_by_year.png", width = 15, height = 10)

# method of transportation to work (16+ people who worked last week)
acs = acs %>%
  mutate(tranwork_grp = case_when(
    tranwork %in% c(10, 11, 12, 13, 14, 15, 20) ~ "Private motorized vehicle",
    tranwork %in% c(31, 32, 33, 34, 35, 36, 37, 39) ~ "Public transport",
    tranwork == 38 ~ "Taxicab or ride-hailing services",
    tranwork == 50 ~ "Bicycle",
    tranwork == 60 ~ "Walked only",
    tranwork == 70 ~ "Other",
    tranwork == 80 ~ "Worked at home",
    TRUE ~ NA_character_))     # 0 = N/A, not a worker last week

