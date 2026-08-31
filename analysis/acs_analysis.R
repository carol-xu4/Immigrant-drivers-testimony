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

write_csv(population_year, "results/driving_pop_by_year.csv")

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
# private motorized vehicle: auto (truck or van), motorcycle
# public transport: bus, trolley, streetcar, light rail, subway, long-distance train or commuter train, ferryboat
# kept taxicab or ride-hailing services as separate from public transport

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

tranwork_by_year = acs %>%
  filter(!is.na()) %>%
  group_by(year, immig_status, ) %>%
  summarise(pop = sum(perwt, na.rm = TRUE), .groups = "drop") %>%
  group_by(year, immig_status) %>%
  mutate(pct = pop / sum(pop) * 100) %>%
  ungroup()

print(tranwork_by_year, n = Inf)

write_csv(tranwork_by_year, "results/tranwork_by_year.csv")

# pct commuting by private motorized vehicle
drive_by_year = tranwork_by_year %>%
  filter( == "Private motorized vehicle") %>%
  select(year, immig_status, pct)

drive_by_year_allimm = acs %>%
  filter(!is.na(), immig_status %in% c("Legal immigrants", "Illegal immigrants")) %>%
  group_by(year) %>%
  summarise(
    immig_status = "All immigrants",
    pct = sum(perwt[ == "Private motorized vehicle"], na.rm = TRUE) / sum(perwt, na.rm = TRUE) * 100,
    .groups = "drop")

drive_by_year = bind_rows(drive_by_year, drive_by_year_allimm) %>%
  mutate(immig_status = factor(immig_status, levels = c(
    "Native-born citizens", "Legal immigrants", "Illegal immigrants", "All immigrants")))

print(drive_by_year, n = Inf)

write_csv(drive_by_year, "results/drive_to_work_by_year.csv")

ggplot(drive_by_year, aes(x = as.numeric(year), y = pct, color = immig_status, linetype = immig_status)) +
  geom_line(linewidth = 1.8) +
  geom_point(size = 2) +
  scale_color_manual(values = colors_4) +
  scale_linetype_manual(values = linetypes_4) +
  scale_x_continuous(breaks = seq(2004, 2024, by = 2), expand = c(0.02, 0)) +
  scale_y_continuous(labels = function(x) paste0(x, "%"), expand = c(0.02, 0), limits = c(70, 90)) +
  labs(
    title = "Share Commuting by Private Motorized Vehicle, by Immigration Status (2004-2024)",
    subtitle = "ACS; workers age 16+, non-GQ",
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

ggsave("results/drive_to_work_by_year.png", width = 15, height = 10)

# 2024 commute breakdown
tranwork_2024 = acs %>%
  filter(year == 2024, !is.na()) %>%
  group_by(immig_status, ) %>%
  summarise(pop = sum(perwt, na.rm = TRUE), .groups = "drop") %>%
  group_by(immig_status) %>%
  mutate(pct = pop / sum(pop) * 100) %>%
  ungroup()

tranwork_2024_allimm = acs %>%
  filter(year == 2024, !is.na(), immig_status %in% c("Legal immigrants", "Illegal immigrants")) %>%
  group_by() %>%
  summarise(pop = sum(perwt, na.rm = TRUE), .groups = "drop") %>%
  mutate(immig_status = "All immigrants",
         pct = pop / sum(pop) * 100)

tranwork_2024 = bind_rows(tranwork_2024, tranwork_2024_allimm) %>%
  mutate(immig_status = factor(immig_status, levels = c(
    "Native-born citizens", "Legal immigrants", "Illegal immigrants", "All immigrants")),
     = factor(, levels = c(
      "Private motorized vehicle", "Public transport", "Taxicab or ride-hailing services",
      "Bicycle", "Walked only", "Other", "Worked at home")))

print(tranwork_2024, n = Inf)

write_csv(tranwork_2024, "results/tranwork_2024.csv")

ggplot(tranwork_2024, aes(x = immig_status, y = pct, fill = )) +
  geom_col(width = 0.6, position = position_stack(reverse = TRUE)) +
  scale_y_continuous(labels = function(x) paste0(x, "%"), expand = c(0, 0)) +
  labs(
    title = "Means of Transportation to Work, by Immigration Status (2024)",
    subtitle = "ACS; workers age 16+, non-GQ",
    x = NULL,
    y = NULL,
    fill = NULL,
    caption = "Source: ACS via IPUMS") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 30, face = "bold", hjust = 0, color = "black"),
    plot.subtitle = element_text(size = 20, color = "gray40", hjust = 0, margin = margin(b = 12)),
    legend.position = "top",
    legend.justification = "left",
    legend.text = element_text(size = 18),
    legend.key.width = unit(1.2, "cm"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(color = "gray90", linewidth = 0.5),
    panel.grid.minor.y = element_blank(),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    axis.text.x = element_text(size = 16, color = "gray40", angle = 20, hjust = 1),
    axis.text.y = element_text(size = 25, color = "gray40"),
    plot.caption = element_text(size = 12, color = "gray40", hjust = 0),
    plot.caption.position = "plot",
    plot.title.position = "plot",
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA))

ggsave("results/tranwork_2024.png", width = 15, height = 10)

# all driving occupations: ambulance drivers (except EMTs), bus drivers, driver/sales workers and truck drivers, taxi drivers and chauffeurs, motor vehicle operators (all other)
# occ2010 universe: age 16+ who had worked within the previous five years
# denominators do not include unemployed
acs = acs %>%
  mutate(
    driving_occ = occ2010 %in% c(9110, 9120, 9130, 9140, 9150),
    truck_driver = occ2010 == 9130)

driving_occ_by_year = acs %>%
  filter(!occ2010 %in% c(9920, 9999)) %>%
  group_by(year, immig_status) %>%
  summarise(
    pop = sum(perwt[driving_occ], na.rm = TRUE),
    total = sum(perwt, na.rm = TRUE),
    pct = pop / total * 100,
    .groups = "drop")

driving_occ_by_year_allimm = acs %>%
  filter(!occ2010 %in% c(9920, 9999), immig_status %in% c("Legal immigrants", "Illegal immigrants")) %>%
  group_by(year) %>%
  summarise(
    immig_status = "All immigrants",
    pop = sum(perwt[driving_occ], na.rm = TRUE),
    total = sum(perwt, na.rm = TRUE),
    pct = pop / total * 100,
    .groups = "drop")

driving_occ_by_year = bind_rows(driving_occ_by_year, driving_occ_by_year_allimm) %>%
  mutate(immig_status = factor(immig_status, levels = c(
    "Native-born citizens", "Legal immigrants", "Illegal immigrants", "All immigrants")))

print(driving_occ_by_year, n = Inf)

write_csv(driving_occ_by_year, "results/driving_occ_share_by_year.csv")

ggplot(driving_occ_by_year, aes(x = as.numeric(year), y = pct, color = immig_status, linetype = immig_status)) +
  geom_line(linewidth = 1.8) +
  geom_point(size = 2) +
  scale_color_manual(values = colors_4) +
  scale_linetype_manual(values = linetypes_4) +
  scale_x_continuous(breaks = seq(2004, 2024, by = 2), expand = c(0.02, 0)) +
  scale_y_continuous(labels = function(x) paste0(x, "%"), expand = c(0.02, 0), limits = c(2, 4.5)) +
  labs(
    title = "Share Working in Driving Occupations, by Immigration Status (2004-2024)",
    subtitle = "ACS; workers age 16+, non-GQ; ambulance/bus/truck/taxi drivers and other motor vehicle operators",
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

ggsave("results/driving_occ_share_by_year.png", width = 15, height = 10)

# truck drivers: OCC2010 == 9130 (Driver/Sales Workers and Truck Drivers)
# occ2010 universe: age 16+ who had worked within the previous five years
# denominators exclude unemployed
truck_driver_by_year = acs %>%
  filter(!occ2010 %in% c(9920, 9999)) %>%
  group_by(year, immig_status) %>%
  summarise(
    pop = sum(perwt[truck_driver], na.rm = TRUE),
    total = sum(perwt, na.rm = TRUE),
    pct = pop / total * 100,
    .groups = "drop")

truck_driver_by_year_allimm = acs %>%
  filter(!occ2010 %in% c(9920, 9999), immig_status %in% c("Legal immigrants", "Illegal immigrants")) %>%
  group_by(year) %>%
  summarise(
    immig_status = "All immigrants",
    pop = sum(perwt[truck_driver], na.rm = TRUE),
    total = sum(perwt, na.rm = TRUE),
    pct = pop / total * 100,
    .groups = "drop")

truck_driver_by_year = bind_rows(truck_driver_by_year, truck_driver_by_year_allimm) %>%
  mutate(immig_status = factor(immig_status, levels = c(
    "Native-born citizens", "Legal immigrants", "Illegal immigrants", "All immigrants")))

print(truck_driver_by_year, n = Inf)

write_csv(truck_driver_by_year, "results/truck_driver_share_by_year.csv")

ggplot(truck_driver_by_year, aes(x = as.numeric(year), y = pct, color = immig_status, linetype = immig_status)) +
  geom_line(linewidth = 1.8) +
  geom_point(size = 2) +
  scale_color_manual(values = colors_4) +
  scale_linetype_manual(values = linetypes_4) +
  scale_x_continuous(breaks = seq(2004, 2024, by = 2), expand = c(0.02, 0)) +
  scale_y_continuous(labels = function(x) paste0(x, "%"), expand = c(0.02, 0), limits = c(1.5, 3)) +
  labs(
    title = "Share Working as Truck Drivers, by Immigration Status (2004-2024)",
    subtitle = "ACS; workers age 16+, non-GQ; Driver/Sales Workers and Truck Drivers (OCC2010 9130)",
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

ggsave("results/truck_driver_share_by_year.png", width = 15, height = 10)

# total number of foreign drivers over time (work commute by private vehicle)
drivers_by_year = acs %>%
  filter(!is.na(tranwork_grp)) %>%
  group_by(year, immig_status) %>%
  summarise(
    drivers = sum(perwt[tranwork_grp == "Private motorized vehicle"], na.rm = TRUE),
    .groups = "drop")

drivers_by_year_allimm = acs %>%
  filter(!is.na(tranwork_grp), immig_status %in% c("Legal immigrants", "Illegal immigrants")) %>%
  group_by(year) %>%
  summarise(
    immig_status = "All immigrants",
    drivers = sum(perwt[tranwork_grp == "Private motorized vehicle"], na.rm = TRUE),
    .groups = "drop")

drivers_by_year = bind_rows(drivers_by_year, drivers_by_year_allimm) %>%
  mutate(immig_status = factor(immig_status, levels = c(
    "Native-born citizens", "Legal immigrants", "Illegal immigrants", "All immigrants")))

print(drivers_by_year, n = Inf)

write_csv(drivers_by_year, "results/drivers_by_year.csv")

ggplot(drivers_by_year %>% filter(immig_status != "Native-born citizens"), aes(x = as.numeric(year), y = drivers, color = immig_status, linetype = immig_status)) +
  geom_line(linewidth = 1.8) +
  geom_point(size = 2) +
  scale_color_manual(values = colors_4) +
  scale_linetype_manual(values = linetypes_4) +
  scale_x_continuous(breaks = seq(2004, 2024, by = 2), expand = c(0.02, 0)) +
  scale_y_continuous(labels = function(x) paste0(x / 1e6, "M"), expand = c(0.02, 0), limits = c(0, 25000000)) +
  labs(
    title = "Number of Foreign-Born Drivers, by Immigration Status (2004-2024)",
    subtitle = "ACS; workers age 16+, non-GQ; commute by private motorized vehicle",
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

ggsave("results/drivers_by_year.png", width = 15, height = 10)

# pct of drivers who are immigrants
# (1) composition of drivers (commute by private motorized vehicle), incl. All immigrants
drivers_composition_by_year = acs %>%
  filter(!is.na(tranwork_grp), tranwork_grp == "Private motorized vehicle") %>%
  group_by(year, immig_status) %>%
  summarise(pop = sum(perwt, na.rm = TRUE), .groups = "drop") %>%
  group_by(year) %>%
  mutate(pct = pop / sum(pop) * 100) %>%
  ungroup()

drivers_composition_by_year_allimm = acs %>%
  filter(!is.na(tranwork_grp), tranwork_grp == "Private motorized vehicle") %>%
  group_by(year) %>%
  summarise(
    total = sum(perwt, na.rm = TRUE),
    pop = sum(perwt[immig_status %in% c("Legal immigrants", "Illegal immigrants")], na.rm = TRUE),
    .groups = "drop") %>%
  mutate(immig_status = "All immigrants", pct = pop / total * 100) %>%
  select(year, immig_status, pop, pct)

drivers_composition_by_year = bind_rows(drivers_composition_by_year, drivers_composition_by_year_allimm) %>%
  mutate(immig_status = factor(immig_status, levels = c(
    "Native-born citizens", "Legal immigrants", "Illegal immigrants", "All immigrants")))

print(drivers_composition_by_year, n = Inf)

write_csv(drivers_composition_by_year, "results/drivers_composition_by_year.csv")

# (2) composition of driving-occupation workers, incl. All immigrants
driving_occ_composition_by_year = acs %>%
  filter(!occ2010 %in% c(9920, 9999), driving_occ) %>%
  group_by(year, immig_status) %>%
  summarise(pop = sum(perwt, na.rm = TRUE), .groups = "drop") %>%
  group_by(year) %>%
  mutate(pct = pop / sum(pop) * 100) %>%
  ungroup()

driving_occ_composition_by_year_allimm = acs %>%
  filter(!occ2010 %in% c(9920, 9999), driving_occ) %>%
  group_by(year) %>%
  summarise(
    total = sum(perwt, na.rm = TRUE),
    pop = sum(perwt[immig_status %in% c("Legal immigrants", "Illegal immigrants")], na.rm = TRUE),
    .groups = "drop") %>%
  mutate(immig_status = "All immigrants", pct = pop / total * 100) %>%
  select(year, immig_status, pop, pct)

driving_occ_composition_by_year = bind_rows(driving_occ_composition_by_year, driving_occ_composition_by_year_allimm) %>%
  mutate(immig_status = factor(immig_status, levels = c(
    "Native-born citizens", "Legal immigrants", "Illegal immigrants", "All immigrants")))

print(driving_occ_composition_by_year, n = Inf)

write_csv(driving_occ_composition_by_year, "results/driving_occ_composition_by_year.csv")

# (3) composition of truck drivers, incl. All immigrants
truck_driver_composition_by_year = acs %>%
  filter(!occ2010 %in% c(9920, 9999), truck_driver) %>%
  group_by(year, immig_status) %>%
  summarise(pop = sum(perwt, na.rm = TRUE), .groups = "drop") %>%
  group_by(year) %>%
  mutate(pct = pop / sum(pop) * 100) %>%
  ungroup()

truck_driver_composition_by_year_allimm = acs %>%
  filter(!occ2010 %in% c(9920, 9999), truck_driver) %>%
  group_by(year) %>%
  summarise(
    total = sum(perwt, na.rm = TRUE),
    pop = sum(perwt[immig_status %in% c("Legal immigrants", "Illegal immigrants")], na.rm = TRUE),
    .groups = "drop") %>%
  mutate(immig_status = "All immigrants", pct = pop / total * 100) %>%
  select(year, immig_status, pop, pct)

truck_driver_composition_by_year = bind_rows(truck_driver_composition_by_year, truck_driver_composition_by_year_allimm) %>%
  mutate(immig_status = factor(immig_status, levels = c(
    "Native-born citizens", "Legal immigrants", "Illegal immigrants", "All immigrants")))

print(truck_driver_composition_by_year, n = Inf)

write_csv(truck_driver_composition_by_year, "results/truck_driver_composition_by_year.csv")

ggplot(drivers_composition_by_year, aes(x = as.numeric(year), y = pct, color = immig_status, linetype = immig_status)) +
  geom_line(linewidth = 1.8) +
  geom_point(size = 2) +
  scale_color_manual(values = colors_4) +
  scale_linetype_manual(values = linetypes_4) +
  scale_x_continuous(breaks = seq(2004, 2024, by = 2), expand = c(0.02, 0)) +
  scale_y_continuous(labels = function(x) paste0(x, "%"), expand = c(0.02, 0), limits = c(0, 100)) +
  labs(
    title = "Composition of Drivers, by Immigration Status (2004-2024)",
    subtitle = "ACS; workers age 16+, non-GQ; commute by private motorized vehicle",
    x = NULL, y = NULL, color = NULL, linetype = NULL,
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

ggsave("results/fig.drivers_composition_by_year.png", width = 15, height = 10)

ggplot(driving_occ_composition_by_year, aes(x = as.numeric(year), y = pct, color = immig_status, linetype = immig_status)) +
  geom_line(linewidth = 1.8) +
  geom_point(size = 2) +
  scale_color_manual(values = colors_4) +
  scale_linetype_manual(values = linetypes_4) +
  scale_x_continuous(breaks = seq(2004, 2024, by = 2), expand = c(0.02, 0)) +
  scale_y_continuous(labels = function(x) paste0(x, "%"), expand = c(0.02, 0), limits = c(0, 100)) +
  labs(
    title = "Composition of Driving-Occupation Workers, by Immigration Status (2004-2024)",
    subtitle = "ACS; workers age 16+, non-GQ; ambulance/bus/truck/taxi drivers and other motor vehicle operators",
    x = NULL, y = NULL, color = NULL, linetype = NULL,
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

ggsave("results/fig.driving_occ_composition_by_year.png", width = 15, height = 10)

ggplot(truck_driver_composition_by_year, aes(x = as.numeric(year), y = pct, color = immig_status, linetype = immig_status)) +
  geom_line(linewidth = 1.8) +
  geom_point(size = 2) +
  scale_color_manual(values = colors_4) +
  scale_linetype_manual(values = linetypes_4) +
  scale_x_continuous(breaks = seq(2004, 2024, by = 2), expand = c(0.02, 0)) +
  scale_y_continuous(labels = function(x) paste0(x, "%"), expand = c(0.02, 0), limits = c(0, 100)) +
  labs(
    title = "Composition of Truck Drivers, by Immigration Status (2004-2024)",
    subtitle = "ACS; workers age 16+, non-GQ; Driver/Sales Workers and Truck Drivers (OCC2010 9130)",
    x = NULL, y = NULL, color = NULL, linetype = NULL,
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

ggsave("results/fig.truck_driver_composition_by_year.png", width = 15, height = 10)

# increase in foreign-born drivers vs. increase in fatal crashes
fatal_crashes = read_csv("data/output/fatalcrashes.csv")

drivers_vs_crashes = drivers_by_year %>%
  filter(immig_status == "All immigrants", year >= 2010) %>%
  select(year, drivers) %>%
  left_join(fatal_crashes, by = "year") %>%
  mutate(
    drivers_index = drivers / drivers[year == 2010] * 100,
    crashes_index = total_crashes / total_crashes[year == 2010] * 100)

print(drivers_vs_crashes)

drivers_vs_crashes_long = drivers_vs_crashes %>%
  select(year, drivers_index, crashes_index) %>%
  pivot_longer(cols = c(drivers_index, crashes_index),
               names_to = "series", values_to = "index") %>%
  mutate(series = recode(series,
    drivers_index = "Foreign-born drivers",
    crashes_index = "Fatal crashes"))

trend_colors = c(
  "Foreign-born drivers" = "#3043B4",
  "Fatal crashes"        = "#C97703")

ggplot(drivers_vs_crashes_long, aes(x = year, y = index, color = series)) +
  geom_line(linewidth = 1.8) +
  geom_point(size = 2) +
  scale_color_manual(values = trend_colors) +
  scale_x_continuous(breaks = seq(2010, 2024, by = 2), expand = c(0.02, 0)) +
  scale_y_continuous(expand = c(0.02, 0)) +
  labs(
    title = "Growth in Foreign-Born Drivers vs. Fatal Crashes (2010 = 100)",
    subtitle = "ACS via IPUMS (drivers, all immigrants); NHTSA (fatal motor vehicle crashes)",
    x = NULL,
    y = NULL,
    color = NULL,
    caption = "Source: ACS via IPUMS; NHTSA") +
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

ggsave("results/drivers_vs_crashes.png", width = 15, height = 10)

# growth year over year
drivers_vs_crashes_yoy = drivers_vs_crashes %>%
  arrange(year) %>%
  mutate(
    drivers_yoy = (drivers / lag(drivers) - 1) * 100,
    crashes_yoy = (total_crashes / lag(total_crashes) - 1) * 100) %>%
  filter(!is.na(drivers_yoy))   # drop 2010, no prior year to compare to

print(drivers_vs_crashes_yoy)

write_csv(drivers_vs_crashes_yoy, "results/drivers_vs_crashes_yoy.csv")

drivers_vs_crashes_yoy_long = drivers_vs_crashes_yoy %>%
  select(year, drivers_yoy, crashes_yoy) %>%
  pivot_longer(cols = c(drivers_yoy, crashes_yoy),
               names_to = "series", values_to = "yoy") %>%
  mutate(series = recode(series,
    drivers_yoy = "Foreign-born drivers",
    crashes_yoy = "Fatal crashes"))

trend_colors = c(
  "Foreign-born drivers" = "#3043B4",
  "Fatal crashes"        = "#C97703")

ggplot(drivers_vs_crashes_yoy_long, aes(x = year, y = yoy, color = series)) +
  geom_hline(yintercept = 0, color = "gray70", linewidth = 0.5) +
  geom_line(linewidth = 1.8) +
  geom_point(size = 2) +
  scale_color_manual(values = trend_colors) +
  scale_x_continuous(breaks = seq(2011, 2024, by = 2), expand = c(0.02, 0)) +
  scale_y_continuous(labels = function(x) paste0(x, "%"), expand = c(0.02, 0)) +
  labs(
    title = "Year-Over-Year Change: Foreign-Born Drivers vs. Fatal Crashes",
    subtitle = "ACS via IPUMS (drivers, all immigrants); NHTSA (fatal motor vehicle crashes)",
    x = NULL,
    y = NULL,
    color = NULL,
    caption = "Source: ACS via IPUMS; NHTSA") +
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

ggsave("results/drivers_vs_crashes_yoy.png", width = 15, height = 10)

# raw values, dual axis: total foreign-born drivers vs. total fatal crashes ---

ratio = max(drivers_vs_crashes$drivers) / max(drivers_vs_crashes$total_crashes)

ggplot(drivers_vs_crashes, aes(x = year)) +
  geom_line(aes(y = drivers, color = "Foreign-born drivers"), linewidth = 1.8) +
  geom_point(aes(y = drivers, color = "Foreign-born drivers"), size = 2) +
  geom_line(aes(y = total_crashes * ratio, color = "Fatal crashes"), linewidth = 1.8) +
  geom_point(aes(y = total_crashes * ratio, color = "Fatal crashes"), size = 2) +
  scale_color_manual(values = trend_colors) +
  scale_x_continuous(breaks = seq(2010, 2024, by = 2), expand = c(0.02, 0)) +
  scale_y_continuous(
    name = NULL,
    labels = function(x) paste0(x / 1e6, "M"),
    expand = c(0.02, 0),
    sec.axis = sec_axis(~ . / ratio, name = NULL, labels = scales::comma)) +
  labs(
    title = "Foreign-Born Drivers vs. Fatal Crashes (2010-2024)",
    subtitle = "Left axis: foreign-born drivers (ACS via IPUMS). Right axis: fatal motor vehicle crashes (NHTSA)",
    x = NULL,
    color = NULL,
    caption = "Source: ACS via IPUMS; NHTSA") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 30, face = "bold", hjust = 0, color = "black"),
    plot.subtitle = element_text(size = 18, color = "gray40", hjust = 0, margin = margin(b = 12)),
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
    axis.text.y.left = element_text(size = 22, color = trend_colors["Foreign-born drivers"]),
    axis.text.y.right = element_text(size = 22, color = trend_colors["Fatal crashes"]),
    plot.caption = element_text(size = 12, color = "gray40", hjust = 0),
    plot.caption.position = "plot",
    plot.title.position = "plot",
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA))

ggsave("results/drivers_vs_crashes_raw.png", width = 15, height = 10)

# total drivers (native-born + immigrants combined) vs. fatal crashes 

total_drivers_by_year = drivers_by_year %>%
  filter(immig_status %in% c("Native-born citizens", "Legal immigrants", "Illegal immigrants")) %>%
  group_by(year) %>%
  summarise(drivers = sum(drivers, na.rm = TRUE), .groups = "drop")

total_drivers_vs_crashes = total_drivers_by_year %>%
  filter(year >= 2010) %>%
  left_join(fatal_crashes, by = "year") %>%
  mutate(
    drivers_index = drivers / drivers[year == 2010] * 100,
    crashes_index = total_crashes / total_crashes[year == 2010] * 100)

print(total_drivers_vs_crashes)

write_csv(total_drivers_vs_crashes, "results/total_drivers_vs_crashes.csv")

total_drivers_vs_crashes_long = total_drivers_vs_crashes %>%
  select(year, drivers_index, crashes_index) %>%
  pivot_longer(cols = c(drivers_index, crashes_index),
               names_to = "series", values_to = "index") %>%
  mutate(series = recode(series,
    drivers_index = "All drivers",
    crashes_index = "Fatal crashes"))

trend_colors_all = c(
  "All drivers"   = "#3043B4",
  "Fatal crashes" = "#C97703")

ratio_all = max(total_drivers_vs_crashes$drivers) / max(total_drivers_vs_crashes$total_crashes)

ggplot(total_drivers_vs_crashes, aes(x = year)) +
  geom_line(aes(y = drivers, color = "All drivers"), linewidth = 1.8) +
  geom_point(aes(y = drivers, color = "All drivers"), size = 2) +
  geom_line(aes(y = total_crashes * ratio_all, color = "Fatal crashes"), linewidth = 1.8) +
  geom_point(aes(y = total_crashes * ratio_all, color = "Fatal crashes"), size = 2) +
  scale_color_manual(values = trend_colors_all) +
  scale_x_continuous(breaks = seq(2010, 2024, by = 2), expand = c(0.02, 0)) +
  scale_y_continuous(
    name = NULL,
    labels = function(x) paste0(x / 1e6, "M"),
    expand = c(0.02, 0),
    sec.axis = sec_axis(~ . / ratio_all, name = NULL, labels = scales::comma)) +
  labs(
    title = "All Drivers vs. Fatal Crashes (2010-2024)",
    subtitle = "Left axis: all drivers, native-born + immigrant (ACS via IPUMS). Right axis: fatal motor vehicle crashes (NHTSA)",
    x = NULL,
    color = NULL,
    caption = "Source: ACS via IPUMS; NHTSA") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 30, face = "bold", hjust = 0, color = "black"),
    plot.subtitle = element_text(size = 18, color = "gray40", hjust = 0, margin = margin(b = 12)),
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
    axis.text.y.left = element_text(size = 22, color = trend_colors_all["All drivers"]),
    axis.text.y.right = element_text(size = 22, color = trend_colors_all["Fatal crashes"]),
    plot.caption = element_text(size = 12, color = "gray40", hjust = 0),
    plot.caption.position = "plot",
    plot.title.position = "plot",
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA))

ggsave("results/total_drivers_vs_crashes_raw.png", width = 15, height = 10)

# transportation time to work (persons age 16+ who worked outside the home)
drive_time_by_year = acs %>%
  filter(tranwork_grp == "Private motorized vehicle",
         !is.na(trantime), !trantime %in% c(0, 888)) %>%
  group_by(year, immig_status) %>%
  summarise(avg_commute = weighted.mean(trantime, w = perwt, na.rm = TRUE),
            .groups = "drop")
 
drive_time_by_year_allimm = acs %>%
  filter(tranwork_grp == "Private motorized vehicle",
         !is.na(trantime), !trantime %in% c(0, 888),
         immig_status %in% c("Legal immigrants", "Illegal immigrants")) %>%
  group_by(year) %>%
  summarise(avg_commute = weighted.mean(trantime, w = perwt, na.rm = TRUE),
            .groups = "drop") %>%
  mutate(immig_status = "All immigrants")

drive_time_by_year = bind_rows(drive_time_by_year, drive_time_by_year_allimm)

print(drive_time_by_year, n = Inf)

ggplot(drive_time_by_year, aes(x = year, y = avg_commute,
                                color = immig_status, linetype = immig_status)) +
  geom_line(linewidth = 1.8) +
  geom_point(size = 2) +
  scale_color_manual(values = colors_4) +
  scale_linetype_manual(values = linetypes_4) +
  scale_x_continuous(breaks = seq(2004, 2024, by = 2), expand = c(0.02, 0)) +
  scale_y_continuous(name = NULL, labels = scales::comma, expand = c(0.02, 0)) +
  labs(
    title = "Average Commute Time for Those Who Drive to Work, by Nativity",
    subtitle = "Minutes, among workers who commute by private motorized vehicle (ACS via IPUMS)",
    x = NULL,
    color = NULL,
    linetype = NULL,
    caption = "Source: ACS via IPUMS") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 30, face = "bold", hjust = 0, color = "black"),
    plot.subtitle = element_text(size = 18, color = "gray40", hjust = 0, margin = margin(b = 12)),
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
    axis.text.y = element_text(size = 22, color = "gray40"),
    plot.caption = element_text(size = 12, color = "gray40", hjust = 0),
    plot.caption.position = "plot",
    plot.title.position = "plot",
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA))

ggsave("results/drive_time_by_year.png", width = 15, height = 10)
