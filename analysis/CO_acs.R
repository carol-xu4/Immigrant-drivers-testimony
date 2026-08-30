## Preliminaries -----------------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggthemes, readxl, data.table, gdata, ipumsr)

# Set working directory 
setwd("C:/Users/CarolXu/OneDrive - Cato Institute/Desktop/Drivers")

# acs_co data -----------------------------------------------------------------
acs_co = fread("data/output/acs_drivers.csv") %>%
  filter(statefip == 8)   # Colorado

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
population_year = acs_co %>%
    group_by(year, immig_status) %>%
    summarise(
        n = n(),
        population = sum(perwt, na.rm = TRUE)) %>% ungroup()

print(population_year, n = Inf)

write_csv(population_year, "results/co_driving_pop_by_year.csv")

# number of vehicles per household (household head's immigration status)
# [VEHICLES] reports the number of cars, vans, and trucks of one-ton capacity or less kept at home for use by household members
acs_co = acs_co %>%
  mutate(vehicles_n = case_when(
    vehicles == 0 ~ NA_real_,   # N/A - vacant unit or GQ
    vehicles == 9 ~ 0,          # "No vehicles available" -> 0
    TRUE ~ as.numeric(vehicles)))

vehicles_by_year = acs_co %>%
  filter(relate == 1) %>%
  group_by(year, immig_status) %>%
  summarise(
    n_hh = n(),
    households = sum(hhwt, na.rm = TRUE),
    avg_cars = weighted.mean(vehicles_n, w = hhwt, na.rm = TRUE),
    .groups = "drop")

vehicles_by_year_allimm = acs_co %>%
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

write_csv(vehicles_by_year, "results/co_vehicles_by_year.csv")

ggplot(vehicles_by_year, aes(x = as.numeric(year), y = avg_cars, color = immig_status, linetype = immig_status)) +
  geom_line(linewidth = 1.8) +
  geom_point(size = 2) +
  scale_color_manual(values = colors_4) +
  scale_linetype_manual(values = linetypes_4) +
  scale_x_continuous(breaks = seq(2004, 2024, by = 2), expand = c(0.02, 0)) +
  scale_y_continuous(expand = c(0.02, 0), limits = c(1.5, 2.2)) +
  labs(
    title = "Average Household Vehicles by Immigration Status, Colorado (2004-2024)",
    subtitle = "acs_co; households only, non-GQ, driving age 16+; household head's status",
    x = NULL,
    y = NULL,
    color = NULL,
    linetype = NULL,
    caption = "Source: acs_co via IPUMS") +
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

ggsave("results/co_vehicles_by_year.png", width = 15, height = 10)

# share of households with zero vehicles, by immigration status
zero_veh_by_year = acs_co %>%
  filter(relate == 1) %>%
  mutate(no_vehicle = vehicles_n == 0) %>%
  group_by(year, immig_status) %>%
  summarise(
    pct = sum(hhwt[no_vehicle], na.rm = TRUE) / sum(hhwt, na.rm = TRUE) * 100,
    .groups = "drop")

zero_veh_by_year_allimm = acs_co %>%
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

write_csv(zero_veh_by_year, "results/co_zero_vehicle_households_by_year.csv")

ggplot(zero_veh_by_year, aes(x = as.numeric(year), y = pct, color = immig_status, linetype = immig_status)) +
  geom_line(linewidth = 1.8) +
  geom_point(size = 2) +
  scale_color_manual(values = colors_4) +
  scale_linetype_manual(values = linetypes_4) +
  scale_x_continuous(breaks = seq(2004, 2024, by = 2), expand = c(0.02, 0)) +
  scale_y_continuous(labels = function(x) paste0(x, "%"), expand = c(0.02, 0), limits = c(4.5, 12)) +
  labs(
    title = "Share of Households With No Vehicle, by Immigration Status, Colorado (2004-2024)",
    subtitle = "acs_co; households only, non-GQ, driving age 16+; reference person's status",
    x = NULL,
    y = NULL,
    color = NULL,
    linetype = NULL,
    caption = "Source: acs_co via IPUMS") +
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

ggsave("results/co_zero_vehicle_households_by_year.png", width = 15, height = 10)

# method of transportation to work (16+ people who worked last week)
acs_co = acs_co %>%
  mutate(tranwork_grp = case_when(
    tranwork %in% c(10, 11, 12, 13, 14, 15, 20) ~ "Private motorized vehicle",
    tranwork %in% c(31, 32, 33, 34, 35, 36, 37, 39) ~ "Public transport",
    tranwork == 38 ~ "Taxicab or ride-hailing services",
    tranwork == 50 ~ "Bicycle",
    tranwork == 60 ~ "Walked only",
    tranwork == 70 ~ "Other",
    tranwork == 80 ~ "Worked at home",
    TRUE ~ NA_character_))     # 0 = N/A, not a worker last week

tranwork_by_year = acs_co %>%
  filter(!is.na(tranwork_grp)) %>%
  group_by(year, immig_status, tranwork_grp) %>%
  summarise(pop = sum(perwt, na.rm = TRUE), .groups = "drop") %>%
  group_by(year, immig_status) %>%
  mutate(pct = pop / sum(pop) * 100) %>%
  ungroup()

print(tranwork_by_year, n = Inf)

write_csv(tranwork_by_year, "results/co_tranwork_by_year.csv")

# pct commuting by private motorized vehicle
drive_by_year = tranwork_by_year %>%
  filter(tranwork_grp == "Private motorized vehicle") %>%
  select(year, immig_status, pct)

drive_by_year_allimm = acs_co %>%
  filter(!is.na(tranwork_grp), immig_status %in% c("Legal immigrants", "Illegal immigrants")) %>%
  group_by(year) %>%
  summarise(
    immig_status = "All immigrants",
    pct = sum(perwt[tranwork_grp == "Private motorized vehicle"], na.rm = TRUE) / sum(perwt, na.rm = TRUE) * 100,
    .groups = "drop")

drive_by_year = bind_rows(drive_by_year, drive_by_year_allimm) %>%
  mutate(immig_status = factor(immig_status, levels = c(
    "Native-born citizens", "Legal immigrants", "Illegal immigrants", "All immigrants")))

print(drive_by_year, n = Inf)

write_csv(drive_by_year, "results/co_drive_to_work_by_year.csv")

ggplot(drive_by_year, aes(x = as.numeric(year), y = pct, color = immig_status, linetype = immig_status)) +
  geom_line(linewidth = 1.8) +
  geom_point(size = 2) +
  scale_color_manual(values = colors_4) +
  scale_linetype_manual(values = linetypes_4) +
  scale_x_continuous(breaks = seq(2004, 2024, by = 2), expand = c(0.02, 0)) +
  scale_y_continuous(labels = function(x) paste0(x, "%"), expand = c(0.02, 0), limits = c(70, 90)) +
  labs(
    title = "Share Commuting by Private Motorized Vehicle, by Immigration Status, Colorado (2004-2024)",
    subtitle = "acs_co; workers age 16+, non-GQ",
    x = NULL,
    y = NULL,
    color = NULL,
    linetype = NULL,
    caption = "Source: acs_co via IPUMS") +
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

ggsave("results/co_drive_to_work_by_year.png", width = 15, height = 10)

# 2024 commute breakdown
tranwork_2024 = acs_co %>%
  filter(year == 2024, !is.na(tranwork_grp)) %>%
  group_by(immig_status, tranwork_grp) %>%
  summarise(pop = sum(perwt, na.rm = TRUE), .groups = "drop") %>%
  group_by(immig_status) %>%
  mutate(pct = pop / sum(pop) * 100) %>%
  ungroup()

tranwork_2024_allimm = acs_co %>%
  filter(year == 2024, !is.na(tranwork_grp), immig_status %in% c("Legal immigrants", "Illegal immigrants")) %>%
  group_by(tranwork_grp) %>%
  summarise(pop = sum(perwt, na.rm = TRUE), .groups = "drop") %>%
  mutate(immig_status = "All immigrants",
         pct = pop / sum(pop) * 100)

tranwork_2024 = bind_rows(tranwork_2024, tranwork_2024_allimm) %>%
  mutate(immig_status = factor(immig_status, levels = c(
    "Native-born citizens", "Legal immigrants", "Illegal immigrants", "All immigrants")),
    tranwork_grp = factor(tranwork_grp, levels = c(
      "Private motorized vehicle", "Public transport", "Taxicab or ride-hailing services",
      "Bicycle", "Walked only", "Other", "Worked at home")))

print(tranwork_2024, n = Inf)

write_csv(tranwork_2024, "results/co_tranwork_2024.csv")

ggplot(tranwork_2024, aes(x = immig_status, y = pct, fill = tranwork_grp)) +
  geom_col(width = 0.6, position = position_stack(reverse = TRUE)) +
  scale_y_continuous(labels = function(x) paste0(x, "%"), expand = c(0, 0)) +
  labs(
    title = "Means of Transportation to Work, by Immigration Status, Colorado (2024)",
    subtitle = "acs_co; workers age 16+, non-GQ",
    x = NULL,
    y = NULL,
    fill = NULL,
    caption = "Source: acs_co via IPUMS") +
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

ggsave("results/co_tranwork_2024.png", width = 15, height = 10)

# all driving occupations
acs_co = acs_co %>%
  mutate(
    driving_occ = occ2010 %in% c(9110, 9120, 9130, 9140, 9150),
    truck_driver = occ2010 == 9130)

driving_occ_by_year = acs_co %>%
  filter(!occ2010 %in% c(9920, 9999)) %>%
  group_by(year, immig_status) %>%
  summarise(
    pop = sum(perwt[driving_occ], na.rm = TRUE),
    total = sum(perwt, na.rm = TRUE),
    pct = pop / total * 100,
    .groups = "drop")

driving_occ_by_year_allimm = acs_co %>%
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

write_csv(driving_occ_by_year, "results/co_driving_occ_share_by_year.csv")

ggplot(driving_occ_by_year, aes(x = as.numeric(year), y = pct, color = immig_status, linetype = immig_status)) +
  geom_line(linewidth = 1.8) +
  geom_point(size = 2) +
  scale_color_manual(values = colors_4) +
  scale_linetype_manual(values = linetypes_4) +
  scale_x_continuous(breaks = seq(2004, 2024, by = 2), expand = c(0.02, 0)) +
  scale_y_continuous(labels = function(x) paste0(x, "%"), expand = c(0.02, 0), limits = c(1,5.25)) +
  labs(
    title = "Share Working in Driving Occupations, by Immigration Status, Colorado (2004-2024)",
    subtitle = "acs_co; workers age 16+, non-GQ; ambulance/bus/truck/taxi drivers and other motor vehicle operators",
    x = NULL,
    y = NULL,
    color = NULL,
    linetype = NULL,
    caption = "Source: acs_co via IPUMS") +
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

ggsave("results/co_driving_occ_share_by_year.png", width = 15, height = 10)

# truck drivers: OCC2010 == 9130
truck_driver_by_year = acs_co %>%
  filter(!occ2010 %in% c(9920, 9999)) %>%
  group_by(year, immig_status) %>%
  summarise(
    pop = sum(perwt[truck_driver], na.rm = TRUE),
    total = sum(perwt, na.rm = TRUE),
    pct = pop / total * 100,
    .groups = "drop")

truck_driver_by_year_allimm = acs_co %>%
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

write_csv(truck_driver_by_year, "results/co_truck_driver_share_by_year.csv")

ggplot(truck_driver_by_year, aes(x = as.numeric(year), y = pct, color = immig_status, linetype = immig_status)) +
  geom_line(linewidth = 1.8) +
  geom_point(size = 2) +
  scale_color_manual(values = colors_4) +
  scale_linetype_manual(values = linetypes_4) +
  scale_x_continuous(breaks = seq(2004, 2024, by = 2), expand = c(0.02, 0)) +
  scale_y_continuous(labels = function(x) paste0(x, "%"), expand = c(0.02, 0), limits = c(1.15, 3.5)) +
  labs(
    title = "Share Working as Truck Drivers, by Immigration Status, Colorado (2004-2024)",
    subtitle = "acs_co; workers age 16+, non-GQ; Driver/Sales Workers and Truck Drivers (OCC2010 9130)",
    x = NULL,
    y = NULL,
    color = NULL,
    linetype = NULL,
    caption = "Source: acs_co via IPUMS") +
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

ggsave("results/co_truck_driver_share_by_year.png", width = 15, height = 10)

# total number of drivers
drivers_by_year_co = acs %>%
  filter(statefip == 8, !is.na(tranwork_grp)) %>%
  group_by(year, immig_status) %>%
  summarise(
    drivers = sum(perwt[tranwork_grp == "Private motorized vehicle"], na.rm = TRUE),
    .groups = "drop")

drivers_by_year_co_allimm = acs %>%
  filter(statefip == 8, !is.na(tranwork_grp), immig_status %in% c("Legal immigrants", "Illegal immigrants")) %>%
  group_by(year) %>%
  summarise(
    immig_status = "All immigrants",
    drivers = sum(perwt[tranwork_grp == "Private motorized vehicle"], na.rm = TRUE),
    .groups = "drop")

drivers_by_year_co = bind_rows(drivers_by_year_co, drivers_by_year_co_allimm) %>%
  mutate(immig_status = factor(immig_status, levels = c(
    "Native-born citizens", "Legal immigrants", "Illegal immigrants", "All immigrants")))

print(drivers_by_year_co, n = Inf)

write_csv(drivers_by_year_co, "results/co_drivers_by_year.csv")

ggplot(drivers_by_year_co %>% filter(immig_status != "Native-born citizens"), aes(x = as.numeric(year), y = drivers, color = immig_status, linetype = immig_status)) +
  geom_line(linewidth = 1.8) +
  geom_point(size = 2) +
  scale_color_manual(values = colors_4) +
  scale_linetype_manual(values = linetypes_4) +
  scale_x_continuous(breaks = seq(2004, 2024, by = 2), expand = c(0.02, 0)) +
  scale_y_continuous(labels = function(x) paste0(x / 1e3, "K"), expand = c(0.02, 0)) +
  labs(
    title = "Number of Foreign-Born Drivers, Colorado (2004-2024)",
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

ggsave("results/co_drivers_by_year.png", width = 15, height = 10)
