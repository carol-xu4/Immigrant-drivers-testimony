## Preliminaries -----------------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggthemes, readxl, data.table, gdata, ipumsr)

# Set working directory 
setwd("C:/Users/CarolXu/OneDrive - Cato Institute/Desktop/Drivers")

# California deaths data ---------------------------------------------------
ca_deaths = read_csv("data/input/Cal-ViDa_Death_08292026.csv") %>%
    select(-Type_of_Event, -State_of_Death, -Residence_or_Place_of_Death, -Last_Data_Refresh)

ca_deaths = ca_deaths %>%
    rename(
    year          = Year_of_Death,
    nativity      = US_or_Foreign_Born,
    COD           = Cause_of_Death,
    total_deaths  = Total_Deaths)

# change 1-10 suppresed range to midpoint 
ca_deaths = ca_deaths %>%
  mutate(
    suppressed = total_deaths == "<11",
    total_deaths_n = case_when(
      suppressed ~ 5.5,   
      TRUE ~ as.numeric(total_deaths)))

# ANALYSIS -----------------------------------------------------------------

# total deaths by nativity in CA by year
total_ca_deaths = ca_deaths %>%
    group_by(year) %>%
    summarise(deaths = sum(total_deaths_n, na.rm = TRUE), .groups = "drop")

print(total_ca_deaths)

write_csv(total_ca_deaths, "results/ca_total_deaths_year.csv")

# motor vehicle deaths, year and nativity
mv_deaths_by_year = ca_deaths %>%
  filter(COD == "Motor vehicle accidents") %>%
  group_by(year, nativity) %>%
  summarise(deaths = sum(total_deaths_n, na.rm = TRUE), .groups = "drop") %>%
  mutate(nativity = recode(nativity, "United States" = "US Born"))

print(mv_deaths_by_year, n = Inf)

write_csv(mv_deaths_by_year, "results/ca_mv_deaths_by_year.csv")

mv_deaths_total = mv_deaths_by_year %>%
  group_by(year) %>%
  summarise(nativity = "Total", deaths = sum(deaths, na.rm = TRUE), .groups = "drop")

mv_deaths_plot = bind_rows(
  mv_deaths_by_year %>% filter(nativity != "Unknown"),
  mv_deaths_total) %>%
  mutate(nativity = factor(nativity, levels = c("US Born", "Foreign Born", "Total")))

nativity_colors = c(
  "US Born"      = "#3043B4",
  "Foreign Born" = "#C97703",
  "Total"        = "#7C756D")

nativity_linetypes = c(
  "US Born"      = "solid",
  "Foreign Born" = "solid",
  "Total"        = "dotted")

ggplot(mv_deaths_plot, aes(x = year, y = deaths, color = nativity, linetype = nativity)) +
  geom_line(linewidth = 1.8) +
  geom_point(size = 2) +
  scale_color_manual(values = nativity_colors) +
  scale_linetype_manual(values = nativity_linetypes) +
  scale_x_continuous(breaks = unique(mv_deaths_plot$year), expand = c(0.02, 0)) +
  scale_y_continuous(labels = scales::comma, expand = c(0.02, 0), breaks = seq(0, 6000, by = 1000), limits = c(0, 6000)) +
  labs(
    title = "Motor Vehicle Accident Deaths, California, by Nativity",
    subtitle = "Cal-ViDa",
    x = NULL, y = NULL, color = NULL, linetype = NULL,
    caption = "Source: Cal-ViDa") +
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

ggsave("results/ca_mv_deaths_by_year.png", width = 15, height = 10)

# exclude 2026
mv_deaths_clean = mv_deaths_by_year %>% filter(year <= 2025)

ca_drivers_clean = drivers_by_year_ca %>% filter(year >= 2016, year <= 2025)

# foreign-born drivers vs. foreign-born MV deaths, California, indexed 2016 = 100 --
fb_drivers = ca_drivers_clean %>%
  filter(immig_status == "All immigrants") %>%
  select(year, drivers)

fb_deaths = mv_deaths_clean %>%
  filter(nativity == "Foreign Born") %>%
  select(year, deaths)

fb_drivers_vs_deaths = fb_drivers %>%
  left_join(fb_deaths, by = "year") %>%
  mutate(
    drivers_index = drivers / drivers[year == 2016] * 100,
    deaths_index  = deaths / deaths[year == 2016] * 100)

print(fb_drivers_vs_deaths, n = Inf)

write_csv(fb_drivers_vs_deaths, "results/ca_fb_drivers_vs_deaths.csv")

fb_drivers_vs_deaths_long = fb_drivers_vs_deaths %>%
  select(year, drivers_index, deaths_index) %>%
  pivot_longer(cols = c(drivers_index, deaths_index), names_to = "series", values_to = "index") %>%
  mutate(series = recode(series,
    drivers_index = "Foreign-born drivers",
    deaths_index  = "Foreign-born MV deaths"))

trend_colors_fb = c(
  "Foreign-born drivers"   = "#0D0E51",
  "Foreign-born MV deaths" = "#C97703")

ggplot(fb_drivers_vs_deaths_long, aes(x = year, y = index, color = series)) +
  geom_line(linewidth = 1.8) +
  geom_point(size = 2) +
  scale_color_manual(values = trend_colors_fb) +
  scale_x_continuous(breaks = seq(2016, 2025, by = 1), expand = c(0.02, 0)) +
  scale_y_continuous(expand = c(0.02, 0)) +
  labs(
    title = "Foreign-Born Drivers vs. Foreign-Born MV Deaths, California (2016 = 100)",
    subtitle = "ACS via IPUMS (drivers); Cal-ViDa (motor vehicle accident deaths)",
    x = NULL, y = NULL, color = NULL,
    caption = "Source: ACS via IPUMS; Cal-ViDa") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 28, face = "bold", hjust = 0, color = "black"),
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
    axis.text.x = element_text(size = 22, color = "gray40"),
    axis.text.y = element_text(size = 25, color = "gray40"),
    plot.caption = element_text(size = 12, color = "gray40", hjust = 0),
    plot.caption.position = "plot",
    plot.title.position = "plot",
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA))

ggsave("results/ca_fb_drivers_vs_deaths.png", width = 15, height = 10)


# death rate per driver, by nativity -------------------------------------

rate_by_nativity = bind_rows(
  ca_drivers_clean %>% filter(immig_status == "All immigrants") %>%
    mutate(nativity = "Foreign Born") %>% select(year, nativity, drivers),
  ca_drivers_clean %>% filter(immig_status == "Native-born citizens") %>%
    mutate(nativity = "US Born") %>% select(year, nativity, drivers)
) %>%
  left_join(mv_deaths_clean %>% filter(nativity %in% c("Foreign Born", "US Born")),
            by = c("year", "nativity")) %>%
  mutate(rate_per_100k_drivers = deaths / drivers * 100000)

print(rate_by_nativity, n = Inf)

write_csv(rate_by_nativity, "results/ca_death_rate_per_driver.csv")

nativity_colors = c("US Born" = "#3043B4", "Foreign Born" = "#C97703")

ggplot(rate_by_nativity, aes(x = year, y = rate_per_100k_drivers, color = nativity)) +
  geom_line(linewidth = 1.8) +
  geom_point(size = 2) +
  scale_color_manual(values = nativity_colors) +
  scale_x_continuous(breaks = seq(2016, 2025, by = 1), expand = c(0.02, 0)) +
  scale_y_continuous(expand = c(0.02, 0), limits = c(20, 45)) +
  labs(
    title = "Motor Vehicle Death Rate per 100,000 Drivers, California, by Nativity",
    subtitle = "Cal-ViDa (deaths) / ACS via IPUMS (drivers commuting by private motorized vehicle)",
    x = NULL, y = NULL, color = NULL,
    caption = "Source: ACS via IPUMS; Cal-ViDa") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 26, face = "bold", hjust = 0, color = "black"),
    plot.subtitle = element_text(size = 16, color = "gray40", hjust = 0, margin = margin(b = 12)),
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
    axis.text.x = element_text(size = 22, color = "gray40"),
    axis.text.y = element_text(size = 25, color = "gray40"),
    plot.caption = element_text(size = 12, color = "gray40", hjust = 0),
    plot.caption.position = "plot",
    plot.title.position = "plot",
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA))

ggsave("results/ca_death_rate_per_driver.png", width = 15, height = 10)
