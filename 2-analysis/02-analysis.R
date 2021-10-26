
# Preamble ----------------------------------------------------------------

# Code author  : Ihsan Fadilah
# Email        : ifadilah@eocru.org
# Last updated : Early October 2021
# Project      : SPARK project on malaria heterogeneity

# This script provides code for visualising the Lorenz curve and estimating
# the Gini index. Other descriptive plots are provided.

# Setup -------------------------------------------------------------------

# Packages
library(tidyverse)   # Tidy and readable code
library(lubridate)   # Handle time objects
library(scales)      # Modify plot scales
library(here)        # Navigate files
library(knitr)       # Tidy tables
library(DescTools)   # Estimate Gini coefficient
library(viridisLite) # Nice colours
library(cowplot)     # Create compound plots
library(readxl)      # Read Excel files
library(extrafont)   # Fonts
loadfonts()

# Set plots to some format
## Colours and theme of my choosing
colours <- c("#ff9a8d", "#aed6dc")
colours_three <- c("gray80", "#ff9a8d", "#aed6dc")
expanded_colours <- c("#aed6dc", "#9cc0c6", "#ff9a8d", "#e58a7e")
theme_set(theme_minimal())

## Further refinement
theme_update(
  text = element_text(size = 9, family = "Fira Code"), # Font
  plot.title = element_text(hjust = 0),      # Centre-align title
  plot.subtitle = element_text(hjust = 0),   # Centre-align subtitle
  legend.title = element_blank(),            # Remove legend title
  legend.position = c(0.85, 0.85),           # Move legend to bottom right
  legend.background = element_blank(),       # Remove legend background
  legend.box.background = element_blank(),   # Remove lengend-box background
  legend.spacing.y = unit(0.01, 'mm'),       # Make legend closer
  legend.key.height = unit(0.4, "cm"),       # Make legend closer
  # panel.grid.minor = element_blank(),      # Remove minor lines
  panel.grid.minor.x = element_blank(),      # Remove minor lines on the x axis
  axis.title.x = element_text(hjust = 1),    # Move title for x-axis
  axis.title.y = element_text(hjust = 0.5)   # Move title for y-axis
)

# Data --------------------------------------------------------------------

# Individual-level data
esismal <- here("0-data", "esismal.rds") %>% read_rds()

# Province-specific population sizes, by year
pop <- here("0-data", "population-size.rds") %>% read_rds()

# District-specific aggregate data, by month and year
time_series_month <- here("0-data", "time-series-month.rds") %>% read_rds()

# Species- and province-specific aggregate data for P. falciparum and/or vivax
monoinfection <- here("0-data", "monoinfection.rds") %>% read_rds()

# Monthly API since 2011 --------------------------------------------------

# Add yearly population sizes
add_pop <- time_series_month %>% 
  group_by(date, province) %>% 
  summarise(case = sum(case)) %>%
  mutate(year = year(date)) %>% 
  group_by(province, year) %>% 
  nest() %>% 
  full_join(y = pop, by = c('year', 'province')) %>%
  unnest(data) %>% 
  ungroup()

# Calculate API
time_series_plotmonth <- add_pop %>% 
  mutate(api = (case / pop) * 1000)

# Plot
by_month <- time_series_plotmonth %>% 
  filter(year != 2010) %>% 
  ggplot(aes(x = date,
             y = api,
             group = province,
             colour = province)) +
  geom_line(size = 1.5, alpha = 0.8) +
  scale_colour_manual(values = colours) +
  scale_y_continuous(limits = c(0, 14.5),
                     breaks = seq(0, 14, by = 2),
                     expand = c(0.01, 0)) +
  scale_x_date(date_breaks = "1 year",
               date_labels = "%Y",
               expand = c(0.01, 0)) +
  theme(legend.position = c(0.8, 0.86),
        axis.title.y = element_text(size = 7),
        plot.margin = unit(c(5, 5, 5, 5), "mm")) +
  labs(x = "\nYear",
       y = "Monthly incidence risk per 1000\n")

by_month

# Weekly proportions of P. vivax 2019–2020 --------------------------------

month <- seq(as.Date("2020-01-01"), as.Date("2020-12-01"), by = "1 month")
month_numeric <- yday(month) / 365 * 52 + 1
month_label <- month(month, label = TRUE)

prop_vivax <- esismal %>% 
  group_by(year_dx, province, sp, week_dx) %>% 
  summarise(weekly_case = n()) %>% 
  pivot_wider(id_cols = c(year_dx, province, week_dx),
              names_from = sp,
              values_from = weekly_case) %>% 
  select(- `NA`) %>% 
  mutate(Other = if_else(is.na(Other), 0, as.numeric(Other))) %>% 
  mutate(`P. falciparum/vivax` = `P. falciparum/vivax` / 2,
         `P. vivax` = if_else(is.na(`P. falciparum/vivax`),
                              `P. vivax` + 0,
                              as.numeric(`P. vivax` + `P. falciparum/vivax`)),
         `P. falciparum` = if_else(is.na(`P. falciparum/vivax`),
                                   `P. falciparum` + 0,
                                   as.numeric(
                                     `P. falciparum` + `P. falciparum/vivax`
                                   ))) %>%
  mutate(n = `P. falciparum` + `P. vivax` + Other,
         prop_pv = `P. vivax` / n) %>% 
  select(year_dx, province, week_dx, prop_pv) %>% 
  mutate(year_dx = factor(year_dx))

prop_vivax_by_province <- prop_vivax %>% 
  ggplot(aes(x = week_dx,
             y = prop_pv)) +
  geom_line(aes(colour = year_dx),
            size = 1.3, alpha = 0.8) +
  facet_grid(rows = vars(province)) +
  theme(legend.position = "right",
        panel.spacing = unit(1, "lines")) +
  scale_colour_manual(values = c('gray', 'black')) +
  scale_x_continuous(breaks = month_numeric,
                     labels = month_label,
                     limits = c(0.5, 52),
                     expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 1),
                     breaks = seq(0, 1, by = 0.2),
                     expand = c(0.01, 0),
                     labels = label_percent()) +
  theme(legend.position = c(0.90, 0.4),
        legend.spacing.x = unit(0.05, "cm"),
        axis.title.y = element_text(size = 7),
        legend.margin = margin(0, 0, 0, 0),
        legend.box.margin = margin(0, 0, 0, 0),
        legend.text = element_text(margin = margin(r = 0, unit = "mm"))) +
  labs(x = "\nMonth",
       y = "Weekly proportion of P. vivax\n")

prop_vivax_by_province

fig_1 <- plot_grid(by_month, prop_vivax_by_province,
                   ncol = 1, align = "v", axis = "l",
                   labels = c('A', 'B'),
                   label_size = 10,
                   rel_heights = c(0.75, 1),
                   rel_widths = c(2, 1))
fig_1
ggsave(filename = "decade_trend.png",
       path = here::here("0-graph"),
       height = 7,
       dpi = 600)

# Province-specific incidence by sex,  species, and age -------------------

monthly_incidence <- monoinfection %>% 
  mutate(monthly_perk = case_when(
    # Papua
    province == 'Papua' & year_dx == 2019 & sex == 'Male' ~
      (n_case / (1774.7 * 1000)) * 1000,
    province == 'Papua' & year_dx == 2020 & sex == 'Male' ~
      (n_case / (1802.2 * 1000)) * 1000,
    province == 'Papua' & year_dx == 2019 & sex == 'Female' ~
      (n_case / (1604.6 * 1000)) * 1000,
    province == 'Papua' & year_dx == 2020 & sex == 'Female' ~
      (n_case / (1633.2 * 1000)) * 1000,
    
    # West Papua
    province == 'West Papua' & year_dx == 2019 & sex == 'Male' ~
      (n_case / (504.8 * 1000)) * 1000,
    province == 'West Papua' & year_dx == 2020 & sex == 'Male' ~
      (n_case / (516.2 * 1000)) * 1000,
    province == 'West Papua' & year_dx == 2019 & sex == 'Female' ~
      (n_case / (454.8 * 1000)) * 1000,
    province == 'West Papua' & year_dx == 2020 & sex == 'Female' ~
      (n_case / (465.6 * 1000)) * 1000,
    TRUE ~ NA_real_
  ))

sex_species <- monthly_incidence %>% 
  ggplot(aes(x = date,
             y = monthly_perk)) +
  geom_area(aes(group = cat, fill = cat)) +
  facet_grid(rows = vars(province),
             scales = "free") +
  scale_x_date(expand = c(0.005, 0),
               date_breaks = "1 month",
               date_labels = "%b-%y") +
  scale_y_continuous(labels = number_format(accuracy = 0.1),
                     expand = c(0.01, 0)) +
  scale_fill_manual(values = expanded_colours) +
  theme(legend.position = "none",
        panel.spacing = unit(1, "lines"),
        axis.text.x = element_text(size = 5.5,
                                   angle = 90, vjust = 0.5, hjust = 1)) +
  labs(x = "\nCalendar time",
       y = "Monthly incidence risk per 1000\n")

sex_species
  

## STRANGE 0-4 API

api_age <- esismal %>% 
  filter(year_dx == 2020) %>% 
  group_by(province, age_cat, sex, sp) %>% 
  na.omit(sex) %>% 
  summarise(case = n()) %>%
  filter(sp == "P. vivax" |
           sp == "P. falciparum" |
           sp == "P. falciparum/vivax") %>%
  pivot_wider(names_from = sp,
              values_from = case) %>%
  mutate(`P. falciparum/vivax` = `P. falciparum/vivax` / 2,
         `P. vivax` = if_else(is.na(`P. falciparum/vivax`),
                              `P. vivax` + 0,
                              as.numeric(`P. vivax` + `P. falciparum/vivax`)),
         `P. falciparum` = if_else(is.na(`P. falciparum/vivax`),
                                   `P. falciparum` + 0,
                                   as.numeric(
                                     `P. falciparum` + `P. falciparum/vivax`
                                   ))) %>%
  ungroup() %>% 
  select(-`P. falciparum/vivax`) %>%
  pivot_longer(cols = starts_with("P. "),
               names_to = "sp",
               values_to = "annual_case") %>% 
  mutate(pop = case_when(
    # Males in Papua across age
    province == "Papua" & age_cat == "0-4" & sex == "Male" ~ (170.1 * 1000),
    province == "Papua" & age_cat == "5-9" & sex == "Male" ~ (164.2 * 1000),
    province == "Papua" & age_cat == "10-14" & sex == "Male" ~ (163.3 * 1000),
    province == "Papua" & age_cat == "15-19" & sex == "Male" ~ (166.4 * 1000),
    province == "Papua" & age_cat == "20-24" & sex == "Male" ~ (168.5 * 1000),
    province == "Papua" & age_cat == "25-29" & sex == "Male" ~ (165.4 * 1000),
    province == "Papua" & age_cat == "30-34" & sex == "Male" ~ (152.5 * 1000),
    province == "Papua" & age_cat == "35-39" & sex == "Male" ~ (141.3 * 1000),
    province == "Papua" & age_cat == "40-44" & sex == "Male" ~ (136.1 * 1000),
    province == "Papua" & age_cat == "45-49" & sex == "Male" ~ (124.9 * 1000),
    province == "Papua" & age_cat == "50-54" & sex == "Male" ~ (100.8 * 1000),
    province == "Papua" & age_cat == "55-59" & sex == "Male" ~ (70.7 * 1000),
    province == "Papua" & age_cat == "60-64" & sex == "Male" ~ (41.0 * 1000),
    province == "Papua" & age_cat == "65-69" & sex == "Male" ~ (20.7 * 1000),
    province == "Papua" & age_cat == "70-74" & sex == "Male" ~ (9.6 * 1000),
    province == "Papua" & age_cat == "75+" & sex == "Male" ~ (6.7 * 1000),
    
    # Females in Papua across age
    province == "Papua" & age_cat == "0-4" & sex == "Female" ~ (166.0 * 1000),
    province == "Papua" & age_cat == "5-9" & sex == "Female" ~ (161.0 * 1000),
    province == "Papua" & age_cat == "10-14" & sex == "Female" ~ (156.0 * 1000),
    province == "Papua" & age_cat == "15-19" & sex == "Female" ~ (148.6 * 1000),
    province == "Papua" & age_cat == "20-24" & sex == "Female" ~ (146.8 * 1000),
    province == "Papua" & age_cat == "25-29" & sex == "Female" ~ (148.0 * 1000),
    province == "Papua" & age_cat == "30-34" & sex == "Female" ~ (142.3 * 1000),
    province == "Papua" & age_cat == "35-39" & sex == "Female" ~ (133.2 * 1000),
    province == "Papua" & age_cat == "40-44" & sex == "Female" ~ (128.2 * 1000),
    province == "Papua" & age_cat == "45-49" & sex == "Female" ~ (112.1 * 1000),
    province == "Papua" & age_cat == "50-54" & sex == "Female" ~ (81.3 * 1000),
    province == "Papua" & age_cat == "55-59" & sex == "Female" ~ (52.1 * 1000),
    province == "Papua" & age_cat == "60-64" & sex == "Female" ~ (29.6 * 1000),
    province == "Papua" & age_cat == "65-69" & sex == "Female" ~ (15.2 * 1000),
    province == "Papua" & age_cat == "70-74" & sex == "Female" ~ (7.3 * 1000),
    province == "Papua" & age_cat == "75+" & sex == "Female" ~ (5.5 * 1000),
    
    # Males in West Papua across age
    province == "West Papua" & age_cat == "0-4" & sex == "Male" ~
      (51.6 * 1000),
    province == "West Papua" & age_cat == "5-9" & sex == "Male" ~
      (48.5 * 1000),
    province == "West Papua" & age_cat == "10-14" & sex == "Male" ~
      (47.0 * 1000),
    province == "West Papua" & age_cat == "15-19" & sex == "Male" ~
      (45.5 * 1000),
    province == "West Papua" & age_cat == "20-24" & sex == "Male" ~
      (43.6 * 1000),
    province == "West Papua" & age_cat == "25-29" & sex == "Male" ~
      (45.3 * 1000),
    province == "West Papua" & age_cat == "30-34" & sex == "Male" ~
      (47.4 * 1000),
    province == "West Papua" & age_cat == "35-39" & sex == "Male" ~
      (45.7 * 1000),
    province == "West Papua" & age_cat == "40-44" & sex == "Male" ~
      (39.5 * 1000),
    province == "West Papua" & age_cat == "45-49" & sex == "Male" ~
      (31.6 * 1000),
    province == "West Papua" & age_cat == "50-54" & sex == "Male" ~
      (25.3 * 1000),
    province == "West Papua" & age_cat == "55-59" & sex == "Male" ~
      (18.7 * 1000),
    province == "West Papua" & age_cat == "60-64" & sex == "Male" ~
      (12.5 * 1000),
    province == "West Papua" & age_cat == "65-69" & sex == "Male" ~
      (7.6 * 1000),
    province == "West Papua" & age_cat == "70-74" & sex == "Male" ~
      (4.0 * 1000),
    province == "West Papua" & age_cat == "75+" & sex == "Male" ~
      (2.4 * 1000),
    
    # Females in West Papua across age
    province == "West Papua" & age_cat == "0-4" & sex == "Female" ~
      (43.4 * 1000), # *
    province == "West Papua" & age_cat == "5-9" & sex == "Female" ~
      (41.2 * 1000),
    province == "West Papua" & age_cat == "10-14" & sex == "Female" ~
      (37.9 * 1000),
    province == "West Papua" & age_cat == "15-19" & sex == "Female" ~
      (37.2 * 1000),
    province == "West Papua" & age_cat == "20-24" & sex == "Female" ~
      (37.4 * 1000),
    province == "West Papua" & age_cat == "25-29" & sex == "Female" ~
      (37.2 * 1000),
    province == "West Papua" & age_cat == "30-34" & sex == "Female" ~
      (32.5 * 1000),
    province == "West Papua" & age_cat == "35-39" & sex == "Female" ~
      (26.0 * 1000),
    province == "West Papua" & age_cat == "40-44" & sex == "Female" ~
      (21.1 * 1000),
    province == "West Papua" & age_cat == "45-49" & sex == "Female" ~
      (16.4 * 1000),
    province == "West Papua" & age_cat == "50-54" & sex == "Female" ~
      (11.8 * 1000),
    province == "West Papua" & age_cat == "55-59" & sex == "Female" ~
      (7.8 * 1000),
    province == "West Papua" & age_cat == "60-64" & sex == "Female" ~
      (4.8 * 1000),
    province == "West Papua" & age_cat == "65-69" & sex == "Female" ~
      (2.7 * 1000),
    province == "West Papua" & age_cat == "70-74" & sex == "Female" ~
      (1.8 * 1000),
    province == "West Papua" & age_cat == "75+" & sex == "Female" ~
      (1.8 * 1000),
    TRUE ~ NA_real_
  ),
  api = (annual_case / pop) * 1000) %>% 
  mutate(cat = case_when(sex == "Male" & sp == "P. falciparum" ~
                           "P. falciparum in males",
                         sex == "Male" & sp == "P. vivax" ~
                           "P. vivax in males",
                         sex == "Female" & sp == "P. falciparum" ~
                           "P. falciparum in females",
                         sex == "Female" & sp == "P. vivax" ~
                           "P. vivax in females",
                         TRUE ~ "Other") %>%  as.factor(),
         cat = ordered(cat, levels = c("P. vivax in males",
                                       "P. vivax in females",
                                       "P. falciparum in males",
                                       "P. falciparum in females")))
  
age_intervals <- api_age %>% 
  ggplot(aes(x = age_cat,
             y = api,
             fill = cat)) +
  geom_col() +
  scale_x_discrete(expand = c(0.04, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  facet_grid(rows = vars(province),
             scales = "free") +
  scale_fill_manual(values = expanded_colours) +
  theme(legend.position = "bottom",
        legend.justification = "right",
        legend.direction = "horizontal",
        axis.text.x = element_text(size = 5.5),
        panel.spacing = unit(1, "lines"),
        legend.spacing.x = unit(0.05, "cm"),
        legend.margin = margin(0, 0, 0, 0),
        legend.box.margin = margin(0, 0, 0, 0),
        legend.text = element_text(margin = margin(r = 9, unit = "mm"))) +
  labs(x = "\nAge interval (years)",
       y = "Annual parasite incidence per 1000\n")

age_intervals

fig_3 <- plot_grid(sex_species, age_intervals,
                   ncol = 1, align = "v",
                   labels = c('A', 'B'),
                   label_size = 10,
                   rel_heights = c(1, 1),
                   rel_widths = c(1, 1))
fig_3

ggsave(filename = "char_distributions.png",
       path = here::here("0-graph"),
       height = 9, width = 7,
       dpi = 600)

# District-specific incidence by sex,  species, and age -------------------

# Keerom
monthly_incidence_keerom <- esismal %>%
  filter(year_dx == 2020) %>% 
  filter(city == "Keerom",
         sp == "P. falciparum" |
           sp == "P. vivax" |
           sp == "P. falciparum/vivax") %>% 
  group_by(month_dx, sex, sp) %>% 
  summarise(n_case = n()) %>% 
  na.omit(sex) %>%
  pivot_wider(names_from = sp,
              values_from = n_case) %>% 
  mutate(`P. falciparum/vivax` = `P. falciparum/vivax` / 2,
         `P. vivax` = if_else(is.na(`P. falciparum/vivax`),
                              `P. vivax` + 0,
                              as.numeric(`P. vivax` + `P. falciparum/vivax`)),
         `P. falciparum` = if_else(is.na(`P. falciparum/vivax`),
                                   `P. falciparum` + 0,
                                   as.numeric(
                                     `P. falciparum` + `P. falciparum/vivax`
                                   ))) %>%
  select(month_dx, sex, `P. vivax`, `P. falciparum`) %>% 
  ungroup() %>% 
  pivot_longer(cols = starts_with("P. "),
               names_to = "sp",
               values_to = "n_case") %>% 
  mutate(cat = case_when(sex == "Male" & sp == "P. falciparum" ~
                           "P. falciparum in males",
                         sex == "Male" & sp == "P. vivax" ~
                           "P. vivax in males",
                         sex == "Female" & sp == "P. falciparum" ~
                           "P. falciparum in females",
                         sex == "Female" & sp == "P. vivax" ~
                           "P. vivax in females",
                         TRUE ~ "Other") %>%  as.factor(),
         cat = ordered(cat, levels = c("P. vivax in males",
                                       "P. vivax in females",
                                       "P. falciparum in males",
                                       "P. falciparum in females"))) %>%
  # https://www.bps.go.id/publication/2015/06/30/b71dd26daebb7280455819b6/
  # proyeksi-penduduk-kabupaten-kota-tahunan-2010-2020-provinsi-papua.html
  mutate(monthly_perk = if_else(sex == "Male",
                                (n_case / (31.114 * 1000)) * 1000,  # *
                                (n_case / (26.789 * 1000)) * 1000),
         city = rep("Keerom", 48)) %>% 
  select(city, month_dx, sex, sp, cat, n_case, monthly_perk)

# Nabire
monthly_incidence_nabire <- esismal %>%
  filter(year_dx == 2020) %>% 
  filter(city == "Nabire",
         sp == "P. falciparum" |
           sp == "P. vivax" |
           sp == "P. falciparum/vivax") %>% 
  group_by(month_dx, sex, sp) %>% 
  summarise(n_case = n()) %>% 
  na.omit(sex) %>%
  pivot_wider(names_from = sp,
              values_from = n_case) %>% 
  mutate(`P. falciparum/vivax` = `P. falciparum/vivax` / 2,
         `P. vivax` = if_else(is.na(`P. falciparum/vivax`),
                              `P. vivax` + 0,
                              as.numeric(`P. vivax` + `P. falciparum/vivax`)),
         `P. falciparum` = if_else(is.na(`P. falciparum/vivax`),
                                   `P. falciparum` + 0,
                                   as.numeric(
                                     `P. falciparum` + `P. falciparum/vivax`
                                   ))) %>%
  select(month_dx, sex, `P. vivax`, `P. falciparum`) %>% 
  ungroup() %>% 
  pivot_longer(cols = starts_with("P. "),
               names_to = "sp",
               values_to = "n_case") %>% 
  mutate(cat = case_when(sex == "Male" & sp == "P. falciparum" ~
                           "P. falciparum in males",
                         sex == "Male" & sp == "P. vivax" ~
                           "P. vivax in males",
                         sex == "Female" & sp == "P. falciparum" ~
                           "P. falciparum in females",
                         sex == "Female" & sp == "P. vivax" ~
                           "P. vivax in females",
                         TRUE ~ "Other") %>%  as.factor(),
         cat = ordered(cat, levels = c("P. vivax in males",
                                       "P. vivax in females",
                                       "P. falciparum in males",
                                       "P. falciparum in females"))) %>%
  # https://www.bps.go.id/publication/2015/06/30/b71dd26daebb7280455819b6/
  # proyeksi-penduduk-kabupaten-kota-tahunan-2010-2020-provinsi-papua.html
  mutate(monthly_perk = if_else(sex == "Male",
                                (n_case / (79.909 * 1000)) * 1000,  # *
                                (n_case / (72.912 * 1000)) * 1000),
         city = rep("Nabire", 48)) %>%
  select(city, month_dx, sex, sp, cat, n_case, monthly_perk)

# Jayawijaya
monthly_incidence_jayawijaya <- esismal %>%
  filter(year_dx == 2020) %>% 
  filter(city == "Jayawijaya",
         sp == "P. falciparum" |
           sp == "P. vivax" |
           sp == "P. falciparum/vivax") %>% 
  group_by(month_dx, sex, sp) %>% 
  summarise(n_case = n()) %>% 
  na.omit(sex) %>%
  pivot_wider(names_from = sp,
              values_from = n_case) %>% 
  mutate(`P. falciparum/vivax` = `P. falciparum/vivax` / 2,
         `P. vivax` = if_else(is.na(`P. falciparum/vivax`),
                              `P. vivax` + 0,
                              as.numeric(`P. vivax` + `P. falciparum/vivax`)),
         `P. falciparum` = if_else(is.na(`P. falciparum/vivax`),
                                   `P. falciparum` + 0,
                                   as.numeric(
                                     `P. falciparum` + `P. falciparum/vivax`
                                   ))) %>%
  select(month_dx, sex, `P. vivax`, `P. falciparum`) %>% 
  ungroup() %>% 
  pivot_longer(cols = starts_with("P. "),
               names_to = "sp",
               values_to = "n_case") %>% 
  mutate(cat = case_when(sex == "Male" & sp == "P. falciparum" ~
                           "P. falciparum in males",
                         sex == "Male" & sp == "P. vivax" ~
                           "P. vivax in males",
                         sex == "Female" & sp == "P. falciparum" ~
                           "P. falciparum in females",
                         sex == "Female" & sp == "P. vivax" ~
                           "P. vivax in females",
                         TRUE ~ "Other") %>%  as.factor(),
         cat = ordered(cat, levels = c("P. vivax in males",
                                       "P. vivax in females",
                                       "P. falciparum in males",
                                       "P. falciparum in females"))) %>%
  # https://www.bps.go.id/publication/2015/06/30/b71dd26daebb7280455819b6/
  # proyeksi-penduduk-kabupaten-kota-tahunan-2010-2020-provinsi-papua.html
  mutate(monthly_perk = if_else(sex == "Male",
                                (n_case / (111.811 * 1000)) * 1000,  # *
                                (n_case / (108.301 * 1000)) * 1000),
         city = rep("Jayawijaya", 48)) %>%
  select(city, month_dx, sex, sp, cat, n_case, monthly_perk)

# Combine selected districts
monthly_incidence_district_20 <- bind_rows(monthly_incidence_keerom,
                                           monthly_incidence_nabire,
                                           monthly_incidence_jayawijaya) %>% 
  mutate(city = as.factor(city))

sex_species_district <- monthly_incidence_district_20 %>% 
  mutate(api = if_else(is.na(monthly_perk), 0, monthly_perk),
         city = factor(city, levels = c("Keerom",
                                        "Nabire",
                                        "Jayawijaya"))) %>%
  ggplot(aes(x = month_dx,
             y = api)) +
  geom_area(aes(group = cat, fill = cat)) +
  facet_grid(rows = vars(city),
             scales = "free") +
  scale_x_discrete(expand = c(0.01, 0)) +
  scale_y_continuous(labels = number_format(accuracy = 0.1),
                     expand = c(0.01, 0)) +
  scale_fill_manual(values = expanded_colours) +
  theme(legend.position = "bottom",
        panel.spacing = unit(1, "lines")) +
  labs(x = "\nMonth",
       y = "Monthly incidence risk per 1000\n")

sex_species_district

ggsave(filename = "sex_sp_district.png",
       path = here::here("0-graph"),
       height = 9,
       dpi = 600)

api_age_district <- esismal %>% 
  filter(year_dx == 2020) %>% 
  filter(city == "Keerom" |
           city == "Nabire" |
           city == "Jayawijaya") %>% 
  group_by(city, age_cat, sex, sp) %>% 
  na.omit(sex) %>% 
  summarise(case = n()) %>% 
  
  filter(sp == "P. vivax" |
           sp == "P. falciparum" |
           sp == "P. falciparum/vivax") %>%
  pivot_wider(names_from = sp,
              values_from = case) %>%
  mutate(`P. falciparum/vivax` = `P. falciparum/vivax` / 2,
         `P. vivax` = if_else(is.na(`P. falciparum/vivax`),
                              `P. vivax` + 0,
                              as.numeric(`P. vivax` + `P. falciparum/vivax`)),
         `P. falciparum` = if_else(is.na(`P. falciparum/vivax`),
                                   `P. falciparum` + 0,
                                   as.numeric(
                                     `P. falciparum` + `P. falciparum/vivax`
                                   ))) %>%
  select(-`P. falciparum/vivax`) %>%
  pivot_longer(cols = starts_with("P. "),
               names_to = "sp",
               values_to = "annual_case") %>%
  
  mutate(pop = case_when(
    # https://www.bps.go.id/publication/2015/06/30/b71dd26daebb7280455819b6/
    # proyeksi-penduduk-kabupaten-kota-tahunan-2010-2020-provinsi-papua.html
    
    # Males in Keerom across age
    city == "Keerom" & age_cat == "0-4" & sex == "Male" ~ (2.928 * 1000), # *
    city == "Keerom" & age_cat == "5-9" & sex == "Male" ~ (2.323 * 1000),
    city == "Keerom" & age_cat == "10-14" & sex == "Male" ~ (2.179 * 1000),
    city == "Keerom" & age_cat == "15-19" & sex == "Male" ~ (2.513 * 1000),
    city == "Keerom" & age_cat == "20-24" & sex == "Male" ~ (3.156 * 1000),
    city == "Keerom" & age_cat == "25-29" & sex == "Male" ~ (3.315 * 1000),
    city == "Keerom" & age_cat == "30-34" & sex == "Male" ~ (2.389 * 1000),
    city == "Keerom" & age_cat == "35-39" & sex == "Male" ~ (2.057 * 1000),
    city == "Keerom" & age_cat == "40-44" & sex == "Male" ~ (2.073 * 1000),
    city == "Keerom" & age_cat == "45-49" & sex == "Male" ~ (2.259 * 1000),
    city == "Keerom" & age_cat == "50-54" & sex == "Male" ~ (1.930 * 1000),
    city == "Keerom" & age_cat == "55-59" & sex == "Male" ~ (1.751 * 1000),
    city == "Keerom" & age_cat == "60-64" & sex == "Male" ~ (0.925 * 1000),
    city == "Keerom" & age_cat == "65-69" & sex == "Male" ~ (0.733 * 1000),
    city == "Keerom" & age_cat == "70-74" & sex == "Male" ~ (0.329 * 1000),
    city == "Keerom" & age_cat == "75+" & sex == "Male" ~ (0.254 * 1000),
    
    # Females in Keerom across age
    city == "Keerom" & age_cat == "0-4" & sex == "Female" ~ (2.946 * 1000), # *
    city == "Keerom" & age_cat == "5-9" & sex == "Female" ~ (2.427 * 1000),
    city == "Keerom" & age_cat == "10-14" & sex == "Female" ~ (2.322 * 1000),
    city == "Keerom" & age_cat == "15-19" & sex == "Female" ~ (2.168 * 1000),
    city == "Keerom" & age_cat == "20-24" & sex == "Female" ~ (2.254 * 1000),
    city == "Keerom" & age_cat == "25-29" & sex == "Female" ~ (2.029 * 1000),
    city == "Keerom" & age_cat == "30-34" & sex == "Female" ~ (1.817 * 1000),
    city == "Keerom" & age_cat == "35-39" & sex == "Female" ~ (1.770 * 1000),
    city == "Keerom" & age_cat == "40-44" & sex == "Female" ~ (2.300 * 1000),
    city == "Keerom" & age_cat == "45-49" & sex == "Female" ~ (2.207 * 1000),
    city == "Keerom" & age_cat == "50-54" & sex == "Female" ~ (1.744 * 1000),
    city == "Keerom" & age_cat == "55-59" & sex == "Female" ~ (1.187 * 1000),
    city == "Keerom" & age_cat == "60-64" & sex == "Female" ~ (0.748 * 1000),
    city == "Keerom" & age_cat == "65-69" & sex == "Female" ~ (0.447 * 1000),
    city == "Keerom" & age_cat == "70-74" & sex == "Female" ~ (0.222 * 1000),
    city == "Keerom" & age_cat == "75+" & sex == "Female" ~ (0.201 * 1000),
    
    # Males in Nabire across age
    city == "Nabire" & age_cat == "0-4" & sex == "Male" ~ (7.384 * 1000), # *
    city == "Nabire" & age_cat == "5-9" & sex == "Male" ~ (6.375 * 1000),
    city == "Nabire" & age_cat == "10-14" & sex == "Male" ~ (5.654 * 1000),
    city == "Nabire" & age_cat == "15-19" & sex == "Male" ~ (6.821 * 1000),
    city == "Nabire" & age_cat == "20-24" & sex == "Male" ~ (7.954 * 1000),
    city == "Nabire" & age_cat == "25-29" & sex == "Male" ~ (8.319 * 1000),
    city == "Nabire" & age_cat == "30-34" & sex == "Male" ~ (7.232 * 1000),
    city == "Nabire" & age_cat == "35-39" & sex == "Male" ~ (5.947 * 1000),
    city == "Nabire" & age_cat == "40-44" & sex == "Male" ~ (5.425 * 1000),
    city == "Nabire" & age_cat == "45-49" & sex == "Male" ~ (4.899 * 1000),
    city == "Nabire" & age_cat == "50-54" & sex == "Male" ~ (4.862 * 1000),
    city == "Nabire" & age_cat == "55-59" & sex == "Male" ~ (4.194 * 1000),
    city == "Nabire" & age_cat == "60-64" & sex == "Male" ~ (2.504 * 1000),
    city == "Nabire" & age_cat == "65-69" & sex == "Male" ~ (1.268 * 1000),
    city == "Nabire" & age_cat == "70-74" & sex == "Male" ~ (0.584 * 1000),
    city == "Nabire" & age_cat == "75+" & sex == "Male" ~ (0.487 * 1000),
    
    # Females in Nabire across age
    city == "Nabire" & age_cat == "0-4" & sex == "Female" ~ (7.763 * 1000), # *
    city == "Nabire" & age_cat == "5-9" & sex == "Female" ~ (6.777 * 1000),
    city == "Nabire" & age_cat == "10-14" & sex == "Female" ~ (5.964 * 1000),
    city == "Nabire" & age_cat == "15-19" & sex == "Female" ~ (6.326 * 1000),
    city == "Nabire" & age_cat == "20-24" & sex == "Female" ~ (6.727 * 1000),
    city == "Nabire" & age_cat == "25-29" & sex == "Female" ~ (6.915 * 1000),
    city == "Nabire" & age_cat == "30-34" & sex == "Female" ~ (6.019 * 1000),
    city == "Nabire" & age_cat == "35-39" & sex == "Female" ~ (5.068 * 1000),
    city == "Nabire" & age_cat == "40-44" & sex == "Female" ~ (5.347 * 1000),
    city == "Nabire" & age_cat == "45-49" & sex == "Female" ~ (5.132 * 1000),
    city == "Nabire" & age_cat == "50-54" & sex == "Female" ~ (4.369 * 1000),
    city == "Nabire" & age_cat == "55-59" & sex == "Female" ~ (2.922 * 1000),
    city == "Nabire" & age_cat == "60-64" & sex == "Female" ~ (1.847 * 1000),
    city == "Nabire" & age_cat == "65-69" & sex == "Female" ~ (0.917 * 1000),
    city == "Nabire" & age_cat == "70-74" & sex == "Female" ~ (0.420 * 1000),
    city == "Nabire" & age_cat == "75+" & sex == "Female" ~ (0.399 * 1000),
    
    # Males in Jayawijaya across age
    city == "Jayawijaya" & age_cat == "0-4" & sex == "Male" ~
      (7.835 * 1000), # *
    city == "Jayawijaya" & age_cat == "5-9" & sex == "Male" ~
      (9.085 * 1000),
    city == "Jayawijaya" & age_cat == "10-14" & sex == "Male" ~
      (10.415 * 1000),
    city == "Jayawijaya" & age_cat == "15-19" & sex == "Male" ~
      (11.885 * 1000),
    city == "Jayawijaya" & age_cat == "20-24" & sex == "Male" ~ (9.885 * 1000),
    city == "Jayawijaya" & age_cat == "25-29" & sex == "Male" ~ (8.927 * 1000),
    city == "Jayawijaya" & age_cat == "30-34" & sex == "Male" ~ (8.794 * 1000),
    city == "Jayawijaya" & age_cat == "35-39" & sex == "Male" ~ (10.709 * 1000),
    city == "Jayawijaya" & age_cat == "40-44" & sex == "Male" ~ (10.696 * 1000),
    city == "Jayawijaya" & age_cat == "45-49" & sex == "Male" ~ (9.909 * 1000),
    city == "Jayawijaya" & age_cat == "50-54" & sex == "Male" ~ (6.346 * 1000),
    city == "Jayawijaya" & age_cat == "55-59" & sex == "Male" ~ (4.090 * 1000),
    city == "Jayawijaya" & age_cat == "60-64" & sex == "Male" ~ (2.095 * 1000),
    city == "Jayawijaya" & age_cat == "65-69" & sex == "Male" ~ (0.756 * 1000),
    city == "Jayawijaya" & age_cat == "70-74" & sex == "Male" ~ (0.276 * 1000),
    city == "Jayawijaya" & age_cat == "75+" & sex == "Male" ~ (0.108 * 1000),
    
    # Females in Jayawijaya across age
    city == "Jayawijaya" & age_cat == "0-4" & sex == "Female" ~ (8.025 * 1000),
    city == "Jayawijaya" & age_cat == "5-9" & sex == "Female" ~ (9.241 * 1000),
    city == "Jayawijaya" & age_cat == "10-14" & sex == "Female" ~
      (10.560 * 1000),
    city == "Jayawijaya" & age_cat == "15-19" & sex == "Female" ~
      (10.792 * 1000),
    city == "Jayawijaya" & age_cat == "20-24" & sex == "Female" ~
      (9.333 * 1000),
    city == "Jayawijaya" & age_cat == "25-29" & sex == "Female" ~
      (10.166 * 1000),
    city == "Jayawijaya" & age_cat == "30-34" & sex == "Female" ~
      (10.904 * 1000),
    city == "Jayawijaya" & age_cat == "35-39" & sex == "Female" ~
      (11.850 * 1000),
    city == "Jayawijaya" & age_cat == "40-44" & sex == "Female" ~
      (10.323 * 1000),
    city == "Jayawijaya" & age_cat == "45-49" & sex == "Female" ~
      (8.162 * 1000),
    city == "Jayawijaya" & age_cat == "50-54" & sex == "Female" ~
      (4.645 * 1000),
    city == "Jayawijaya" & age_cat == "55-59" & sex == "Female" ~
      (2.438 * 1000),
    city == "Jayawijaya" & age_cat == "60-64" & sex == "Female" ~
      (1.233 * 1000),
    city == "Jayawijaya" & age_cat == "65-69" & sex == "Female" ~
      (0.468 * 1000),
    city == "Jayawijaya" & age_cat == "70-74" & sex == "Female" ~
      (0.105 * 1000),
    city == "Jayawijaya" & age_cat == "75+" & sex == "Female" ~ (0.56 * 1000),
    TRUE ~ NA_real_
  ),
  api = (annual_case / pop) * 1000) %>% 
  mutate(cat = case_when(sex == "Male" & sp == "P. falciparum" ~
                           "P. falciparum in males",
                         sex == "Male" & sp == "P. vivax" ~
                           "P. vivax in males",
                         sex == "Female" & sp == "P. falciparum" ~
                           "P. falciparum in females",
                         sex == "Female" & sp == "P. vivax" ~
                           "P. vivax in females",
                         TRUE ~ "Other") %>%  as.factor(),
         cat = ordered(cat, levels = c("P. vivax in males",
                                       "P. vivax in females",
                                       "P. falciparum in males",
                                       "P. falciparum in females")))

age_intervals_district <- api_age_district %>% 
  mutate(api = if_else(is.na(api), 0, api),
         city = factor(city, levels = c("Keerom",
                                        "Nabire",
                                        "Jayawijaya"))) %>% 
  ggplot(aes(x = age_cat,
             y = api,
             fill = cat)) +
  geom_col() +
  scale_x_discrete(expand = c(0.04, 0)) +
  scale_y_continuous(expand = c(0, 0),
                     labels = number_format(big.mark = ",")) +
  facet_grid(rows = vars(city),
             scales = "free") +
  scale_fill_manual(values = expanded_colours) +
  theme(legend.position = "bottom",
        legend.justification = "right",
        legend.direction = "horizontal",
        axis.text.x = element_text(size = 6),
        panel.spacing = unit(1, "lines"),
        legend.spacing.x = unit(0.05, "cm"),
        legend.margin = margin(0, 0, 0, 0),
        legend.box.margin = margin(0, 0, 0, 0),
        legend.text = element_text(margin = margin(r = 5, unit = "mm"))) +
  labs(x = "\nAge interval (years)",
       y = "Annual parasite incidence per 1000\n")

age_intervals_district

ggsave(filename = "age_int_district.png",
       path = here::here("0-graph"),
       height = 8,
       dpi = 600)

# Level-specific Gini index --------------------------------------------

## Across districts
api_city_sp_raw <- esismal %>% 
  filter(sp == "P. falciparum" |
           sp == "P. vivax" |
           sp == "P. falciparum/vivax") %>% 
  group_by(year_dx, province, city, sp) %>% 
  summarise(case = n(),
            pop = unique(n)) %>%
  ungroup() %>% 
  pivot_wider(names_from = sp,
              values_from = case) %>%
  
  mutate(`P. falciparum/vivax` = if_else(is.na(`P. falciparum/vivax`),
                                         0, as.double(`P. falciparum/vivax`)),
         `P. vivax` = if_else(is.na(`P. vivax`), 0, as.double(`P. vivax`)),
         `P. falciparum` = if_else(is.na(`P. falciparum`),
                                   0, as.double(`P. falciparum`))
  ) %>% 
  mutate(`P. falciparum/vivax` = `P. falciparum/vivax` / 2,
         `P. vivax` = `P. vivax` + `P. falciparum/vivax`,
         `P. falciparum` = `P. falciparum` + `P. falciparum/vivax`) %>%
  select(-`P. falciparum/vivax`) %>%
  
  pivot_longer(cols = starts_with("P. "),
               names_to = "sp",
               values_to = "case") %>%
  
  mutate(api = (case / pop) * 1000) %>%
  ungroup() %>% 
  select(year_dx, province, city, sp, case, pop, api)

api_city_sp <- api_city_sp_raw %>% 
  group_by(year_dx, province, sp) %>% 
  arrange(desc(api), .by_group = TRUE) %>%
  mutate(case_cumsum = cumsum(case),
         pop_cumsum = cumsum(pop))

total_case_pop <- api_city_sp %>% 
  group_by(year_dx, province, sp) %>% 
  summarise(totcase = max(case_cumsum),
            totpop = max(pop_cumsum)) %>% 
  ungroup()

total_case_pop <- api_city_sp %>% 
  group_by(year_dx, province, sp) %>% 
  summarise(totcase = max(case_cumsum),
            totpop = max(pop_cumsum)) %>% 
  ungroup()

cum_dist_sp <- api_city_sp %>% 
  group_by(year_dx, province, sp) %>% 
  nest() %>% 
  full_join(total_case_pop, by = c('year_dx', 'province', 'sp')) %>% 
  unnest(data) %>% 
  ungroup() %>% 
  mutate(c_i = case_cumsum / totcase,
         p_i = pop_cumsum / totpop) %>% 
  ungroup()

initial_lorcurv <- total_case_pop %>% 
  select(year_dx, province, sp) %>% 
  mutate(c_i = rep(0, nrow(total_case_pop)),
         p_i = rep(0, nrow(total_case_pop)))

cum_dist_sp <- bind_rows(cum_dist_sp, initial_lorcurv) %>% 
  arrange(year_dx, province, sp, c_i, p_i)

lorenz_district <- cum_dist_sp %>% 
  ggplot(aes(x = p_i,
             y = c_i,
             colour = province,
             linetype = factor(year_dx, levels = c('2020', '2019')))) +
  geom_line(size = 0.8) +
  geom_abline(slope = 1, intercept = 0, linetype = "dotted", alpha = 0.25) +
  scale_x_continuous(limits = c(0, 1),
                     breaks = seq(0, 1, by = 0.2),
                     labels = label_percent(),
                     expand = c(0.01, 0)) +
  scale_y_continuous(limits = c(0, 1),
                     breaks = seq(0, 1, by = 0.2),
                     labels = label_percent(),
                     expand = c(0, 0.01)) +
  scale_colour_manual(values = colours) +
  theme(plot.margin = unit(c(5, 5, 5, 5), "mm"),
        legend.position = "none",
        panel.spacing = unit(2, "lines"),
  ) +
  coord_fixed() +
  facet_grid(cols = vars(sp)) +
  labs(x = "\nCumulative proportion of population from lowest to highest API",
       y = "Cumulative proportion of cases\n")

lorenz_district

## Across health units
# Source: http://bppsdmk.kemkes.go.id/info_sdmk/info/

hu_papua <- here::here("0-data", "health_units_papua.xlsx") %>%
  read_xlsx() %>% 
  dplyr::select(`Nama Kab/Kota`, `Jumlah Unit`) %>% 
  mutate(city = str_to_title(`Nama Kab/Kota`) %>% factor(),
         n_hu = `Jumlah Unit`,
         province = rep("Papua", 29) %>% factor) %>% 
  select(province, city, n_hu)

hu_wpapua <- here::here("0-data", "health_units_west_papua.xlsx") %>%
  read_xlsx() %>% 
  dplyr::select(`Nama Kab/Kota`, `Jumlah Unit`) %>% 
  mutate(city = str_to_title(`Nama Kab/Kota`) %>% factor(),
         n_hu = `Jumlah Unit`,
         province = rep("West Papua", 13) %>% factor) %>% 
  select(province, city, n_hu)

hu <- bind_rows(hu_papua, hu_wpapua)

# Estimate numbers of health units
nhu_esismal20 <- esismal |> 
  filter(year_dx == 2020) |> 
  group_by(province, city, health_unit) |> 
  count() |> 
  select(-n) |> 
  group_by(province, city) |> 
  count() |> 
  rename(n_hu_obs20 = n)

nhu_esismal19 <- esismal |> 
  filter(year_dx == 2019) |> 
  group_by(province, city, health_unit) |> 
  count() |> 
  select(-n) |> 
  group_by(province, city) |> 
  count() |> 
  rename(n_hu_obs19 = n)

nhu_est <- nhu_esismal20 |> 
  full_join(y = hu, by = c('province', 'city')) |> 
  full_join(y = nhu_esismal19, by = c('province', 'city')) |> 
  rename(n_hu_web = n_hu) |> 
  rowwise() |> 
  mutate(nhu_est = max(n_hu_obs20, n_hu_obs19, n_hu_web, na.rm = TRUE)) |>
  ungroup() |> 
  select(province, city, nhu_est) |> 
  rename(n_hu = nhu_est)

api_hu_sp_raw <- esismal %>% 
  filter(sp == "P. falciparum" |
           sp == "P. vivax" |
           sp == "P. falciparum/vivax") %>% 
  group_by(year_dx, city, health_unit, sp) %>% 
  summarise(province = unique(province),
            case_hu = n(),
            pop_city = unique(n)) %>%
  ungroup() %>% 
  pivot_wider(names_from = sp,
              values_from = case_hu) %>%
  mutate(`P. falciparum/vivax` = if_else(is.na(`P. falciparum/vivax`),
                                         0, as.double(`P. falciparum/vivax`)),
         `P. vivax` = if_else(is.na(`P. vivax`), 0, as.double(`P. vivax`)),
         `P. falciparum` = if_else(is.na(`P. falciparum`),
                                   0, as.double(`P. falciparum`))
  ) %>% 
  mutate(`P. falciparum/vivax` = `P. falciparum/vivax` / 2,
         `P. vivax` = `P. vivax` + `P. falciparum/vivax`,
         `P. falciparum` = `P. falciparum` + `P. falciparum/vivax`) %>%
  select(-`P. falciparum/vivax`) %>%
  pivot_longer(cols = starts_with("P. "),
               names_to = "sp",
               values_to = "case_hu") %>%
  mutate(province = factor(province),
         city = factor(city),
         health_unit = factor(health_unit),
         sp = factor(sp),
         case_hu = if_else(is.na(case_hu), 0, as.double(case_hu))) %>% 
  group_by(province, city) %>% 
  nest() %>% 
  left_join(x = ., y = nhu_est, by = c('city', 'province')) %>%
  unnest(data) %>%
  mutate(pop_hu = pop_city / n_hu,
         api_hu = (case_hu / pop_hu) * 1000)

api_hu_sp_exp <- api_hu_sp_raw |> 
  select(-c(pop, api)) |> 
  # Add rows with zero cases
  group_by(province, city, year_dx, sp) |> 
  nest() |> 
  mutate(
    data_exp = map(
      .x = data,
      .f = function(data = .x) {
        # Extract n health units
        n_hu_est <- data$n_hu[1]
        n_hu_esismal <- nrow(data)
        
        # Extract the difference then add rows if positive
        if (n_hu_est > n_hu_esismal) {
          n_hu_diff <- n_hu_est - n_hu_esismal
          
          add_df <- tibble(.rows = n_hu_diff,
                           health_unit = NA,
                           case = 0,
                           pop_city = data$pop_city[1],
                           n_hu = data$n_hu[1])
          
          data <- data |> add_row(add_df)
        } else {
          data <- data
        }
      }
    )
  ) |> 
  select(-data) |> 
  unnest(data_exp) |> 
  ungroup() |> 
  mutate(pop_hu = pop_city / n_hu,
         api_hu = (case / pop_hu) * 1000) |> 
  rename(case_hu = case)

api_hu_sp <- api_hu_sp_exp %>% 
  group_by(year_dx, province, sp) %>% 
  arrange(desc(api_hu), .by_group = TRUE) %>% 
  mutate(case_cumsum = cumsum(case_hu),
         pop_cumsum = cumsum(pop_hu),
         c_i = case_cumsum / max(case_cumsum),
         p_i = pop_cumsum / max(pop_cumsum)) %>%
  ungroup()

cum_dist_sp_hu <- bind_rows(api_hu_sp, initial_lorcurv) %>% 
  arrange(year_dx, province, sp, c_i, p_i)

lorenz_health_unit <- cum_dist_sp_hu %>% 
  ggplot(aes(x = p_i,
             y = c_i,
             colour = province,
             linetype = factor(year_dx, levels = c('2020', '2019')))) +
  geom_line(size = 0.8) +
  geom_abline(slope = 1, intercept = 0, linetype = "dotted", alpha = 0.25) +
  scale_x_continuous(limits = c(0, 1),
                     breaks = seq(0, 1, by = 0.2),
                     labels = label_percent(),
                     expand = c(0.01, 0)) +
  scale_y_continuous(limits = c(0, 1),
                     breaks = seq(0, 1, by = 0.2),
                     labels = label_percent(),
                     expand = c(0, 0.01)) +
  scale_colour_manual(values = colours) +
  theme(legend.position = c(0.9, 0.2),
        panel.spacing = unit(2, "lines"),
        plot.margin = unit(c(5, 5, 5, 5), "mm")) +
  coord_fixed() +
  facet_grid(cols = vars(sp)) +
  labs(x = "\nCumulative proportion of population from lowest to highest API",
       y = "Cumulative proportion of cases\n")

lorenz_health_unit

fig_5 <- plot_grid(lorenz_district, lorenz_health_unit,
                   nrow = 2,
                   align = "v",
                   labels = c('A', 'B'),
                   label_size = 10,
                   rel_heights = c(1, 1),
                   rel_widths = c(1, 1),
                   label_y = 0.93)

fig_5

ggsave(filename = "lorenz_curves.png",
       path = here::here("0-graph"),
       height = 8,
       dpi = 600)

# Characteristics across districts ----------------------------------------

male_proportion <- esismal %>% 
  group_by(year_dx, province, city) %>% 
  mutate(is_male = if_else(sex == "Male", 1, 0)) %>% 
  relocate(is_male, .after = 10) %>% 
  summarise(sum_male = sum(is_male, na.rm = TRUE),
            sum_male_female = n(),
            male_prop = sum_male / sum_male_female) %>% 
  select(province, city, male_prop) %>% 
  ungroup()

total_api <- esismal %>% 
  group_by(year_dx, province, city) %>% 
  summarise(case = n(),
            pop = unique(n)) %>% 
  ungroup() %>% 
  mutate(api = (case / pop) * 1000)

male_proportion <- full_join(male_proportion, total_api,
                             by = c('year_dx', 'province', 'city'))

params_span <- 1

male_proportion_plot <- male_proportion %>% 
  ggplot(aes(x = api,
             y = male_prop)) +
  geom_smooth(aes(linetype = factor(year_dx)),
              formula = "y ~ x",
              method = "loess",
              span = params_span,
              se = 1,
              size = 1.3,
              fill = "gray80",
              colour = colours_three[1]) +
  geom_point(aes(size = case,
                 colour = province),
             alpha = 0.4) +
  scale_colour_manual(values = colours) +
  scale_x_continuous(trans = "log10",
                     limits = c(0.1, 1000),
                     labels = number_format(accuracy = 1, big.mark = ","),
                     expand = c(0, 0.05)) +
  scale_y_continuous(limits = c(0, 1),
                     breaks = seq(0, 1, by = 0.2),
                     labels = percent_format(),
                     expand = c(0, 0.01)) +
  scale_size_continuous(breaks = c(1000, 5000, 25000, 50000),
                        labels = number_format(big.mark = ",")) +
  theme(panel.grid.minor.x = element_line(),
        legend.position = c(0.5, 0.15),
        # legend.position = "none",
        legend.box = "horizontal",
        legend.spacing.x = unit(0.01, 'mm'),     
        legend.key.width = unit(0.8, "cm"),
        plot.margin = unit(c(5, 5, 5, 5), "mm")) +      
  guides(size = guide_legend(override.aes = list(colour = "gray70"))) +      
  # geom_text(aes(x = 8, y = 0.25, label = "Number of cases"), 
  #                hjust = 0, vjust = 0, size = 2.5, colour = "#55555f", 
  #                label.size = NA, family = "Fira Code") +
  coord_fixed(ratio = 5) +
  labs(x = "\nAnnual parasite incidence per 1000",
       y = "Proportion of males\n")

male_proportion_plot

adult_proportion <- esismal %>% 
  group_by(year_dx, province, city) %>% 
  mutate(is_adult = if_else(age >= 18, 1, 0)) %>% 
  relocate(is_adult, .after = 8) %>% 
  summarise(sum_adult = sum(is_adult, na.rm = TRUE),
            sum_all_age = n(),
            adult_prop = sum_adult / sum_all_age) %>% 
  select(province, city, adult_prop) %>% 
  ungroup()

adult_proportion <- full_join(adult_proportion, total_api,
                              by = c('year_dx', 'province', 'city'))

adult_proportion_plot <- adult_proportion %>% 
  ggplot(aes(x = api,
             y = adult_prop)) +
  geom_smooth(aes(linetype = factor(year_dx)),
              formula = "y ~ x",
              method = "loess",
              span = params_span,
              se = 1,
              size = 1.3,
              fill = "gray80",
              colour = colours_three[1]) +
  geom_point(aes(size = case,
                 colour = province),
             alpha = 0.4) +
  scale_colour_manual(values = colours) +
  scale_x_continuous(trans = "log10",
                     limits = c(0.1, 1000),
                     labels = number_format(accuracy = 1, big.mark = ","),
                     expand = c(0, 0.05)) +
  scale_y_continuous(limits = c(0, 1),
                     breaks = seq(0, 1, by = 0.2),
                     labels = percent_format(),
                     expand = c(0, 0.01)) +
  scale_size_continuous(breaks = c(1000, 5000, 25000, 50000),
                        labels = number_format(big.mark = ",")) +
  theme(panel.grid.minor.x = element_line(),
        # legend.position = c(0.7, 0.15),
        legend.position = "none",
        legend.box = "horizontal",
        legend.spacing.x = unit(0.01, 'mm'),     
        legend.key.width = unit(0.8, "cm"),
        plot.margin = unit(c(5, 5, 5, 5), "mm")) +      
  guides(size = guide_legend(override.aes = list(colour = "gray70"))) +   
  # geom_text(aes(x = 8, y = 0.25, label = "Number of cases"), 
  #                hjust = 0, vjust = 0, size = 2.5, colour = "#55555f", 
  #                label.size = NA, family = "Fira Code") +
  coord_fixed(ratio = 5) +
  labs(x = "",
       y = "Proportion of adults\n")

adult_proportion_plot

vivax_proportion <- esismal %>% 
  group_by(year_dx, province, city) %>%
  mutate(is_vivax = if_else(sp == "P. vivax" |
                              sp == "P. falciparum/vivax", 1, 0)) %>% 
  relocate(is_vivax, .after = 7) %>% 
  summarise(sum_vivax = sum(is_vivax, na.rm = TRUE),
            sum_all_sp = n(),
            vivax_prop = sum_vivax / sum_all_sp) %>% 
  select(province, city, vivax_prop) %>% 
  ungroup()

vivax_proportion <- full_join(vivax_proportion, total_api,
                              by = c('year_dx', 'province', 'city'))

vivax_proportion_plot <- vivax_proportion %>% 
  ggplot(aes(x = api,
             y = vivax_prop)) +
  geom_smooth(aes(linetype = factor(year_dx)),
              formula = "y ~ x",
              method = "loess",
              span = params_span,
              se = 1,
              size = 1.3,
              fill = "gray80",
              colour = colours_three[1]) +
  geom_point(aes(size = case,
                 colour = province),
             alpha = 0.4) +
  scale_colour_manual(values = colours) +
  scale_x_continuous(trans = "log10",
                     limits = c(0.1, 1000),
                     labels = number_format(accuracy = 1, big.mark = ","),
                     expand = c(0, 0.05)) +
  scale_y_continuous(limits = c(0, 1),
                     breaks = seq(0, 1, by = 0.2),
                     labels = percent_format(),
                     expand = c(0, 0.01)) +
  scale_size_continuous(breaks = c(1000, 5000, 25000, 50000),
                        labels = number_format(big.mark = ",")) +
  theme(panel.grid.minor.x = element_line(),
        # legend.position = c(0.80, 0.77),
        legend.position = "none",
        # legend.justification = "right",
        legend.box = "horizontal",
        plot.margin = unit(c(5, 5, 5, 5), "mm")) +      
  guides(size = guide_legend(override.aes = list(colour = "gray70"))) +
  # geom_text(aes(x = 62, y = 0.95, label = "Number of cases"), 
  #                hjust = 0, vjust = 0, size = 2.5, colour = "#55555f", 
  #                label.size = NA, family = "Fira Code") +
  coord_fixed(ratio = 5) +
  labs(x = "",
       y = "Proportion of Plasmodium vivax\n")

vivax_proportion_plot

fig_4 <- plot_grid(vivax_proportion_plot,
                   adult_proportion_plot,
                   male_proportion_plot,
                   nrow = 1, align = "hv",
                   labels = c('A', 'B', 'C'),
                   label_size = 10,
                   rel_heights = c(1, 1, 1),
                   rel_widths = c(1, 1, 1),
                   label_y = 0.87)

fig_4

ggsave(filename = "proportions.png",
       path = here::here("0-graph"),
       width = 12,
       dpi = 600)

# Top health units by absolute cases --------------------------------------

top19 <- esismal %>% 
  filter(year_dx == 2019 &
           sp == 'P. vivax' | sp == 'P. falciparum' |
           sp == 'P. falciparum/vivax')

top19_f <- top19 %>% 
  filter(sp == 'P. falciparum' | sp == 'P. falciparum/vivax') %>% 
  group_by(health_unit) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n)) %>% 
  mutate(cumsum = cumsum(n),
         cumprop = 100 * cumsum / max(cumsum)) %>% 
  arrange(cumprop)

top19_v <- top19 %>% 
  filter(sp == 'P. vivax' | sp == 'P. falciparum/vivax') %>% 
  group_by(health_unit) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n)) %>% 
  mutate(cumsum = cumsum(n),
         cumprop = 100 * cumsum / max(cumsum)) %>% 
  arrange(cumprop)

top20 <- esismal %>% 
  filter(year_dx == 2020 &
           sp == 'P. vivax' | sp == 'P. falciparum' |
           sp == 'P. falciparum/vivax')

top20_f <- top20 %>% 
  filter(sp == 'P. falciparum' | sp == 'P. falciparum/vivax') %>% 
  group_by(health_unit) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n)) %>% 
  mutate(cumsum = cumsum(n),
         cumprop = 100 * cumsum / max(cumsum)) %>% 
  arrange(cumprop)

top20_v <- top20 %>% 
  filter(sp == 'P. vivax' | sp == 'P. falciparum/vivax') %>% 
  group_by(health_unit) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n)) %>% 
  mutate(cumsum = cumsum(n),
         cumprop = 100 * cumsum / max(cumsum)) %>% 
  arrange(cumprop)

# Confidence intervals ----------------------------------------------------

# Functions for the Gini index 
# Gini index
g <- function(subdata) {
  g <- subdata |>
    mutate(p_imin1 = c(NA, p_i[1:(nrow(subdata) - 1)]),
           c_imin1 = c(NA, c_i[1:(nrow(subdata) - 1)]),
           p = p_i - p_imin1,
           c = c_i + c_imin1,
           pc = 0.5 * p * c) |>
    summarise(sum_pc = sum(pc, na.rm = TRUE)) |>
    mutate(g = (2 * sum_pc) - 1,
           g = 100 * g |> round(digits = 3)) |>
    pull(g)
  
  return(g) # Gini index in percentage form (correct to 3 s.f.)
}

gini_index <- function(data) { # Data must contain `p_i` and `c_i`
  # But also, `province`, `year_dx`, `sp`
  
  # Subset data
  ppf19 <- filter(data, province == 'Papua',
                  year_dx == 2019,
                  sp == 'P. falciparum')
  
  ppv19 <- filter(data, province == 'Papua',
                  year_dx == 2019,
                  sp == 'P. vivax')
  ppf20 <- filter(data, province == 'Papua',
                  year_dx == 2020,
                  sp == 'P. falciparum')
  ppv20 <- filter(data, province == 'Papua',
                  year_dx == 2020,
                  sp == 'P. vivax')
  
  wpf19 <- filter(data, province == 'West Papua',
                  year_dx == 2019,
                  sp == 'P. falciparum')
  wpv19 <- filter(data, province == 'West Papua',
                  year_dx == 2019,
                  sp == 'P. vivax')
  wpf20 <- filter(data, province == 'West Papua',
                  year_dx == 2020,
                  sp == 'P. falciparum')
  wpv20 <- filter(data, province == 'West Papua',
                  year_dx == 2020,
                  sp == 'P. vivax')
  
  # Merge the results
  gini_index <- data |>
    group_by(province, year_dx, sp) |>
    nest() |>
    mutate(g = case_when(
      province == 'Papua' & year_dx == 2019 & sp == 'P. falciparum' ~ g(ppf19),
      province == 'Papua' & year_dx == 2019 & sp == 'P. vivax' ~ g(ppv19),
      province == 'Papua' & year_dx == 2020 & sp == 'P. falciparum' ~ g(ppf20),
      province == 'Papua' & year_dx == 2020 & sp == 'P. vivax' ~ g(ppv20),
      
      province == 'West Papua' &
        year_dx == 2019 &
        sp == 'P. falciparum' ~ g(wpf19),
      province == 'West Papua' &
        year_dx == 2019 &
        sp == 'P. vivax' ~ g(wpv19),
      province == 'West Papua' &
        year_dx == 2020 &
        sp == 'P. falciparum' ~ g(wpf20),
      province == 'West Papua' &
        year_dx == 2020 &
        sp == 'P. vivax' ~ g(wpv20),
      TRUE ~ NA_real_
    )) |>
    ungroup()
  
  gini_index <- gini_index |> select(province, year_dx, sp, g)
  return(gini_index)
}

(g_districts <- gini_index(data = cum_dist_sp))
(g_healthunits <- gini_index(data = cum_dist_sp_hu))

# District level
# set.seed(13)
n_boot <- 10^2

ci_pi <- function(data) {
  df <- data %>% 
    arrange(desc(api)) %>% 
    mutate(case_cumsum = cumsum(case),
           pop_cumsum = cumsum(pop),
           c_i = case_cumsum / max(case_cumsum),
           p_i = pop_cumsum / max(pop_cumsum))
  return(df)
}

g_ppf19 <- c()
g_ppv19 <- c()
g_ppf20 <- c()
g_ppv20 <- c()

g_wpf19 <- c()
g_wpv19 <- c()
g_wpf20 <- c()
g_wpv20 <- c()

ci_gini_index <- function(data) {
  
  # Subset data, by year, province, and species
  ppf19 <- filter(data, province == 'Papua',
                  year_dx == 2019,
                  sp == 'P. falciparum')
  ppf19_boot <- slice_sample(ppf19, n = nrow(ppf19), replace = TRUE)
  ppf19_boot <- ci_pi(ppf19_boot)
  g_temp <- g(ppf19_boot)
  g_ppf19 <<- c(g_ppf19, g_temp)
  
  ppv19 <- filter(data, province == 'Papua',
                  year_dx == 2019,
                  sp == 'P. vivax')
  ppv19_boot <- slice_sample(ppv19, n = nrow(ppv19), replace = TRUE)
  ppv19_boot <- ci_pi(ppv19_boot)
  g_temp <- g(ppv19_boot)
  g_ppv19 <<- c(g_ppv19, g_temp)
  
  ppf20 <- filter(data, province == 'Papua',
                  year_dx == 2020,
                  sp == 'P. falciparum')
  ppf20_boot <- slice_sample(ppf20, n = nrow(ppf20), replace = TRUE)
  ppf20_boot <- ci_pi(ppf20_boot)
  g_temp <- g(ppf20_boot)
  g_ppf20 <<- c(g_ppf20, g_temp)
  
  ppv20 <- filter(data, province == 'Papua',
                  year_dx == 2020,
                  sp == 'P. vivax')
  ppv20_boot <- slice_sample(ppv20, n = nrow(ppv20), replace = TRUE)
  ppv20_boot <- ci_pi(ppv20_boot)
  g_temp <- g(ppv20_boot)
  g_ppf20 <<- c(g_ppf20, g_temp)
  
  wpf19 <- filter(data, province == 'West Papua',
                  year_dx == 2019,
                  sp == 'P. falciparum')
  wpf19_boot <- slice_sample(wpf19, n = nrow(wpf19), replace = TRUE)
  wpf19_boot <- ci_pi(wpf19_boot)
  g_temp <- g(wpf19_boot)
  g_wpf19 <<- c(g_wpf19, g_temp)
  
  wpv19 <- filter(data, province == 'West Papua',
                  year_dx == 2019,
                  sp == 'P. vivax')
  wpv19_boot <- slice_sample(wpv19, n = nrow(wpv19), replace = TRUE)
  wpv19_boot <- ci_pi(wpv19_boot)
  g_temp <- g(wpv19_boot)
  g_wpv19 <<- c(g_wpv19, g_temp)
  
  wpf20 <- filter(data, province == 'West Papua',
                  year_dx == 2020,
                  sp == 'P. falciparum')
  wpf20_boot <- slice_sample(wpf20, n = nrow(wpf20), replace = TRUE)
  wpf20_boot <- ci_pi(wpf20_boot)
  g_temp <- g(wpf20_boot)
  g_wpf20 <<- c(g_wpf20, g_temp)
  
  wpv20 <- filter(data, province == 'West Papua',
                  year_dx == 2020,
                  sp == 'P. vivax')
  wpv20_boot <- slice_sample(wpv20, n = nrow(wpv20), replace = TRUE)
  wpv20_boot <- ci_pi(wpv20_boot)
  g_temp <- g(wpv20_boot)
  g_wpv20 <<- c(g_wpv20, g_temp)
}

for (i in 1:n_boot) {
  ci_gini_index(data = api_city_sp_raw)
}

ci_districts <- tibble(
  year = c(
    rep(2019, 4 * n_boot),
    rep(2020, 4 * n_boot)
  ),
  province = c(
    rep('Papua', 2 * n_boot),
    rep('West Papua', 2 * n_boot),
    rep('Papua', 2 * n_boot),
    rep('West Papua', 2 * n_boot)
  ),
  sp = c(
    rep('P. falciparum', n_boot),
    rep('P. vivax', n_boot),
    rep('P. falciparum', n_boot),
    rep('P. vivax', n_boot),
    
    rep('P. falciparum', n_boot),
    rep('P. vivax', n_boot),
    rep('P. falciparum', n_boot),
    rep('P. vivax', n_boot)
  ),
  g = c(
    g_ppf19,
    g_ppv19,
    g_wpf19,
    g_wpv19,
    
    g_ppf20,
    g_ppv20,
    g_wpf20,
    g_wpv20
  )
) %>% 
  mutate(year = factor(year),
         province = factor(province),
         sp = factor(sp),
         g = g / 100)

(ci_districts_plot <- ci_districts %>% 
    ggplot(aes(x = g, y = ..density.., fill = province)) +
    geom_density(alpha = 0.7, colour = 'transparent') +
    facet_grid(rows = vars(sp),
               cols = vars(year)) +
    scale_x_continuous(limits = c(0, 1),
                       breaks = seq(from = 0, to = 1, by = 0.2),
                       expand = c(0.05, 0)) +
    scale_y_continuous(expand = c(0.01, 0)) +
    theme(legend.position = c(0.55, 0.1)) +
    scale_fill_manual(values = colours) +
    labs(y = 'Density\n',
         x = '\nGini index'))

ggsave(filename = "boot_district.png",
       path = here::here("0-graph"),
       width = 8,
       height = 8,
       dpi = 600)

# Percentile-bootstrap CIs
(ci_districts_df <- ci_districts %>% 
    group_by(year, province, sp) %>% 
    summarise(lci = 100 * quantile(g, probs = 0.025),
              uci = 100 * quantile(g, probs = 1 - 0.025)))

# Health unit level
g_ppf19 <- c()
g_ppv19 <- c()
g_ppf20 <- c()
g_ppv20 <- c()

g_wpf19 <- c()
g_wpv19 <- c()
g_wpf20 <- c()
g_wpv20 <- c()

api_hu_sp_raw <- api_hu_sp_raw %>% 
  rename(api = api_hu,
         case = case_hu,
         pop = pop_hu)
api_hu_sp_raw <- ungroup(api_hu_sp_raw)

for (i in 1:n_boot) {
  ci_gini_index(data = api_hu_sp_raw)
}

ci_health_units <- tibble(
  year = c(
    rep(2019, 4 * n_boot),
    rep(2020, 4 * n_boot)
  ),
  province = c(
    rep('Papua', 2 * n_boot),
    rep('West Papua', 2 * n_boot),
    rep('Papua', 2 * n_boot),
    rep('West Papua', 2 * n_boot)
  ),
  sp = c(
    rep('P. falciparum', n_boot),
    rep('P. vivax', n_boot),
    rep('P. falciparum', n_boot),
    rep('P. vivax', n_boot),
    
    rep('P. falciparum', n_boot),
    rep('P. vivax', n_boot),
    rep('P. falciparum', n_boot),
    rep('P. vivax', n_boot)
  ),
  g = c(
    g_ppf19,
    g_ppv19,
    g_wpf19,
    g_wpv19,
    
    g_ppf20,
    g_ppv20,
    g_wpf20,
    g_wpv20
  )
) %>% 
  mutate(year = factor(year),
         province = factor(province),
         sp = factor(sp),
         g = g / 100)

(ci_health_units_plot <- ci_health_units %>% 
    ggplot(aes(x = g, y = ..density.., fill = province)) +
    geom_density(alpha = 0.7, colour = 'transparent') +
    facet_grid(rows = vars(sp),
               cols = vars(year)) +
    scale_x_continuous(limits = c(0, 1),
                       breaks = seq(from = 0, to = 1, by = 0.2),
                       expand = c(0.05, 0)) +
    scale_y_continuous(expand = c(0.01, 0)) +
    theme(legend.position = c(0.55, 0.1)) +
    scale_fill_manual(values = colours) +
    labs(y = 'Density\n',
         x = '\nGini index'))

ggsave(filename = "boot_hu.png",
       path = here::here("0-graph"),
       width = 8,
       height = 8,
       dpi = 600)

# Percentile-bootstrap CIs
(ci_health_units_df <- ci_health_units %>% 
    group_by(year, province, sp) %>% 
    summarise(lci = 100 * quantile(g, probs = 0.025),
              uci = 100 * quantile(g, probs = 1 - 0.025)))

# Exploratory graphs ------------------------------------------------------

colours_four <- c('#077b8a', '#a2d5c6', '#5c3c92', '#d72631')

# District-specific API
esismal_api <- esismal |> 
  group_by(year_dx, province, city) |> 
  summarise(n = unique(n),
            case = n()) |> 
  mutate(api = (case / n) * 1000) |> 
  ungroup() |> 
  mutate(api_cat = case_when(api < 1 ~ '[0, 1)',
                             api >= 1 & api < 5 ~ '[1, 5)',
                             api >= 5 ~ '[5, ∞)',
                             TRUE ~ NA_character_) |> factor())
esismal_api20 <- filter(esismal_api, year_dx == 2020)
esismal_api19 <- filter(esismal_api, year_dx == 2019)

api_plot_overall <- esismal_api |>
  ggplot(aes(x = api,
             y = reorder(city, api),
             colour = factor(year_dx),
             shape = province)) +
  # geom_bar(stat = 'identity', position = 'dodge2') +
  geom_point(size = 3, alpha = 0.7) +
  scale_x_continuous(limits = c(0.1, 1000),
                     trans = "log10",
                     expand = c(0, 0),
                     labels = number_format(accuracy = 1, big.mark = ",")) +
  scale_y_discrete(expand = c(0.01, 0)) +
  scale_colour_manual(values = colours) +
  theme(
    # axis.text.y = element_blank(),
    legend.position = c(0.95, 0.1),
    legend.justification = "right",
    legend.direction = "vertical",
    axis.text.x = element_text(size = 6),
    panel.spacing = unit(1, "lines"),
    legend.spacing.x = unit(0.05, "cm"),
    legend.margin = margin(0.5, 0.5, 0.5, 0.5),
    legend.box.margin = margin(0, 0, 0, 0),
    legend.text = element_text(margin = margin(r = 5, unit = "mm")),
    plot.margin = unit(c(1, 1, 1, 0), "cm"),
    text = element_text(size = 9)) +
  labs(y = '',
       x = '\nAnnual parasite incidence per 1000')

api_plot_overall
ggsave(filename = "api_overall.png",
       path = here::here("0-graph"),
       width = 7,
       height = 9,
       dpi = 600)

# Health unit types by district
(fig_ht_papua <- esismal |> 
  mutate(hu_type = factor(hu_type, levels = c('Hospital',
                                              'Health centre',
                                              'Clinic',
                                              'Other')),
         city2 = reorder(esismal$city,
                         esismal$hu_type,
                         FUN = function(x) mean(as.numeric(x)))) |>
  filter(province == 'Papua') |> 
  ggplot(aes(y = city2,
             fill = hu_type)) +
    geom_bar(position = position_fill(reverse = TRUE)) +
    scale_fill_manual(values = colours_four) +
    facet_grid(cols = vars(year_dx)) +
    theme(legend.position = "none",
          axis.text.x = element_text(size = 6),
          panel.spacing = unit(1, "lines"),
          legend.spacing.x = unit(0.05, "cm"),
          legend.margin = margin(0, 0, 0, 0),
          legend.box.margin = margin(0, 0, 0, 0),
          legend.text = element_text(margin = margin(r = 5, unit = "mm"))) +
    scale_y_discrete(expand = c(0, 0)) +
    labs(y = '',
         x = ''))

(fig_ht_wpapua <- esismal |> 
    mutate(hu_type = factor(hu_type, levels = c('Hospital',
                                                'Health centre',
                                                'Clinic',
                                                'Other')),
           city2 = reorder(esismal$city,
                           esismal$hu_type,
                           FUN = function(x) mean(as.numeric(x)))) |>
  filter(province == 'West Papua') |> 
  ggplot(aes(y = city2,
             fill = hu_type)) +
  geom_bar(position = position_fill(reverse = TRUE)) +
  scale_fill_manual(values = colours_four) +
  facet_grid(cols = vars(year_dx)) +
  theme(legend.position = "bottom",
        legend.justification = "right",
        legend.direction = "horizontal",
        axis.text.x = element_text(size = 6),
        panel.spacing = unit(1, "lines"),
        legend.spacing.x = unit(0.05, "cm"),
        legend.margin = margin(0, 0, 0, 0),
        legend.box.margin = margin(0, 0, 0, 0),
        legend.text = element_text(margin = margin(r = 5, unit = "mm"))) +
  scale_y_discrete(expand = c(0, 0)) +
  labs(y = '',
       x = '\nProportion of health unit type'))

fig_ht <- plot_grid(fig_ht_papua,
                    fig_ht_wpapua,
                    ncol = 1, align = "v",
                    labels = c('A', 'B'),
                    label_size = 10,
                    rel_heights = c(1, 0.65),
                    rel_widths = c(1, 1),
                    label_y = 1,
                    label_x = 0)

fig_ht

ggsave(filename = "hu_types.png",
       path = here::here("0-graph"),
       width = 7,
       height = 9,
       dpi = 600)

# Clinical severity

fig_severity_overall <- esismal |> 
  mutate(hu_type = factor(hu_type, levels = c('Hospital',
                                              'Health centre',
                                              'Clinic',
                                              'Other'))) |> 
  ggplot(aes(x = hu_type, fill = severe)) +
    geom_bar(position = 'fill') +
    scale_fill_manual(values = colours) +
    theme(legend.position = "bottom",
          legend.justification = "right",
          legend.direction = "horizontal",
          axis.text.x = element_text(size = 6),
          panel.spacing = unit(1, "lines"),
          legend.spacing.x = unit(0.05, "cm"),
          legend.margin = margin(0, 0, 0, 0),
          legend.box.margin = margin(0, 0, 0, 0),
          legend.text = element_text(margin = margin(r = 5, unit = "mm"))) +
    facet_grid(cols = vars(year_dx),
               rows = vars(province)) +
    labs(x = '',
         y = 'Proportion of clinical severity\n')

fig_severity_overall

ggsave(filename = "severity_overall.png",
       path = here::here("0-graph"),
       width = 7,
       height = 7.5,
       dpi = 600)
  
# Occupation

api_plot_papua20 <- esismal_api20 |> 
  filter(province == 'Papua') |> 
  ggplot(aes(x = api,
             y = reorder(city, api))) +
    # geom_bar(stat = 'identity') +
    geom_point(shape = 18, size = 2.5) +
    scale_x_continuous(limits = c(0.1, 1000),
                       trans = "log10",
                       expand = c(0, 0),
                       labels = number_format(accuracy = 1, big.mark = ",")) +
    scale_y_discrete(expand = c(0.015, 0)) +
    theme(
          axis.text.y = element_blank(),
          legend.position = "none",
          legend.justification = "right",
          legend.direction = "horizontal",
          axis.text.x = element_text(size = 6),
          panel.spacing = unit(1, "lines"),
          legend.spacing.x = unit(0.05, "cm"),
          legend.margin = margin(0, 0, 0, 0),
          legend.box.margin = margin(0, 0, 0, 0),
          legend.text = element_text(margin = margin(r = 5, unit = "mm")),
          plot.margin = unit(c(1, 1, 1, 0), "cm"),
          text = element_text(size = 7.5)) +
    labs(y = '',
         x = '\nAnnual parasite incidence per 1000')

esismal_api_joined <- esismal |> 
  group_by(year_dx, city) |> 
  summarise(case = n(),
            pop = unique(n),
            api = (case / pop) * 1000)

occupation_papua20 <- esismal |> 
  group_by(year_dx, city) |> 
  nest() |> 
  full_join(y = esismal_api_joined, by = c('year_dx', 'city')) |> 
  unnest(data) |> 
  ungroup() |> 
  filter(age >= 18) |> 
  filter(province == 'Papua' & year_dx == 2020) |> 
  mutate(city = fct_reorder(city, api)) |> 
  ggplot(aes(y = city,
             fill = occupation)) +
    geom_bar(position = 'fill') +
    scale_fill_brewer(type = "qual",
                      palette = "Paired",
                      na.value = 'Gray25') +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_discrete(expand = c(0.01, 0)) +
    theme(legend.position = c(2.6, -0.15),
          legend.justification = "right",
          legend.direction = "horizontal",
          axis.text.x = element_text(size = 6),
          panel.spacing = unit(1, "lines"),
          legend.spacing.x = unit(0.05, "cm"),
          legend.margin = margin(0, 0, 0, 0),
          legend.box.margin = margin(0, 0, 0, 0),
          legend.text = element_text(margin = margin(r = 5, unit = "mm")),
          plot.margin = unit(c(1, 0, 3, 1), "cm"),
          text = element_text(size = 7.5)) +
    labs(y = '',
         x = '\nProportion of occupation')

fig_occupation_papua <- plot_grid(occupation_papua20,
                                  api_plot_papua20,
                                  align = 'h',
                                  ncol = 2,
                                  labels = c('', ''),
                                  label_size = 10,
                                  rel_heights = c(1, 1),
                                  rel_widths = c(1, 1),
                                  label_y = 1,
                                  label_x = 0)
fig_occupation_papua

ggsave(filename = "occupation_papua.png",
       path = here::here("0-graph"),
       width = 8,
       height = 8,
       dpi = 600)

api_plot_wpapua20 <- esismal_api20 |> 
  filter(province == 'West Papua') |> 
  ggplot(aes(x = api,
             y = reorder(city, api))) +
  # geom_bar(stat = 'identity') +
  geom_point(shape = 18, size = 2.5) +
  scale_x_continuous(limits = c(0.1, 1000),
                     trans = "log10",
                     expand = c(0, 0),
                     labels = number_format(accuracy = 1, big.mark = ",")) +
  scale_y_discrete(expand = c(0.035, 0)) +
  theme(
    axis.text.y = element_blank(),
    legend.position = "none",
    legend.justification = "right",
    legend.direction = "horizontal",
    axis.text.x = element_text(size = 6),
    panel.spacing = unit(1, "lines"),
    legend.spacing.x = unit(0.05, "cm"),
    legend.margin = margin(0, 0, 0, 0),
    legend.box.margin = margin(0, 0, 0, 0),
    legend.text = element_text(margin = margin(r = 5, unit = "mm")),
    plot.margin = unit(c(1, 1, 1, 0), "cm"),
    text = element_text(size = 7.5)) +
  labs(y = '',
       x = '\nAnnual parasite incidence per 1000')

occupation_wpapua20 <- esismal |> 
  group_by(year_dx, city) |> 
  nest() |> 
  full_join(y = esismal_api_joined, by = c('year_dx', 'city')) |> 
  unnest(data) |> 
  ungroup() |> 
  filter(age >= 18) |> 
  filter(province == 'West Papua' & year_dx == 2020) |> 
  mutate(city = fct_reorder(city, api)) |> 
  ggplot(aes(y = city,
             fill = occupation)) +
  geom_bar(position = 'fill') +
  scale_fill_brewer(type = "qual",
                    palette = "Paired",
                    na.value = 'Gray25') +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) +
  theme(legend.position = c(2.55, -0.35),
        legend.justification = "right",
        legend.direction = "horizontal",
        axis.text.x = element_text(size = 6),
        panel.spacing = unit(1, "lines"),
        legend.spacing.x = unit(0.05, "cm"),
        legend.margin = margin(0, 0, 0, 0),
        legend.box.margin = margin(0, 0, 0, 0),
        legend.text = element_text(margin = margin(r = 5, unit = "mm")),
        plot.margin = unit(c(1, 0, 3, 1), "cm"),
        text = element_text(size = 7.5)) +
  labs(y = '',
       x = '\nProportion of occupation')

fig_occupation_wpapua <- plot_grid(occupation_wpapua20,
                     api_plot_wpapua20,
                     align = 'h',
                     ncol = 2,
                     labels = c('', ''),
                     label_size = 10,
                     rel_heights = c(1, 1),
                     rel_widths = c(1, 1),
                     label_y = 1,
                     label_x = 0)
fig_occupation_wpapua

ggsave(filename = "occupation_wpapua.png",
       path = here::here("0-graph"),
       width = 8,
       height = 4.5,
       dpi = 600)

# Gini-index, more resolution
temp <- esismal |> 
  group_by(year_dx, province, city, health_unit) |> 
  summarise(case = n(),
            pop_city = unique(n)) |>
  ungroup() |> 
  group_by(province, city) |> 
  nest() |> 
  left_join(y = nhu_est,
            by = c('province', 'city')) |> 
  unnest(data) |> 
  ungroup() |> 
  mutate(pop = pop_city / n_hu, # May be improved
         api = case / pop) |> 
  
  # Add rows with zero cases
  group_by(province, city, year_dx) |> 
  nest() |> 
  mutate(
    data_exp = map(
      .x = data,
      .f = function(data = .x) {
        # Extract n health units
        n_hu_est <- data$n_hu[1]
        n_hu_esismal <- nrow(data)
        
        # Extract the difference then add rows if positive
        if (n_hu_est > n_hu_esismal) {
          n_hu_diff <- n_hu_est - n_hu_esismal
          
          add_df <- tibble(.rows = n_hu_diff,
                           health_unit = NA,
                           case = 0,
                           pop_city = data$pop_city[1],
                           n_hu = data$n_hu[1],
                           pop = data$pop[1],
                           api = 0)
          
          data <- data |> add_row(add_df)
        } else {
          data <- data
        }
      }
    )
  ) |> 
  mutate(ci_pi = map(.x = data_exp, .f = ~ci_pi(.x)),
         g = map_dbl(.x = ci_pi, .f = ~g(.x)) / 100) |> 
  select(year_dx, province, city, g, data, data_exp, ci_pi) |> 
  arrange(year_dx, province, city)

api_nonsp <- esismal |> 
  group_by(year_dx, province, city) |> 
  summarise(case = n(),
            pop = unique(n),
            api = (case / pop) * 1000)

gresol_api <- temp |> 
  full_join(y = api_nonsp, by = c('year_dx', 'province', 'city')) |> 
  select(year_dx, province, city, g, case, api) |> 
  mutate(year_dx = factor(year_dx))

model_gresol <- lm(log(api) ~ 1 + g + year_dx, data = gresol_api)
broom::tidy(model_gresol)
performance::check_model(model_gresol)

fig_gresol <- gresol_api |> 
  ggplot(aes(x = api, y = g)) +
  geom_smooth(aes(linetype = year_dx),
              method = 'lm',
              se = TRUE,
              size = 1.3,
              fill = "gray80",
              colour = colours_three[1]) +  
  geom_point(aes(colour = year_dx, size = case), alpha = 0.4) +
  scale_size_continuous(breaks = c(1000, 5000, 25000, 50000),
                        labels = number_format(big.mark = ",")) +
  scale_colour_manual(values = colours) +
  scale_x_continuous(trans = "log10",
                     limits = c(0.1, 1000),
                     labels = number_format(accuracy = 1, big.mark = ","),
                     expand = c(0, 0.05)) +
  scale_y_continuous(limits = c(0, 1),
                     breaks = seq(0, 1, by = 0.2),
                     labels = percent_format(),
                     expand = c(0, 0.01)) +
  theme(panel.grid.minor.x = element_line(),
        legend.position = c(0.75, 0.15),
        legend.box = "horizontal",
        legend.spacing.x = unit(0.01, 'mm'),     
        legend.key.width = unit(0.8, "cm"),
        plot.margin = unit(c(5, 5, 5, 5), "mm")) +
  guides(size = guide_legend(override.aes = list(colour = "gray70"))) +
  coord_fixed(ratio = 5) +
  labs(x = "\nAnnual parasite incidence per 1000",
       y = "Gini index\n")

fig_gresol

ggsave(filename = "gini_hires.png",
       path = here::here("0-graph"),
       width = 6,
       height = 8,
       dpi = 600)

# Appendix ----------------------------------------------------------------

sessioninfo::platform_info()

# # pekerjaan, lab, kematian
# 
# Number of unique health units per district
# api_nhu <- esismal |>
#   filter(hu_type == 'Health centre') |>
#   group_by(year_dx, city) |>
#   summarise(n_hu = unique(health_unit)) |>
#   summarise(n = n()) |>
#   ungroup() |>
#   group_by(year_dx) |>
#   arrange(desc(n), .by_group = TRUE) |> 
#   ungroup() |>
#   full_join(esismal_api, by = c('year_dx', 'city')) |> 
#   select(year_dx, city, n.x, province, api) |> 
#   rename(nhu = n.x)
# model <- lm(log(api) ~ 1 + nhu + year_dx, data = api_nhu)
# broom::tidy(model)
# broom::glance(model)
# performance::check_model(model)
# qplot(api_nhu$nhu, log(api_nhu$api)) +
#   geom_smooth(method = 'lm', aes(colour = factor(api_nhu$year_dx)))

# 
# esismal |>
#   na.omit(sex) |>
#   ggplot(aes(y = age_cat,
#              fill = severe)) +
#   geom_bar(position = 'fill') +
#   scale_fill_manual(values = colours_four) +
#   facet_grid(cols = vars(sex),
#              rows = vars(province)) +
#   theme(legend.position = "bottom",
#         legend.justification = "right",
#         legend.direction = "horizontal",
#         axis.text.x = element_text(size = 6),
#         panel.spacing = unit(1, "lines"),
#         legend.spacing.x = unit(0.05, "cm"),
#         legend.margin = margin(0, 0, 0, 0),
#         legend.box.margin = margin(0, 0, 0, 0),
#         legend.text = element_text(margin = margin(r = 5, unit = "mm"))) +
#   scale_y_discrete(expand = c(0, 0)) +
#   labs(y = '',
#        x = '')
# 
# esismal |> 
#   mutate(severe = factor(severe, levels = c('Severe malaria',
#                                             'Uncomplicated malaria')),
#          city2 = reorder(esismal$city,
#                          esismal$severe,
#                          FUN = function(x) mean(as.numeric(x)))) |>
#   filter(province == 'Papua') |> 
#   ggplot(aes(y = city2,
#              fill = severe)) +
#   geom_bar(position = 'fill') +
#   scale_fill_manual(values = colours_four) +
#   facet_grid(cols = vars(year_dx)) +
#   theme(legend.position = "bottom",
#         legend.justification = "right",
#         legend.direction = "horizontal",
#         axis.text.x = element_text(size = 6),
#         panel.spacing = unit(1, "lines"),
#         legend.spacing.x = unit(0.05, "cm"),
#         legend.margin = margin(0, 0, 0, 0),
#         legend.box.margin = margin(0, 0, 0, 0),
#         legend.text = element_text(margin = margin(r = 5, unit = "mm"))) +
#   scale_y_discrete(expand = c(0, 0)) +
#   labs(y = '',
#        x = '\nCumulative proportion of clinical severity')

