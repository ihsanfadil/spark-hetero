
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
  mutate(`P. vivax` = `P. vivax` + `P. falciparum/vivax`,
         `P. falciparum` = `P. falciparum` + `P. falciparum/vivax`) %>% 
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
ggsave(filename = "figure_1.png",
       path = here::here("0-graph"),
       height = 7,
       dpi = 600)









# Appendix ----------------------------------------------------------------

sessioninfo::platform_info()

