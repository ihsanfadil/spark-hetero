
# Preamble ----------------------------------------------------------------

# Code author  : Ihsan Fadilah
# Email        : ifadilah@eocru.org
# Last updated : Early November 2021
# Project      : SPARK project on malaria heterogeneity

# This script provides code for visualising the geospatial maps.

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
  text = element_text(size = 7, family = "Fira Code"), # Font
  plot.title = element_text(hjust = 0),      # Centre-align title
  plot.subtitle = element_text(hjust = 0),   # Centre-align subtitle
  legend.title = element_blank(),            # Remove legend title
  legend.position = c(0.85, 0.85),           # Move legend to bottom right
  legend.background = element_blank(),       # Remove legend background
  legend.box.background = element_blank(),   # Remove legend-box background
  legend.spacing.y = unit(0.01, 'mm'),       # Make legend closer
  legend.key.height = unit(0.25, "cm"),       # Make legend closer
  # panel.grid.minor = element_blank(),        # Remove minor lines
  panel.grid.minor.x = element_blank(),      # Remove minor lines on the x axis
  axis.title.x = element_text(hjust = 1),    # Move title for x-axis
  axis.title.y = element_text(hjust = 0.5)     # Move title for y-axis
)

# Data --------------------------------------------------------------------

# Read in the datasets
## 2019
esismal_19 <- here::here("0-data", "esismal.rds") %>%
  read_rds() %>% 
  filter(year_dx == 2019)

## 2020
esismal_20 <- here::here("0-data", "esismal.rds") %>%
  read_rds() %>% 
  filter(year_dx == 2020)

# API
## 2019
api_city_raw_19 <- esismal_19 %>% 
  group_by(city) %>% 
  dplyr::summarise(province = unique(province),
                   case = n(),
                   pop = unique(n),
                   api = (case / pop) * 1000) %>% 
  ungroup()

## 2020
api_city_raw_20 <- esismal_20 %>% 
  group_by(city) %>% 
  dplyr::summarise(province = unique(province),
                   case = n(),
                   pop = unique(n),
                   api = (case / pop) * 1000) %>% 
  ungroup()

# Maps --------------------------------------------------------------------

# Maps generation
library(maptools)
library(raster)
library(plyr)
library(rgdal)
library(ggmap)

# Get the Indonesian map data
idn_two <- raster::getData("GADM",
                           path = here::here('0-data'),
                           country = "IDN",       # Indonesia
                           level = 2)             # Level: Kabupaten/Kota

# Filter: Papua and West Papua
papua_region <- idn_two[idn_two$NAME_1 == "Papua" | # 2019
                        idn_two$NAME_1 == "Papua Barat", ]

papua_region_20 <- idn_two[idn_two$NAME_1 == "Papua" | # 2019
                           idn_two$NAME_1 == "Papua Barat", ]

# Convert to a dataframe
papua_region_df <- fortify(papua_region)
papua_region_df_20 <- fortify(papua_region)

# Get the risk data
api_papua <- api_city_raw_19 %>%
  dplyr::select(city, api) %>%
  dplyr::rename(NAME_2 = city)

api_papua_20 <- api_city_raw_20 %>%
  dplyr::select(city, api) %>%
  dplyr::rename(NAME_2 = city)

# Join the risk data to map
papua_region@data$id <- rownames(papua_region@data)
papua_region@data <- full_join(by = "NAME_2",
                               api_papua,
                               papua_region@data)
papua_api_map <- fortify(papua_region)
papua_api_map <- full_join(papua_api_map, papua_region@data, by = "id")

# Join the risk data to map (2020)
papua_region_20@data$id <- rownames(papua_region_20@data)
papua_region_20@data <- full_join(by = "NAME_2",
                                  api_papua_20,
                                  papua_region_20@data)
papua_api_map_20 <- fortify(papua_region_20)
papua_api_map_20 <- full_join(papua_api_map_20, papua_region_20@data, by = "id")

# Map
## 2019
papua_api_map <- papua_api_map %>%
  mutate(
    api_cat = case_when(api < 10 ~ "[0-10)",
                        api < 100 ~ "[10-100)",
                        api < 1000 ~ "[100-1000)",
                        api >= 1000 ~ "[1000-∞)",
                        TRUE ~ NA_character_),
    api_cat = factor(api_cat),
    api_cat = ordered(api_cat,
                      levels = c("[1000-∞)",
                                 "[100-1000)",
                                 "[10-100)",
                                 "[0-10)"))
  )

## 2020
papua_api_map_20 <- papua_api_map_20 %>%
  mutate(
    api_cat = case_when(api < 10 ~ "[0-10)",
                        api < 100 ~ "[10-100)",
                        api < 1000 ~ "[100-1000)",
                        api >= 1000 ~ "[1000-∞)",
                        TRUE ~ NA_character_),
    api_cat = factor(api_cat),
    api_cat = ordered(api_cat,
                      levels = c("[1000-∞)",
                                 "[100-1000)",
                                 "[10-100)",
                                 "[0-10)"))
  )

# Plot
papua_map_2019 <- ggplot() +
  geom_polygon(data = papua_api_map,
               aes(x = long,
                   y = lat,
                   group = group,
                   fill = api_cat),
               color = "gray50",
               size = 0.2) +
  scale_fill_manual(na.value = "gray58",
                    values = rev(c("#455558", "#688084", "#8babb0", "#aed6dc")),
                    name = "Annual parasite incidence\nper 1000 population") +
  theme(aspect.ratio = 1,
        legend.position = c(0.23, 0.25),
        legend.title = element_text(size = 8),
        legend.text = element_text(size = 7.5),
        legend.title.align = 0,
        legend.spacing.y = unit(3, 'mm'),
        legend.key.size = unit(1, 'lines'),
        legend.key.height = unit(3, "mm"),
        panel.grid = element_blank(),
        axis.text = element_blank()
  ) +
  labs(x = "",
       y = "")

papua_map_2020 <- ggplot() +
  geom_polygon(data = papua_api_map_20,
               aes(x = long,
                   y = lat,
                   group = group,
                   fill = api_cat),
               color = "gray50",
               size = 0.2) +
  scale_fill_manual(na.value = "gray58",
                    values = rev(c("#455558", "#688084", "#8babb0", "#aed6dc")),
                    name = "Annual parasite incidence\nper 1000 population") +
  theme(aspect.ratio = 1,
        legend.position = c(0.23, 0.25),
        legend.title = element_text(size = 8),
        legend.text = element_text(size = 7.5),
        legend.title.align = 0,
        legend.spacing.y = unit(3, 'mm'),
        legend.key.size = unit(1, 'lines'),
        legend.key.height = unit(3, "mm"),
        panel.grid = element_blank(),
        axis.text = element_blank()
  ) +
  labs(x = "",
       y = "")

papua_map_2019
papua_map_2020

fig_2 <- plot_grid(papua_map_2019, papua_map_2020,
                   ncol = 1, align = "v", axis = "l",
                   labels = c('A', 'B'),
                   label_size = 15)
fig_2

ggsave(filename = "figure_2.png",
       path = here::here("0-graph"),
       height = 10, width = 5,
       dpi = 600)

# Appendix ----------------------------------------------------------------

sessioninfo::platform_info()
