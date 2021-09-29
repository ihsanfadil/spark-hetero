
# Preamble ----------------------------------------------------------------

# Code author  : Ihsan Fadilah
# Email        : ifadilah@eocru.org
# Last updated : Late September 2021
# Project      : SPARK project on malaria heterogeneity

# This script provides code from importing to further cleaning the data for the
# next steps of analysis.

# Setup -------------------------------------------------------------------

library(tidyverse)  # Tidy and readable code
library(lubridate)  # Handle time objects
library(labelled)   # Label variables
library(readxl)     # Read in Excel files
library(haven)      # Read .dta files
library(here)       # Navigate files

# Import ------------------------------------------------------------------

# Read in the raw data
## 2019-2020's individual data
raw_esismal <- here("0-data", # Includes 2020's data
                    "esismal 2019-2020_ieprocess210801.dta") %>% 
  read_dta()

## 2010-2020's aggregate data, not including 2018
raw_timeseries <- here("0-data", "pcd_data_papua.csv") %>% read_csv()

## 2018's aggregate data, added later on
path_2018 <- here("0-data", "REKAP 2018 EDIT FAJAR 02 APR 19 final.xlsx")
sheets_2018 <- excel_sheets(path_2018)[1:12]

sheets_2018 %>%
  map(function(sheet){ # Iterate through each sheet name
    assign(x = sheet,
           value = read_xlsx(path = path_2018, sheet = sheet),
           envir = .GlobalEnv)
  })

list_2018 <- list(JAN, FEB, MAR, APR, MEI, JUN, JUL, AGS, SEPT, OKT, NOV, DES)

raw_timeseries_2018 <- list_2018[[1]] %>% # Initialise
  mutate(month = rep(1, nrow(JAN)),
         month = month(month, label = TRUE, abbr = TRUE))

for (i in 2:12) {
  dat <- list_2018[[i]] %>%
    mutate(month = rep(i, nrow(list_2018[[i]])),
           month = month(month, label = TRUE, abbr = TRUE))
  raw_timeseries_2018 <- add_row(raw_timeseries_2018, dat)
}

## Projected district-specific population sizes (2019-2020)
raw_pop_papua <- here("0-data", "estpop-papua.xlsx") %>% read_xlsx()
raw_pop_wpapua <- here("0-data", "estpop-papua-barat.xlsx") %>% read_xlsx()

# Merge and clean ---------------------------------------------------------

# eSismal (2019-2020)
esismal <- raw_esismal %>% 
  mutate(
    id = as.character(id),
    
    # Make a date object
    date_dx = dmy(tglkun),
    
    # Make month
    bulankun = if_else(bulankun %in% 1:12, bulankun, NA_real_),
    bulankun = month(bulankun),
    
    # Fill in missing dates from `tahun` and `bulankun`
    date_dx = if_else(
      is.na(date_dx),
      str_c(as.character(tahun), '-', as.character(bulankun), '-', '01'),
      as.character(date_dx)
    ),
    
    # Make a date object
    date_dx = ymd(date_dx),
    
    week_dx = isoweek(date_dx),
    month_dx = month(date_dx, label = TRUE, abbr = TRUE),
    year_dx = year(date_dx),
    
    # Parasite species
    sp = case_when(
      # 2019
      year_dx == 2019 & parasit == 'Pf' ~ 'P. falciparum',
      year_dx == 2019 & parasit == 'Pv' ~ 'P. vivax',
      year_dx == 2019 & parasit == 'Pmix' ~ 'P. falciparum/vivax',
      
      year_dx == 2019 & parasit == 'Pm' ~ 'Other',
      year_dx == 2019 & parasit == 'Po' ~ 'Other',
      year_dx == 2019 & parasit == 'Pk' ~ 'Other',
      year_dx == 2019 & parasit == 'Suspek Pk' ~ 'Other',
      
      # 2020
      year_dx == 2020 & parasit == 'Pf' ~ 'P. falciparum',
      year_dx == 2020 & parasit == 'Pv' ~ 'P. vivax',
      year_dx == 2020 & parasit == 'Mix' ~ 'P. falciparum/vivax',
      
      year_dx == 2020 & parasit == 'Pm' ~ 'Other',
      year_dx == 2020 & parasit == 'Po' ~ 'Other',
      year_dx == 2020 & parasit == 'Pk' ~ 'Other',
      year_dx == 2020 & parasit == 'Suspek Pk' ~ 'Other',
      
      TRUE ~ NA_character_
    ),
    sp = factor(sp,
                levels = c('P. falciparum',
                           'P. vivax',
                           'P. falciparum/vivax',
                           'Other'),
                labels = c('P. falciparum',
                           'P. vivax',
                           'P. falciparum/vivax',
                           'Other')),
    
    # Age
    age = umur_th,
    
    # Biological sex
    sex = case_when(
      # 2019
      year_dx == 2019 & jnskel == 'L' ~ 'Male',
      year_dx == 2019 & jnskel == 'P' ~ 'Female',
      
      # 2020
      year_dx == 2020 & jnskel == 'l' ~ 'Male',
      year_dx == 2020 & jnskel == 'L' ~ 'Male',
      year_dx == 2020 & jnskel == 'p' ~ 'Female',
      year_dx == 2020 & jnskel == 'P' ~ 'Female',
      
      TRUE ~ NA_character_
    ),
    sex = factor(sex,
                 levels = c('Male', 'Female'),
                 labels = c('Male', 'Female')), 
    
    # Administrative areas
    province = str_to_title(nama_propinsi),
    city = str_to_title(nama_kabupaten),
    health_unit = str_to_title(nama_fasyankes)
  ) %>%
  
  # Select only those variables required
  select(id, # Unique identifier
         date_dx, week_dx, month_dx, year_dx, # Date of visit
         sp, # Parasite diagnosis
         age, sex, # Age and sex
         province, city, health_unit) %>%
  
  # Filter only those in Papua and West Papua
  filter(province == "Papua" | province == "Papua Barat") %>% 
  mutate(province = if_else(province == "Papua Barat",
                            "West Papua",
                            province)) %>% 
  
  # Filter only those in 2019 and 2020 when diagnosed
  # 391 had year 2021, excluded
  # 66 had year missing, excluded
  filter(year_dx == 2019 | year_dx == 2020)

# Projected district-specific population sizes (2019-2020)
## Papua
pop_papua <- raw_pop_papua %>%
  rename(city = "Kabupaten",
         year_18 = "Jumlah Penduduk Proyeksi (Jiwa)",
         year_19 = "...3",
         year_20 = "...4") %>%
  slice(-c(1, 31:35)) %>%
  mutate(year_18 = as.numeric(year_18),
         year_19 = as.numeric(year_19),
         year_20 = as.numeric(year_20),
         province = rep("Papua", n())) %>%
  select(province, city, year_19, year_20)

## West Papua
pop_wpapua <- raw_pop_wpapua %>%
  select("Kabupaten/Kota", "...9", "...10") %>%
  rename(city = "Kabupaten/Kota",
         year_19 = "...9",
         year_20 = "...10") %>%
  slice(-c(1:2, 17:20)) %>%
  mutate(year_19 = str_sub(year_19, end = -4),
         year_20 = str_sub(year_20, end = -4)) %>%
  mutate(year_19 = as.numeric(year_19),
         year_20 = as.numeric(year_20)) %>%
  slice(-14) %>%
  mutate(province = rep("West Papua", n())) %>%
  select(province, city, year_19, year_20)

## Combine datasets of estimated district-specific population sizes (2019-2020)
pop_city <- bind_rows(pop_wpapua, pop_papua) %>%
  pivot_longer(cols = starts_with("year_"),
               names_to = "year",
               values_to = "n") %>%
  mutate(year_dx = if_else(year == "year_19", 2019, 2020)) %>% 
  select(year_dx, province, city, n) %>% 
  arrange(year_dx, province, city)

# Add estimated annual district-specific population sizes by city to eSismal
esismal <- esismal %>% 
  group_by(year_dx, province, city) %>% 
  nest() %>% 
  full_join(pop_city, by = c('year_dx', 'province', 'city')) %>%
  unnest(data) %>% 
  select(id,
         province, city, n,
         date_dx, week_dx, month_dx, year_dx,
         sp,
         age, sex,
         health_unit) %>% 
  ungroup()

# Label the selected variables
var_label(esismal) <- list(
  id = 'Patient unique identifier',
  province = 'Province',
  city = 'Kota/Kabupaten',
  n = 'Projected population size for districts',
  date_dx = 'Date of visit',
  week_dx = 'Week number of visit',
  month_dx = 'Month of visit',
  year_dx = 'Year of visit',
  sp = 'Malaria species',
  age = 'Age (years)',
  sex = 'Biological sex',
  health_unit = 'Health unit'
)

# Time series data
# https://www.bps.go.id/publication/2013/10/07/
# 053d25bed2e4d62aab3346ec/proyeksi-penduduk-indonesia-2010-2035.html

# By year (not used in the paper)
time_series_year <- raw_timeseries %>%
  mutate(POS_TOTAL = pmax(POS_GENDER_TOTAL, POS_SPECIES_TOTAL)) %>%
  group_by(YEAR, ADMIN1) %>%
  summarise(total_case = sum(POS_TOTAL)) %>%
  ungroup() %>%
  select(YEAR, total_case, ADMIN1) %>%
  mutate(year = as.numeric(YEAR),
         province = str_to_title(ADMIN1)) %>%
  select(year, province, total_case) %>%
  add_row(.before = 2,
          year = as.numeric(2010),
          province = "Papua Barat") %>%
  add_row(.before = 17,
          year = as.numeric(c(2018, 2018)),
          province = c("Papua", "Papua Barat")) %>%
  add_row(year = as.numeric(c(2020, 2020)),
          province = c("Papua", "Papua Barat")) %>%
  mutate(province = if_else(province == "Papua", "Papua", "West Papua"),
         province = factor(province),
         
         # Province and year-specific projected population sizes
         pop = c(
           2857.0 * 1000, # 2010 Papua
           765.3 * 1000 , # 2010 West Papua
           
           2915.3 * 1000, # 2011 Papua
           786.0 * 1000 , # 2011 West Papua
           
           2973.8 * 1000, # 2012 Papua
           807.0 * 1000 , # 2012 West Papua
           
           3032.5 * 1000, # 2013 Papua
           828.3 * 1000 , # 2013 West Papua
           
           3091.0 * 1000, # 2014 Papua
           849.8 * 1000 , # 2014 West Papua
           
           3149.4 * 1000, # 2015 Papua
           871.5 * 1000 , # 2015 West Papua
           
           3207.4 * 1000, # 2016 Papua
           893.4 * 1000 , # 2016 West Papua
           
           3265.2 * 1000, # 2017 Papua
           915.4 * 1000 , # 2017 West Papua
           
           3322.5 * 1000, # 2018 Papua
           937.5 * 1000 , # 2018 West Papua
           
           3379.3 * 1000, # 2019 Papua
           959.6 * 1000 , # 2019 West Papua
           
           3435.4 * 1000, # 2020 Papua
           981.8 * 1000   # 2020 West Papua
         ),
         
         api = (total_case / pop) * 1000)

## By month
time_series_month <- raw_timeseries %>% 
  mutate(POS_TOTAL = pmax(POS_GENDER_TOTAL, POS_SPECIES_TOTAL),
         day = rep(1, dim(raw_timeseries)[1]),
         date = str_c(YEAR, "-", MONTH, "-", day)) %>%
  relocate(day, .after = MONTH) %>% 
  relocate(date, .after = day) %>% 
  mutate(date = ymd(date),
         year = year(date),
         month = month(date, label = TRUE, abbr = TRUE)) %>% 
  relocate(month) %>% 
  relocate(year) %>% 
  mutate(province = ADMIN1,
         city = ADMIN2,
         case = POS_TOTAL) %>% 
  select(year, month, date, province, city, case) %>% 
  mutate(province = str_to_title(province),
         province = if_else(province == "Papua", "Papua", "West Papua") %>%
           factor(),
         city = str_to_title(city) %>% factor())

# Add 2020's individual data for the monthly time series plot
time_series_2020 <- esismal %>% 
  filter(year_dx == 2020) %>% 
  group_by(year_dx, month_dx, province, city) %>% 
  summarise(case = n()) %>% 
  ungroup()

time_series_2020 <- time_series_2020 %>% 
  mutate(day = rep(1, dim(time_series_2020)[1]),
         date = str_c(year_dx, '-', month_dx, '-', day),
         date = ymd(date)) %>% 
  rename(year = year_dx,
         month = month_dx) %>% 
  select(year, month, date, province, city, case)

time_series_month <- bind_rows(time_series_month, time_series_2020)

# Add 2018's aggregate data
time_series_2018 <- raw_timeseries_2018 %>% 
  filter(Provinsi == 'Papua' | Provinsi == 'Papua Barat')

time_series_2018 <- time_series_2018 %>% 
  mutate(year = rep(2018, nrow(time_series_2018))) %>% 
  select(year, month, Provinsi, `Kabupaten/Kota`, Positif) %>% 
  rename(province = Provinsi,
         city = `Kabupaten/Kota`,
         case = Positif) %>% 
  mutate(province = if_else(province == 'Papua Barat','West Papua', province),
         province = factor(province),
         city = str_to_title(city) %>% factor(),
         day = rep(1, nrow(time_series_2018)),
         date = str_c(year, '-', month, '-', day),
         date = ymd(date)) %>% 
  select(-day) %>% 
  relocate(date, .after = 2) %>% 
  mutate(case = if_else(is.na(case), 0, case))

time_series_month <- bind_rows(time_series_month, time_series_2018) %>% 
  arrange(year)

# Replace 2019's aggregate with individual data
time_series_2019 <- esismal %>%
  filter(year_dx == 2019) %>%
  group_by(year_dx, month_dx, province, city) %>%
  summarise(case = n()) %>%
  ungroup()

time_series_2019 <- time_series_2019 %>%
  mutate(day = rep(1, dim(time_series_2019)[1]),
         date = str_c(year_dx, '-', month_dx, '-', day),
         date = ymd(date)) %>%
  rename(year = year_dx,
         month = month_dx) %>%
  select(year, month, date, province, city, case)

## Remove aggregate
time_series_month <- filter(time_series_month, year != 2019) 

## Replace with individual data
time_series_month <- bind_rows(time_series_month, time_series_2019) %>% 
  arrange(year)

# Save --------------------------------------------------------------------

## 2019-2020 individual data
esismal %>% write_rds(file = here("0-data", "esismal.rds"))

## For time-series per month
time_series_month %>% write_rds(file = here("0-data", "time-series-month.rds"))

## Mixed incorporated into mono (aggregate data derived from individual)
### P. falciparum


### P. vivax


## Province and year-specific population sizes
time_series_year %>%
  select(year, province, pop) %>%
  write_rds(file = here("0-data", "population-size.rds"))

# Appendix ----------------------------------------------------------------

sessioninfo::platform_info()




















