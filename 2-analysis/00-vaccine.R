
library(tidyverse)
library(haven)
library(here)

raw_esismal <- here('0-data', 'esismal 2019-2020_ieprocess210801.dta') |> 
  read_dta()

admin <- here('0-data', 'pop-andra', 'ADMIN2_LIST.csv') |> 
  read_csv() |> 
  filter((ADMIN1 == 'NUSA TENGGARA TIMUR' &
            ADMIN2 %in% c('SUMBA BARAT',
                          'SUMBA BARAT DAYA',
                          'SUMBA TENGAH',
                          'SUMBA TIMUR')) |
         ADMIN1 == 'PAPUA BARAT' |
         ADMIN1 == 'PAPUA') |> 
  mutate(ADMIN1 = str_to_title(ADMIN1),
         ADMIN2 = str_to_title(ADMIN2),
         REGION = str_to_title(REGION)) |> 
  rename(idprov = IDADMIN1,
         iddist = IDADMIN2,
         province = ADMIN1,
         district = ADMIN2,
         region = REGION)

pop_by_age <- here('0-data',
                   'pop-andra',
                   'pop_age_simplegrowth_2010_2020.rds') |> 
  read_rds() |> 
  bind_rows(.id = 'list') |> 
  rename(iddist = IDADMIN2,
         age_group = AGE.GROUP,
         year = YEAR,
         pop = POP) |> 
  filter(year == 2019 | year == 2020)

pop_df <- pop_by_age |> 
  group_by(iddist) |> 
  nest() |> 
  left_join(admin, by = c('iddist')) |> 
  filter(province %in% c('Papua', 'Papua Barat', 'Nusa Tenggara Timur')) |> 
  unnest(cols = data) |> 
  ungroup() |> 
  select(province, district, year, age_group, pop) |> 
  arrange(province, district, year, age_group) |> 
  pivot_wider(names_from = age_group,
             values_from = pop) |> 
  mutate(`[0, 5)` = `0-4`,
         `[5, 10)` = `5-9`,
         `[10, 15)` = `10-14`,
         `[15, 65)` = rowSums(across(`15-19`:`60-64`)),
         `[65, ∞)` = rowSums(across(`65-69`:`75+`))) |> 
  select(province, district, year, `[0, 5)`:`[65, ∞)`) |> 
  pivot_longer(cols = starts_with('['),
               names_to = 'age_cat',
               values_to = 'pop')

d <- raw_esismal |> 
  select(tahun, nama_propinsi, nama_kabupaten, parasit, umur_th) |> 
  filter(
   tahun == 2019 |
     tahun == 2020,
   
   nama_propinsi == 'PAPUA' |
     nama_propinsi == 'PAPUA BARAT' | 
     (nama_propinsi == 'NUSA TENGGARA TIMUR' &
        nama_kabupaten %in% c('SUMBA BARAT',
                              'SUMBA BARAT DAYA',
                              'SUMBA TENGAH',
                              'SUMBA TIMUR'))
  ) |> 
  mutate(
    age_cat =  case_when(
      umur_th < 5 ~ '[0, 5)',
      umur_th < 10 ~ '[5, 10)',
      umur_th < 15 ~ '[10, 15)',
      umur_th < 65 ~ '[15, 65)',
      is.na(umur_th) ~ NA_character_,
      TRUE ~ '[65, ∞)'
    ) |> ordered(levels = c('[0, 5)',
                            '[5, 10)',
                            '[10, 15)',
                            '[15, 65)',
                            '[65, ∞)'),
                 labels = c('[0, 5)',
                            '[5, 10)',
                            '[10, 15)',
                            '[15, 65)',
                            '[65, ∞)')),
    sp = case_when(
      parasit == 'Pf' ~ 'P. falciparum',
      parasit == 'Pv' ~ 'P. vivax',
      parasit == 'Mix' | parasit == 'Pmix' ~ 'Mixed',
      parasit == '' ~ NA_character_,
      is.na(parasit) ~ NA_character_,
      TRUE ~ 'Other'
    ) |> factor(levels = c('P. falciparum', 'P. vivax', 'Mixed', 'Other'),
                labels = c('P. falciparum', 'P. vivax', 'Mixed', 'Other')),
    nama_propinsi = str_to_title(nama_propinsi),
    nama_kabupaten = str_to_title(nama_kabupaten)
  ) |>
  rename(
    year = tahun,
    province = nama_propinsi,
    district = nama_kabupaten
  ) |>
  select(province, district, year, sp, age_cat) |> 
  arrange(province, district, year, sp, age_cat)

d_summary <- d |> 
  group_by(province, district, year, sp, age_cat) |> 
  summarise(cases = n()) |> 
  arrange(province, district, year, sp, age_cat) |> 
  group_by(province, district, year, age_cat) |> 
  nest() |> 
  full_join(pop_df, by = c('province', 'district', 'year', 'age_cat')) |> 
  unnest(cols = data) |> 
  pivot_wider(names_from = sp,
              values_from = cases) |> 
  mutate(`P. falciparum` = if_else(is.na(`P. falciparum`),
                                   0, as.numeric(`P. falciparum`)),
         `P. vivax` = if_else(is.na(`P. vivax`),
                              0, as.numeric(`P. vivax`)),
         Mixed = if_else(is.na(Mixed), 0, as.numeric(Mixed))) |> 
  mutate(`P. falciparum` = `P. falciparum` + Mixed,
         `P. vivax` = `P. vivax` + Mixed) |> 
  select(-c(Other, Mixed, `NA`)) |> 
  pivot_longer(cols = starts_with('P. '),
               names_to = 'sp',
               values_to = 'cases') |> 
  mutate(api = 1000 * (cases / pop)) |> 
  ungroup() |> 
  mutate(age_cat = ordered(age_cat, levels = c('[0, 5)',
                                      '[5, 10)',
                                      '[10, 15)',
                                      '[15, 65)',
                                      '[65, ∞)'),
                           labels = c('[0, 5)',
                                      '[5, 10)',
                                      '[10, 15)',
                                      '[15, 65)',
                                      '[65, ∞)'))) |> 
  arrange(province, district, year, age_cat)

  # pivot_wider(names_from = age_group,
  #             values_from = pop) |> 
  # mutate(`[0, 5)` = `0-4`,
  #        `[5, 10)` = `5-9`,
  #        `[10, 15)` = `10-14`,
  #        `[15, 65)` = rowSums(across(`15-19`:`60-64`)),
  #        `[65, ∞)` = rowSums(across(`65-69`:`75+`))) |> 
  # select(province, district, year, `[0, 5)`:`[65, ∞)`) |> 
  # pivot_longer(cols = starts_with('['),
  #              names_to = 'age_cat',
  #              values_to = 'pop')
  
  # mutate(
  #   pop = case_when(
  #     # NTT
  #     year == 2019 & district == 'Sumba Barat' & age_cat == '[0, 5)' ~ 17599,
  #     year == 2019 & district == 'Sumba Barat' & age_cat == '[5, 10)' ~ 15766,
  #     year == 2019 & district == 'Sumba Barat' & age_cat == '[10, 15)' ~
  # 15396,
  #     year == 2019 & district == 'Sumba Barat' & age_cat == '[15, 65)' ~
  #       129710 - (17599 + 15766 + 15396 + 2331 + 1588 + 1386),
  #     year == 2019 & district == 'Sumba Barat' & age_cat == '[65, ∞)' ~
#       2331 + 1588 + 1386,
#     
#     year == 2020 & district == 'Sumba Barat' & age_cat == '[0, 5)' ~ 17771,
#     year == 2020 & district == 'Sumba Barat' & age_cat == '[5, 10)' ~ 15835,
#     year == 2020 & district == 'Sumba Barat' & age_cat == '[10, 15)' ~ 15459,
#     year == 2020 & district == 'Sumba Barat' & age_cat == '[15, 65)' ~
#       131600 - (17771 + 15835 + 15459 + 2429 + 1632 + 1414),
#     year == 2020 & district == 'Sumba Barat' & age_cat == '[65, ∞)' ~
#       2429 + 1632 + 1414,
#     
#     year == 2019 & district == 'Sumba Timur' & age_cat == '[0, 5)' ~ 32873,
#     year == 2019 & district == 'Sumba Timur' & age_cat == '[5, 10)' ~ 27381,
#     year == 2019 & district == 'Sumba Timur' & age_cat == '[10, 15)' ~ 27202,
#     year == 2019 & district == 'Sumba Timur' & age_cat == '[15, 65)' ~
#       258486 - (32873 + 27381 + 27202 + 4931 + 3609 + 3850),
#     year == 2019 & district == 'Sumba Timur' & age_cat == '[65, ∞)' ~
#       4931 + 3609 + 3850,
#     
#     year == 2020 & district == 'Sumba Timur' & age_cat == '[0, 5)' ~ 33079,
#     year == 2020 & district == 'Sumba Timur' & age_cat == '[5, 10)' ~ 27403,
#     year == 2020 & district == 'Sumba Timur' & age_cat == '[10, 15)' ~ 27215,
#     year == 2020 & district == 'Sumba Timur' & age_cat == '[15, 65)' ~
#       261503  - (33079 + 27403 + 27215 + 5122 + 3699 + 3910),
#     year == 2020 & district == 'Sumba Timur' & age_cat == '[65, ∞)' ~
#       5122 + 3699 + 3910,
#     
#     year == 2019 & district == 'Sumba Tengah' & age_cat == '[0, 5)' ~ 10404,
#     year == 2019 & district == 'Sumba Tengah' & age_cat == '[5, 10)' ~ 8861,
#     year == 2019 & district == 'Sumba Tengah' & age_cat == '[10, 15)' ~ 8404,
#     year == 2019 & district == 'Sumba Tengah' & age_cat == '[15, 65)' ~
#       72800 - (10404 + 8861 + 8404 + 1551 + 1081 + 955),
#     year == 2019 & district == 'Sumba Tengah' & age_cat == '[65, ∞)' ~
#       1551 + 1081 + 955,
#     
#     year == 2020 & district == 'Sumba Tengah' & age_cat == '[0, 5)' ~ 10500,
#     year == 2020 & district == 'Sumba Tengah' & age_cat == '[5, 10)' ~ 8894,
#     year == 2020 & district == 'Sumba Tengah' & age_cat == '[10, 15)' ~ 8432,
#     year == 2020 & district == 'Sumba Tengah' & age_cat == '[15, 65)' ~
#       73820  - (10500 + 8894 + 8432 + 1616 + 1111 + 975),
#     year == 2020 & district == 'Sumba Tengah' & age_cat == '[65, ∞)' ~
#       1616 + 1111 + 975,
#     
#     year == 2019 & district == 'Sumba Barat Daya' & age_cat == '[0, 5)' ~
#       52685,
#     year == 2019 & district == 'Sumba Barat Daya' & age_cat == '[5, 10)' ~
#       46445,
#     year == 2019 & district == 'Sumba Barat Daya' & age_cat == '[10, 15)' ~
#       44021,
#     year == 2019 & district == 'Sumba Barat Daya' & age_cat == '[15, 65)' ~
#       344720 - (52685 + 46445 + 44021 + 4790 + 3702 + 3359),
#     year == 2019 & district == 'Sumba Barat Daya' & age_cat == '[65, ∞)' ~
#       4790 + 3702 + 3359,
#     
#     year == 2020 & district == 'Sumba Barat Daya' & age_cat == '[0, 5)' ~
#       53418,
#     year == 2020 & district == 'Sumba Barat Daya' & age_cat == '[5, 10)' ~
#       46836,
#     year == 2020 & district == 'Sumba Barat Daya' & age_cat == '[10, 15)' ~
#       44378,
#     year == 2020 & district == 'Sumba Barat Daya' & age_cat == '[15, 65)' ~
#       350923  - (53418 + 46836 + 44378 + 5014 + 3823 + 3437),
#     year == 2020 & district == 'Sumba Barat Daya' & age_cat == '[65, ∞)' ~
#       5014 + 3823 + 3437,
#     
#     # Papua
#     
#     
#     # West Papua
#     
#     
#     TRUE ~ NA_real_
#   )
# )
