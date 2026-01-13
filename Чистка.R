library(dplyr)
library(readxl)
library(stringr)
path <- "/Users/wak1zahe/Downloads/Проект_Long-2.xlsx"
df_raw <- read_excel(path, sheet = 1)
df_clean <- df_raw %>%
  mutate(
    districts = as.factor(districts),   
    year = as.integer(year)             
  )

df_clean <- df_clean %>%
  mutate(
    onp_flag = ifelse(
      is.na(`ONP Status`) | trimws(`ONP Status`) == "",
      "non_ONP",
      "ONP"
    ),
    onp_flag = as.factor(onp_flag),
    
    onp_status = ifelse(
      is.na(`ONP Status`) | trimws(`ONP Status`) == "",
      NA_character_,
      trimws(`ONP Status`)
    ),
    onp_status = as.factor(onp_status)
  )

df_model <- df_clean %>%
  filter(!is.na(onp_status))

num_vars <- df_model %>%
  select(where(is.numeric)) %>%
  select(-year) %>%        
  names()
