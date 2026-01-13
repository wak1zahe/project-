library(dplyr)
library(tidyr)
library(purrr)
library(readxl)
library(stringr)
library(broom)

path <- "/Users/wak1zahe/Downloads/Проект_Long-2.xlsx"

df <- read_excel(path, sheet = 1) %>%
  mutate(
    districts = as.factor(districts),
    year = as.integer(year),
    onp_status = ifelse(is.na(`ONP Status`) | trimws(`ONP Status`) == "", NA_character_, trimws(`ONP Status`))
  ) %>%
  filter(!is.na(onp_status)) %>%
  mutate(onp_status = as.factor(onp_status))

cat("\n===== ONP Status (категории) =====\n")
print(df %>% count(onp_status, sort = TRUE))

num_vars <- df %>%
  select(where(is.numeric)) %>%
  select(-year) %>%
  names()

y_patterns <- c(
  "satisfaction", "school", "schools", "preschool", "preschools",
  "education", "student", "students",
  "culture", "libraries",
  "roads", "transport",
  "water", "gas", "heating", "electricity",
  "sports",
  "housing_area", "housing_improvement",
  "no_transport_access",
  "energy_consumption"
)
Y_vars <- num_vars[str_detect(num_vars, str_c(y_patterns, collapse = "|"))]

x_patterns <- c(
  "tax", "revenue", "admin_expenses",
  "investment",
  "SMEs",
  "municipal_assets",
  "utilities_private_participation",
  "land_plots",
  "population_total"
)
X_vars <- num_vars[str_detect(num_vars, str_c(x_patterns, collapse = "|"))]

if (length(Y_vars) == 0) {
  Y_vars <- num_vars[str_detect(num_vars, "satisfaction|education|culture|housing|roads|transport|school|preschool|sports")]
}
if (length(X_vars) == 0) {
  X_vars <- setdiff(num_vars, Y_vars)
}

X_vars <- setdiff(X_vars, Y_vars)
X_vars <- setdiff(X_vars, "wage_arrears_share")
Y_vars <- setdiff(Y_vars, "wage_arrears_share")

cat("\n===== 2.2: переменные =====\n")
cat("Y:", length(Y_vars), "\n"); print(Y_vars)
cat("X:", length(X_vars), "\n"); print(X_vars)

grid <- tidyr::crossing(y = Y_vars, x = X_vars)

run_status_model <- function(data, y, x, min_n = 30) {
  
  dat <- data %>%
    select(year, onp_status, all_of(c(y, x))) %>%
    filter(!is.na(.data[[y]]), !is.na(.data[[x]])) %>%
    filter(is.finite(.data[[y]]), is.finite(.data[[x]]))
  
  if (nrow(dat) < min_n) return(NULL)
  if (n_distinct(dat$onp_status) < 2) return(NULL)
  if (sd(dat[[x]], na.rm = TRUE) == 0) return(NULL)
  
  f <- as.formula(paste0(y, " ~ ", x, " * onp_status + factor(year)"))
  m <- try(lm(f, data = dat), silent = TRUE)
  if (inherits(m, "try-error")) return(NULL)
  
  broom::tidy(m) %>%
    mutate(
      y = y,
      x = x,
      n = nrow(dat),
      adj_r2 = summary(m)$adj.r.squared
    )
}

models_all <- pmap(grid, ~ run_status_model(df, ..1, ..2, min_n = 30)) %>%
  compact() %>%
  bind_rows()

if (nrow(models_all) == 0) stop("Ни одна модель не собралась (проверь данные).")


interactions <- models_all %>%
  filter(str_detect(term, "^.+:onp_status")) %>%     # только взаимодействия
  mutate(
    p_adj_fdr_all = p.adjust(p.value, method = "fdr"),
    abs_est = abs(estimate)
  ) %>%
  arrange(p_adj_fdr_all, desc(abs_est))

tab_A <- interactions %>%
  filter(!is.na(p_adj_fdr_all), p_adj_fdr_all < 0.05) %>%
  select(y, x, term, estimate, std.error, statistic, p.value, p_adj_fdr_all, adj_r2, n) %>%
  slice_head(n = 20)

cat("\n===== ТАБЛИЦА A: ТОП-20 значимых взаимодействий (FDR<0.05) =====\n")
print(tab_A, n = Inf, width = Inf)


sig_flag <- interactions %>%
  mutate(sig = !is.na(p_adj_fdr_all) & p_adj_fdr_all < 0.05)

tab_Bx <- sig_flag %>%
  group_by(x) %>%
  summarise(
    sig_cnt = sum(sig),
    all_cnt = n(),
    sig_share = sig_cnt / all_cnt,
    .groups = "drop"
  ) %>%
  arrange(desc(sig_cnt), desc(sig_share))

tab_By <- sig_flag %>%
  group_by(y) %>%
  summarise(
    sig_cnt = sum(sig),
    all_cnt = n(),
    sig_share = sig_cnt / all_cnt,
    .groups = "drop"
  ) %>%
  arrange(desc(sig_cnt), desc(sig_share))

cat("\n===== ТАБЛИЦА B1: Значимые взаимодействия по X =====\n")
print(tab_Bx, n = Inf, width = Inf)

cat("\n===== ТАБЛИЦА B2: Значимые взаимодействия по Y =====\n")
print(tab_By, n = Inf, width = Inf)


focus_x <- "municipal_assets_bankruptcy_share"

tab_C <- interactions %>%
  filter(x == focus_x) %>%
  filter(!is.na(p_adj_fdr_all), p_adj_fdr_all < 0.05) %>%
  arrange(p_adj_fdr_all, desc(abs_est)) %>%
  select(y, term, estimate, p.value, p_adj_fdr_all, adj_r2, n) %>%
  slice_head(n = 20)

cat("\n===== ТАБЛИЦА C: ТОП-20 взаимодействий для X =", focus_x, "(FDR<0.05) =====\n")
print(tab_C, n = Inf, width = Inf)
