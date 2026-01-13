library(dplyr)
library(tidyr)
library(purrr)
library(ineq)
library(readxl)
library(stringr)

path <- "/Users/wak1zahe/Downloads/Проект_Long-2.xlsx"

df <- read_excel(path, sheet = 1) %>%
  mutate(
    districts = as.factor(districts),
    year = as.integer(year)
  )

cv_fun <- function(x) {
  x <- x[is.finite(x)]
  x <- x[!is.na(x)]
  if (length(x) < 3) return(NA_real_)
  m <- mean(x)
  if (m == 0) return(NA_real_)
  sd(x) / m
}

gini_boot_ci <- function(x, B = 300, conf = 0.95, seed = 42) {
  set.seed(seed)
  x <- x[is.finite(x)]
  x <- x[!is.na(x)]
  if (length(x) < 5) return(tibble(gini = NA_real_, lo = NA_real_, hi = NA_real_))
  minx <- min(x)
  if (minx < 0) x <- x - minx
  
  g0 <- ineq(x, type = "Gini")
  boots <- replicate(B, {
    xs <- sample(x, replace = TRUE)
    ineq(xs, type = "Gini")
  })
  
  alpha <- (1 - conf) / 2
  tibble(
    gini = g0,
    lo = as.numeric(quantile(boots, probs = alpha, na.rm = TRUE)),
    hi = as.numeric(quantile(boots, probs = 1 - alpha, na.rm = TRUE))
  )
}

num_vars <- df %>%
  select(where(is.numeric)) %>%
  select(-year) %>%
  names()


df2 <- df %>%
  mutate(
    onp_flag = ifelse(is.na(`ONP Status`) | trimws(`ONP Status`) == "", "non_ONP", "ONP"),
    onp_flag = as.factor(onp_flag),
    onp_status = ifelse(is.na(`ONP Status`) | trimws(`ONP Status`) == "", NA, trimws(`ONP Status`)),
    onp_status = as.factor(onp_status)
  )

wilcox_all <- purrr::map_dfr(num_vars, function(v) {
  df2 %>%
    group_by(year) %>%
    group_modify(~{
      dat <- .x %>% select(onp_flag, all_of(v)) %>% filter(!is.na(.data[[v]]))
      
      if (n_distinct(dat$onp_flag) < 2) {
        return(tibble(variable = v, p_value = NA_real_, n = nrow(dat),
                      med_ONP = NA_real_, med_nonONP = NA_real_))
      }
      
      wt <- wilcox.test(dat[[v]] ~ dat$onp_flag, exact = FALSE)
      tibble(
        variable = v,
        p_value = wt$p.value,
        n = nrow(dat),
        med_ONP = median(dat[[v]][dat$onp_flag == "ONP"], na.rm = TRUE),
        med_nonONP = median(dat[[v]][dat$onp_flag == "non_ONP"], na.rm = TRUE)
      )
    }) %>%
    ungroup()
})

wilcox_all <- wilcox_all %>%
  group_by(year) %>%
  mutate(p_adj_fdr = p.adjust(p_value, method = "fdr")) %>%
  ungroup()

cat("\n===== ONP vs non-ONP (Wilcoxon) — значимые после FDR =====\n")
print(
  wilcox_all %>%
    filter(!is.na(p_adj_fdr)) %>%
    arrange(year, p_adj_fdr) %>%
    filter(p_adj_fdr < 0.05),
  n = Inf, width = Inf
)

kw_all <- purrr::map_dfr(num_vars, function(v) {
  df2 %>%
    filter(!is.na(onp_status)) %>%
    group_by(year) %>%
    group_modify(~{
      dat <- .x %>% select(onp_status, all_of(v)) %>% filter(!is.na(.data[[v]]))
      
      if (n_distinct(dat$onp_status) < 2) {
        return(tibble(variable = v, p_value = NA_real_, n = nrow(dat)))
      }
      
      kt <- kruskal.test(dat[[v]] ~ dat$onp_status)
      tibble(variable = v, p_value = kt$p.value, n = nrow(dat))
    }) %>%
    ungroup()
})

kw_all <- kw_all %>%
  group_by(year) %>%
  mutate(p_adj_fdr = p.adjust(p_value, method = "fdr")) %>%
  ungroup()

cat("\n===== ONP Status (Kruskal–Wallis) — значимые после FDR =====\n")
print(
  kw_all %>%
    filter(!is.na(p_adj_fdr)) %>%
    arrange(year, p_adj_fdr) %>%
    filter(p_adj_fdr < 0.05),
  n = Inf, width = Inf
)

all_ineq <- map_dfr(num_vars, function(v) {
  
  stats_by_year <- df %>%
    group_by(year) %>%
    summarise(
      variable = v,
      n = sum(!is.na(.data[[v]])),
      mean = mean(.data[[v]], na.rm = TRUE),
      median = median(.data[[v]], na.rm = TRUE),
      sd = sd(.data[[v]], na.rm = TRUE),
      min = min(.data[[v]], na.rm = TRUE),
      max = max(.data[[v]], na.rm = TRUE),
      cv = cv_fun(.data[[v]]),
      .groups = "drop"
    )
  
  gini_by_year <- df %>%
    group_by(year) %>%
    summarise(tmp = list(gini_boot_ci(.data[[v]])), .groups = "drop") %>%
    unnest(tmp)
  
  stats_by_year %>%
    left_join(gini_by_year, by = "year") %>%
    mutate(gini_significant = !is.na(lo) & lo > 0)
})

rank_2024 <- all_ineq %>%
  filter(year == 2024) %>%
  arrange(desc(gini)) %>%
  mutate(rank_gini_2024 = row_number())

rank_avg <- all_ineq %>%
  group_by(variable) %>%
  summarise(
    gini_avg = mean(gini, na.rm = TRUE),
    cv_avg = mean(cv, na.rm = TRUE),
    gini_signif_share = mean(gini_significant, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(gini_avg)) %>%
  mutate(rank_gini_avg = row_number())

cat("\n===== ВСЕ ПЕРЕМЕННЫЕ × ГОДЫ =====\n")
print(all_ineq %>% arrange(variable, year), n = Inf, width = Inf)

cat("\n===== РЕЙТИНГ ПО GINI (2024) =====\n")
print(
  rank_2024 %>%
    select(rank_gini_2024, variable, gini, cv, gini_significant),
  n = Inf,
  width = Inf
)

cat("\n===== РЕЙТИНГ ПО СРЕДНЕМУ GINI (2020–2024) =====\n")
print(
  rank_avg %>%
    select(rank_gini_avg, variable, gini_avg, cv_avg, gini_signif_share),
  n = Inf,
  width = Inf
)

cat("\n===== ТОП-10 НАИБОЛЕЕ ДИФФЕРЕНЦИРОВАННЫХ =====\n")
print(rank_avg %>% slice_head(n = 10), n = Inf, width = Inf)

cat("\n===== ТОП-10 НАИБОЛЕЕ РАВНОМЕРНЫХ =====\n")
print(rank_avg %>% slice_tail(n = 10), n = Inf, width = Inf)
