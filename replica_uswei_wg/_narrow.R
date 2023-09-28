# NARROW REPLICATION
## 4) Make growth rates --------------------------------------------------------

# Already in PCY
redbook.raw <- dta.raw %>% 
  filter(id == "ustrad4628") %>% 
  select(id, time, value)

withhold.raw <- dta.raw %>% 
  filter(id == "withhold") %>% 
  select(id, time, value)

dta.diffy <- dta.raw %>% 
  filter(!id %in% c("ustrad4628", 
                    "withhold")) %>% 
  group_by(id) %>%
  mutate(value = log10(value)) %>%
  mutate(diffy = value - lag(value, 52)) %>%
  # mutate(diffy = (value - lag(value, 52))/lag(value, 52)) %>%  
  ungroup() %>%
  select(id, time, value = diffy)  %>%
  ts_c(redbook.raw, 
       withhold.raw) %>% 
  ts_na_omit() 

## Basis = 9 indicators
# weekly_factor.mod <- dta.diffy %>% 
#   ts_span(start = "2008-01-01", 
#           end = "2020-02-28") %>% 
#   ts_scale() %>% 
#   dfm(method = c("pc"), # bayes und PC erhöhen volatilität
#       factors = 1, # -> getestet, mehr Faktoren zerstören die Ladungen
#       lags = 1,    # mehr lags führen zu mehr Volatilität und Ausschlägen              
#       scale = FALSE, 
#       diffs = FALSE,
#       pre_differenced = distinct(dta.diffy, id) %>% as.character(), 
#       outlier_threshold = 15, # je tiefer desto kleiner aktuelle Krise 
#       identification = "pc_long") 

# fact_prcomp <- dta.diffy %>% 
#   ts_span(start = "2008-01-01", 
#           end = "2020-02-28") %>% 
#   ts_scale() %>% 
#   ts_ts() %>% 
#   ts_prcomp()


# Estimate principal component 

dta_moments_est <- dta.diffy %>% 
  ts_span(start = "2008-01-01", 
          end = "2020-02-29") %>% 
  group_by(id) %>% 
  summarize(mean = mean(value, na.rm = TRUE), 
            sd = sd(value, na.rm = TRUE))

dta_norm_est <- dta.diffy %>% 
  ts_span(start = "2008-01-01", 
          end = "2020-02-29") %>% 
  left_join(dta_moments_est, by = "id") %>% 
  group_by(id) %>% 
  mutate(norm_value = (value-mean)/sd) %>% 
  select(id, time, value = norm_value) %>% 
  ungroup() %>% 
  ts_xts()

dta_norm_full <- dta.diffy %>% 
  ts_span(start = "2008-01-01", 
          end = end.horizon) %>% 
  left_join(dta_moments_est, by = "id") %>% 
  group_by(id) %>% 
  mutate(norm_value = (value-mean)/sd) %>% 
  select(id, time, value = norm_value) %>% 
  ungroup() %>% 
  ts_xts()
  
pc <- dta_norm_est %>% 
  prcomp(center = FALSE, scale = FALSE)

sum_pc <- summary(pc)

factor_loading <- pc$rotation[,"PC1"] %>% 
  as.data.frame() %>% 
  as_tibble(rownames = "id") %>% 
  rename(loading = ".") %>% 
  mutate(weight = loading^(-1))

fl_narrow <- factor_loading %>% 
  select(id, loading)

wei_factor <- dta_norm_full %>% 
  ts_tbl() %>% 
  left_join(factor_loading, by = "id") %>% 
  mutate(new_val = value*loading) %>% 
  group_by(time) %>% 
  summarize(factor = sum(new_val, na.rm = TRUE))

# # Factor Loadings
# factor_basis <- weekly_factor.mod$H
# factor_loading <- as_tibble(factor_basis, rownames = "id") %>% rename(value = V1)
# 

# wei_base <- weekly_factor.mod$factors %>%
#   mutate(id = "WEI base") %>%
#   mutate(time = time + 1) %>%
#   mutate(time = as.Date(time))
 
wei_base <- wei_factor %>%
  mutate(id = "WEI base") %>% 
  select(id, time, value = factor)

source(paste0(here::here(), "/_scaling_to_us_gdp_yoy.R"), encoding = "UTF-8")

wei_scaled_pcy_narrow <- wei_scaled_pcy
