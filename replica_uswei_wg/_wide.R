## 3) Seasonal adjustment ------------------------------------------------------
# EEI
source(paste0(here::here(), "/eei.R"))        #-> Electricity Output            --> WSA
source(paste0(here::here(), "/withhold.R"))   #-> Federal withholding tax       --> in PCY, WSA
source(paste0(here::here(), "/staffing.R"))   #-> US Staffing Index             --> Index, WSA
source(paste0(here::here(), "/uslama3294.R")) #-> Initial claims                --> WSA
source(paste0(here::here(), "/uslama3349.R")) #-> Continued Claims              --> WSA
source(paste0(here::here(), "/ustrad4628.R")) #-> Redbook Retail Sales          --> in PCY, WSA
source(paste0(here::here(), "/usprod0685.R")) #-> Metal production              --> WSA
source(paste0(here::here(), "/ustran0035.R")) #-> Railroad traffic              --> WSA
source(paste0(here::here(), "/usprod0983.R")) #-> Fuel sales to End Consumer    --> WSA
source(paste0(here::here(), "/consumer.R"))   #-> Rasmussen Consumer Index      --> Index, no season

dta.w_sa <- ts_c(eei_w_wsa,
                 withhold_w_wsa,
                 staffing_w_wsa,
                 uslama3294_w_wsa,
                 uslama3349_w_wsa,
                 usprod0685_w_wsa,
                 redbook_w_wsa,
                 railroad_w_wsa,
                 fuel_sales_w_wsa,
                 consumer_w_wsa)

dta.w_sa %>% ts_summary()

descriptive_stats <- ts_c(eei_w_wsa,
     staffing_w_wsa,
     uslama3294_w_wsa,
     uslama3349_w_wsa,
     usprod0685_w_wsa,
     railroad_w_wsa,
     fuel_sales_w_wsa,
     consumer_w_wsa) %>% 
  group_by(id) %>% 
  mutate(value = log10(value)) %>% 
  mutate(diffy = if_else(year(time) %in% c(1993, 1999, 2005, 2010, 2016, 2021),
                         value - lag(value, 51),
                         value - lag(value, 52)
                         )
  ) %>% 
                         # (value - lag(value, 51))/lag(value, 51),
                         # (value - lag(value, 52))/lag(value, 52)
  ungroup() %>% 
  select(id, time, value = diffy) %>% 
  ts_c(redbook_w_wsa, 
       withhold_w_wsa) %>%
  ts_span(start = "2010-01-01", 
          end = "2019-12-31") %>% 
  group_by(id) %>% 
  summarize(mean = mean(value, na.rm = TRUE), 
            sd = sd(value, na.rm = TRUE), 
            min = min(value, na.rm = TRUE), 
            max = max(value, na.rm = TRUE))

descriptive_stats

# no smoothing by means of MA indicated

# calculating growth rates 
dta.diffy <- ts_c(eei_w_wsa,
                  staffing_w_wsa,
                  uslama3294_w_wsa,
                  uslama3349_w_wsa,
                  usprod0685_w_wsa,
                  railroad_w_wsa,
                  fuel_sales_w_wsa,
                  consumer_w_wsa) %>%
  group_by(id) %>%
  mutate(value = log10(value)) %>% 
  mutate(diffy = if_else(year(time) %in% c(1993, 1999, 2005, 2010, 2016, 2021),
                         value - lag(value, 51),
                         value - lag(value, 52)
  )
  ) %>% 
  ungroup() %>% 
  select(id, time, value = diffy)  %>%
  ts_c(redbook_w_wsa, 
       withhold_w_wsa) %>% 
  ts_na_omit()

dta.diffy %>%
  ggplot() +
  geom_line(aes(x = time, y = value)) +
  facet_wrap(vars(id), scales = "free")

source(paste0(here::here(), "/_remove_outliers.R"), encoding = "UTF-8")

dta.final <- ts_c(consumer_outlier_adj,
                  redbook_outlier_adj,
                  railroad_diffy_outlier_adj,
                  usprod0685_diffy_outlier_adj,
                  uslama3349_diffy_outlier_adj,
                  uslama3294_diffy_outlier_adj,
                  staffing_diffy_outlier_adj,
                  withhold_diffy_outlier_adj,
                  fuel_sales_diffy_outlier_adj,
                  eei_diffy_outlier_adj) 

# Estimate principal component 

dta_moments_est <- dta.final %>% 
  ts_span(start = "2008-01-01", 
          end = "2020-02-29") %>% 
  group_by(id) %>% 
  summarize(mean = mean(value, na.rm = TRUE), 
            sd = sd(value, na.rm = TRUE))

dta_norm_est <- dta.final %>% 
  ts_span(start = "2008-01-01", 
          end = "2020-02-29") %>% 
  left_join(dta_moments_est, by = "id") %>% 
  group_by(id) %>% 
  mutate(norm_value = (value-mean)/sd) %>% 
  select(id, time, value = norm_value) %>% 
  ungroup() %>% 
  ts_xts()

pc <- dta_norm_est %>% 
  prcomp(center = FALSE, scale = FALSE)

sum_pc_wide <- summary(pc)

factor_loading <- pc$rotation[,"PC1"] %>% 
  as.data.frame() %>% 
  as_tibble(rownames = "id") %>% 
  rename(loading = ".") %>% 
  mutate(weight = loading^(-1))

fl_wide <- factor_loading %>% 
  select(id, loading)

dta_norm_full <- dta.final %>% 
  ts_span(start = "2008-01-01", 
          end = end.horizon) %>% 
  left_join(dta_moments_est, by = "id") %>% 
  group_by(id) %>% 
  mutate(norm_value = (value-mean)/sd) %>% 
  select(id, time, value = norm_value) %>% 
  ungroup() %>% 
  ts_xts()

wei_factor <- dta_norm_full %>% 
  ts_tbl() %>% 
  left_join(factor_loading, by = "id") %>% 
  mutate(new_val = value*loading) %>% 
  group_by(time) %>% 
  summarize(factor = sum(new_val, na.rm = TRUE))

wei_base <- wei_factor %>%
  mutate(id = "WEI base") %>% 
  select(id, time, value = factor)

source(paste0(here::here(), "/_scaling_to_us_gdp_yoy.R"), encoding = "UTF-8")

wei_scaled_pcy_wide <- wei_scaled_pcy %>% 
  mutate(id = "us wei wsa")
