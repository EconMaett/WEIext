# STEP 1
# Adjust for the 53rd calendar week

tmp <- withhold_w_raw %>% 
  filter(ISOyear %in% years_w53) %>% 
  group_by(ISOyear) %>%
  mutate(value_w53 = last(value)) %>% 
  filter(ISOweek != 53) %>% 
  mutate(value = value + (1/52)*value_w53) %>% 
  select(-value_w53) %>% 
  ungroup()  

withhold_w <- withhold_w_raw %>% 
  filter(!(ISOyear %in% years_w53)) %>% 
  full_join(tmp) %>% 
  arrange(time)

withhold_w_hist <- withhold_w 

wd <- withhold_w_hist %>%
  left_join(work_days, by = c("ISOyear", "ISOweek")) %>%
  select(time, count) %>%
  ts_long() %>%
  ts_xts()

restrict_forecast_withhold_w_hist <- seq.Date(from = withhold_w_hist %>%
                                                  filter(time == max(time)) %>%
                                                  pull(time)+7,
                                                length.out = 105,
                                                by = "weeks") %>%
  as_tibble() %>%
  mutate(KW = date2ISOweek(value)) %>%
  separate(KW, c("ISOyear", "ISOweek", "day"), sep = "-") %>%
  mutate(ISOweek = as.numeric(str_remove(ISOweek, patter = "^W")),
         ISOyear = as.numeric(ISOyear)) %>%
  rename(time = "value") %>%
  filter(ISOweek != 53)

wd_fc_withhold_w <- work_days %>%
  left_join(restrict_forecast_withhold_w_hist, by = c("ISOyear", "ISOweek")) %>%
  filter(!is.na(time)) %>%
  select(time, count) %>%
  ts_long() %>%
  ts_xts()


withhold_w.mod <- withhold_w_hist  %>%
  select(id, time, value) %>%
  ts_xts() %>%
  s31::wsa(Log = FALSE,
           outliers = FALSE,
           order = c(0,1,0,1,0,1),
           regressor = wd[,1],
           forecast_regressor = wd_fc_withhold_w[,1]
  )


withhold_w_sfac <- withhold_w.mod$seas_comp %>%
  ts_tbl()

withhold_w_wsa <- withhold_w %>%
  select(id, time, value) %>%
  ts_c(withhold_w_sfac) %>%
  ts_wide() %>%
  mutate(value = (`withhold`)-(cal_fac + s_fac)) %>%
  mutate(id = "withhold") %>%
  select(id, time, value) %>% 
  ts_na_omit() %>% 
  ts_span(end = fc_time)
