# STEP 1
# Adjust for the 53rd calendar week

tmp <- eei_w_raw %>% 
  filter(ISOyear %in% years_w53) %>% 
  group_by(ISOyear) %>%
  mutate(value_w53 = last(value)) %>% 
  filter(ISOweek != 53) %>% 
  mutate(value = value + (1/52)*value_w53) %>% 
  select(-value_w53) %>% 
  ungroup()  

eei_w <- eei_w_raw %>% 
  filter(!(ISOyear %in% years_w53)) %>% 
  full_join(tmp) %>% 
  arrange(time) %>% 
  ts_span(start = "2000")

# STEPS 2 and 3
# Seasonal and calendar adjustment

eei_w_hist <- eei_w %>%
  ts_span(end = "2019-12-31")

wd <- eei_w_hist %>% 
  left_join(work_days, by = c("ISOyear", "ISOweek")) %>% 
  select(time, count) %>% 
  ts_long() %>% 
  ts_xts()

restrict_forecast_eei_w_hist <- seq.Date(from = eei_w_hist %>% 
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

wd_fc_eei_w <- work_days %>% 
  left_join(restrict_forecast_eei_w_hist, by = c("ISOyear", "ISOweek")) %>% 
  filter(!is.na(time)) %>% 
  select(time, count) %>% 
  ts_long() %>%
  ts_xts()

eei_w.mod <- eei_w_hist  %>%
  select(id, time, value) %>%
  ts_xts() %>%
  s31::wsa(Log = TRUE,
           outliers = FALSE,
           order = c(1,1,1,1,1,0),
           regressor = wd[,1],
           forecast_regressor = wd_fc_eei_w[,1]
  )


eei_w_sfac <- eei_w.mod$seas_comp %>%
  ts_tbl()

eei_w_wsa <- eei_w %>%
  select(id, time, value) %>%
  ts_c(eei_w_sfac) %>%
  ts_wide() %>%
  mutate(value = (`eei`)/(cal_fac*s_fac)) %>%
  mutate(id = "eei") %>%
  select(id, time, value) %>% 
  ts_na_omit() %>% 
  ts_span(end = fc_time)
