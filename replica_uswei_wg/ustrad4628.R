
# Already in PCY
redbook_w_raw <- dta.raw %>% 
  filter(id == "ustrad4628") %>% 
  mutate(KW = date2ISOweek(time)) %>%
  separate(KW, c("ISOyear", "ISOweek", "day"), sep = "-") %>%
  mutate(ISOweek = as.numeric(str_remove(ISOweek, patter = "^W")),
         ISOyear=as.numeric(ISOyear))  %>%
  select(-day) %>% 
  mutate(value = value/100)


tmp <- redbook_w_raw %>% 
  filter(ISOyear %in% years_w53) %>% 
  group_by(ISOyear) %>%
  mutate(value_w53 = last(value)) %>% 
  filter(ISOweek != 53) %>% 
  # mutate(value = value + (1/52)*value_w53) %>%  # not distribute, just drop
  select(-value_w53) %>% 
  ungroup()  

redbook_w <- redbook_w_raw %>% 
  filter(!(ISOyear %in% years_w53)) %>% 
  full_join(tmp) %>% 
  arrange(time)


# STEPS 2 and 3
# Seasonal and calendar adjustment

redbook_w_hist <- redbook_w %>%
  ts_span(end = "2019-12-31")

wd <- redbook_w_hist %>% 
  left_join(work_days, by = c("ISOyear", "ISOweek")) %>% 
  select(time, count) %>% 
  ts_long() %>% 
  ts_xts()

restrict_forecast_redbook_w_hist <- seq.Date(from = redbook_w_hist %>% 
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

wd_fc_redbook_w <- work_days %>% 
  left_join(restrict_forecast_redbook_w_hist, by = c("ISOyear", "ISOweek")) %>% 
  filter(!is.na(time)) %>% 
  select(time, count) %>% 
  ts_long() %>%
  ts_xts()

redbook_w.mod <- redbook_w_hist  %>%
  select(id, time, value) %>%
  ts_xts() %>% 
  s31::wsa(Log = FALSE,
           outliers = FALSE,
           order = c(1,1,1,0,0,1),
           regressor = wd[,1],
           forecast_regressor = wd_fc_redbook_w[,1]
  )

redbook_w_sfac <- redbook_w.mod$seas_comp %>%
  ts_tbl()

redbook_w_wsa <- redbook_w %>%
  select(id, time, value) %>%
  ts_c(redbook_w_sfac) %>%
  ts_wide() %>%
  mutate(value = (`ustrad4628`)-(cal_fac + s_fac)) %>%
  mutate(id = "ustrad4628") %>%
  select(id, time, value) %>% 
  ts_na_omit() %>% 
  ts_span(end = fc_time)
