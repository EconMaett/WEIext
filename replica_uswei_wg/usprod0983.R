# STEP 1
# Adjust for the 53rd calendar week

fuel_sales_w_raw <- dta.raw %>% 
  filter(id == "fuel_sales") %>% 
  mutate(KW = date2ISOweek(time)) %>%
  separate(KW, c("ISOyear", "ISOweek", "day"), sep = "-") %>%
  mutate(ISOweek = as.numeric(str_remove(ISOweek, patter = "^W")),
         ISOyear=as.numeric(ISOyear))  %>%
  select(-day) 

tmp <- fuel_sales_w_raw %>% 
  filter(ISOyear %in% years_w53) %>% 
  group_by(ISOyear) %>%
  mutate(value_w53 = last(value)) %>% 
  filter(ISOweek != 53) %>% 
  mutate(value = value + (1/52)*value_w53) %>% 
  select(-value_w53) %>% 
  ungroup()  

fuel_sales_w <- fuel_sales_w_raw %>% 
  filter(!(ISOyear %in% years_w53)) %>% 
  full_join(tmp) %>% 
  arrange(time)

# STEPS 2 and 3
# Seasonal and calendar adjustment

fuel_sales_w_hist <- fuel_sales_w %>%
  ts_span(end = "2019-12-31")

wd <- fuel_sales_w_hist %>% 
  left_join(work_days, by = c("ISOyear", "ISOweek")) %>% 
  select(time, count) %>% 
  ts_long() %>% 
  ts_xts()

restrict_forecast_fuel_sales_w_hist <- seq.Date(from = fuel_sales_w_hist %>% 
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

wd_fc_fuel_sales_w <- work_days %>% 
  left_join(restrict_forecast_fuel_sales_w_hist, by = c("ISOyear", "ISOweek")) %>% 
  filter(!is.na(time)) %>% 
  select(time, count) %>% 
  ts_long() %>%
  ts_xts()

# complete seasonal adjustment

fuel_sales_w.mod <- fuel_sales_w_hist  %>%
  select(id, time, value) %>%
  ts_xts() %>% 
  s31::wsa(Log = FALSE,
           outliers = FALSE,
           order = c(0,1,3,1,0,0),
           regressor = wd[,1],
           forecast_regressor = cbind(wd_fc_fuel_sales_w[,1])
  )

fuel_sales_w_sfac <- fuel_sales_w.mod$seas_comp %>%
  ts_tbl()

fuel_sales_w_wsa <- fuel_sales_w %>%
  select(id, time, value) %>%
  ts_c(fuel_sales_w_sfac) %>%
  ts_wide() %>%
  mutate(value = (`fuel_sales`)-(cal_fac + s_fac)) %>%
  mutate(id = "fuel_sales") %>%
  select(id, time, value) %>% 
  ts_na_omit() %>% 
  ts_span(end = fc_time)

fuel_sales_w_raw_diffy <- fuel_sales_w_raw %>%
  select(id, time, value) %>%
  mutate(diffy = (log(value)-lag(log(value), 52))) %>%
  select(id, time, diffy) %>%
  mutate(id = "raw")

fuel_sales_w_wsa_diffy <- fuel_sales_w_wsa %>%
  mutate(diffy = if_else(year(time) %in% c(1993, 1999, 2005, 2010, 2016, 2021),
                         (value - lag(value, 51))/lag(value, 51),
                         (value - lag(value, 52))/lag(value, 52)
  )) %>% 
  select(id, time, diffy) %>%
  mutate(id = "csa")
