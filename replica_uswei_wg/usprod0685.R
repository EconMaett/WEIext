# STEP 1
# Adjust for the 53rd calendar week

usprod0685_w_raw <- dta.raw %>% 
  filter(id == "usprod0685") %>% 
  mutate(KW = date2ISOweek(time)) %>%
  separate(KW, c("ISOyear", "ISOweek", "day"), sep = "-") %>%
  mutate(ISOweek = as.numeric(str_remove(ISOweek, patter = "^W")),
         ISOyear=as.numeric(ISOyear))  %>%
  select(-day)  

tmp <- usprod0685_w_raw %>% 
  filter(ISOyear %in% years_w53) %>% 
  group_by(ISOyear) %>%
  mutate(value_w53 = last(value)) %>% 
  filter(ISOweek != 53) %>% 
  mutate(value = value + (1/52)*value_w53) %>% 
  select(-value_w53) %>% 
  ungroup()  

usprod0685_w <- usprod0685_w_raw %>% 
  filter(!(ISOyear %in% years_w53)) %>% 
  full_join(tmp) %>% 
  arrange(time)

# STEPS 2 and 3
# Seasonal and calendar adjustment

# complete seasonal adjustment
wd <- usprod0685_w %>% 
  left_join(work_days, by = c("ISOyear", "ISOweek")) %>% 
  select(time, count) %>% 
  ts_long() %>% 
  ts_xts()

usprod0685_w.mod <- usprod0685_w  %>%
  select(id, time, value) %>%
  ts_xts() %>%
  s31::wsa(Log = FALSE,
           outliers = FALSE,
           order = c(2,1,2,1,0,0),
           regressor = wd[,1],
           forecast_regressor = cbind(wd_fc[,1])
  )

usprod0685_w_wsa <- usprod0685_w.mod$output$seas_adj %>% 
  ts_tbl() %>% 
  mutate(id = "usprod0685") %>% 
  ts_span(end = fc_time)


usprod0685_w_raw_diffy <- usprod0685_w_raw %>%
  select(id, time, value) %>%
  mutate(diffy = (log(value)-lag(log(value), 52))) %>%
  select(id, time, diffy) %>%
  mutate(id = "raw")

usprod0685_w_wsa_diffy <- usprod0685_w_wsa %>%
  mutate(diffy = if_else(year(time) %in% c(1993, 1999, 2005, 2010, 2016, 2021),
                         (value - lag(value, 51))/lag(value, 51),
                         (value - lag(value, 52))/lag(value, 52)
  )) %>% 
  select(id, time, diffy) %>%
  mutate(id = "csa")
