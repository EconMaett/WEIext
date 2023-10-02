# uslama3349.R ----

## STEP 1 ----
# Adjust for the 53rd calendar week

# Series has internal NA,fill with previous value
uslama3349_w_raw <- dta.raw |>
  filter(id == "uslama3349") |>
  mutate(KW = date2ISOweek(time)) |>
  separate(KW, c("ISOyear", "ISOweek", "day"), sep = "-") |>
  mutate(
    ISOweek = as.numeric(str_remove(ISOweek, patter = "^W")),
    ISOyear = as.numeric(ISOyear)
  ) |>
  select(-day)


tmp <- uslama3349_w_raw |>
  filter(ISOyear %in% years_w53) |>
  group_by(ISOyear) |>
  mutate(value_w53 = last(value)) |>
  filter(ISOweek != 53) |>
  mutate(value = value + (1 / 52) * value_w53) |>
  select(-value_w53) |>
  ungroup()


uslama3349_w <- uslama3349_w_raw |>
  filter(!(ISOyear %in% years_w53)) |>
  full_join(tmp) |>
  arrange(time)


## STEPS 2 and 3 ----
# Seasonal and calendar adjustment
uslama3349_w_hist <- uslama3349_w |>
  ts_span(end = "2019-12-31")


wd <- uslama3349_w_hist |>
  left_join(work_days, by = c("ISOyear", "ISOweek")) |>
  select(time, count) |>
  ts_long() |>
  ts_xts()


restrict_forecast_uslama3349_w_hist <- seq.Date(
  from = uslama3349_w_hist |>
    filter(time == max(time)) |>
    pull(time) + 7,
  length.out = 105,
  by = "weeks"
  ) |>
  as_tibble() |>
  mutate(KW = date2ISOweek(value)) |>
  separate(KW, c("ISOyear", "ISOweek", "day"), sep = "-") |>
  mutate(
    ISOweek = as.numeric(str_remove(ISOweek, patter = "^W")),
    ISOyear = as.numeric(ISOyear)
  ) |>
  rename(time = "value") |>
  filter(ISOweek != 53)


wd_fc_uslama3349_w <- work_days |>
  left_join(restrict_forecast_uslama3349_w_hist, by = c("ISOyear", "ISOweek")) |>
  filter(!is.na(time)) |>
  select(time, count) |>
  ts_long() |>
  ts_xts()


uslama3349_w.mod <- uslama3349_w_hist |>
  select(id, time, value) |>
  ts_xts() |>
  s31::wsa(
    Log = TRUE,
    outliers = FALSE,
    order = c(3, 1, 0, 1, 1, 0),
    regressor = wd[, 1],
    forecast_regressor = wd_fc_uslama3349_w[, 1]
  )


uslama3349_w_sfac <- uslama3349_w.mod$seas_comp |>
  ts_tbl()

uslama3349_w_wsa <- uslama3349_w |>
  select(id, time, value) |>
  ts_c(uslama3349_w_sfac) |>
  ts_wide() |>
  mutate(value = (`uslama3349`) / (cal_fac * s_fac)) |>
  mutate(id = "uslama3349") |>
  select(id, time, value) |>
  ts_na_omit() |>
  ts_span(end = fc_time)


uslama3349_w_raw_diffy <- uslama3349_w_raw |>
  select(id, time, value) |>
  mutate(diffy = (log(value) - lag(log(value), 52))) |>
  select(id, time, diffy) |>
  mutate(id = "raw")


uslama3349_w_wsa_diffy <- uslama3349_w_wsa |>
  mutate(diffy = if_else(year(time) %in% c(1993, 1999, 2005, 2010, 2016, 2021),
    (value - lag(value, 51)) / lag(value, 51),
    (value - lag(value, 52)) / lag(value, 52)
  )) |>
  select(id, time, diffy) |>
  mutate(id = "csa")

# END