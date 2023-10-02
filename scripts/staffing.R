# staffing.R ----

## STEP 1 ----
# Adjust for the 53rd calendar week
tmp <- staffing_w_raw |>
  filter(ISOyear %in% years_w53) |>
  group_by(ISOyear) |>
  mutate(value_w53 = last(value)) |>
  filter(ISOweek != 53) |>
  mutate(value = value + (1 / 52) * value_w53) |>
  select(-value_w53) |>
  ungroup()


staffing_w <- staffing_w_raw |>
  filter(!(ISOyear %in% years_w53)) |>
  full_join(tmp) |>
  arrange(time)


## STEPS 2 and 3 ----
# Seasonal and calendar adjustment
staffing_w_hist <- staffing_w |>
  ts_span(end = "2019-12-31")


wd <- staffing_w_hist |>
  left_join(work_days, by = c("ISOyear", "ISOweek")) |>
  select(time, count) |>
  ts_long() |>
  ts_xts()


restrict_forecast_staffing_w_hist <- seq.Date(
  from = staffing_w_hist |>
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


wd_fc_staffing_w <- work_days |>
  left_join(restrict_forecast_staffing_w_hist, by = c("ISOyear", "ISOweek")) |>
  filter(!is.na(time)) |>
  select(time, count) |>
  ts_long() |>
  ts_xts()


staffing_w.mod <- staffing_w_hist |>
  select(id, time, value) |>
  ts_xts() |>
  s31::wsa(
    Log = TRUE,
    outliers = FALSE,
    order = c(3, 1, 0, 1, 1, 0),
    regressor = wd[, 1],
    forecast_regressor = wd_fc_staffing_w[, 1]
  )


staffing_w_sfac <- staffing_w.mod$seas_comp |>
  ts_tbl()


staffing_w_wsa <- staffing_w |>
  select(id, time, value) |>
  ts_c(staffing_w_sfac) |>
  ts_wide() |>
  mutate(value = (`staffing`) / (cal_fac * s_fac)) |>
  mutate(id = "staffing") |>
  select(id, time, value) |>
  ts_na_omit() |>
  ts_span(end = fc_time)

# END