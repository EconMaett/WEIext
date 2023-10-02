# ustran0035.R ----

## STEP 1 ----

# Adjust for the 53rd calendar week
railroad_w_raw <- dta.raw |>
  filter(id == "railroad") |>
  mutate(KW = date2ISOweek(time)) |>
  separate(KW, c("ISOyear", "ISOweek", "day"), sep = "-") |>
  mutate(
    ISOweek = as.numeric(str_remove(ISOweek, patter = "^W")),
    ISOyear = as.numeric(ISOyear)
  ) |>
  select(-day)


tmp <- railroad_w_raw |>
  filter(ISOyear %in% years_w53) |>
  group_by(ISOyear) |>
  mutate(value_w53 = last(value)) |>
  filter(ISOweek != 53) |>
  mutate(value = value + (1 / 52) * value_w53) |>
  select(-value_w53) |>
  ungroup()


railroad_w <- railroad_w_raw |>
  filter(!(ISOyear %in% years_w53)) |>
  full_join(tmp) |>
  arrange(time)


## STEPS 2 and 3 -----
# Seasonal and calendar adjustment

# complete seasonal adjustment
wd <- railroad_w |>
  left_join(work_days, by = c("ISOyear", "ISOweek")) |>
  select(time, count) |>
  ts_long() |>
  ts_xts()


railroad_w.mod <- railroad_w |>
  select(id, time, value) |>
  ts_xts() |>
  s31::wsa(
    Log = TRUE,
    outliers = FALSE,
    order = c(3, 1, 1, 1, 1, 0),
    regressor = wd[, 1],
    forecast_regressor = cbind(wd_fc[, 1])
  )


railroad_w_wsa <- railroad_w.mod$output$seas_adj |>
  ts_tbl() |>
  mutate(id = "railroad") |>
  ts_span(end = fc_time)



railroad_w_raw_diffy <- railroad_w_raw |>
  select(id, time, value) |>
  mutate(diffy = (log(value) - lag(log(value), 52))) |>
  select(id, time, diffy) |>
  mutate(id = "raw")


railroad_w_wsa_diffy <- railroad_w_wsa |>
  mutate(diffy = if_else(year(time) %in% c(1993, 1999, 2005, 2010, 2016, 2021),
    (value - lag(value, 51)) / lag(value, 51),
    (value - lag(value, 52)) / lag(value, 52)
  )) |>
  select(id, time, diffy) |>
  mutate(id = "csa")

# END