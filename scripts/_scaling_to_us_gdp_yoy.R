# _scaling_to_us_gdp_yoy.R ----
# Scale with GDP CSSA YOY

## aggregate both WEI to quarter ----
last_bip_quarter <- max(gdp_last_vintage$time)

wei_base_q <- wei_base |>
  mutate(time = as.Date(time)) |>
  mutate(quarter = lubridate::quarter(time)) |>
  mutate(year = lubridate::year(time)) |>
  group_by(quarter, year) |>
  summarize(value = mean(value)) |>
  ungroup() |>
  arrange(year, quarter) |>
  mutate(
    day = "01",
    month = case_when(
      quarter == 1 ~ "01",
      quarter == 2 ~ "04",
      quarter == 3 ~ "07",
      quarter == 4 ~ "10"
    ),
    time = paste(year, month, day, sep = "-")
  ) |>
  mutate(
    time = as.Date(time),
    id = "wei_base_q"
  ) |>
  ts_span(template = gdp_last_vintage) |>
  select(time, id, value)


## Scaling WEI base with BIP PCY -----
gdp_pcy <- gdp_last_vintage |> select(id, time, value)

mod_pcy <- lm(ts_ts(ts_span(gdp_pcy, template = wei_base_q)) ~ ts_ts(wei_base_q)) |> summary()

wei_scaled_pcy <- wei_base |>
  mutate(
    inter = mod_pcy$coefficients[, "Estimate"][1],
    coeff = mod_pcy$coefficients[, "Estimate"][2]
  ) |>
  mutate(value_scale = inter + value * coeff) |>
  select(id, time, value = value_scale) |>
  mutate(id = "WEI scaled")

# END