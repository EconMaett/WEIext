# _remove_outliers.R ----
# Remove outliers and data anomalies

## Filter outliers -----
eei_diffy_raw <- dta.diffy |>
  filter(id == "eei") |>
  ts_ts() |>
  ts_tbl() |>
  mutate(value = ifelse(is.na(value), -1, value)) |>
  ts_ts()


eei_diffy_raw_outlier_adj <- hampel(eei_diffy_raw, k = 6, 1)

eei_diffy_outlier_adj <- eei_diffy_raw_outlier_adj$y |>
  ts_tbl() |>
  mutate(id = "eei") |>
  mutate(time = time + 1) |>
  mutate(time = as.Date(time)) |>
  mutate(isodate = date2ISOweek(time)) |>
  separate(isodate, c("year", "week", "day"), sep = "-") |>
  filter(week != "W53") |>
  select(-year, -week, -day)


fuel_sales_diffy_raw <- dta.diffy |>
  filter(id == "fuel_sales") |>
  ts_ts() |>
  ts_tbl() |>
  mutate(value = ifelse(is.na(value), -1, value)) |>
  ts_ts()


fuel_sales_diffy_raw_outlier_adj <- hampel(fuel_sales_diffy_raw, k = 6, 2)

fuel_sales_diffy_outlier_adj <- fuel_sales_diffy_raw_outlier_adj$y |>
  ts_tbl() |>
  mutate(id = "fuel_sales") |>
  mutate(time = time + 1) |>
  mutate(time = as.Date(time)) |>
  mutate(isodate = date2ISOweek(time)) |>
  separate(isodate, c("year", "week", "day"), sep = "-") |>
  filter(week != "W53") |>
  select(-year, -week, -day)


withhold_diffy_raw <- dta.diffy |>
  filter(id == "withhold") |>
  ts_ts() |>
  ts_tbl() |>
  mutate(value = ifelse(is.na(value), -1, value)) |>
  ts_ts()


withhold_diffy_raw_outlier_adj <- hampel(withhold_diffy_raw, k = 6, 2)

withhold_diffy_outlier_adj <- withhold_diffy_raw_outlier_adj$y |>
  ts_tbl() |>
  mutate(id = "withhold") |>
  mutate(time = time + 1) |>
  mutate(time = as.Date(time)) |>
  mutate(isodate = date2ISOweek(time)) |>
  separate(isodate, c("year", "week", "day"), sep = "-") |>
  filter(week != "W53") |>
  select(-year, -week, -day)


staffing_diffy_raw <- dta.diffy |>
  filter(id == "staffing") |>
  ts_ts() |>
  ts_tbl() |>
  mutate(value = ifelse(is.na(value), -1, value)) |>
  ts_ts()


staffing_diffy_raw_outlier_adj <- hampel(staffing_diffy_raw, k = 6, 2)

staffing_diffy_outlier_adj <- staffing_diffy_raw_outlier_adj$y |>
  ts_tbl() |>
  mutate(id = "staffing") |>
  mutate(time = time + 1) |>
  mutate(time = as.Date(time)) |>
  mutate(isodate = date2ISOweek(time)) |>
  separate(isodate, c("year", "week", "day"), sep = "-") |>
  filter(week != "W53") |>
  select(-year, -week, -day)


uslama3294_diffy_raw <- dta.diffy |>
  filter(id == "uslama3294") |>
  ts_ts() |>
  ts_tbl() |>
  mutate(value = ifelse(is.na(value), -1, value)) |>
  ts_ts()


uslama3294_diffy_raw_outlier_adj <- hampel(uslama3294_diffy_raw, k = 6, 2)

uslama3294_diffy_outlier_adj <- uslama3294_diffy_raw_outlier_adj$y |>
  ts_tbl() |>
  mutate(id = "uslama3294") |>
  mutate(time = time + 1) |>
  mutate(time = as.Date(time)) |>
  mutate(isodate = date2ISOweek(time)) |>
  separate(isodate, c("year", "week", "day"), sep = "-") |>
  filter(week != "W53") |>
  select(-year, -week, -day)

uslama3349_diffy_raw <- dta.diffy |>
  filter(id == "uslama3349") |>
  ts_ts() |>
  ts_tbl() |>
  mutate(value = ifelse(is.na(value), -1, value)) |>
  ts_ts()


uslama3349_diffy_raw_outlier_adj <- hampel(uslama3349_diffy_raw, k = 6, 2)

uslama3349_diffy_outlier_adj <- uslama3349_diffy_raw_outlier_adj$y |>
  ts_tbl() |>
  mutate(id = "uslama3349") |>
  mutate(time = time + 1) |>
  mutate(time = as.Date(time)) |>
  mutate(isodate = date2ISOweek(time)) |>
  separate(isodate, c("year", "week", "day"), sep = "-") |>
  filter(week != "W53") |>
  select(-year, -week, -day)


usprod0685_diffy_raw <- dta.diffy |>
  filter(id == "usprod0685") |>
  ts_ts() |>
  ts_tbl() |>
  mutate(value = ifelse(is.na(value), -1, value)) |>
  ts_ts()


usprod0685_diffy_raw_outlier_adj <- hampel(usprod0685_diffy_raw, k = 6, 2)

usprod0685_diffy_outlier_adj <- usprod0685_diffy_raw_outlier_adj$y |>
  ts_tbl() |>
  mutate(id = "usprod0685") |>
  mutate(time = time + 1) |>
  mutate(time = as.Date(time)) |>
  mutate(isodate = date2ISOweek(time)) |>
  separate(isodate, c("year", "week", "day"), sep = "-") |>
  filter(week != "W53") |>
  select(-year, -week, -day)


railroad_diffy_raw <- dta.diffy |>
  filter(id == "railroad") |>
  ts_ts() |>
  ts_tbl() |>
  mutate(value = ifelse(is.na(value), -1, value)) |>
  ts_ts()


railroad_diffy_raw_outlier_adj <- hampel(railroad_diffy_raw, k = 6, 2)

railroad_diffy_outlier_adj <- railroad_diffy_raw_outlier_adj$y |>
  ts_tbl() |>
  mutate(id = "railroad") |>
  mutate(time = time + 1) |>
  mutate(time = as.Date(time)) |>
  mutate(isodate = date2ISOweek(time)) |>
  separate(isodate, c("year", "week", "day"), sep = "-") |>
  filter(week != "W53") |>
  select(-year, -week, -day)


redbook_raw <- dta.diffy |>
  filter(id == "ustrad4628") |>
  ts_ts() |>
  ts_tbl() |>
  mutate(value = ifelse(is.na(value), -1, value)) |>
  ts_ts()


redbook_raw_outlier_adj <- hampel(redbook_raw, k = 6, 2)

redbook_outlier_adj <- redbook_raw_outlier_adj$y |>
  ts_tbl() |>
  mutate(id = "ustrad4628") |>
  mutate(time = time + 1) |>
  mutate(time = as.Date(time)) |>
  mutate(isodate = date2ISOweek(time)) |>
  separate(isodate, c("year", "week", "day"), sep = "-") |>
  filter(week != "W53") |>
  select(-year, -week, -day)


consumer_raw <- dta.diffy |>
  filter(id == "consumer") |>
  ts_ts() |>
  ts_tbl() |>
  mutate(value = ifelse(is.na(value), -1, value)) |>
  ts_ts()


consumer_raw_outlier_adj <- hampel(consumer_raw, k = 6, 2)

consumer_outlier_adj <- consumer_raw_outlier_adj$y |>
  ts_tbl() |>
  mutate(id = "consumer") |>
  mutate(time = time + 1) |>
  mutate(time = as.Date(time)) |>
  mutate(isodate = date2ISOweek(time)) |>
  separate(isodate, c("year", "week", "day"), sep = "-") |>
  filter(week != "W53") |>
  select(-year, -week, -day)

# END