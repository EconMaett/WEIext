# _load_data.R ----
# id  obs                 diff     freq      start        end
# 1 ustrad4628 1295                7 day 52.17750 1996-07-01 2021-04-19
# 2 uslama3294 2834                7 day 52.17750 1967-01-02 2021-04-19
# 3 uslama3349 2833                7 day 52.17750 1967-01-02 2021-04-12
# 4 usprod0685 1895                7 day 52.17750 1984-12-31 2021-04-19
# 5 usprod0980 1590                7 day 52.17750 1990-11-05 2021-04-19
# 6 ustran0035 1686                7 day 52.17750 1989-01-02 2021-04-19
# 7   staffing  773                7 day 52.17750 2006-06-12 2021-03-29
# 8   withhold  832                7 day 52.17750 2005-01-24 2020-12-28
# 9        eei 4775 214.666666666667 day  1.70144 1928-01-02 2021-04-12


## EEI Output (starts in 1928, I restrict it to start in 1995) ----
eei_w_raw <- read_xlsx(paste0(here::here(), "/eei.xlsx"),
  sheet = "data",
  range = "C5:D4873",
  col_types = c("date", "numeric")
  ) |>
  rename(
    time = `Week Ended`,
    value = Output
  ) |>
  filter(!is.na(value)) |>
  mutate(time = as.Date(time) - 5) |>
  mutate(id = "eei") |>
  mutate(KW = date2ISOweek(time)) |>
  separate(KW, c("ISOyear", "ISOweek", "day"), sep = "-") |>
  mutate(
    ISOweek = as.numeric(str_remove(ISOweek, patter = "^W")),
    ISOyear = as.numeric(ISOyear)
  ) |>
  select(ISOyear, ISOweek, id, time, value) |>
  arrange(time) |>
  ts_tbl() |>
  filter(ISOyear >= "1995")


## Withholding Tax ----
withhold_w_raw <- read_xlsx(paste0(here::here(), "/withholding_tax.xlsx"),
  sheet = "Daily",
  range = "A6:H4170"
  ) |>
  rename(
    time = Date,
    value = `law basis (**)`
  ) |>
  select(time, value) |>
  mutate(time = as.Date(time)) |>
  filter(!is.na(time)) |>
  mutate(id = "withhold") |>
  mutate(KW = date2ISOweek(time)) |>
  separate(KW, c("ISOyear", "ISOweek", "day"), sep = "-") |>
  mutate(
    ISOweek = as.numeric(str_remove(ISOweek, patter = "^W")),
    ISOyear = as.numeric(ISOyear)
  ) |>
  fill(value, .direction = "down") |>
  filter(!is.na(value)) |>
  group_by(ISOyear, ISOweek) |>
  mutate(week_value = ifelse(value > 0,
    max(value, na.rm = TRUE),
    min(value, na.rm = TRUE)
  )) |>
  filter(day == 1) |>
  ungroup() |>
  select(ISOyear, ISOweek, id, time, value = week_value) |>
  arrange(time) |>
  ts_tbl() |>
  filter(ISOyear >= "1995")


## Staffing Index ----
staffing_w_raw <- read_xlsx(paste0(here::here(), "/staffing.xlsx"),
  sheet = "Historical Data",
  range = "A3:D776"
  ) |>
  rename(value = `Index Value`) |>
  filter(!is.na(value)) |>
  mutate(id = "staffing") |>
  mutate(date = ifelse(Week >= 10,
    paste0(Year, "-W", Week, "-1"),
    paste0(Year, "-W0", Week, "-1")
  )) |>
  mutate(time = ISOweek2date(date)) |>
  select(ISOyear = Year, ISOweek = Week, id, time, value) |>
  arrange(time) |>
  ts_tbl() |>
  filter(ISOyear >= "1995")


## Rasmussen Consumer Index ----
month_conv <- tribble(
  ~month, ~mm,
  "May", "05",
  "Apr", "04",
  "Mar", "03",
  "Feb", "02",
  "Jan", "01",
  "Dec", "12",
  "Nov", "11",
  "Oct", "10",
  "Sep", "09",
  "Aug", "08",
  "Jul", "07",
  "Jun", "06"
)


rasmussen_w_raw <- read_xlsx(paste0(here::here(), "/rasmussen.xlsx"),
  sheet = "data",
  range = "A1:C5419",
  col_types = c("text", "numeric", "numeric")
  ) |>
  separate(time, into = c("day", "month", "year"), sep = "-") |>
  left_join(month_conv, by = "month") |>
  mutate(time = as.Date(paste0(year, "-", mm, "-", day))) |>
  select(time, consumer, investors) |>
  pivot_longer(-time, names_to = "id", values_to = "value") |>
  mutate(KW = date2ISOweek(time)) |>
  separate(KW, c("ISOyear", "ISOweek", "day"), sep = "-") |>
  mutate(
    ISOweek = as.numeric(str_remove(ISOweek, patter = "^W")),
    ISOyear = as.numeric(ISOyear)
  ) |>
  select(ISOyear, ISOweek, day, id, time, value) |>
  group_by(id, ISOyear, ISOweek) |>
  # mutate(value = mean(value, na.rm = TRUE)) |>
  mutate(value = ifelse(value > 0,
    max(value, na.rm = TRUE),
    min(value, na.rm = TRUE)
  )) |>
  ungroup() |>
  arrange(time) |>
  filter(id == "consumer") |>
  filter(day == 1) |>
  select(-day) |>
  left_join(eei_w_raw, by = c("ISOyear", "ISOweek", "time")) |>
  mutate(value.x = ifelse(value.x == "NaN", lag(value.x, 1), value.x)) |> # Some series have internal NA,fill with previous value
  select(ISOyear, ISOweek, id = id.x, time, value = value.x)

# - ustrad4628: United States, Domestic Trade, Retail Trade, Johnson Redbook Index, Total, Change Y/Y
# - usprod0685: United States, Metal Production, Raw Steel Estimate (AISI)
# - usprod0983: United States, Oil & Gas, Petroleum Products Supplied, Total, per Day
# - ustran0035: United States, Traffic, Rail, Intermodal Units Originated, Total

railroad_w_raw <- macrobond(c(
  "ustran0035",
  "ustran0032"
  ), class = "tbl") |>
  ts_wide() |>
  mutate(sum = ustran0035 + ustran0032) |>
  select(time, value = sum) |>
  mutate(id = "railroad") |>
  mutate(KW = date2ISOweek(time)) |>
  separate(KW, c("ISOyear", "ISOweek", "day"), sep = "-") |>
  mutate(
    ISOweek = as.numeric(str_remove(ISOweek, patter = "^W")),
    ISOyear = as.numeric(ISOyear)
  ) |>
  select(ISOyear, ISOweek, id, time, value) |>
  mutate(value = ifelse(value == "NaN", lag(value, 1), value)) |> # Some series have internal NA, fill with previous value
  filter(!is.na(value)) |>
  ungroup() |>
  filter(ISOyear >= "1995")


fuel_sales_w_raw <- macrobond(c(
  "usprod0981",
  "usprod0982",
  "usprod0983"
  ), class = "tbl") |>
  ts_wide() |>
  mutate(sum = usprod0981 + usprod0982 + usprod0983) |>
  select(time, value = sum) |>
  mutate(id = "fuel_sales") |>
  mutate(KW = date2ISOweek(time)) |>
  separate(KW, c("ISOyear", "ISOweek", "day"), sep = "-") |>
  mutate(
    ISOweek = as.numeric(str_remove(ISOweek, patter = "^W")),
    ISOyear = as.numeric(ISOyear)
  ) |>
  select(ISOyear, ISOweek, id, time, value) |>
  mutate(value = ifelse(value == "NaN", lag(value, 1), value)) |> # Some series have internal NA, fill with previous value
  filter(!is.na(value)) |>
  ungroup() |>
  filter(ISOyear >= "1995")


mb_raw <- macrobond(c(
  "ustrad4628",
  # "uslama3294",
  # "uslama3349",
  "usprod0685"
  ), class = "tbl") |>
  group_by(id) |>
  mutate(KW = date2ISOweek(time)) |>
  separate(KW, c("ISOyear", "ISOweek", "day"), sep = "-") |>
  mutate(
    ISOweek = as.numeric(str_remove(ISOweek, patter = "^W")),
    ISOyear = as.numeric(ISOyear)
  ) |>
  select(ISOyear, ISOweek, id, time, value) |>
  mutate(value = ifelse(value == "NaN", lag(value, 1), value)) |> # Some series have internal NA,fill with previous value
  filter(!is.na(value)) |>
  ungroup() |>
  filter(ISOyear >= "1995")


## Restricted sample ----
# - uslama3294: United States, Unemployment, National, Jobless Claims, Initial, Total
# - uslama3349: United States, Unemployment, National, Jobless Claims, Continuing, Total
init_claims_rt <- get_alfred_series(
  "ICNSA",
  series_name = "value",
  observation_start = "1995-01-01",
  observation_end = "2020-12-31",
  realtime_start = "2020-12-31",
  realtime_end = "2021-01-10",
  api_key = NULL
  ) |>
  rename(
    `ref_date` = "date",
    `pub_date` = "realtime_period"
  ) |>
  mutate(series_name = "uslama3294") |>
  ts_tbl()


init_claims <- init_claims_rt |>
  filter(pub_date == "2021-01-07") |>
  select(id = series_name, value, time = ref_date) |>
  mutate(time = time - 5)


cont_claims_rt <- get_alfred_series(
  "CCNSA",
  series_name = "value",
  observation_start = "1995-01-01",
  observation_end = "2020-12-31",
  realtime_start = "2020-12-31",
  realtime_end = "2021-01-10",
  api_key = NULL
  ) |>
  rename(
    `ref_date` = "date",
    `pub_date` = "realtime_period"
  ) |>
  mutate(series_name = "uslama3349") |>
  ts_tbl()


cont_claims <- cont_claims_rt |>
  filter(pub_date == "2021-01-07") |>
  select(id = series_name, value, time = ref_date) |>
  mutate(time = time - 5)


dta.crude <- mb_raw |>
  bind_rows(
    railroad_w_raw,
    fuel_sales_w_raw,
    staffing_w_raw,
    withhold_w_raw,
    eei_w_raw,
    rasmussen_w_raw
  ) |>
  select(id, time, value) |>
  bind_rows(init_claims, cont_claims)


## Full sample ----
crude_summary <- dta.crude |>
  ts_summary()


dta.raw <- dta.crude |>
  ts_wide() |>
  ts_long() |>
  group_by(id) |>
  fill(value, .direction = "down") |>
  ungroup() |>
  ts_na_omit()


## Real time vintages ----

### REAL TIME BIP ----
tf <- "M:/Org/DPKJ/07_weitere_produkte/realtime/realtime_database.xlsx"

sheets <- readxl::excel_sheets(tf)

### VINTAGES CSA ----
convert_quarter <- tribble(
  ~Quarter, ~Date,
  "q1", "01-01",
  "q2", "04-01",
  "q3", "07-01",
  "q4", "10-01"
)


us_gdp_rt <- data_frame(var = setdiff("gdp_us", "title")) |>
  rowwise() |>
  mutate(data = list(read_excel(path = tf, sheet = var, skip = 10))) |>
  ungroup() |>
  unnest() |>
  rename(ref_date = "time") |>
  gather(pub_date, value, -var, -ref_date) |>
  separate(pub_date, into = c("Year", "Quarter"), sep = 4) |>
  left_join(convert_quarter, by = "Quarter") |>
  mutate(
    pub_date = as.Date(paste0(Year, "-", Date)),
    ref_date = as.Date(ref_date)
  ) |>
  select(pub_date, id = var, time = ref_date, value) |>
  filter(!is.na(value)) |>
  ts_pcy() |>
  mutate(pub_date = add_to_date(pub_date, by = "2 month")) |>
  mutate(id = "gdp") |>
  filter(time >= "1990-01-01")


### Real time US WEI ----
us_wei_rt <- get_alfred_series(
  "WEI",
  series_name = "value",
  observation_start = "1980-01-13",
  observation_end = "2020-12-31",
  realtime_start = "2020-12-31",
  realtime_end = "2021-01-10",
  api_key = NULL
  ) |>
  rename(
    `ref_date` = "date",
    `pub_date` = "realtime_period"
  ) |>
  mutate(series_name = "us wei") |>
  ts_tbl()


wei_original <- us_wei_rt |>
  filter(pub_date == "2021-01-07") |>
  select(id = series_name, value, time = ref_date) |>
  mutate(time = time - 5)

wei_original_mb <- macrobond("ussurv01117", class = "tbl")


### Plot ----
if (FALSE) {
  wei_original |> 
    ts_c(wei_original_rt, wei_scaled_pcy) |> 
    ts_plot()
  
}

# END