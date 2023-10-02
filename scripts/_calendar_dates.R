# _calendar_dates.R ----

## A timeDate Sequence ----
date.sequence <- timeSequence(
  as.Date("1995-01-01"),
  as.Date("2025-12-31")
) 
# a short example period with three London holidays


## holidays in the period ----
years.included <- unique(as.integer(format(x = date.sequence, format = "%Y")))
holidays <- holidayNYSE(years.included) 
# locale was not specified by OP in question nor in profile, 
# so this assumes for example: holidayLONDON
# also supported by timeDate are: holidayNERC, holidayNYSE, holidayTSX & holidayZURICH


## Subset business days ----
business.days <- date.sequence[isBizday(date.sequence, holidays)]


## Get business days per week ----
business_days <- as_tibble(business.days) |> 
  rename(bus_days = `GMT:value`) |>
  mutate(bus_days = as.Date(bus_days, format = "%Y-%m-%d")) |>
  mutate(KW = date2ISOweek(bus_days)) |>
  separate(KW, c("ISOyear", "ISOweek", "day"), sep = "-", remove = FALSE) |>
  mutate(
    ISOweek = as.numeric(str_remove(ISOweek, patter = "^W")),
    ISOyear = as.numeric(ISOyear)
  ) |>
  group_by(ISOyear, ISOweek) |>
  mutate(count = n()) |>
  summarize(count = last(count)) |>
  ungroup()


week_end_month_dummy <- date.sequence |>
  as_tibble() |>
  rename(time = `GMT:value`) |>
  mutate(time = as.Date(time)) |>
  mutate(KW = date2ISOweek(time)) |>
  separate(KW, c("ISOyear", "ISOweek", "day"), sep = "-", remove = FALSE) |>
  mutate(
    ISOweek = as.numeric(str_remove(ISOweek, patter = "^W")),
    ISOyear = as.numeric(ISOyear)
  ) |>
  mutate(month = month(time)) |>
  filter(day == 1) |>
  group_by(ISOyear, month) |>
  mutate(dummy = ifelse(ISOweek == max(ISOweek), 1, 0)) |>
  ungroup() |>
  select(ISOyear, ISOweek, dummy)


years_w53 <- c(
  "1992",
  "1998",
  "2004",
  "2009",
  "2015",
  "2020",
  "2026",
  "2032"
)

years_w1 <- c(
  "1993",
  "1999",
  "2005",
  "2010",
  "2016",
  "2021",
  "2027",
  "2033"
)


work_days <- business_days |>
  mutate(KW53_dummy = case_when(
    ISOyear %in% years_w53 & ISOweek == 52 ~ 1,
    TRUE ~ 0
  )) |>
  mutate(KW1_dummy = case_when(
    ISOyear %in% years_w1 & ISOweek == 1 ~ 1,
    TRUE ~ 0
  )) |>
  mutate(KW52_all_dummy = case_when(
    ISOweek == 52 ~ 1,
    TRUE ~ 0
  )) |>
  mutate(KW1_all_dummy = case_when(
    ISOweek == 1 ~ 1,
    TRUE ~ 0
  )) |>
  mutate(KW52_1 = case_when(
    ISOweek %in% c(52, 1) ~ 1,
    TRUE ~ 0
  ))


restrict_forecast <- seq.Date(
  from = last_week_monday + 7,
  length.out = 53,
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


wd_fc <- work_days |>
  left_join(restrict_forecast, by = c("ISOyear", "ISOweek")) |>
  filter(!is.na(time)) |>
  select(time, count, KW53_dummy, KW1_dummy, KW52_all_dummy, KW1_all_dummy, KW52_1) |>
  ts_long() |>
  ts_xts()

# END