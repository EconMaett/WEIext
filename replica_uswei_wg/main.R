# Improving US-WEI
# PREAMBLE ------------------------------------------------------------
rm(list=ls())

## Packages --------------------------------------------------------------------

### CRAN-Packages
library(tidyverse)
library(tsbox)
library(dsa) 
library(ISOweek)
library(here)
library(lubridate)
library(readxl)
library(pracma)
library(forecast)
library(timeDate)
library(alfred)
library(zoo)
library(ggthemes)

library(timemachine) # remotes::install_github("christophsax/timemachine")

## Settings --------------------------------------------------------------------
today_monday <- as.Date(cut(Sys.Date(), "week"))
last_week_monday <- today_monday - 14
fc_time <- last_week_monday + 56

## 1) Load calendar days -------------------------------------------------------
source(paste0(here::here(), "/_calendar_dates.R"))

## 2) Load data ----------------------------------------------------------------
source(paste0(here::here(), "/_load_data.R"))

start.horizon <- min(crude_summary$start)
end.horizon <- min(crude_summary$end)

gdp_last_vintage <- us_gdp_rt %>% 
  filter(pub_date == "2020-12-01")

source(paste0(here::here(), "/_narrow.R"))
source(paste0(here::here(), "/_wide.R"))

us_wei <- ts_c(wei_original, wei_scaled_pcy_wide) 
us_wei %>% ts_plot()

out <- list(
  results = us_wei)
save(out, file = paste0(here::here(), "/us_wei.Rdata"))

# Results to be replicated from paper

fl_paper <- tribble(
 ~name,   ~id         , ~ value,
 "Initial claims",   "uslama3294"  , -0.37,
 "Continued claims", "uslama3349"  , -0.41,
 "Steel production", "usprod0685"  , 0.36,
 "Railroad traffic", "railroad"    , 0.34,
 "Fuel sales",       "fuel_sales"  , 0.22,
 "Staffing index",   "staffing"    , 0.40,
 "Electricity output", "eei"         , 0.12,
 "Consumer confidence", "consumer"    , 0.23,
 "Same-store retail sales", "ustrad4628"  , 0.28,
 "Tax withholding", "withhold"    , 0.30
)

var_paper <- tribble(
  ~name     , ~paper,  ~narrow, ~wide, ~stl, 
  "Total variance explained",	
  55.44 , 
  (sum_pc$importance[,"PC1"]["Proportion of Variance"])*100, 
  (sum_pc_wide$importance[,"PC1"]["Proportion of Variance"])*100,
  (sum_pc_stl$importance[,"PC1"]["Proportion of Variance"])*100
)
var_paper

fl_narrow %>% 
  mutate(loading = loading*(-1)) %>% 
  left_join(fl_wide, by = "id") %>% 
  rename(`narrow` = "loading.x", 
         `wide` = "loading.y") %>%
  left_join(fl_wide_stl, by = "id") %>% 
  rename(`stl` = "loading") %>% 
  left_join(fl_paper, by = "id") %>% 
  rename(`paper` = "value") %>% 
  mutate(narrow = round(narrow, 2)) %>% 
  mutate(wide = round(wide, 2)) %>%
  mutate(stl = round(stl, 2)) %>%
  select(name, paper, narrow, wide, stl) %>% 
  bind_rows(var_paper)

# Figure comparing WEI narrow and wide
ts_c(`narrow` = ts_ts(wei_scaled_pcy_narrow), 
     `wide` = ts_ts(wei_scaled_pcy_wide),
     `stl` = ts_ts(wei_scaled_pcy_wide_stl),
     `original` = wei_original, 
     `GDP` = gdp_pcy) %>% 
  ts_span(start = 2010, end = 2019) %>% 
  ts_plot()

ts_c(`wide` = ts_ts(wei_scaled_pcy_wide),
     `original` = wei_original) %>% 
  ts_span(start = 2008, end = 2021) %>% 
  ts_plot()


ts_c(`wide` = ts_ts(wei_scaled_pcy_wide),
     `original` = wei_original,
     `GDP` = gdp_pcy) %>%
  mutate(quarter = quarter(time),
         year = year(time)) %>%
  filter(year == "2020") %>%
  ggplot() +
  geom_line(aes(x = time, y = value, color = id)) +
  facet_wrap(vars(quarter))

path <- here::here()

df1 <- us_gdp_rt %>%
  group_by(pub_date) %>% 
  filter(time == max(time)) %>% 
  ungroup() %>% 
  select(id, time, value) %>% 
  shift_time_middle() %>%
  ts_span(start = "2008-01-01", 
          end = "2021-01-01") %>% 
  mutate(id = "GDP (y-o-y)")

df2 <- ts_c(`Original` = ts_ts(wei_original),
            `Weekly seasonal adjusted` = ts_ts(wei_scaled_pcy_wide)) %>% 
  ts_span(start = "2008-01-01", 
          end = "2020-12-31") %>% 
  ts_tbl() %>% 
  mutate(time = as.Date(time))

fig1 <- ggplot() +
  geom_col(
    data = df1,
    mapping = aes(x = time, 
                  y = value, 
                  fill = id)
  ) +
  geom_line(
    data = df2, 
    mapping = aes(y = value,
                  x = time,
                  colour = id), 
    size = 1,
  ) +
  geom_hline(yintercept=0 , colour="#999999", size = .5, alpha = 0.5) +
  theme_tufte(base_size = 12, base_family = "serif", ticks = TRUE) +
  scale_color_manual(values = c("#000000", "#999999", "#7F7F7F")) +
  scale_fill_manual(values = c("#7F7F7F")) +
  labs(x = "Time",
       y = "52-week change, in %")+
  theme(axis.text = element_text(size=10))+
  scale_y_continuous(
    expand = expand_scale(0.005),
    limits = c(-12, 6),
    breaks = seq(-12, 6, by = 3)) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  theme(panel.grid = element_line(color = "#C3C3C3",
                                  size = 0.25,
                                  linetype = 2)) + 
  guides(color = guide_legend(order = 1),
         fill = guide_legend(order = 2)) +
  theme(legend.text=element_text(size=10),
        legend.position = "bottom",
        legend.title = element_blank())

fig1
ggsave(paste0(path, "/_results/","wei_comparison.png"), 
       width = 8, height = 4.5, dpi = 300, units = "in", device = "png")

#load(file = paste0(here::here(), "/us_wei.Rdata"))

wei <- out$results %>% 
  ts_xts() %>% 
  ts_frequency(to = "quarter", aggregate = "mean", na.rm = TRUE) %>% 
  ts_span(start = "2010-01-01", end = "2019-10-01") 

gdp_last_vintage <- us_gdp_rt %>%
  filter(pub_date == "2021-03-01") %>% 
  select(-pub_date) %>% 
  ts_xts() %>% 
  ts_span(start = "2010-01-01", end = "2019-10-01") 

ts_c(wei, gdp_last_vintage) %>% ts_plot()

Metrics::rmse(gdp_last_vintage, wei$`us wei`)
Metrics::rmse(gdp_last_vintage, wei$`us wei wsa`)
