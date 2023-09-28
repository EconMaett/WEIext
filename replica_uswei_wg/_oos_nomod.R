# Out of Sample evaluation by MIDAS --------------------------------------------------
library(forecast)
library(timemachine)

# ---- SETTINGS ----------------------------------------------------------------

REAL_TIME <- TRUE

# Telling the time machine where to evaluate
dates <- seq(as.Date("2009-01-01"),
             to = as.Date("2020-12-21"),
             by = "week")

# ---- DATEN EINLESEN ----------------------------------------------------------

# (2.1) adjusted (not scaled)
wwa_adj <- wei_scaled_pcy_wide %>% 
  mutate(id = "wei_wide") %>% 
  pseudo_history(by = "7 days")

wwa_unadj <- wei_original %>% 
  mutate(id = "wei_original") %>% 
  pseudo_history(by = "7 days")

gdp_prt_hist <- pseudo_history(gdp_pcy, by = "5 months")

# History 

if(isTRUE(REAL_TIME)){
  # History REAL TIME
  history <- us_gdp_rt %>% 
    rename(`ref_date` = "time") %>% 
    mutate(id = "bip") %>%
    bind_rows(wwa_adj,
              wwa_unadj) %>%
    check_history()
  }else{
      # History PSEUDO REAL TIME
      bip_pseudo_rt <- pseudo_history(gdp_pcy, by = "5 month") %>% 
        # ts_span(start = "1995-01-01") %>% 
        mutate(id = "bip") %>% 
        select(pub_date, ref_date, value, id)
      history <- bind_rows(bip_pseudo_rt,
                           wwa_adj,
                           wwa_unadj) %>%
        check_history()
    }

# timemachine with NO MODEL -----------------------------------------------------------------

nomod <- timemachine(
  last_adjusted = {
    wei_wide %>% 
      ts_tbl() %>% 
      mutate(time = as.Date(time+1)) %>% 
      mutate(quarter=lubridate::quarter(time)) %>% 
      mutate(year=lubridate::year(time)) %>% 
      filter(time >= add_to_date(ts_summary(bip)$end, by = "3 months")) %>% 
      mutate(day="01",
             month= case_when(
               quarter == 1 ~ "01",
               quarter == 2 ~ "04",
               quarter == 3 ~ "07",
               quarter == 4 ~ "10"),
             time=paste(year, month, day, sep="-")) %>%
      mutate(time=as.Date(time)) %>% 
      select(time, value) %>% 
      complete(time = seq(min(time), 
                          max(add_to_date(ts_summary(bip)$end, by = "4 quarter")), 
                          by = "1 quarter")) %>% 
      fill(value) %>% 
      group_by(time) %>% 
      mutate(h = seq(n())) %>% 
      filter(h == max(h)) %>% 
      select(time, value) %>% 
      ungroup() %>% 
      ts_ts()
  },
  last_original = {
    wei_original %>% 
      ts_tbl() %>% 
      mutate(time = as.Date(time+1)) %>% 
      mutate(quarter=lubridate::quarter(time)) %>% 
      mutate(year=lubridate::year(time)) %>% 
      # filter(time == max(time)) %>% 
      filter(time >= add_to_date(ts_summary(bip)$end, by = "3 months")) %>% 
      mutate(day="01",
             month= case_when(
               quarter == 1 ~ "01",
               quarter == 2 ~ "04",
               quarter == 3 ~ "07",
               quarter == 4 ~ "10"),
             time=paste(year, month, day, sep="-")) %>%
      mutate(time=as.Date(time)) %>% 
      select(time, value) %>% 
      complete(time = seq(min(time), 
                          max(add_to_date(ts_summary(bip)$end, by = "4 quarter")), 
                          by = "1 quarter")) %>% 
      fill(value) %>% 
      group_by(time) %>% 
      mutate(h = seq(n())) %>% 
      filter(h == max(h)) %>% 
      select(time, value) %>% 
      ungroup() %>% 
      ts_ts()
  },
  mean_adjusted = {
    wei_wide %>%
      ts_tbl() %>% 
      mutate(time = as.Date(time+1))  %>% 
      mutate(quarter=lubridate::quarter(time)) %>% 
      mutate(year=lubridate::year(time)) %>% 
      filter(time >= add_to_date(ts_summary(bip)$end, by = "3 months")) %>% 
      group_by(year, quarter) %>% 
      # filter(year == max(year)) %>%
      # filter(quarter == max(quarter)) %>% 
      mutate(value = mean(value, na.rm = TRUE)) %>% 
      # filter(time == max(time)) %>% 
      mutate(h = seq(n())) %>% 
      filter(h == max(h)) %>% 
      ungroup() %>% 
      mutate(day="01",
             month= case_when(
               quarter == 1 ~ "01",
               quarter == 2 ~ "04",
               quarter == 3 ~ "07",
               quarter == 4 ~ "10"),
             time=paste(year, month, day, sep="-")) %>%
      mutate(time=as.Date(time)) %>% 
      select(time, value) %>% 
      complete(time = seq(min(time), 
                          max(add_to_date(ts_summary(bip)$end, by = "4 quarter")), 
                          by = "1 quarter")) %>% 
      fill(value) %>% 
      ts_ts() },
  mean_original = {
    wei_original %>%
      ts_tbl() %>% 
      mutate(time = as.Date(time+1))  %>% 
      mutate(quarter=lubridate::quarter(time)) %>% 
      mutate(year=lubridate::year(time)) %>% 
      filter(time >= add_to_date(ts_summary(bip)$end, by = "3 months")) %>% 
      group_by(year, quarter) %>% 
      # filter(year == max(year)) %>%
      # filter(quarter == max(quarter)) %>% 
      mutate(value = mean(value, na.rm = TRUE)) %>% 
      # filter(time == max(time)) %>% 
      mutate(h = seq(n())) %>% 
      filter(h == max(h)) %>% 
      ungroup() %>% 
      mutate(day="01",
             month= case_when(
               quarter == 1 ~ "01",
               quarter == 2 ~ "04",
               quarter == 3 ~ "07",
               quarter == 4 ~ "10"),
             time=paste(year, month, day, sep="-")) %>%
      mutate(time=as.Date(time)) %>% 
      select(time, value) %>% 
      complete(time = seq(min(time), 
                          max(add_to_date(ts_summary(bip)$end, by = "4 quarter")), 
                          by = "1 quarter")) %>% 
      fill(value) %>% 
      ts_ts() },
  history = history,
  dates = dates
)

save(nomod, file = paste0(here::here(), "/nomod_rt.Rdata"))
