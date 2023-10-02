
# Index, not to be in PCY
consumer_w_raw <- dta.raw %>% 
  filter(id == "consumer") %>% 
  mutate(KW = date2ISOweek(time)) %>%
  separate(KW, c("ISOyear", "ISOweek", "day"), sep = "-") %>%
  mutate(ISOweek = as.numeric(str_remove(ISOweek, patter = "^W")),
         ISOyear=as.numeric(ISOyear))  %>%
  select(-day) 

tmp <- consumer_w_raw %>% 
  filter(ISOyear %in% years_w53) %>% 
  group_by(ISOyear) %>%
  mutate(value_w53 = last(value)) %>% 
  filter(ISOweek != 53) %>% 
  # mutate(value = value + (1/52)*value_w53) %>%  # Instead of distributing value, just drop it
  select(-value_w53) %>% 
  ungroup()  

consumer_w <- consumer_w_raw %>% 
  filter(!(ISOyear %in% years_w53)) %>% 
  full_join(tmp) %>% 
  arrange(time)

consumer_w_wsa <- consumer_w %>%
  select(id, time, value) %>%
  mutate(id = "consumer") %>%
  ts_na_omit() %>%
  ts_span(end = fc_time)

# Series has no season
