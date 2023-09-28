### Internal Packages
library(timemachine)
library(multDM)
library(kableExtra)
library(ggthemes)

# Out of Sample evaluation -----------------------------------------------------

# GRAPH 1: Original vs Adjusted wide replication

model_select <- tribble(
  ~expr, ~name,  ~type, 
  "mean_original",   "MEAN",  "original",
  "last_original",   "LAST",  "original",
  "mean_wide",       "MEAN",  "wsa",
  "last_wide",       "LAST",  "wsa",
  "mean_stl",        "MEAN",  "stl",
  "last_stl",        "LAST",  "stl"
)

## Settings --------------------------------------------------------------------
today_monday <- as.Date(cut(Sys.Date(), "week"))
last_week_monday <- today_monday - 14

## 1) Load calendar days -------------------------------------------------------
source(paste0(here::here(), "/_calendar_dates.R"))

## 2) Load data ----------------------------------------------------------------
source(paste0(here::here(), "/_load_data.R"))

load(paste0(here::here(), "/bench_rt.Rdata"))
benchmarks_rt <- out$results

load(paste0(here::here(), "/oos_rt.Rdata"))
wei_oos_rt <- out$results

# Evaluation period
start_date <- "2009-07-01"
end_date <- "2019-12-31"

# Full sample
# start_date <- "2009-01-01"
# end_date <- "2020-12-31"

gdp_last_vintage <- us_gdp_rt %>%
  filter(pub_date == "2021-03-01") 


bench_data <- us_gdp_rt %>%
  group_by(pub_date) %>%
  summarize(time = last(time), value = last(value)) %>%
  select(ref_date = time, ref_value = value)

# Asses errors of quarterly models
benchmarks_errors <- benchmarks_rt %>% 
  left_join(bench_data, by = "ref_date") %>% 
  filter(ref_date > start_date) %>% 
  filter(ref_date < end_date) %>% 
  group_by(expr, ref_date) %>% 
  arrange(desc(pub_date)) %>% 
  mutate(h = seq(n())) %>% 
  ungroup() %>%
  mutate(error = ref_value - value) %>%
  filter(!is.na(error)) %>%
  filter(h > 9) %>%
  mutate(h = h-9)

# Asses Model errors
model_errors <- wei_oos_rt %>% 
  left_join(bench_data, by = "ref_date") %>% 
  filter(ref_date > start_date) %>% 
  filter(ref_date < end_date) %>% 
  group_by(expr, ref_date) %>% 
  arrange(desc(pub_date)) %>% 
  mutate(h = seq(n())) %>% 
  ungroup() %>%
  mutate(error = ref_value - value) %>%
  filter(!is.na(error)) %>%
  filter(h > 9) %>%
  mutate(h = h-9)

# Compute RMSE
rmse.bench <- benchmarks_errors %>%
  filter(expr == "a11") %>% # aa (ar1), etf, randomwalk
  group_by(expr, h) %>%
  summarize(rmse = sqrt(sum(error^2)), 
            mae = mean(abs(error))) %>%
  ungroup() %>%
  gather(-expr, -h, key = "key", value = "value") %>%
  filter(key == "rmse")

rmse.models_errors <- model_errors %>%
  group_by(expr, h) %>%
  summarize(mse = sum(error^2),
            rmse = sqrt(sum(error^2)),
            mae = mean(abs(error))) %>%
  ungroup() %>%
  gather(-expr, -h, key = "key", value = "value") %>%
  filter(key == "rmse")

bench_rmse_abs <- rmse.bench %>% 
  filter(h %in% c(1,7,13,19,25)) %>% 
  pivot_wider(names_from = "h", values_from = "value") %>% 
  mutate(type = "bench", name = "a11") %>% 
  select(-expr, -key)

mods_relrmse <- left_join(rmse.models_errors, rmse.bench, by = c("key", "h")) %>%
  mutate(rel_ar = value.x/value.y) %>%
  select(expr = expr.x, h, rel_ar) %>% 
  left_join(model_select, by = "expr") %>% 
  filter(!is.na(name)) %>% 
  filter(h %in% c(1,7,13,19,25)) %>% 
  select(-expr) %>% 
  pivot_wider(names_from = "h", values_from = "rel_ar")

bind_rows(bench_rmse_abs, 
          mods_relrmse) %>%  
  # filter(type != "stl") %>% 
  mutate_if(is.numeric, round, 2) %>% 
  kable(format = "latex",
        booktabs = TRUE, 
        linesep = "") %>%
  kable_styling(font_size = 9, full_width = F)


dm_test <- function(y, 
                    f1, 
                    f2, 
                    Hor, 
                    Bench = TRUE, 
                    Bench_name, 
                    Ref_mod, 
                    Mod){
  
  y_err <- y %>%
    filter(expr == Bench_name) %>% 
    filter(h == Hor) %>% 
    select(time = ref_date, 
           value = ref_value) %>% 
    ts_ts()
  
  if(isTRUE(Bench)){
  f1_err <- f1 %>%
    filter(expr == Bench_name) %>%
    filter(h == Hor) %>%
    select(time = ref_date,
           value = value) %>%
    ts_ts()
  } else {
      f1_err <- f1 %>%
        filter(expr == Ref_mod) %>%
        filter(h == Hor) %>%
        select(time = ref_date,
               value = value) %>%
        ts_ts()
    }
  
  f2_err <- f2 %>%
    filter(expr == Mod) %>%
    filter(h == Hor) %>%
    select(time = ref_date,
           value = value) %>%
    ts_ts()
  
  h_step <- ifelse(Hor <= 13, 1, 2)
  
  out <- DM.test(f1_err, 
                 f2_err, 
                 y_err, 
                 loss.type = "SE",
                 h = h_step, 
                 c = TRUE,
                 H1 = "less")
  
  p_value <- out$p.value
}

res <- c()
for (i in c(1,25)) {
  out <- dm_test(y = benchmarks_errors, 
                 f1 = model_errors, 
                 f2 = model_errors, 
                 Hor = i, 
                 Bench = FALSE, 
                 Bench_name = "a11",
                 Ref_mod = "mean_stl",
                 Mod = "mean_wide"
  )
  res <- c(res, out)
}
res

# PLOT of pandemic period

df1 <- benchmarks_rt %>% 
  filter(expr == "a11") %>% 
  mutate(expr = "Benchmark ARMA(1,1)") %>% 
  filter(year(ref_date) == "2020") %>% 
  group_by(ref_date) %>% 
  arrange(desc(pub_date)) %>% 
  mutate(h = seq(n())) %>% 
  ungroup() %>% 
  arrange(ref_date) %>% 
  filter(h >= 10) %>% 
  filter(h <= 23) %>% 
  select(-h)

df2 <- bench_data %>% 
  filter(year(ref_date) == "2020") %>% 
  mutate(expr = "Realized GDP growth") %>% 
  left_join(df1 %>% select(pub_date, ref_date), by = "ref_date") 

df3 <- wei_oos_rt %>% 
  filter(expr %in% c("last_original", 
                   "mean_original", 
                   "last_wide", 
                   "mean_wide")) %>% 
  filter(year(ref_date) == "2020") %>% 
  group_by(expr, ref_date) %>% 
  arrange(desc(pub_date)) %>% 
  mutate(h = seq(n())) %>% 
  ungroup() %>% 
  arrange(ref_date) %>% 
  filter(h >= 10) %>% 
  filter(h <= 23) %>% 
  select(-h) %>% 
  left_join(model_select, by = "expr")

fig1 <- ggplot() + 
  geom_line(data = df1, 
            aes(x = pub_date, y = value, colour = expr),
            size = 1) +
  geom_line(data = df2,
            aes(x = pub_date, y = ref_value, colour = expr),
            size = 1) +
  geom_line(data = df3,
            aes(x = pub_date, y = value, colour = name, linetype = type),
            size = 1) +
  facet_wrap(vars(ref_date),
             scales = "free") +
  geom_hline(yintercept=0 , colour="#999999", size = .5, alpha = 0.5) +
  theme_tufte(base_size = 12, base_family = "serif", ticks = TRUE) +
  # scale_color_manual(values = c("#000000", "#999999", "#7F7F7F")) +
  labs(x = "Time",
       y = "GDP growth (y-o-y, in %") +
  theme(axis.text = element_text(size=10))+
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  theme(panel.grid = element_line(color = "#C3C3C3",
                                  size = 0.25,
                                  linetype = 2)) + 
  guides(color = guide_legend(order = 1),
         fill = guide_legend(order = 2)) +
  theme(legend.text=element_text(size=10),
        legend.position = "bottom",
        legend.title = element_blank())

fig1
ggsave(paste0(here::here(), "/_results/","wei_covid.png"), 
       width = 8, height = 4.5, dpi = 300, units = "in", device = "png")

# Diebold Mariano Test ----------------------------------------------------

# Frage 1: Sind die WWA Now und Forecasts signifikant besser als der AR(1)-Benchmark? 
# Frage 2: Ist der Nowcast/Forecast mit adjustiertem WWA signifikant besser als nicht-adjustiert?

# Beispiel: 

# H0: Method 2 ist besser als Method 1 (Benchmark)

# Es variert h von 1 bis 25
# Es variert expr in model_errors

mb.e <- benchmarks_errors %>%
  filter(h < 35) %>%
  filter(h > 9) %>%
  mutate(h = h-9) %>% 
  filter(expr == "a11" & h == 13) %>% 
  select(error) %>% 
  deframe()

m1.e <- model_errors %>%
  filter(h < 35) %>%
  filter(h > 9) %>%
  mutate(h = h-9) %>% 
  filter(expr == "mean_wide" & h == 13) %>% 
  select(error) %>% 
  deframe()

out <- dm.test(mb.e, m1.e, alternative = "greater" , h = 1, power = 2)$p.value

# Tabelle inklusive D-M-Test

model_select <- tribble(
  ~mod, ~name,  ~type, 
  "br_adjusted",     "BRIDGE",            "wsa",
  "abr_adjusted",    "AR-BRIDGE",         "wsa", 
  "mean_original",   "MEAN",              "original",
  "last_original",   "LAST",              "original",
  "mean_wide",   "MEAN",          "wsa",
  "last_wide",   "LAST",          "wsa",
  "mean_stl",   "MEAN",          "stl",
  "last_stl",   "LAST",          "stl"  
)

tab <- left_join(rmse.models_errors, rmse.bench, by = c("key", "h")) %>%
  mutate(rel_ar = value.x/value.y) %>%
  select(mod = expr.x, h, rel_ar) %>% 
  left_join(model_select, by = "mod") %>% 
  filter(!is.na(name)) %>% 
  filter(h < 35) %>%
  filter(h > 9) %>%
  mutate(h = h-9) %>%
  filter(h %in% c(1,7,13,19,25)) %>% 
  select(-mod) %>% 
  pivot_wider(names_from = "h", values_from = "rel_ar") %>% 
  mutate_if(is.numeric, round, 3) %>% 
  rename(Model = name)

# paste the code in the latex document and adapt it
tab %>% 
  mutate_if(is.numeric, round, 2) %>% 
  filter(type != "stl") %>% 
  kable(format = "latex",
        booktabs = TRUE, 
        linesep = "") %>%
  kable_styling(font_size = 9, full_width = F) %>% 
  add_header_above(c("Benchmark: AR(2)" = 1, "Horizon" = 5))

rmse.bench %>% 
  filter(h < 35) %>%
  filter(h > 9) %>%
  mutate(h = h-9) %>%
  filter(h %in% c(1,7,13,19,25)) %>% 
  pivot_wider(names_from = "h", values_from = "value") %>% 
  mutate_if(is.numeric, round, 2) %>% 
  kable(format = "latex",
        booktabs = TRUE, 
        linesep = "") %>%
  kable_styling(font_size = 9, full_width = F)

# Diebold Mariano Test ----------------------------------------------------

# H0: Method 2 ist besser als Method 1 (Quartalsbenchmark)

# p-value < 0.5 --> reject H0
# p-value >= 

# Es variert h von 1 bis 25
# Es variert expr in model_errors

b.e <- benchmarks_errors %>%
  filter(h < 35) %>%
  filter(h > 9) %>%
  mutate(h = h - 9) %>% 
  filter(expr == "a11")

mod.e <- model_errors %>%
  filter(h < 35) %>%
  filter(h > 9) %>%
  mutate(h = h - 9) 


# Frage 1: Sind die WWA Now und Forecasts signifikant besser als der AR(1)-Benchmark? 

# Create a function that iterate over all time horizons

dm.test_h <-function(x){
  
  res.1 <- list()
  
  for (i in 1:25) {
    e.1 <- b.e %>%
      filter(h == i) %>%
      select(error) %>%
      deframe()
    
    e.2 <- mod.e %>%
      filter(expr == x & h == i) %>%
      select(error) %>%
      deframe()
    
    res.1[[i]] <- tibble("mod.1" = "a11",
                         "mod.2" = x, 
                         "horizon" = i,
                         test_value = round(dm.test(e.1,
                                                              e.2,
                                                              alternative = "greater" ,
                                                              h = i, power = 2)$statistic, 2),
                         p_value  = round(dm.test(e.1,
                                                            e.2,
                                                            alternative = "greater",
                                                            h = i, power = 2)$p.value, 3))}
                                                  
                         # test_value = round(forecast::dm.test(e.1,
                         #                                      e.2,
                         #                                      alternative = "greater" ,
                         #                                      h = i, power = 2)$statistic, 2),
                         # p_value  = round(forecast::dm.test(e.1,
                         #                                    e.2,
                         #                                    alternative = "greater",
                         #                                    h = i, power = 2)$p.value, 3))}
  test_res1 <- res.1 %>% bind_rows() 
  
}

# use lapply to loop over all models

models <- model_errors %>% 
  distinct(expr) %>% 
  deframe() %>% 
  as.list()

results.1 <- lapply(models[c(1,3,4,6)], function(x) dm.test_h(x)) 

p.value_fct <- function (p.value) {
  unclass(symnum(p.value, corr = FALSE, na = FALSE, 
                 cutpoints = c(0, 0.01, 0.05, 0.1, 1), 
                 symbols = c("***", "**", "*", " ")))}

tab_res1 <- results.1 %>% 
  bind_rows() %>% 
  filter(horizon %in% c(1,7,13,19,25)) %>% 
  filter(mod.2 %in% model_select$mod) %>% 
  mutate(stars = p.value_fct(p_value)) %>% 
  unite(value, c(test_value, stars), sep = "") %>% 
  select(-p_value) %>% 
  pivot_wider(names_from = "horizon", values_from = "value") %>% 
  rename(mod = mod.2) %>% 
  left_join(model_select, by = "mod") %>% 
  rename(Model = name) %>% 
  select(-mod.1, -mod) %>% 
  select(Model, everything()) %>% 
  arrange(Model)

tab_res1 %>% 
  kable(format = "latex",
        booktabs = TRUE, 
        linesep = "") %>%
  kable_styling(font_size = 9, full_width = F) %>% 
  add_header_above(c(" " = 1, "Horizon" = 5))

# Frage 2: Ist der Nowcast/Forecast mit adjustiertem WWA signifikant besser als mean nicht-adjustiert?

unadj.e <- mod.e %>% 
  filter(expr %in% c("mean_original"))

adj.e <-  mod.e %>% 
  filter(expr %in% c("last_wide",
                     "mean_wide",
                     "last_stl",
                     "mean_stl")) 

dm.test_adj_unadj <- function(y){
  
  res.2 <- list()
  
  for (j in 1:25) {
    e.1 <- unadj.e %>%
      filter(h == j) %>%
      select(error) %>%
      deframe()
    
    e.2 <- adj.e %>%
      filter(expr == y & h == j) %>%
      select(error) %>%
      deframe()
    
    res.2[[j]] <- tibble("mod.1" = "mean_original",
                         "mod.2" = y,
                         horizon = j,
                         test_value = round(forecast::dm.test(e.1,
                                                              e.2,
                                                              alternative = "greater" ,
                                                              h = j, power = 2)$statistic, 2),
                         p_value = round(forecast::dm.test(e.1,
                                                           e.2,
                                                           alternative = "greater" ,
                                                           h = j, power = 2)$p.value, 3))}
  test_res2 <- res.2 %>% bind_rows() 
  
  
}

models_adj <- adj.e %>% 
  distinct(expr) %>% 
  deframe() %>% 
  as.list()

results.2 <- lapply(models_adj[c(2,4)], function(y) dm.test_adj_unadj(y)) 

tab_res2 <- results.2 %>% 
  bind_rows() %>% 
  filter(horizon %in% c(1,7,13,19,25)) %>% 
  mutate(stars = p.value_fct(p_value)) %>% 
  unite(value, c(test_value, stars), sep = "") %>% 
  select(-p_value) %>% 
  pivot_wider(names_from = "horizon", values_from = "value") %>% 
  rename(mod = mod.2) %>% 
  left_join(model_select, by = "mod") %>% 
  rename(Model = name) %>% 
  select(-mod.1, -mod) %>% 
  select(Model, everything()) %>% 
  arrange(Model)

tab_res2 %>% 
  kable(format = "latex",
        booktabs = TRUE, 
        linesep = "") %>%
  kable_styling(font_size = 9, full_width = F) %>% 
  add_header_above(c(" " = 1, "Horizon" = 5))
