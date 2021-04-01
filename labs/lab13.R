library(tidyverse)
library(glue)
library(wooldridge)
library(dynlm)
library(texreg)
library(broom)
library(lmtest)
library(sandwich)
library(prais)
library(orcutt)

tr2 <- traffic2 %>% 
  select(totacc, fatacc, prcfat, beltlaw, spdlaw, wkends, unem) %>%
  ts(start = c(1981, 1), frequency = 12)

ts_dates <- function(ts) {
  y <- as.integer(time(ts))
  m <- cycle(ts)
  lubridate::make_date(year = y, month = m)
}

ts_to_df <- function(ts) {
  df <- as_tibble(ts) %>% mutate(.Index = ts_dates(ts))
  df
}

ts_pull <- function(data, var) {
  v <- rlang::as_name(rlang::ensym(var))
  data[, v]
}


ts_plot <- function(data, x) {
  df <- ts_to_df(data)
  plot_db <- df %>% select(date = .Index, {{ x }}) 
  ggplot(plot_db, aes(x = date, y = {{ x }})) +
    geom_line() +
    scale_x_date(date_labels = "%b %Y") +
    xlab(NULL)
}

ts_lag <- function(ts, k = 1, fill = NA) {
  ts_start <- start(ts)
  ts_end <- end(ts)
  lagged <- window(stats::lag(ts, k = -k), 
                   start = ts_start, end = ts_end, extend = TRUE)
  window(lagged, start = ts_start, end = ts_start) <- fill 
  colnames(lagged) <- glue("L{k}_{colnames(ts)}")
  lagged
}

ts_diff <- function(ts, k = 1, fill = NA) {
  ts_start <- start(ts)
  ts_end <- end(ts)
  lagged <- ts_lag(ts, k = k)
  d <- ts - lagged
  colnames(d) <- glue("D{k}_{colnames(ts)}")
  d
}

ts_det <- function(ts) {
  seas_names <- function(x, width) {
    glue("seas{formatC(x, flag = '0', width = width)}")
  }

  seas_fn <- function(x, y) {
    ifelse(x == y, 1, 0)
  }
  
  det <- cbind(time(ts) - start(ts)[1], 
                  outer(cycle(ts), 1:12, seas_fn))
  colnames(det) <- c("t", seas_names(1:12, 2))
  det  
}

ts_merge <- function(...) {
  args <- list(...)
  all <- do.call(ts.union, args)
  all_names <- unlist(lapply(args, colnames))
  colnames(all) <- all_names
  all
}


tr2_det <- ts_det(tr2)
L1_tr2 <- ts_lag(tr2)
D1_tr2 <- ts_diff(tr2)
tr2_plus <- ts_merge(tr2, tr2_det, L1_tr2, D1_tr2)



ts_plot(tr2, prcfat)

zz <- ts_pull(tr2, prcfat)

frml_1 <- prcfat ~ t + seas02 + seas03 + seas04 + seas05 + 
  seas06 + seas07 + seas08 + seas09 + seas10 + seas11 + seas12 +
  wkends + unem + spdlaw + beltlaw
model1 <- dynlm(frml_1, data = tr2_plus)
screenreg(model1, digits = 4)

uhat_1 <- resid(model1)

ggplot(tidy(uhat_1), aes(x = index, y = value)) +
  geom_line() + 
  scale_x_yearmon()

## AR(1) test
L.uhat_1 <- window(stats::lag(uhat_1, k = -1), start = c(1981, 1), extend = TRUE)
window(L.uhat_1, start = c(1981, 1), end = c(1981, 1)) <- 0
ar1_db <- ts.intersect(uhat_1, L.uhat_1, tr2)
colnames(ar1_db) <- c("uhat_1", "L.uhat_1", colnames(tr2))
ar1_frml <- update(frml_1, uhat_1 ~ . + L.uhat_1)
ar1_test <- dynlm(ar1_frml, data = ar1_db)
screenreg(ar1_test, digits = 4)

summary(dynlm(uhat_1 ~ L.uhat_1, data = ar1_db))

coeftest(model1, vcov. = NeweyWest)

co <- cochrane.orcutt(model1)
summary(co)

tidx <- time(tr2) - 1981
seas <- as.matrix(outer(cycle(tr2), 1:12, function(x, y) {
  ifelse(x == y, 1, 0)
}))
tr2_plus <- ts.intersect(tr2, seas, tidx)
colnames(tr2_plus) <- c(colnames(tr2), month.abb, "t")
frml_pw <- prcfat ~ t + 
  Feb + Mar + Apr + May + Jun + Jul + Aug + Sep + Oct + Nov + Dec + 
  wkends + unem + spdlaw + beltlaw
pw <- prais_winsten(frml_pw, data = tr2_plus)
