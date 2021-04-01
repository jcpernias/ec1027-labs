library(tidyverse)
library(wooldridge)
library(dynlm)
library(texreg)
library(broom)

years <- 1947:1988
db <- hseinv %>%
  select(inv, pop, price) %>%
  mutate(year = 1947:1988,
         invpc = inv / pop,
         l_invpc = log(invpc),
         l_price = log(price),
         dl_invpc = l_invpc - lag(l_invpc),
         dl_price = l_price - lag(l_price)) %>%
  filter(year != 1947) %>%
  ts(start = 1948, frequency = 1)


db_long <- tidy(db)

ggplot(db_long %>% filter(series == "l_invpc"),
       aes(x = index, y = value, group = series)) +
  geom_line()

ggplot(db_long %>% filter(series == "l_price"),
       aes(x = index, y = value, group = series)) +
  geom_line()


model_1 <- dynlm(l_invpc ~ l_price, data = db)
model_2 <- dynlm(l_invpc ~ l_price + trend(l_invpc), data = db)
screenreg(list(model_1, model_2), digits = 4)

invpc_ar <- dynlm(l_invpc ~ L(l_invpc, 1), data = db)
coef(invpc_ar)
invpc_art <- update(invpc_ar, . ~ . + trend(l_invpc))
coef(invpc_art)

price_ar <- dynlm(l_price ~ L(l_price, 1), data = db)
coef(price_ar)
price_art <- update(price_ar, . ~ . + trend(l_price))
coef(price_art)


ggplot(db_long %>% filter(series == "dl_invpc"),
       aes(x = index, y = value, group = series)) +
  geom_line()

ggplot(db_long %>% filter(series == "dl_price"),
       aes(x = index, y = value, group = series)) +
  geom_line()


model_3 <- dynlm(l_invpc ~ dl_price + trend(l_invpc), data = db)
screenreg(list(model_1, model_2, model_3))

invpc_t <- dynlm(l_invpc ~ trend(l_invpc), data = db)
linvpc_not <- resid(invpc_t) 

model_4 <- dynlm(linvpc_not ~ dl_price , data = db)
screenreg(list(model_3, model_4), digits = 4)

model_5 <- dynlm(dl_invpc ~ dl_price + trend(l_invpc) , data = db)
model_6 <- dynlm(dl_invpc ~ dl_price, data = db)
screenreg(list(model_5, model_6), digits = 4)
