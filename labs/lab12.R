library(tidyverse)
library(wooldridge)
library(dynlm)
library(texreg)
library(broom)
library(lmtest)
library(sandwich)

db <- earns %>%
  select(year, hrwage, outphr) %>%
  mutate(lhrwage = log(hrwage),
         loutphr = log(outphr), 
         d_lhrwage = lhrwage - lag(lhrwage),
         d_loutphr = loutphr - lag(loutphr),
         d2_loutphr = d_loutphr - lag(d_loutphr)) %>%
  ts(start = 1947)

db_long <- tidy(db)
ggplot(db_long %>% filter(series == "lhrwage"),
       aes(x = index, y = value, group = series)) + 
  geom_line()

ggplot(db_long %>% filter(series == "loutphr"),
       aes(x = index, y = value, group = series)) + 
  geom_line()

ggplot(db_long %>% filter(series == "d_lhrwage", index != 1947),
       aes(x = index, y = value, group = series)) + 
  geom_line()

ggplot(db_long %>% filter(series == "d_loutphr", index != 1947),
       aes(x = index, y = value, group = series)) + 
  geom_line()

model_1 <- dynlm(lhrwage ~ loutphr + trend(lhrwage), data = db, start = 1949)  
screenreg(model_1, digits = 4)
bgtest(model_1)

uhat_1 <- resid(model_1)
ggplot(tidy(uhat_1),
       aes(x = index, y = value)) + 
  geom_line()

model_2 <- dynlm(d_lhrwage ~ d_loutphr, data = db, start = 1949)  
screenreg(model_2, digits = 4)
bgtest(model_2)


model_3 <- dynlm(d_lhrwage ~ d_loutphr + L(d_loutphr), data = db, start = 1949)  
screenreg(model_3, digits = 4)
bgtest(model_3)


model_4 <- dynlm(d_lhrwage ~ d2_loutphr  + L(d_loutphr), data = db, start = 1949)  
screenreg(model_4, digits = 4)
bgtest(model_4)

model_5 <- dynlm(d_lhrwage ~ d_loutphr  + L(d_loutphr) + L(d_loutphr, 2), data = db, start = 1949)  
screenreg(model_5, digits = 4)
bgtest(model_5)
