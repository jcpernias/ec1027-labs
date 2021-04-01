library(tidyverse)
library(wooldridge)
library(dynlm)
library(texreg)

t <- 1948:2003
db <- ts(intdef, start = 1948)

frml_1 <- i3 ~ inf + def
model_1 <- dynlm(frml_1, db)
summary(model_1)

d1980 <- as.integer(t > 1979)
db <- cbind(i3 = db[ , "i3"], inf = db[ , "inf"], def = db[ , "def"], t, d1980)
frml_2 <- update(frml_1, . ~ . + d1980)
model_2 <- dynlm(frml_2, db)
summary(model_2)

frml_3 <- update(frml_2, . ~ . + L(inf) + L(def))
model_3 <- dynlm(frml_3, db)
summary(model_3)

frml_4 <- update(frml_2, . ~ . + d(inf) + d(def))
model_4 <- dynlm(frml_4, db)
summary(model_3)


screenreg(list(model_1, model_2, model_3, model_4), digits = 4)
