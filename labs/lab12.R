library(ec1027)
library(dynlm)
library(ggplot2)

# library(tidyverse)
# library(wooldridge)
# library(texreg)
# library(broom)
# library(lmtest)
# library(sandwich)

data(earns)

earns <- within(earns, {
  hrwage <- wkearns / wkhours
  lhrwage <- log(hrwage)
  loutphr <- log(outphr)
})

db <- zoo(earns, earns$year)

autoplot(db$lhrwage)
autoplot(db$loutphr)


model1 <- dynlm(lhrwage ~ loutphr + year, data = db, start = 1949)
summary(model1)
uhat1 <- resid(model1)
autoplot(uhat1)
db_ar <- merge(db, uhat1, fill = 0)
aux_ar <- update(model1, uhat1 ~ . + L(uhat1), data = db_ar)
summary(aux_ar)


db$ghrwage <- diff(db$lhrwage)
db$goutphr <- diff(db$loutphr)

autoplot(db$ghrwage)
autoplot(db$goutphr)

model2 <- dynlm(ghrwage ~ goutphr, data = db, start = 1949)
summary(model2)

db_ar <- merge(db, uhat2 = resid(model2), fill = 0)
aux_ar <- update(model2, uhat2 ~ . + L(uhat2), data = db_ar)
summary(aux_ar)

model3 <- update(model2, . ~ . + L(goutphr))
summary(model3)

db$dgoutphr <- diff(db$goutphr)

model4 <- dynlm(ghrwage ~ dgoutphr  + L(goutphr), data = db, start = 1949)
summary(model4)


model5 <- update(model3, . ~ . + L(goutphr, 2), data = db, start = 1949)
summary(model5)

model_lst <- list("No lags" = model2,
                  "1 lag" = model3,
                  "2 lags" = model5)
vapply(model_lst, AIC, numeric(1))
vapply(model_lst, BIC, numeric(1))



