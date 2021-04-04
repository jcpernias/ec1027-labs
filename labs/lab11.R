library(ec1027)
library(dynlm)
library(ggplot2)

data(hseinv)

hseinv <- within(hseinv, {
  invpc <- inv / pop
  linvpc <- log(invpc)
  lprice <- log(price)
})

db <- zoo(hseinv, hseinv$year)


autoplot(db$linvpc)
autoplot(db$lprice)

model1 <- dynlm(linvpc ~ lprice, data = db)
summary(model1)

model2 <- dynlm(linvpc ~ lprice + year, data = db)
summary(model2)

invpc_ar <- dynlm(linvpc ~ L(linvpc, 1), data = db)
coef(invpc_ar)
invpc_art <- update(invpc_ar, . ~ . + year)
coef(invpc_art)

price_ar <- dynlm(lprice ~ L(lprice, 1), data = db)
coef(price_ar)
price_art <- update(price_ar, . ~ . + year)
coef(price_art)


db$ginvpc <- diff(db$linvpc)
db$gprice <- diff(db$lprice)

head(db)

db2 <- window(db, start = 1948)

autoplot(db2$ginvpc)
autoplot(db2$gprice)

model3 <- dynlm(linvpc ~ gprice + year, data = db2)
summary(model3)

invpc_t <- dynlm(linvpc ~ year, data = db2)
linvpc_not <- resid(invpc_t)

model4 <- update(model3, linvpc_not ~ .)
summary(model4)

model5 <- update(model3, ginvpc ~ .)
summary(model5)

model6 <- update(model5, . ~ . - year)
summary(model6)
