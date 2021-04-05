library(ec1027)
library(ggplot2)
library(dynlm)
library(sandwich)

data(traffic2)

monthly_index <- function(year, month) {
  yearmon(year + (month - 1) / 12)
}

v <- traffic2$month
N <- length(v)
values <- unique(v)
lst <- lapply(values, function(x) (v == x) + 0)
names(lst) <- paste0("seas", formatC(1:12, width = 2, flag = "0"))

traffic2 <- data.frame(traffic2, lst)

idx <- with(traffic2, monthly_index(year, month))

traffic2 <- within(traffic2, {
  trend <- (year - 1981) * 12 + month - 1
  prcfat <- 100 * fatacc / totacc
})

db <- zoo(traffic2, idx)

autoplot(db$prcfat)

frml1 <- prcfat ~ trend + seas02 + seas03 + seas04 + seas05 +
  seas06 + seas07 + seas08 + seas09 + seas10 + seas11 + seas12 +
  wkends + unem + spdlaw + beltlaw
model1 <- dynlm(frml1, data = db)
summary(model1)

uhat1 <- resid(model1)
autoplot(uhat1)

## AR(1) test
uhat <- resid(model1)
db_u <- merge(uhat, uhat_1 = lag(uhat, k = -1), fill = 0)
db_ar <- merge(db, db_u)
aux_ar <- update(model1, uhat ~ . + uhat_1, data = db_ar)
summary(aux_ar)

summary(dynlm(uhat1 ~ uhat_1, data = db_ar))

coef_table(model1, vce = "NW")


## CO estimation
model <- model1
mf <- model$model
y <- model.response(mf)
mm <- as.zoo(model.matrix(model, mf), index(y))

uhat <- resid(model)

db_rho <- merge(uhat, uhat_1 = lag(uhat, k = -1))
mod_rho <- lm(uhat ~ uhat_1 - 1, data = db_rho)
rho <- coef(mod_rho)[[1]]
rho

Xtr <- mm - rho * lag(mm, k = -1)
ytr <- y - rho * lag(y, k = -1)
co <- dynlm(ytr ~ Xtr - 1)
uhat <- y - mm %*% coef(co)

summary(co)

## PW estimation
uhat <- resid(model)

db_rho <- merge(uhat, uhat_1 = lag(uhat, k = -1))
mod_rho <- lm(uhat ~ uhat_1 - 1, data = db_rho)
rho <- coef(mod_rho)[[1]]
rho

Xtr <- rbind(sqrt(1-rho^2) * mm[1, ], mm - rho * lag(mm, k = -1))
ytr <- c(sqrt(1-rho^2) * y[1], y - rho * lag(y, k = -1))
co <- dynlm(ytr ~ Xtr - 1)
uhat <- y - mm %*% coef(co)

summary(co)


model2 <- update(model1, . ~ . + L(prcfat))
summary(model2)

uhat <- resid(model2)
db_u <- merge(uhat, uhat_1 = lag(uhat, k = -1), fill = 0)
db_ar <- merge(db, db_u)
aux_ar <- update(model1, uhat ~ . + uhat_1, data = db_ar)
summary(aux_ar)

summary(dynlm(uhat ~ uhat_1 + 0, data = db_ar))


model3 <- update(model1, . ~ . + L(prcfat, 1) - trend)
summary(model3)
uhat <- resid(model3)
autoplot(uhat)
db_u <- merge(uhat, uhat_1 = lag(uhat, k = -1), fill = 0)
db_ar <- merge(db, db_u)
aux_ar <- update(model3, uhat ~ . + uhat_1, data = db_ar)
summary(aux_ar)
