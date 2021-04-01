library(ec1027)
library(sandwich)
library(lmtest)
library(ggplot2)

mod_5 <- lm(frml_1, data = hprice1, lotsize <= 20000)
summary(mod_5)

bp_test(mod_5)
white_test(mod_5)


hprice1$huge <- ifelse(hprice1$lotsize > 50000, 1, 0)
hprice1h <- subset(hprice1, lotsize <= 20000)
frml_1h <- update(frml_1, . ~ . + huge)
model_1h <- lm(frml_1h, data = hprice1)
summary(model_1h)
coeftest(model_1h, vcovHC, type = "HC3")
hprice1h <- within(hprice1h, {
  uhat <- resid(model_1h)
  sq_uhat <- uhat ^ 2
  yhat <- fitted(model_1h)
  yhat_sq <- yhat^2
})

ggplot(hprice1h, aes(x = lotsize, y = uhat)) +
  geom_point(alpha = 0.5)

summary(update(model_1h, sq_uhat ~ .))


ggplot(hprice1, aes(x = bdrms, y = sq_uhat_1)) +
  geom_point(alpha = 0.5)

