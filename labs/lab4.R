library(ec1027)
library(sandwich)
# library(lmtest)
# library(ggplot2)



model_1 <- lm(price ~ lotsize + sqrft + bdrms, data = hprice1)
coef_table(model_1, vcovHC, type = "HC2")
coef_table(model_1, vcovHC)
coef_table(model_1)
summary(model_1)

model_0 <- lm(price ~ colonial + I(colonial^2), data = hprice1, na.action = na.exclude)
coef_table(model_0, vcovHC, type = "HC2")
coef_table(model_0, vcovHC)
coef_table(model_0)
summary(model_0)



frml_1 <- price ~ lotsize + sqrft + bdrms
model_1 <- lm(frml_1, data = hprice1)

summary(model_1)

omit_test(model_1, .vcov = vcovHC)

waldtest(model_1, vcov = vcovHC)

se_MCO <- se(model_1)
se_HC <- se(model_1, vcovHC)

se_comp <- cbind(MCO = se_MCO, 
                 HC = se_HC,
                 "Ratio HC / MCO" = se_HC / se_MCO)
round(se_comp, 5)

hprice1$uhat_1 <- resid(model_1)
frml_2 <- update(frml_1, uhat_1 ~ .)
model_2 <- lm(frml_2, data = hprice1)
summary(model_2)


hprice1$sq_uhat_1 <- hprice1$uhat_1^2
frml_3 <- update(frml_1, sq_uhat_1 ~ .)
model_3 <- lm(frml_3, data = hprice1)
summary(model_3)

bp_test(model_1)


hprice_white <- within(hprice1, {
  sq_lotsize <- lotsize^2
  sq_sqrft <- sqrft^2
  sq_bdrms <- bdrms^2
  lotsize_sqrft <- lotsize * sqrft
  lotsize_bdrms <- lotsize * bdrms
  sqrft_bdrms <- sqrft * bdrms
})

frml_white <- update(frml_1, sq_uhat_1 ~ . + sq_lotsize + sq_sqrft + sq_bdrms +
                       lotsize_sqrft + lotsize_bdrms + sqrft_bdrms)
mod_white <- lm(frml_white, data = hprice_white)
summ_white <- summary(mod_white)
summ_white
Rsq <- summ_white$r.squared
W <- nobs(mod_white) * Rsq
k <- length(coef(mod_white)) - 1 
pchisq(W, k, lower.tail = FALSE)

white_test(model_1, chisq = TRUE)


hprice1 <- within(hprice1, {
  yhat_1 <- fitted(model_1)
  sq_yhat_1 <- yhat_1^2
})

mod_white2 <- lm(sq_uhat_1 ~ yhat_1 + sq_yhat_1, data = hprice1)
summary(mod_white2)

with(hprice1,
     bp_test(model_1, ~ yhat_1 + sq_yhat_1))

hprice1 <- within(hprice1, {
  lprice <- log(price)
  llotsize <- log(lotsize)
  lsqrft <- log(sqrft)
})

frml_l <- lprice ~ llotsize + lsqrft + bdrms
model_l <- lm(frml_l, data = hprice1)
summary(model_l)
coef_table(model_l, vcovHC)


bp_test(model_l)
white_test(model_l, chisq = TRUE)

hprice1 <- within(hprice1, {
  yhat_l <- fitted(model_l)
  sq_yhat_l <- yhat_l^2
})

with(hprice1,
     bp_test(model_l, ~ yhat_l + sq_yhat_l))





