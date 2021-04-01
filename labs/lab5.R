library(wooldridge)
library(sandwich)
library(lmtest)

frml_1 <- colGPA ~ hsGPA + ACT + skipped + PC
model_1 <- lm(frml_1, data = gpa1)
summary(model_1)
coeftest(model_1, vcovHC)

gpa1$uhat_1 <- resid(model_1)
gpa1$sq_uhat_1 <- gpa1$uhat_1^2

gpa1_white <- within(gpa1, {
  sq_hsGPA <- hsGPA^2
  sq_ACT <- ACT^2
  sq_skipped <- skipped^2
  hsGPA_ACT <- hsGPA * ACT
  hsGPA_skipped <- hsGPA * skipped
  hsGPA_PC <- hsGPA * PC
  ACT_skipped <- ACT * skipped
  ACT_PC <- ACT * PC
  skipped_PC <- skipped * PC
})

frml_white <- update(frml_1, sq_uhat_1 ~ . + sq_hsGPA + sq_ACT + sq_skipped + 
                       hsGPA_ACT + hsGPA_skipped + hsGPA_PC + ACT_skipped + 
                       ACT_PC + skipped_PC)
mod_white <- lm(frml_white, data = gpa1_white)
summary(mod_white)

frml_white2 <- update(frml_1, sq_uhat_1 ~ . + sq_hsGPA + sq_ACT + sq_skipped)
mod_white2 <- lm(frml_white2, data = gpa1_white)
summary(mod_white2)

gpa1 <- within(gpa1, {
  yhat <- fitted(model_1)
  sq_yhat <- yhat^2
})

mod_white3 <- lm(sq_uhat_1 ~ yhat + sq_yhat, data = gpa1)
summary(mod_white3)

model_2 <- lm(frml_1, data = gpa1, weights = 1 / hsGPA)
summary(model_2)
summary(model_1)

gpa1_wls <- within(gpa1, l_sq_uhat_1 <- log(sq_uhat_1))

aux <- lm(l_sq_uhat_1 ~ yhat + sq_yhat, data = gpa1_wls)
gpa1_wls$h <- exp(fitted(aux))
model_3 <- lm(frml_1, data = gpa1_wls, weights = 1 / h)
summary(model_3)
summary(model_1)

