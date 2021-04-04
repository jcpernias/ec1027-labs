library(ec1027)
library(dynlm)
library(ggplot2)

data(intdef)

db <- zoo(intdef, intdef$year)

autoplot(db$i3)
autoplot(db$inf)
autoplot(db$def)

model1 <- dynlm(i3 ~ inf + def, db, start = 1949)
coef_table(model1)

db$d1980 <- as.integer(db$year > 1979)
model2 <- update(model1, . ~ . + d1980)
summary(model2)

model3 <- update(model2, . ~ . + L(inf) + L(def))
summary(model3)

model4 <- update(model2, . ~ . + d(inf) + d(def))
summary(model4)


library(texreg)
screenreg(list(model1, model2, model3, model4), digits = 4)
