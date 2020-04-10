library(tidyverse)
library(lmtest)
library(sandwich)

glimpse(diamonds)

qplot(data=diamonds, x=carat, y=price)
qplot(data=diamonds, x=log(carat), y=log(price))

model_a <- lm(data = diamonds, price ~ carat + x + y + z)
coeftest(model_a) # с гомоскедастичностью

coeftest(model_a, vcov. = vcovHC) # default - HC3

?vcovHC

coeftest(model_a, vcov. = vcovHC(model_a, type='HC1'))


model_b <- lm(data=diamonds, price ~ carat)

waldtest(model_b, model_a) # гомоскедастичный
waldtest(model_b, model_a, test='Chisq') # гомоскедастичный

waldtest(model_b, model_a, vcov=vcovHC, test='Chisq') # гетероскедастичный(n->inf)

?waldtest

# правильная спецификация
model_log <- lm(data = diamonds, log(price) ~ log(carat) + log(1 + x) +
                log(1+y) + log(1+z))
coeftest(model_log, vcov. = vcovHC)
coeftest(model_log, vcov. = vcovHC(model_log, type='HC1'))


# тесты на гетероскедастичеость

# тест Уайта с корректировкой Коинкера
bptest(data=diamonds, price ~ carat + x + y + z, varformula = ~ carat * x * y * z)

# тест Уайта без корректировки Коинкера
bptest(data=diamonds, price ~ carat + x + y + z, studentize = F,
       varformula = ~ carat * x * y * z)

# тест Голдфельда-Квандта
# H0: homoscedocity
# H1: heteroscedocity: Var(u_i|X) = f(carat_i)
gqtest(data=diamonds, fraction = 0.2, price ~ carat + x + y + z, order.by = ~ carat)


