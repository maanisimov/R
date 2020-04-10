library(tidyverse)
library(broom)
library(lmtest)
library(sandwich)
library(rio)

# Возьмем набор данных по бриллиантам
glimpse(diamonds)
library(pander)
pander(head(diamonds)) # table in html

head(diamonds)

# Посмотрим на наши даннные

qplot(data = diamonds, x= log(carat), y = log(price))

# Оценим две модели

model_ur <- lm(data=diamonds, log(price) ~ log(carat) + clarity + depth + table + x + y + z)
model_r <- lm(data=diamonds, log(price) ~ log(carat) + depth + table + x + y + z)

# Отчеты по каждой модели

summary(model_ur)

tidy(model_ur)

glance(model_ur)
