library('memisc')
library('dplyr')
library('psych')
library('lmtest')
library('sjPlot')
library('sgof')
library('ggplot2')
library('foreign')
library('car')
library('hexbin')

# генерация случайных величин
# 100 с.в. ~ N(5, 9)
z <- rnorm(100, 5, 9)
z[55]
z[2:10]
qplot(z)

# пострение функции плотности
x <- seq(-10, 15, by=0.5)
y <- dnorm(x, mean=5, sd=3)
qplot(x, y)
qplot(x,y, geom='line')

# рассчет вероятностей
#P(Z < 3) = F(3)
pnorm(3, mean=5, sd=3)

pnorm(9, mean=5, sd=3) - pnorm(4, 5, 3)


# квантили распределения
qnorm(0.7, mean=5, sd=3)

qchisq(1-0.022, df=1)

#chisq, t, f - функции распределения


# множественная регрессия: проверка гипотез
h <- swiss
glimpse(swiss)
help(swiss)

model <- lm(data=h, Fertility ~ Catholic + Agriculture + Examination)
summary(model)

coef(model)

coeftest(model) # только коэффициенты
confint(model)

sjp.lm(model) # красивый график о значимости коэффициентов


# проверка гипотезы b_Cath = b_Agri
model_aux <- lm(data=h,
                Fertility ~ Catholic + I(Catholic + Agriculture) + Examination)
# I() - совместный коэффициент для параметров 

summary(model_aux)

linearHypothesis(model, 'Catholic-Agriculture=0') # easier


# стандартизированные коэффициенты
h_st <- mutate_each(h, 'scale') # standartization
glimpse(h_st)

model_st <- lm(data=h_st, Fertility ~ Catholic + Agriculture + Examination)
summary(model_st)
sjp.lm(model_st)

sjp.lm(model, showStadardBeta=T) # error?


# искуственный эксперимент
D <- matrix(nrow=100, rnorm(100*41, mean=0, sd=1))
df <- data.frame(D)
glimpse(df)

model_pusto <- lm(data=df, X1 ~. ) # all regressors
summary(model_pusto)
# при большом числе регрессоров даже фигня что-то будет объяснять

# сравнение нескольких моделей
model2 <- lm(data=h, Fertility ~ Catholic +
              Agriculture)



compar_12 <- mtable(model, model2)
compar_12


# сохранение результатов
stuff <- list(data=h, model=model2)
stuff

saveRDS(file='my_data.RDS', stuff)

mylist<- readRDS('my_data.RDS')
mylist$model

summary(mylist$model)

# RDS - внутренний формат R
# csv (comma separated values) - более распространенный формат; разделение может быть с помощью чего угодно


qt(0.95, df=28)
-3 + 0.5*qt(0.95, df=28)

round(pchisq(9, df=10), digits = 2)

data <- diamonds
glimpse(data)

min(data$price)

length(filter(data, cut=='Very Good'))

d_model <- lm(data=data, price ~ carat + table)
coeftest(d_model)

simple_model <- lm(data=data, price ~ carat)
linearHypothesis(simple_model, 'carat=0')
summary(simple_model)

glimpse(data)

model_18 <- lm(data=data, price ~ carat + x + y + z)
summary(model_18)

coeftest(model_18, level=0.01)

linearHypothesis(model_18, 'y=0')


model <- lm(data=data, price ~ carat + y + x)
coeftest(model)
confint(model, level = 0.1)


