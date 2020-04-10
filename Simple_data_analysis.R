library('psych')
library('dplyr')
library('ggplot2')
library('GGally')

d <- cars
glimpse(d)

help(cars)

head(d)
tail(d)
describe(d)
ncol(d)
nrow(d)
str(d) # structure of dataframe
tail(d, 3) # 3 last rows

mean(d$speed)
mean(d$dist)


d2 <- mutate(d, speed=1.67*speed, dist=0.3*dist,
             ratio=dist/speed) # changing the data
glimpse(d2)

qplot(data=d2, dist)
qplot(data=d2, dist, xlab='Длина тормозного пути, м', 
      ylab='Кол-во машин', main='Данные 1920-х годов')
qplot(data=d2, speed, dist)


model <- lm(data=d2, dist~speed)
model

beta_hat = coef(model)
beta_hat
eps_hat <- residuals(model)
eps_hat
y <- d2$dist
y_hat <- fitted(model)
y
y_hat
RSS <- deviance(model)
RSS
TSS <- sum((y - mean(y))^2)
TSS
ESS <- TSS - RSS
R2 <- ESS/TSS
R2

cor(y, y_hat)^2 # this is R2 too

X <- model.matrix(model)
X

nd = data.frame(speed=c(40, 60)) # new data
nd
predict(model, nd)

qplot(data=d2, speed, dist) + stat_smooth(method='lm')


t <- swiss
help(swiss)

glimpse(t)
describe(t)

ggpairs(t) # many distribution graphs in one click!

model2 <- lm(data=t,
             Fertility~Agriculture + Education + Catholic)
coef(model2)
fitted(model2)
residuals(model2)
deviance(model2) # RSS

report <-summary(model2)
report
report$r.squared # R2

cor(t$Fertility, fitted(model2))^2

nd2 <- data.frame(Agriculture=0.5, Catholic=0.5,
                  Education=20)
predict(model2, nd2)

two <- c("AA", "AS")
lut <- c("AA" = "American", 
         "AS" = "Alaska", 
         "B6" = "JetBlue")
two <- lut[two]
two
