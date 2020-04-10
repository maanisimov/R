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
library('vcd')
library('pander')
library('knitr')
library('devtools')


h <- diamonds
glimpse(h)
help(diamonds)

qplot(data=h, carat, price)
bg <- qplot(data=h, log(carat), log(price))

bg + geom_hex()


f <- read.csv('flats_moscow.txt', sep='\t', header=T, dec='.')
glimpse(f)

qplot(data=f, totsp, price)

qplot(data=f, log(totsp), log(price))

mosaic(data=f, ~walk + brick + floor, shade=T)

f <- mutate_each(f, 'factor', walk, brick, floor, code)
glimpse(f)

qplot(data=f, log(price))
qplot(data=f, price)
qplot(data=f, log(price), fill=brick)

qplot(data=f, log(price), fill=brick, position = 'dodge')

g2 <- qplot(data=f, log(price), fill=brick, geom='density', alpha=0.5)

g2 + facet_grid(walk~floor)

g2 + facet_grid(~floor)


model_0 <- lm(data=f, log(price) ~ log(totsp))
model_1 <- lm(data=f, log(price) ~ log(totsp) + brick)
model_2 <- lm(data=f, log(price) ~ log(totsp) + brick + brick:log(totsp))
# brick:log(totsp) - произведение переменных

summary(model_0)
mtable(model_2)

model_2b <- lm(data=f, log(price) ~ brick*log(totsp))
# brick*log(totsp) - берет все эти переменные и их попарное перемножение

mtable(model_2, model_2b)

sjp.lm(model_2)

mtable(model_0, model_1, model_2)

nw <- data.frame(totsp=c(60, 60), brick=factor(c(1, 0)))
nw

predict(model_2, newdata = nw)
exp(predict(model_2, newdata = nw))

predict(model_2, newdata = nw, interval = 'confidence')
exp(predict(model_2, newdata = nw, interval = 'confidence'))
# стоимость среднестат. квартиры

predict(model_2, newdata = nw, interval = 'prediction')
exp(predict(model_2, newdata = nw, interval = 'prediction'))
# стоимость конкретной квартиры


waldtest(model_0, model_1) # основная гипотеза о верности модели 0 отвергается
waldtest(model_1, model_2) # H_0 is rejected

gg0 <- qplot(data=f, log(totsp), log(price))
gg0 + stat_smooth(method = 'lm') + facet_grid(~walk)
gg0 + stat_smooth(method = 'lm') + facet_grid(~walk) + aes(col=brick)


f$nonbrick <- memisc::recode(f$brick, 1 <- 0, 0 <- 1)

glimpse(f)

mtable(model_0, model_1, model_2)

resettest(model_2) # test with H_0: there are no missing regressors


# контрошка

(70/2)*20/25
round(qf(p=0.95, df1=2, 23), digits = 2)

21.9 + 80^2*0.01 + 2*80*(-0.46)

1259.265 + 21.9 + 80^2*0.01 + 2*80*(-0.46)


df <- diamonds
glimpse(df)
summary(df)  

model <- lm(data=df, log(price) ~ carat)
summary(model)

model <- lm(data=df, price ~ carat + y + x)
summary(model)

model <- lm(data=df, price ~ carat + factor(clarity))
summary(model)

model <- lm(data=df, price ~ carat + depth)
summary(model)
AIC(model, k=3)
mtable(model)

model_a <- lm(data=df, price ~ carat)
model_b <- lm(data=df, price ~ carat + depth)
model_c <- lm(data=df, price ~ carat + depth + factor(cut))
mtable(model_a, model_b, model_c)

model_x <- lm(data=df, price ~ carat + depth)
model_y <- lm(data=df, price ~ carat + depth + factor(cut))
waldtest(model_x, model_y)

resettest(model_y, power = 2:3, data=df) # test with H_0: there are no missing regressors

qplot(data = df, log(price), fill=clarity, geom = "density")
qplot(data = df, log(price), fill=clarity, geom = "density", alpha = 0.5) + facet_grid(~clarity)
qplot(data = df, log(price), fill=clarity, geom = "density", alpha = 0.5) + facet_wrap(~clarity)

qplot(data=df, log(carat), log(price), color = clarity) + facet_wrap(~cut)

help(diamonds)
help(f)