library('HSAUR')
library('dplyr')
library('psych')
library('lmtest')
library('glmnet')
library('ggplot2')
library('car')

log(2.7)
2*(31*log(31/48.98) + 127*log(127/109.02) + 62*log(62/44.02) + 80*log(80/97.98))

# MK
h <- cars
qplot(data=h, speed, dist)
model <- lm(data=h, dist ~ speed)
summary(model)

h <- mutate(h, speed2 = speed^2, speed3 = speed^3)
model_mk <- lm(data=h, dist ~ speed + speed2 + speed3) # multicollinearity
summary(model_mk) # coefs are bad, but all model is OK

vif(model_mk) # variance inflation factor

x0 <- model.matrix(data=h, dist ~ 0 + speed + speed2 + speed3)
head(x0)
cor(x0)

nd <- data.frame(speed=10, speed2=100, speed3=1000)

predict(model, newdata = nd, interval = 'prediction')
predict(model_mk, newdata = nd, interval = 'prediction') 

confint(model)
confint(model_mk) # intervals are extremely larger


# LASSO and Ridge regressions

y <- h$dist 
x0 <- model.matrix(data=h, dist ~ 0 + speed + speed2 + speed3)

# LASSO
lambdas <- seq(50, 0.1, length=30) # for glmnet from largest to smallest
m_lasso <- glmnet(x0, y, alpha=1, lambda=lambdas)
par(mar = rep(2, 4)) # to avoid an error
plot(m_lasso, xvar = 'lambda', label=T)
plot(m_lasso, xvar = 'dev', label=T) # deviation explained
plot(m_lasso, xvar = 'norm', label=T) # vector's norm???

coef(m_lasso, s=c(0.1, 1))

# Ridge

m_rr <- glmnet(x0, y, alpha=0, lambda=lambdas)
# other actions are the same

# How to choose lambda? Cross validation!

cv <- cv.glmnet(x0, y, alpha=1)
plot(cv)

cv$lambda.min
cv$lambda.1se

coef(cv, s='lambda.1se')

# PCA
h <- heptathlon
help("heptathlon")
glimpse(h)
h <- select(h, -score) # delete score
describe(h)

cor(h)

h.pca <- prcomp(h, scale=T) # scale - standartization
pca1 <- h.pca$x[,1]
v1 <- h.pca$rotation[,1]
v1
head(pca1)
summary(h.pca)

cor(heptathlon$score, pca1)
plot(h.pca)

biplot(h.pca, xlim=c(-1, 1))

# quiz
df <- airquality
glimpse(df)
qplot(data=df, Ozone, Solar.R)

model_1 <- lm(data=df, Ozone ~ Solar.R + Wind + Temp)
round(vif(model_1), digits = 3)

df <- na.omit(df)
Y <- df$Ozone
X0 <- model.matrix(data=df, Ozone ~ 0 + Solar.R + Wind + Temp)
lambdas <- seq(50, 0.1, length = 30)
model_LASSO <- glmnet(X0, Y, alpha = 1, lambda = lambdas)
round(coef(model_LASSO, s=1), digits = 3)

model_Ridge <- glmnet(X0, Y, alpha = 0, lambda = lambdas)
round(coef(model_Ridge, s=2), digits = 3)

plot(model_LASSO, xvar = 'lambda')

p <- prcomp(X0, scale=T)
qplot(p$x[,1], p$x[,2])
biplot(p, xlim=c(-1, 1))

