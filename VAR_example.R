library(vars)
library(dplyr)
data("Canada")
?Canada
summary(Canada)
str(Canada)
glimpse(Canada)

plot(Canada, nc=1, xlab='') # nc - number of columns

# Augmented Dickey-Fuller test
adf1 <- summary(ur.df(Canada[, "prod"],
                     type = "trend", 
                     lags = 2))
adf1

adf2 <- summary(ur.df(diff(Canada[, "prod"]), type = "drift",
                      lags = 1))
adf2


# Optimal lag length for VAR
VARselect(Canada, lag.max = 8, type='both')

# Just for sound demostration
Canada <- Canada[, c("prod", "e", "U", "rw")]

p1ct <- VAR(Canada, p = 1, type = "both")
p1ct
summary(p1ct, equation='e')
plot(p1ct, names='e')
