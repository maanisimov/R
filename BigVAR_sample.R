library(devtools)
if (!require(Quandl)) {
  install_github("quandl/R-package") }
# to install BigVAR from Github
if (!require(BigVAR)) { install_github("wbnicholson/BigVAR/BigVAR")
}
suppressPackageStartupMessages(library(Quandl))
# Gross Domestic Product (Relative to 2000)
GDP <- Quandl("FRED/GDP", type = "xts")
GDP <- GDP/mean(GDP["2000"]) * 100
# Transformation Code: First Difference of Logged Variables
GDP <- diff(log(GDP))
# Federal Funds Rate
FFR <- Quandl("FRED/FEDFUNDS", type = "xts", collapse = "quarterly")
# Transformation Code: First Difference
FFR <- diff(FFR)
# CPI ALL URBAN CONSUMERS, relative to 1983
CPI <- Quandl("FRED/CPIAUCSL", type = "xts", collapse = "quarterly")
CPI <- CPI/mean(CPI["1983"]) * 100
# Transformation code: Second difference of logged variables
CPI <- diff(log(CPI), 2)
glimpse(CPI)

k=3
Y <- cbind(CPI, FFR, GDP)
Y <- na.omit(Y)
# Demean
Y <- Y - (c(rep(1, nrow(Y)))) %*% t(c(apply(Y, 2, mean))) # Standarize Variance
for (i in 1:k) {
  Y[, i] <- Y[, i]/apply(Y, 2, sd)[i] }
# Plot
plot(as.zoo(Y), main = "", xlab = "", ylab = c("GDP", "FFR", "CPI"))
typeof(Y)
head(Y)

Model1 = constructModel(as.matrix(Y), p = 4, struct = "OwnOther", gran = c(25, 10),
                        verbose = FALSE, VARX = list())
Model1Results = cv.BigVAR(Model1)
Model1Results

# Coefficient matrix at end of evaluation period
Model1Results@betaPred
# Residuals at end of evaluation period 
Model1Results@resids
# Lagged Values at end of evaluation period
Model1Results@Zvals

plot(Model1Results) # visualization of the position of the best lambda

# Since Figure 4 shows that all series tend to exhibit a substantial degree of persistence, 
# the Minnesota VAR might be a more appropriate choice than shrinking toward zero.
Model1@Minnesota = TRUE
mean(cv.BigVAR(Model1)@OOSMSFE)

SparsityPlot.BigVAR.results(Model1Results)

predict(Model1Results,1)

# Estimation with fixed λ
lambda <- Model1Results@OptimalLambda
Model1@ownlambdas = TRUE
Model1@Granularity <- lambda
BigVAR.est(Model1)

# VARX-L Estimation
# For example, if we want to forecast GDP and the Federal Funds Rate using CPI
# as an exogenous series (with s = 4), we simply need to specify:
VARX = list()
VARX$k = 2 # 2 endogenous series
VARX$s = 4 # maximum lag order of 4 for exogenous series
Model1 = constructModel(as.matrix(Y), p = 4, struct = "Basic",
                        MN = F, gran = c(25, 10), verbose = F, VARX = VARX)
Model1Results = cv.BigVAR(Model1)

# Univariate (одномерная) Estimation
# Univariate estimation (i.e. k = 1) is possible, however,
# many of the structures break down (e.g. Own/Other has no context in this setting). 
# Hence, the only structures that offer univariate support are
# Basic VARX-L, Lag Group VARX-L, and Componentwise HVAR.

