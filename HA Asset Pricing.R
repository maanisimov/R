# Download libraries ####
library(tidyverse)
library(forecast)
library(tseries)
library(xts)
library(ggplot2)
library(readxl)
library(gdata)
library(Metrics)
library(corrplot)
library(matlib)
library(WriteXLS)
library(RColorBrewer)
library(reshape2)
theme_set(theme_light())

## Set parameters ####
setwd("/Users/maxim_anisimov/Desktop/ERASMUS/Studying/1 module/Asset Pricing/Assingment/data")

# Chapter 1 ####

# Exercise 1
file_path <- 'size_book_to_market.CSV'

# Download and preprocess data ####

# Portfolio data
raw_data <- read.csv(file_path, skip=15, stringsAsFactors = FALSE) # portfoio returns in % 
raw_data$X <- as.Date(as.yearmon(as.character(raw_data$X), "%Y%m"), frac=1)
raw_data <- raw_data %>% rename(time = X) %>% filter(time >= '1963-07-28' & 
                                                     time <= '2017-01-31')

raw_data[,2:ncol(raw_data)] <- sapply(raw_data[,2:ncol(raw_data)], as.numeric)
row.names(raw_data) <- raw_data$time
raw_data <- raw_data[,2:ncol(raw_data)]


# rename some columns
raw_data <- raw_data %>% rename(`ME1.BM1` = `SMALL.LoBM`,
                                `ME1.BM5` = `SMALL.HiBM`,
                                `ME5.BM1` = `BIG.LoBM`,
                                `ME5.BM5` = `BIG.HiBM`)

glimpse(raw_data)

sum(raw_data == -99.99 | raw_data == -999) # non NaNs

raw_data <- 1 + raw_data/100 # to gross returns in decimals

data <- raw_data # data to work with

# Market data
market_file_path <- 'Data_Assignment_AP19.xlsx'
raw_market <- as.data.frame(read_excel(market_file_path,
                            sheet='FamaFrench Factors', skip=4)) # returns in %
glimpse(raw_market)
raw_market$X__1 <- as.Date(as.yearmon(raw_market$X__1, "%Y%m"), frac=1)
raw_market <- raw_market %>% rename(time = X__1) %>% filter(time >= '1963-07-28' & 
                                                       time <= '2017-01-31')
row.names(raw_market) <- raw_market$time
raw_market <- raw_market[,2:ncol(raw_market)]
raw_market[,'mkt_return'] <- raw_market[,'Mkt-RF'] + raw_market[,'RF']

market <- as.data.frame(1 + raw_market$mkt_return/100) # to gross returns in decimals
colnames(market)[1] <- 'Market'
row.names(market) <- row.names(raw_market)

market_avgret <- mean(market$Market)
market_retstd <- sd(market$Market)

market_portfolio <- data.frame(gross_return=market_avgret, variance=market_retstd^2)

# 2) Data description ####
summary(data)
mean_returns <- (colMeans(data)-1)*100 # means in % to show more differences
variances <- sapply(data*100, sd, na.rm = TRUE)^2 # variancaes in %
 
mean_var <- data.frame(matrix(c(mean_returns, variances), ncol=2), 
                      row.names=colnames(data))
colnames(mean_var) <- c('mean, %', 'variance, %') 

#WriteXLS(mean_var, ExcelFileName = "mean_var.xls", row.names=T)

cor_matrix <- cor(cbind(data, market))
#WriteXLS(as.data.frame(cor_matrix), ExcelFileName = "cor_matrix.xls", row.names=T)

# Corr significance
# mat : is a matrix of data
# ... : further arguments to pass to the native R cor.test function
# cor.mtest <- function(mat, ...) {
#   mat <- as.matrix(mat)
#   n <- ncol(mat)
#   p.mat<- matrix(NA, n, n)
#   diag(p.mat) <- 0
#   for (i in 1:(n - 1)) {
#     for (j in (i + 1):n) {
#       tmp <- cor.test(mat[, i], mat[, j], ...)
#       p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
#     }
#   }
#   colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
#   p.mat
# }
# matrix of the p-value of the correlation
#p.mat <- cor.mtest(data)

colors <- colorRampPalette(c(rep("red", 10), rep('aquamarine', 5),
          'cyan', 'cornflowerblue', 'deepskyblue3', 'dodgerblue3', 'blue4'))

corr_plot <- corrplot(cor_matrix, method='color', type="full",
                      addCoef.col="white",
                      order='alphabet', tl.col="black", tl.srt=30,
                      number.cex=0.6, tl.cex = .5,
                      col = colors(20))
corr_plot
#ggsave('corr_plot_full.png', dpi=300, width=10, height=5)


# Efficient portfolios USING GROSS RETURNS IN DECIMALS ####
gross_returns <- (cbind(data, market$Market))
colnames(gross_returns)[ncol(gross_returns)] <- 'Market' 
mu <- as.matrix(colMeans(gross_returns)) #expected gross returns for each asset ((N+1)x1 vector)
Sigma <- cov(gross_returns) # covariance matrix of gross returns (NxN matrix)
Sigma_inverted <- solve(Sigma) # NxN matrix
# it's inverse matrix indeed
sum(near(Sigma%*%Sigma_inverted, diag(dim(Sigma)[1])) == FALSE)

ones_vector <- as.matrix(c(rep(1, dim(Sigma)[1]))) # ones vector (N+1)x1

# using A, B, C notations from L3
A <- (t(mu)%*%Sigma_inverted%*%mu)[1,1]
B <- (t(mu)%*%Sigma_inverted%*%ones_vector)[1,1]
C <- (t(ones_vector)%*%Sigma_inverted%*%ones_vector)[1,1]

# 3) GMV portfolio ####
pi_gmv <- (1/C)*Sigma_inverted%*%ones_vector # GMV portfolio weights
near(sum(pi_gmv), 1) == TRUE # check constraint
return_gmv <- B/C # expected GMV portfolio gross return
sigma2_gmv <- 1/C # return variance of GMV portfolio (in decimals!)

# 3) MU portfolio ####
pi_mu <- (1/B)*Sigma_inverted%*%mu # mu portfolio weights
near(sum(pi_mu), 1) == TRUE # check constraint
return_mu <- A/B # mu portfolio expected gross return
sigma2_mu <- A/(B^2) # return variance of mu portfolio (in decimals!)

gmv_mu_weights <- data.frame(round(matrix(c(pi_gmv, pi_mu), ncol = 2)*100,1),
                             row.names = colnames(gross_returns))
colnames(gmv_mu_weights) <- c('GMV', '\U003BC')
#WriteXLS(gmv_pi_weights, 'gmv_pi_weights.xls', row.names=TRUE) 
gmv_mu_weights$portfolio <- row.names(gmv_mu_weights)

gmv_mu_weights_reshaped <- melt(gmv_mu_weights, id.vars = 3)

gmv_mu_weights_plot <- ggplot(gmv_mu_weights_reshaped,
                              aes(x = portfolio, y = value, fill = variable)) + 
                              geom_bar(stat = "identity", position = "dodge") + 
                              geom_text(aes(label=value), vjust=1.6, 
                                        color="black", size=1.9,
                                        position = position_dodge(width = 1)) +
                              labs(x='Portfolio', y='Weight, %', fill='') + 
                              theme(axis.text.x = element_text(angle = 90, size=7), 
                                    legend.position = 'bottom')
gmv_mu_weights_plot

#ggsave("gmv_mu_weights.png", dpi=300, width=10, height=5)

# Portfolio frontier plot ####
# including mu return!
risky_assets_frontier <- data.frame(mu_targ_eff=sort(c(return_mu,
                                    seq(return_gmv, 1.07, by=0.001))))
risky_assets_frontier$stand_dev_eff <- sqrt((A-2*B*risky_assets_frontier$mu_targ_eff + 
                                       C*risky_assets_frontier$mu_targ_eff^2)/(A*C-B^2))

risky_assets_frontier$mu_targ_noneff <- seq(0.95, return_gmv, 
                                            length.out=nrow(risky_assets_frontier))
risky_assets_frontier$stand_dev_noneff <- sqrt((A-2*B*risky_assets_frontier$mu_targ_noneff + 
                                        C*risky_assets_frontier$mu_targ_noneff^2)/(A*C-B^2))

portfolios_ret_sd <- data.frame(gross_return=mu, sd=sqrt(diag(Sigma)))

mean_variance_plot1 <- ggplot() + 
                      geom_line(data=risky_assets_frontier, aes(x=stand_dev_eff, y=mu_targ_eff,
                                 color='efficient part'), size=1) +
                      geom_line(data=risky_assets_frontier, aes(x=stand_dev_noneff, y=mu_targ_noneff, 
                                 color='inefficient part'), size=1) + 
                      geom_point(data=risky_assets_frontier%>%filter(mu_targ_eff==return_gmv),
                                  aes(x=stand_dev_eff, y=mu_targ_eff, color="GMV portfolio"), 
                                 size=5, alpha=0.75) + 
                      geom_point(data=risky_assets_frontier%>%filter(mu_targ_eff==return_mu),
                                  aes(x=stand_dev_eff, y=mu_targ_eff, color="\U003BC portfolio"),
                                  size=1) + 
                      geom_point(data=portfolios_ret_sd, aes(x=sd, y=gross_return, 
                                                             color='ME/BM portfolio'), size=1) + 
                      geom_point(data=market_portfolio, aes(sqrt(variance), gross_return,
                                                            color='market portfolio'), size=4, alpha=0.75) +
                      scale_colour_manual(values = c("black", "green", 'grey', "red", "blue", 'orange'),
                      guide = guide_legend(override.aes = list(
                      linetype = c("blank", "blank", "blank", "solid", "solid", "blank"),
                      shape = c(16, 16, 16, NA, NA, 16), size=c(1,5,4,1,1,4)))) + 
                      theme(legend.position = 'bottom') + 
                      labs(x='Standard Deviation', y='Gross Return', color='')

# With risk-free rate
excess_returns <- gross_returns - (1+raw_market$RF/100)

excess_mu <- as.matrix(colMeans(excess_returns))

# 4) Tangency portfolio ####
pi_tang <- (1/(t(ones_vector)%*%Sigma_inverted%*%excess_mu)[1,1])*Sigma_inverted%*%excess_mu
near(sum(pi_tang),1) == TRUE
return_tang <- (mean(raw_market$RF/100) + t(pi_tang)%*%excess_mu)[1,1] # mean Rf + excess tang portf return 

excess_return_tang <- ((t(excess_mu)%*%Sigma_inverted%*%excess_mu)/(t(ones_vector)%*%Sigma_inverted%*%excess_mu))[1,1]
near(excess_return_tang + mean(raw_market$RF/100), return_tang) # check

std_tang <- (abs(excess_return_tang)/sqrt(t(excess_mu)%*%Sigma_inverted%*%excess_mu))[1,1]

# Plot tang structure
pi_tang_df <- data.frame(weight=round(pi_tang*100,1),
                         portfolio=colnames(gross_returns))

p <- ggplot(pi_tang_df, aes(portfolio, weight))
p + geom_bar(stat = "identity", fill='royalblue') +
  geom_text(aes(label=weight), vjust=1.6, color="black", size=2) + 
  labs(x='Portfolio', y='Weight, %') + theme(axis.text.x = element_text(angle = 90, size=7))
#ggsave("tang_weights.png", dpi=300, height=5, width=10)

rf_assets_frontier <- data.frame(stand_dev=c(0, std_tang, std_tang*10))
rf_assets_frontier$mu_targ_eff <- rf_assets_frontier$stand_dev*sqrt((t(excess_mu)%*%Sigma_inverted%*%excess_mu)[1,1])
rf_assets_frontier$mu_targ_noneff <- -rf_assets_frontier$stand_dev*sqrt((t(excess_mu)%*%Sigma_inverted%*%excess_mu)[1,1])

# 5) Altogether (add market portfolio) ####

# geom_text_repel(data=portfolios_ret_sd, 
#aes(x=sd, y=gross_return, label=as.character(row.names(portfolios_ret_sd))),
#hjust=0, vjust=0, size=1) +

mean_variance_plot2 <- ggplot() + 
                       geom_line(data=risky_assets_frontier, 
                                 aes(x=stand_dev_eff, y=mu_targ_eff, color='efficient part'), size=1) +
                       geom_line(data=risky_assets_frontier, aes(x=stand_dev_noneff, y=mu_targ_noneff,
                                                                  color='inefficient part'), size=1) + 
                       geom_point(data=risky_assets_frontier%>%filter(mu_targ_eff==return_gmv),
                                  aes(x=stand_dev_eff, y=mu_targ_eff, color="GMV portfolio"), size=6, alpha=0.75) + 
                       geom_point(data=risky_assets_frontier%>%filter(mu_targ_eff==return_mu),
                                  aes(x=stand_dev_eff, y=mu_targ_eff, color="\U003BC portfolio"), size=2) + 
                       geom_line(data=rf_assets_frontier,
                                  aes(x=stand_dev, y=1+mean(raw_market$RF/100) + mu_targ_eff, color='efficient part'),
                                  size=1) +
                       geom_line(data=rf_assets_frontier, 
                       aes(x=stand_dev, y=1+mean(raw_market$RF/100) + mu_targ_noneff, color='inefficient part'),
                       size=1) +
                       geom_point(data=portfolios_ret_sd, aes(x=sd, y=gross_return, color='ME/BM portfolio'), 
                       size=2) + 
                       geom_point(data=rf_assets_frontier%>%filter(stand_dev==std_tang),
                                  aes(stand_dev, 1+mean(raw_market$RF/100)+mu_targ_eff, 
                                  color='tangency portfolio'), size=5) + 
                       geom_point(data=market_portfolio,
                                  aes(sqrt(variance), gross_return, color='market portfolio'), size=5, alpha=0.75) + 
                       coord_cartesian(xlim = c(0,0.1),ylim=c(0.975,1.05)) + theme(legend.position = 'bottom') +
                       scale_colour_manual(values = c("black", "green", "grey", "red", "blue", "orange", "purple"),
                                            guide = guide_legend(override.aes = list(
                                              linetype = c("blank", "blank", "blank", "solid", "solid", rep("blank", 2)),
                                              shape = c(16,16,16, NA, NA, rep(16, 2)), size=c(2,6,2,1,1,5,5)))) +
                       labs(x='Standard Deviation', y='Gross Return', color='') + 
                       scale_y_continuous(breaks=c(0.98 ,1, 1.02, 1.04)) + 
                       scale_x_continuous(breaks=c(0, 0.025, 0.05, 0.075, 0.1))
mean_variance_plot2
#ggsave("mean_variance_full.png", dpi=300, width=10, height=5)

# 6) CAPM regressions ####
capm_data <- 100*excess_returns # sorted portfolios + market portfolio in %!

# ME1
me1bm1 <- lm(data=capm_data, `ME1.BM1` ~ Market)
summary(me1bm1)
me1bm2 <- lm(data=capm_data, `ME1.BM2` ~ Market)
summary(me1bm2)
me1bm3 <- lm(data=capm_data, `ME1.BM3` ~ Market)
summary(me1bm3)
me1bm4 <- lm(data=capm_data, `ME1.BM4` ~ Market)
summary(me1bm4)
me1bm5 <- lm(data=capm_data, `ME1.BM5` ~ Market)
summary(me1bm5)

#ME2
me2bm1 <- lm(data=capm_data, `ME2.BM1` ~ Market)
summary(me2bm1)
me2bm2 <- lm(data=capm_data, `ME2.BM2` ~ Market)
summary(me2bm2)
me2bm3 <- lm(data=capm_data, `ME2.BM3` ~ Market)
summary(me2bm3)
me2bm4 <- lm(data=capm_data, `ME2.BM4` ~ Market)
summary(me2bm4)
me2bm5 <- lm(data=capm_data, `ME2.BM5` ~ Market)
summary(me2bm5)


#ME3
me3bm1 <- lm(data=capm_data, `ME3.BM1` ~ Market)
summary(me3bm1)
me3bm2 <- lm(data=capm_data, `ME3.BM2` ~ Market)
summary(me3bm2)
me3bm3 <- lm(data=capm_data, `ME3.BM3` ~ Market)
summary(me3bm3)
me3bm4 <- lm(data=capm_data, `ME3.BM4` ~ Market)
summary(me3bm4)
me3bm5 <- lm(data=capm_data, `ME3.BM5` ~ Market)
summary(me3bm5)

#ME4
me4bm1 <- lm(data=capm_data, `ME4.BM1` ~ Market)
summary(me4bm1)
me4bm2 <- lm(data=capm_data, `ME4.BM2` ~ Market)
summary(me4bm2)
me4bm3 <- lm(data=capm_data, `ME4.BM3` ~ Market)
summary(me4bm3)
me4bm4 <- lm(data=capm_data, `ME4.BM4` ~ Market)
summary(me4bm4)
me4bm5 <- lm(data=capm_data, `ME4.BM5` ~ Market)
summary(me4bm5)

#ME5
me5bm1 <- lm(data=capm_data, `ME5.BM1` ~ Market)
summary(me5bm1)
me5bm2 <- lm(data=capm_data, `ME5.BM2` ~ Market)
summary(me5bm2)
me5bm3 <- lm(data=capm_data, `ME5.BM3` ~ Market)
summary(me5bm3)
me5bm4 <- lm(data=capm_data, `ME5.BM4` ~ Market)
summary(me5bm4)
me5bm5 <- lm(data=capm_data, `ME5.BM5` ~ Market)
summary(me5bm5)

# 7) Conduct the GRS-test and interpret its outcome in relation to the CAPM ####
library(GRS.test)

nrow(capm_data) > 150 # if yes, asymptotic GRS can be applied

E_matrix <- matrix(c(residuals(me1bm1), residuals(me1bm2), residuals(me1bm3), residuals(me1bm4), residuals(me1bm5),
                     residuals(me2bm1), residuals(me2bm2), residuals(me2bm3), residuals(me2bm4), residuals(me2bm5),
                     residuals(me3bm1), residuals(me3bm2), residuals(me3bm3), residuals(me3bm4), residuals(me3bm5),
                     residuals(me4bm1), residuals(me4bm2), residuals(me4bm3), residuals(me4bm4), residuals(me4bm5),
                     residuals(me5bm1), residuals(me5bm2), residuals(me5bm3), residuals(me5bm4), residuals(me5bm5)), 
                     ncol=ncol(data))

alpha_hat <- matrix(c(coef(me1bm1)[1], coef(me1bm2)[1], coef(me1bm3)[1], coef(me1bm4)[1], coef(me1bm5)[1],
                      coef(me2bm1)[1], coef(me2bm2)[1], coef(me2bm3)[1], coef(me2bm4)[1], coef(me2bm5)[1],
                      coef(me3bm1)[1], coef(me3bm2)[1], coef(me3bm3)[1], coef(me3bm4)[1], coef(me3bm5)[1],
                      coef(me4bm1)[1], coef(me4bm2)[1], coef(me4bm3)[1], coef(me4bm4)[1], coef(me4bm5)[1],
                      coef(me5bm1)[1], coef(me5bm2)[1], coef(me5bm3)[1], coef(me5bm4)[1], coef(me5bm5)[1]))

beta_hat <- matrix(c(coef(me1bm1)[2], coef(me1bm2)[2], coef(me1bm3)[2], coef(me1bm4)[2], coef(me1bm5)[2],
                     coef(me2bm1)[2], coef(me2bm2)[2], coef(me2bm3)[2], coef(me2bm4)[2], coef(me2bm5)[2], 
                     coef(me3bm1)[2], coef(me3bm2)[2], coef(me3bm3)[2], coef(me3bm4)[2], coef(me3bm5)[2],
                     coef(me4bm1)[2], coef(me4bm2)[2], coef(me4bm3)[2], coef(me4bm4)[2], coef(me4bm5)[2],
                     coef(me5bm1)[2], coef(me5bm2)[2], coef(me5bm3)[2], coef(me5bm4)[2], coef(me5bm5)[2]))

coeffs <- data.frame(t(matrix(c(alpha_hat, beta_hat), ncol=2)), row.names=c('alpha', 'beta'))
colnames(coeffs) <- colnames(data)

avg_forecast <- alpha_hat + beta_hat*mean(capm_data$Market)
mispricing <- avg_forecast - mean_returns/100

#WriteXLS(round(coeffs, 3), 'CAPM_coefs.xls', row.names=T)

Sigma_tilda <- (t(E_matrix)%*%E_matrix)/nrow(E_matrix)
dim(Sigma_tilda) == c(ncol(data), ncol(data))

FiniteSampleGRS <- function(market_mean, market_std, T_period, alpha_hat, Sigma_tilda){
  market_sharpe <- market_mean/market_std
  n <- dim(Sigma_tilda)[1]
  z = (T_period-n-1)/n * (1 + market_sharpe^2)^(-1) * (t(as.matrix(alpha_hat))%*%solve(Sigma_tilda)%*%as.matrix(alpha_hat))[1,1]
  return(z)
}

AsymptoticGRS <- function(market_mean, market_std, T_period, alpha_hat, Sigma_tilda){
  market_sharpe <- market_mean/market_std
  z = T_period/(1 + market_sharpe^2) * (t(as.matrix(alpha_hat))%*%solve(Sigma_tilda)%*%as.matrix(alpha_hat))[1,1]
  return(z)
}

z_finsamp <- FiniteSampleGRS(market_mean=market_avgret-1, market_std=market_retstd, T_period=nrow(data),
                   alpha_hat=alpha_hat, Sigma_tilda=Sigma_tilda)

z_as <- AsymptoticGRS(market_mean=market_avgret-1, market_std=market_retstd, T_period=nrow(data),
              alpha_hat=alpha_hat, Sigma_tilda=Sigma_tilda)

1-pf(z_finsamp, df1=25, df2=nrow(data)-25-1) # p-value
1-pchisq(z_as, df=25) # p-value

sharpe_market <- (market_avgret-1)/market_retstd
sharpe_improved <- sqrt(t(as.matrix(alpha_hat))%*%solve(Sigma_tilda)%*%as.matrix(alpha_hat) + 
                          sharpe_market^2)
sharpe_improved - sharpe_market


# PART 2 ####
df$Adjusted_price <- df$Price * cumprod(df$Adjustment_factor)
