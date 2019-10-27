library(readxl)
library(tidyverse)
library(knitr)
"
Student Number: 527144
"
column_number <- '44'

# Functions 
multiv_norm.mean <- function(y, X, b, B){
  mean <- solve(t(X)%*%X + solve(B)) %*% (t(X)%*%y + solve(B)%*%b)
  return(mean)
}

multiv_norm.variance <- function(X, B, sigma_squared){
  var <- sigma_squared * solve(t(X)%*%X + solve(B))
  return(var)
}

# Settings
set.seed(1997)
setwd('/Users/maxim_anisimov/Desktop/ERASMUS/Studying/1 module/Bayesian Econometrics/HA/')

# Data Preprocessing ####
returns <- as.data.frame(read_excel('data/returns.xls')) %>% 
           dplyr::select(year, month, column_number)
Jan <- read_excel('data/Jan.xls')
rf <- read_excel('data/rf.xls')
rm <- read_excel('data/rm.xls')

df <- as.data.frame(merge(merge(merge(returns, Jan), rf), rm))

"Model: rt =β0 +β1rtf +β2rtm +β3Jant +εt

Prior
β|σ^2 ∼ N(0, σ^2*I_4),
p(σ^2) ∝ σ^{−2}.
"

# Data 
ones_vector <- as.matrix(rep(1, nrow(df)))
y <- as.matrix(df[,column_number])
X <- cbind(ones_vector, as.matrix(df[,c(5,6,4)]))
colnames(X) <- c('const', '3M Yield Change', 'SP500 Return', 'Jan')
b <- as.matrix(c(0,0,0,0)) # mean beta prior
B <- diag(1, nrow=4) # prior proportional matrix
Time <- nrow(y)
k <- ncol(X)

# Gibbs sampler parameters
n_sim <- 10^5 # number of simulations (size of sample if final posterior sample)
n_burn <- 10^3 # burn-in size
k <- 10 # thin value
"Then the number of ALL simulations is n_sim*k + n_burn"
beta.initial <- as.matrix(c(0,0,0,0)) # initial beta values for the draws in the first simulation


##################
# SIMULATIONS ####
##################

# matrix to save draws
draw.matrix <- matrix(NA, nrow = n_burn+n_sim*k, ncol = 5)
colnames(draw.matrix) <- c('sigma_sqrd', 
                           'const', '3M Yield Change', 'SP500 Return', 'Jan')

# Prepare mean of the prior
beta_mean <- multiv_norm.mean(y, X, b, B)

# create n_sim*k + n_burn draws
for (n_draw in 1:(n_sim*k+n_burn)){
  print(paste0('Draw #', n_draw))
  if (n_draw == 1){
    #first draw -> use initial betas
    beta.prev <- beta.initial
  }else {
    # there were previous draws -> take them to simulate sigma^2
    beta.prev <- draw.matrix[n_draw-1, 2:5]
  }
  
  ### draw sigma_sqrd conditional on betas and y
  RSS.prev <- t(y - X%*%beta.prev)%*%(y - X%*%beta.prev)  # construct residual sum of squares
  mu.IG <- as.numeric(RSS.prev + t(b-beta.prev)%*%solve(B)%*%(b-beta.prev))
  chisq_rv <- rchisq(1, Time+k) # random chi-squared rv with T+k degrees of freedom
  sigma_sqrd.draw <- mu.IG/chisq_rv
  
  # draw betas conditional on sigma_squared_draw and beta
  beta_var <- multiv_norm.variance(X=X, B=B, sigma_squared=sigma_sqrd.draw)
  beta.draw <- MASS::mvrnorm(n=1, mu=beta_mean, Sigma=beta_var)
  
  current_draw <- cbind(sigma_sqrd.draw, t(as.matrix(beta.draw)))
  #print(current_draw)
  
  draw.matrix[n_draw,] <- current_draw
  
}

draw.matrix <- draw.matrix[n_burn:(n_burn+n_sim*k),] # discard burn-in sample
# Is correlation of draws present?
for (col_name in colnames(draw.matrix)){
  acf(draw.matrix[,col_name], main=col_name)
}
"Significant first AC in sigma squared"

# Thinning ####
thin_filter <- seq(1, nrow(draw.matrix), k)
draw.matrix <- draw.matrix[thin_filter,]
# check whether thinning helps
for (col_name in colnames(draw.matrix)){
  acf(draw.matrix[,col_name], main=col_name)
}
# Now, no significant spikes at all

# Traceplots ####
for (col_name in colnames(draw.matrix)){
  plot(draw.matrix[,col_name], type='l', main=col_name)
  par()
  }

# Posterior Results ####
colMeans(draw.matrix) # posterior mean
# posterior population variance with correction 
diag(var(draw.matrix)) * (nrow(draw.matrix)-1)/nrow(draw.matrix)

# Posterior Odd ####
sum(draw.matrix[,'3M Yield Change'] > 0) / 
  sum(draw.matrix[,'3M Yield Change'] < 0)

# Posterior Distribution of 3M Yield Change
hist(draw.matrix[,'3M Yield Change'], breaks=20, col="red")
