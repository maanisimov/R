# Conjugate prior ####
# 

library(mvtnorm)

dt_ls <- function(x, df, mu, a) 1/a * dt((x - mu)/a, df)
pt_ls <- function(x, df, mu, a) pt((x - mu)/a, df)
qt_ls <- function(prob, df, mu, a) qt(prob, df)*a + mu
rt_ls <- function(n, df, mu, a) rt(n,df)*a + mu

"b: prior mean of coeffs
  Var matrix of coeffs: \sigma^2*B"

ConjugatePrior <- function(y, X, b, B){
  
  y <- as.matrix(y)
  X <- as.matrix(X)
  b <- as.matrix(b)
  B <- as.matrix(B)
  
  B_sqrt <- t(chol(B))
  B_inverse_sqrt <- solve(B_sqrt)
  
  w <- rbind(y, B_inverse_sqrt%*%b)
  V <- rbind(X, B_inverse_sqrt)
  
  beta_loc <- solve(t(V)%*%V) %*% t(V)%*%w # location parameter
  sigma_sqrt <- (t(w-V%*%beta_loc) %*% (w-V%*%beta_loc))[1,1]
  beta_scale <- sigma_sqrt * solve(t(V)%*%V) # scale parameter
  DF <- dim(V)[2] # degrees of freedom
  
  CP_res <- list(beta_loc, beta_scale, DF, w, V)
  names(CP_res) <- c('location_param', 'scale_param', 'DF', 'w', 'V')
  
  return(CP_res)
  
}

X <- matrix(rnorm(n=1000),nrow=200, ncol=5)
y <- matrix(rnorm(n=200),nrow=200)

colMeans(X)

B.prior <- matrix(c(3,0.5,0.5,0.5,0.5, 0.5,1,0.5,0.5,0.5, 0.5,0.5,5,0.5,0.5, 
                    0.5,0.5,0.5,2,0.5, 0.5,0.5,0.5,0.5,1), ncol=5)
CP_res <- ConjugatePrior(y=y,X=X, b=c(0,0.5,0.1,0.7,-0.5),
               B=B.prior)

CP_res$location_param
X_new <- CP_res$V[nrow(X):nrow(V),]
y_new <- CP_res$w[nrow(y):nrow(w),]

sim <- mvtnorm::rmvt(n=100000, delta=CP_res[['location_param']], 
                     sigma=CP_res[["scale_param"]], df=CP_res[["DF"]])

# Posterior Distribution
hist(sim[,1], breaks=1000, col="red")
