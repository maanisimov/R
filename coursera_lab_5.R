f <- function(x) {
  res<- x^2
  return(res)
}

f(3)
f(-1)

fpow <- function(x, power=2) {
  res<- x^power
  return(res)
}

fpow(2)
fpow(2, 3)


d <- cars
d[1, 2] <- NA
d[3, 1] <- NA
d

is.na(d)
sum(is.na(d)) # True + True = 2

res <- sum(is.na(d)/nrow(d)/ncol((d)))
res

na_perc <- function(d) {
  res <- sum(is.na(d)/nrow(d)/ncol((d)))
  return(res)
}

na_perc(d)

na_perc <- function(d) {
  if (!is.data.frame(d)) stop("d should be a data.frame")
  res <- sum(is.na(d)/nrow(d)/ncol((d)))
  return(res)
}

x <- c(2, 1)
na_perc(x) # does not work???

# cycles
for (i in 5:10) {
  k <- i^2
  cat("i=", i, "i^2=", k, "\n") # like print()
} # doesn not work???

all_data <- NULL # empty dataframe
for (fname in c("file01.csv", "file02.csv")) { # random files
  temp <- read.csv("fname")
  all_data <- rbind(all_data, temp)
}

-10/sqrt(3)
qt(0.975, 998)
round(10-1.96*sqrt(18.18), 2)

round(-10-1.96*sqrt(18), 2)
round(1.96*sqrt(18)-10, 2)

(-10-(-18))/sqrt(18)
(-10+1.68)/sqrt(18)












