library(tidyverse)
library(plm)

### TASK 1
?Cigar
data("Cigar")
Cigar
cigar80 <- filter(Cigar, year == 80, sales <= 200, sales >= 100)

ggplot(cigar80, aes(ndi, sales)) + geom_point()

cigar_80 <- select(cigar80, sales, starts_with("pop"))
cigar80$youth_ratio <- 1 - cigar80$pop16 / (cigar80$pop) 
ggplot(cigar80, aes(youth_ratio, sales)) + geom_point()

cigar_4 <- select(Cigar, price, cpi, sales)
cigar_4$real_pr <- cigar_4$price/cigar_4$cpi
ggplot(cigar_4, aes(real_pr, sales)) + geom_point()

cigar_5 <- arrange(Cigar, desc(sales))
cigar_5
cigar_5 <- filter(cigar_5, state !=30, state!= 9, state!=18)
select(cigar_5, price, cpi, sales)
cigar_5$real_pr <- cigar_5$price/cigar_5$cpi
ggplot(cigar_5, aes(real_pr, sales)) + geom_point()

cigar_6 <- select(Cigar, price, cpi, sales, year)
cigar_6 <- filter(cigar_6, year %% 5 == 0)
cigar_6$real_pr <- cigar_6$price/cigar_6$cpi
ggplot(cigar_6) +
  geom_point(aes(x = real_pr, y = sales)) + facet_wrap(~ year)

cigar_7 <- filter(Cigar, state == 1 | state == 7 | state == 3)
ggplot(cigar_7) +
  geom_line(aes(x = year, y = sales)) + 
  facet_wrap(~ state)

cigar_8 <- Cigar %>% group_by(year)
cigar_8

cigar_means <- aggregate(Cigar, list(Cigar$year), mean)
ggplot(cigar_means) + geom_line((aes(year, sales)))

cigar_9 <- Cigar %>% group_by(year)
cigar_9_sum <- summarise(cigar_9, mean_sales = mean(sales),
                     max_sales = max(sales), min_sales = min(sales))
cigar_9_sum

ggplot(cigar_9_sum) + geom_line(aes(year, mean_sales), color='blue') + 
  geom_line(aes(year, max_sales), color='red') +
  geom_line(aes(year, min_sales), color='green')


##### TASK 2
library(forecast)
library(tseries)

fedfunds <- read.csv('/Users/maxim_anisimov/Desktop/fed_funds.csv', head=T)
head(fedfunds)            
class(fedfunds)
str(fedfunds)

ffr <- fedfunds %>% arrange(Date) %>% select(-Date) %>% 
  ts(start=c(1960, 01), frequency = 12)

class(ffr)
ffr[1:36]
?window
ffr
ffr2 <-window(ffr, end=c(2008, 12))

autoplot(ffr2) + ggtitle('Federal Funds rate') + xlab('') + ylab('')

dffr <- diff(ffr2, 1)
ggPacf(dffr)
ggAcf(dffr)

Box.test(dffr, lag=10, type='Lj')

A <- matrix(rep(0, 25), nrow=5, ncol=5)

for (i in 0:4) {
  for (j in 0:4){
    model <- arma(dffr, order=c(i, j))
    A[i+1, j+1]<- model$aic
  }
}

