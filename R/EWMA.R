library('MTS')


la <- 0.96 #"decay" factor, try for differing values

#---------------------------
#READ PRICES DATA
dataraw <- read.table("appl.txt")
prices <- dataraw$V1 #data frame
prices <- rev(prices) #reverse order (OLDEST TO NEWEST)

#WEEKLY, YEARLY, MONTHLY PRICES?? mth-days (week=5, month=21, ...)
m <- 1 #mth day-interval
prices <- prices[seq(from=1, to=length(prices), by=m)]


#---------------------------
#CALCULATE RETURNS
returns <- diff(log(prices))
n <- length(returns)

#FIRST/SECOND MOMENTS
mu <- mean(returns)
sigma <- sd(returns)


#SQUARED RETURNS (volatility)
returns_squared <- returns ^2

#CALCULATE EMWA time series with the given decay factor la
ewma_ts <- EWMAvol(returns_squared, lambda = la)


#PLOTS
g <- ggplot() + geom_area(aes(x=1:n, y=returns_squared), alpha=0.8) + ylim(0, 0.015)
g <- g + geom_line(aes(x=1:n, y=ewma_ts$Sigma.t^0.5), colour='deeppink2', size=1.5)
g

g <- ggplot() + geom_area(aes(x=1:n, y=returns_squared), alpha=0.8) + ylim(0, 0.015) + xlim(500, 1000)
g <- g + geom_line(aes(x=1:n, y=ewma_ts$Sigma.t^0.5), colour='deeppink2', size=1.5)
g