library('rugarch')
#library('MTS')
#library('zoo')

#---------------------------
#READ PRICES DATA
dataraw <- read.table("thyssen.txt")
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

spec <- ugarchspec(variance.model = list(model = 'sGARCH', garchOrder = c(1, 1)), distribution = 'std')
g_ts <- ugarchfit(spec, returns_squared, solver = "hybrid")

#PLOTS
g <- ggplot() + geom_area(aes(x=1:n, y=returns_squared), alpha=0.8) + ylim(0, 0.015)
g <- g + geom_line(aes(x=1:n, y=g_ts@fit$fitted.values), colour='olivedrab3', size=1.5)
g

g <- ggplot() + geom_area(aes(x=1:n, y=returns_squared), alpha=0.8) + ylim(0, 0.015) + xlim(500, 1000)
g <- g + geom_line(aes(x=1:n, y=g_ts@fit$fitted.values), colour='olivedrab3', size=1.5)
g

