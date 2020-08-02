################################################################################
# Volatility Modeling BTP 10Y

# (c) Thomas Ludwig, August 2020
################################################################################

library(ggplot2)
library(gridExtra)
library(MTS)
library(rugarch)
library(zoo)

################################################################################

# Information and to do's
# https://vlab.stern.nyu.edu/analysis/VOL.EWI%3AUS-R.GARCH

# create time filter to change output period
################################################################################

# data import and preparation

# read data into R
data <- read.csv("C:/Users/User/Desktop/Fixed-Income/Input/BTP_10Y.csv")

# convert to date
data$Date <- as.Date(data$Date, "%Y-%m-%d")

# calculate price of zero bond
data$price_bond <- 100*(1/(1+data$close/100)^10)


# calculate returns for price and yield data
ret_yield <- diff(log(data$close))
ret_price <- diff(log(data$price_bond))


# proxy for daily volatility
ret_yield_abs <- abs(ret_yield)
ret_yield_sqared <- ret_yield^2
ret_price_abs <- abs(ret_price)
ret_price_squared <- ret_price^2

# extract date vector for returns
date_ret <- data$Date[-1]

# some plots of input data
g <- ggplot() + 
  ggtitle("absolute Returns BTP 10Y Yield") + xlab("Time") + ylab("absolute Returns") +
  geom_line(aes(x=date_ret, y=ret_yield_abs), colour='orangered', size=1) +
  theme_minimal()
g

g <- ggplot(data = data) +
  ggtitle("BTP 10Y Yield") + xlab("Time") + ylab("10Y Yield") +
  geom_line(aes(x=Date, y=close), colour='orangered', size=1) +
  theme_minimal()
g

g <- ggplot(data = data) +
  ggtitle("BTP 10Y Price") + xlab("Time") + ylab("10Y Bond Price") +
  geom_line(aes(x=Date, y=price_bond), colour='orangered', size=1) +
  theme_minimal()
g


################################################################################

# fit GARCH model for yield volatility

spec <- ugarchspec(variance.model = list(model = 'sGARCH', 
                                         garchOrder = c(1, 1)), 
                   distribution = 'std')

fit_yield_vola <- ugarchfit(spec, 
                            ret_yield, 
                            solver = "hybrid")

# plot fitted conditional volatility
g <- ggplot() +
  ggtitle("Yield Volatility GARCH (1,1) Model BTP 10Y") + xlab("Time") + ylab("Daily Volatility") +
  geom_line(aes(x=date_ret, y=sigma(fit_yield_vola)), colour='orangered', size=1) +
  theme_minimal()
g

# plot scaled volatility (scaled to annual by using 250^0.5 as scaling factor)
g1 <- ggplot() +
  ggtitle("Yield Volatility GARCH (1,1) Model BTP 10Y") + xlab("Time") + ylab("Daily Volatility") +
  geom_line(aes(x=date_ret, y=sigma(fit_yield_vola)*250^0.5), colour='orangered', size=1) +
  theme_minimal()
g1

# plot daily log returns of yield data
g2 <- ggplot() +
  ggtitle("LOG Returns Yield BTP 10Y") + xlab("Time") + ylab("Log Returns") +
  geom_line(aes(x=date_ret, y=ret_yield), colour='orangered', size=0.5) +
  theme_minimal()
g2

# plot absolute returns vs. conditional GARCH volatility
g3 <- ggplot() +
  ggtitle("absolute Returns vs. conditional volatility") + xlab("Time") + ylab("Log Returns") +
  geom_line(aes(x=date_ret, y=sigma(fit_yield_vola)), colour='orangered', size=1, alpha=1) +
  geom_line(aes(x=date_ret, y=ret_yield_abs), colour='steelblue', size=0.5, alpha=0.4) +
  theme_minimal()
g3

# arrange plots
# grid.arrange(g, arrangeGrob(g1, g2, ncol=2), nrow = 2)
grid.arrange(g, g2, nrow = 2)

################################################################################

# standard GARCH model yield VOLATILITY

fit_price_vola <- ugarchfit(spec, 
                            ret_price, 
                            solver = "hybrid")

# plot fitted conditional volatility
g <- ggplot() +
  ggtitle("Price Volatility GARCH (1,1) Model BTP 10Y") + xlab("Time") + ylab("Daily Volatility") +
  geom_line(aes(x=date_ret, y=sigma(fit_price_vola)), colour='orangered', size=1) +
  theme_minimal()
g

# plot scaled volatility (scaled to annual by using 250^0.5 as scaling factor)
g1 <- ggplot() +
  ggtitle("Price Volatility GARCH (1,1) Model BTP 10Y") + xlab("Time") + ylab("Daily Volatility") +
  geom_line(aes(x=date_ret, y=sigma(fit_price_vola)*250^0.5), colour='orangered', size=1) +
  theme_minimal()
g1

# plot daily log returns of yield data
g2 <- ggplot() +
  ggtitle("LOG Returns Price BTP 10Y") + xlab("Time") + ylab("Log Returns") +
  geom_line(aes(x=date_ret, y=ret_price), colour='orangered', size=0.5) +
  theme_minimal()
g2

# plot absolute returns vs. conditional GARCH volatility
g3 <- ggplot() +
  ggtitle("absolute Returns vs. conditional volatility") + xlab("Time") + ylab("Log Returns") +
  geom_line(aes(x=date_ret, y=sigma(fit_price_vola)), colour='orangered', size=1, alpha=1) +
  geom_line(aes(x=date_ret, y=ret_price_abs), colour='steelblue', size=0.5, alpha=0.4) +
  theme_minimal()
g3

# arrange plots
# grid.arrange(g, arrangeGrob(g1, g2, ncol=2), nrow = 2)
grid.arrange(g, g2, nrow = 2)

################################################################################
################################################################################