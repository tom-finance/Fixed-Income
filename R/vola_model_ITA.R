################################################################################
# BTP 10Y Analyses
################################################################################

library(ggplot2)
library(gridExtra)
library(MTS)
library(rugarch)
library(zoo)
library(quantmod)

################################################################################

# Topics Covered

# Fixed Income (Price and Yield Volatility)
# Equity Markets (ITA, GER, EUROPE)
# ECB Data

# OPEN POINTS
# annualize volatility?
# implement VaR model?

# check this out:
# https://vlab.stern.nyu.edu/analysis/VOL.EWI%3AUS-R.GARCH

# volatility models
# EWMA, GARCH (1,1) --> add more sophisticated models!

# add VaR Models with time changing volatility GARCH models

################################################################################

###################
# FIXED INCOME
###################

# data preperation

data <- read.csv("BTP_10Y.csv")
data$Date <- as.Date(data$Date, "%Y-%m-%d")


# calculate price of zero bond, returns for price and yield volatility

data$price_bond <- 100*(1/(1+data$close/100)^10)

data$price_vola <- c(NA, diff(log(data$price_bond)))
data$yield_vola <- c(NA, diff(log(data$close)))

# price volatility
returns_absolute <- abs(data$price_vola)[-1]
returns <- data$price_vola[-1]
returns_squared <- (data$price_vola^2)[-1]


# yield volatility

returns_squared_yield <- data$yield_vola^2




################################################################################

# EWMA Model Fixed Income

# la <- 0.96 #"decay" factor, try for differing values
# 
# #CALCULATE EMWA time series with the given decay factor la
# ewma_ts <- EWMAvol(returns_squared, lambda = la)
# 
# #PLOTS
# g <- ggplot()
# g <- g + geom_line(aes(x=1:n, y=ewma_ts$Sigma.t^0.5), colour='deeppink2', size=1)
# g <- g + theme_minimal()
# g

################################################################################

# standard GARCH model Price VOLATILITY

spec <- ugarchspec(variance.model = list(model = 'sGARCH', garchOrder = c(1, 1)), distribution = 'std')
g_ts <- ugarchfit(spec, returns_squared, solver = "hybrid")

#PLOTS
g <- ggplot()
g <- g + ggtitle("Price Volatility GARCH (1,1) Model BTP 10Y") + xlab("Time") + ylab("Daily Volatility")
g <- g + geom_line(aes(x=data$Date[-1], y=g_ts@fit$fitted.values^0.5), colour='orangered', size=1)
#g <- g + scale_y_log10() # could be useful to show large differences better!
g <- g + theme_minimal()
g

g1 <- ggplot()
g1 <- g1 + ggtitle("LOG Returns BTP 10Y") + xlab("Time") + ylab("Log Returns")
g1 <- g1 + geom_line(aes(x=data$Date[-1], y=data$price_vola[-1]), colour='orangered', size=0.5)
g1 <- g1 + theme_minimal()
g1


# plot price of 10Y zerobond
g2 <- ggplot()
g2 <- g2 + ggtitle("Price 10Y BTP Zerobond") + xlab("Time") + ylab("Bondprice")
g2 <- g2 + geom_line(aes(x=data$Date[-1], y=data$price_bond[-1]), colour='orangered', size=0.5)
g2 <- g2 + theme_minimal()
g2


# plot 10Y Yield over time
g3 <- ggplot()
g3 <- g3 + ggtitle("BTP 10Y Yield") + xlab("Time") + ylab("Yield (%)")
g3 <- g3 + geom_line(aes(x=data$Date, y=data$close), colour='orangered', size=1)
g3 <- g3 + theme_minimal()
g3


# arrange plots

# grid.arrange(g, g1, g2, nrow = 2)
grid.arrange(g, arrangeGrob(g1, g2, ncol=2), nrow = 2)

################################################################################

# standard GARCH model yield VOLATILITY

spec <- ugarchspec(variance.model = list(model = 'sGARCH', garchOrder = c(1, 1)), distribution = 'std')
g_ts_yield <- ugarchfit(spec, returns_squared_yield[-1], solver = "hybrid")

#PLOTS
g <- ggplot()
g <- g + ggtitle("Yield Volatility GARCH (1,1) Model BTP 10Y") + xlab("Time") + ylab("Daily Volatility")
g <- g + geom_line(aes(x=data$Date[-1], y=g_ts_yield@fit$fitted.values^0.5), colour='orangered', size=1)
#g <- g + scale_y_log10() # could be useful to show large differences better!
g <- g + theme_minimal()
g

g1 <- ggplot()
g1 <- g1 + ggtitle("LOG Returns BTP 10Y Yield") + xlab("Time") + ylab("Log Returns")
g1 <- g1 + geom_line(aes(x=data$Date[-1], y=data$yield_vola[-1]), colour='orangered', size=0.5)
g1 <- g1 + theme_minimal()
g1




# arrange plots

# grid.arrange(g, g1, g2, nrow = 2)
grid.arrange(g, arrangeGrob(g1, g2, ncol=2), nrow = 2)


################################################################################

#####################
# EQUITY MARKETS
#####################


# 
# data <-  getSymbols("EWI", src="yahoo")
# 
# data <- getSymbols("EWI")
# 
# plot(EWI$EWI.Close)
# 
# # DAX
# getSymbols("^GDAXI", src="yahoo")
# 
# # delete missing values!
# 
# # ITA EQUITY
getSymbols("EWI", src="yahoo")
# 
# # many missing values!! find something better?
# getSymbols("FTSEMIB.MI", src="yahoo")

ret_equ_ita <- diff(log(EWI$EWI.Close))
returns_squared_equ_ita <- ret_equ_ita^2


# fit GARCH model

spec <- ugarchspec(variance.model = list(model = 'sGARCH', garchOrder = c(1, 1)), distribution = 'std')
g_ts_equity <- ugarchfit(spec, returns_squared_equ_ita[-1], solver = "hybrid")


# plots of results
g <- ggplot()
g <- g + ggtitle("Volatility Italien Equity GARCH (1,1) Model") + xlab("Time") + ylab("Daily Volatility")
g <- g + geom_line(aes(x=1:length(returns_squared_equ_ita[-1]), y=g_ts_equity@fit$fitted.values^0.5), colour='orangered', size=1)
g <- g + theme_minimal()
g

g1 <- ggplot()
g1 <- g1 + ggtitle("LOG Returns Ita Equity") + xlab("Time") + ylab("Log Returns")
g1 <- g1 + geom_line(aes(x=1:length(ret_equ_ita), y=ret_equ_ita), colour='orangered', size=0.5)
g1 <- g1 + theme_minimal()
g1


# plot price of 10Y zerobond
g2 <- ggplot()
g2 <- g2 + ggtitle("Price Chart Equity Italy") + xlab("Time") + ylab("Price")
g2 <- g2 + geom_line(aes(x=1:dim(EWI)[1], y=EWI$EWI.Close), colour='orangered', size=0.5)
g2 <- g2 + theme_minimal()
g2

grid.arrange(g, arrangeGrob(g1, g2, ncol=2), nrow = 2)


################################################################################
