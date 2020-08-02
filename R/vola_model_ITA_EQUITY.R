################################################################################
# VOLATILITY ANALYSIS ITALY EQUITY
################################################################################

library(ggplot2)
library(gridExtra)
library(MTS)
library(rugarch)
library(zoo)
library(quantmod)

#############################
# EQUITY MARKETS GARCH MODEL  
#############################

# ITA EQUITY
getSymbols("EWI", src="yahoo")
# 
# # many missing values!! find something better?
# getSymbols("FTSEMIB.MI", src="yahoo")

ret_equ_ita <- diff(log(EWI$EWI.Close))

# calculate squared returns
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


# plot price chart
g2 <- ggplot()
g2 <- g2 + ggtitle("Price Chart Equity Italy") + xlab("Time") + ylab("Price")
g2 <- g2 + geom_line(aes(x=1:dim(EWI)[1], y=EWI$EWI.Close), colour='orangered', size=0.5)
g2 <- g2 + theme_minimal()
g2

grid.arrange(g, arrangeGrob(g1, g2, ncol=2), nrow = 2)

################################################################################
