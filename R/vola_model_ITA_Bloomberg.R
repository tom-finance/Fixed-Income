################################################################################
# BTP 10Y Analyses
################################################################################

library(ggplot2)
library(gridExtra)
library(MTS)
library(rugarch)
library(zoo)
library(quantmod)
library(openxlsx)
library(xts)
library(plotly)
library(magrittr)

################################################################################
###################
# FIXED INCOME
###################

# data preperation


data <- read.xlsx("C:/Users/User/Desktop/Volatility_Dashboard/data_BBG/daten_BBG_clean.xlsx",
                   sheet = "ITA")

data$Date <- convertToDate(data$Date, origin = "1900-01-01")

plot(data$GTITL3M.Corp ~ data$Date, type = "l")


# reverse order
data<- data[seq(dim(data)[1],1),]

Date <- data$Date[-1]

Date_all <- data$Date

data$Date <- NULL

###################

# calculate yield vola squared

calc_yield_vola <- function(x) {
  
  y <- x+2 # shift to deal with negative yields
  
  y <- diff(log(y))
  
  y <- y^2
  
  return(y)
}

# calculate matrix
data_yield_ret <- sapply(data, calc_yield_vola)

################################################################################


garch_fit <- function(x) {
  
  spec <- ugarchspec(variance.model = list(model = 'sGARCH', garchOrder = c(1, 1)), distribution = 'std')
  
  g_ts <- ugarchfit(spec, x, solver = "hybrid")
  
  fitted <- g_ts@fit$fitted.values^0.5
  
  return(fitted)
  
}

# fit model to all series
garch_fitted <- apply(data_yield_ret, MARGIN = 2, FUN = garch_fit)

garch_fitted <- garch_fitted * 250^0.5


################################################################################
# PLOT VOLATILITY STRUCTURE

rates_xts <- xts(garch_fitted, order.by = Date)

plot(rates_xts)


test <- rates_xts %>%
  # convert to numeric matrix
  data.matrix() %>% 
  # transpose
  t() %>%
  # draw our Plotly 3d surface
  plot_ly(
    x=as.Date(index(rates_xts)),
    y=c(0.25,1,5,10,15),
    z=.,
    type="surface"
  ) %>%
  plotly::layout(
    scene=list(
      xaxis=list(title="date"),
      yaxis=list(title="term"),
      zaxis=list(title="yield")
    )
  )

test




#PLOTS
g <- ggplot()
g <- g + ggtitle("Price Volatility GARCH (1,1) Model BTP 10Y") + xlab("Time") + ylab("Daily Volatility")
g <- g + geom_line(aes(x=Date[-1], y=test), colour='orangered', size=1)
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


rates_xts <- xts(data, order.by = Date_all)


test <- rates_xts %>%
  # convert to numeric matrix
  data.matrix() %>% 
  # transpose
  t() %>%
  # draw our Plotly 3d surface
  plot_ly(
    x=as.Date(index(rates_xts)),
    y=c(0.25,1,5,10,15),
    z=.,
    type="surface"
  ) %>%
  plotly::layout(
    scene=list(
      xaxis=list(title="date"),
      yaxis=list(title="term"),
      zaxis=list(title="yield")
    )
  )

test
