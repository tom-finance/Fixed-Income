################################################################################
# ITALY YIELD CURVE PLOT

# (c) Thomas Ludwig, August 2020.
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

# reverse order
data<- data[seq(dim(data)[1],1),]

Date <- data$Date[-1]

Date_all <- data$Date

data$Date <- NULL

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

################################################################################