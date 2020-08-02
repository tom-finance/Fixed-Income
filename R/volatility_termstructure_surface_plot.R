################################################################################
# ITALY YIELD CURVE PLOT

# (c) Thomas Ludwig, August 2020.
################################################################################

# packages for analysis

library(readxl) # read input data from Excel into R
library(plotly) # create 3d plot
library(magrittr) # use pipe operator

################################################################################

# sources and additional information

# http://timelyportfolio.blogspot.com/2013/05/japan-jgb-yieldsmore-lattice-charts.html
# http://timelyportfolio.blogspot.com/2013/05/even-more-jgb-yield-charts-with-r.html
# https://www.nytimes.com/interactive/2015/03/19/upshot/3d-yield-curve-economic-growth.html
# https://gist.github.com/timelyportfolio/4da9d6b6c89cce26effabccca30124dd

################################################################################

# load data into R
data <- read_excel("../Input/data_BBG_Yield_Govis.xlsx",
                   sheet = "ITA")

# convert to date class
data$Date <- as.Date(as.POSIXct(data$Date))

# change row ordering (now from old to new)
data <- data[dim(data)[1L]:1,]

################################################################################

# create 3d plot of Yield Curve in Italy

# create plot title
title_plot <- paste0("Yield Curve Italy ",
                format(min(data$Date), "%Y"),
                "-",
                format(max(data$Date), "%Y"))

# create 3d surface plot
plot_yield <- data[, -1] %>%
  # convert to numeric matrix
  data.matrix() %>% 
  # transpose
  t() %>%
  # draw our Plotly 3d surface
  plot_ly(
    x=data$Date,
    y=c(0.25, 1, 5, 10, 15),
    z=.,
    type="surface"
  ) %>%
  plotly::layout(
    title = title_plot,
    scene=list(
      xaxis=list(title=""),
      yaxis=list(title="Term Structure"),
      zaxis=list(title="Yield (%)")
    )
  )

# show result
plot_yield

# save result as html
htmlwidgets::saveWidget(as_widget(plot_yield), 
                        "../Output/yield_curve_ita.html")

################################################################################

# create 3d plot with additional surface

# create surface at (0,0,0)
zero_surface <- matrix(data = rep(0, dim(data[, -1])[1] * dim(data[, -1])[2]),
                       ncol = dim(data[, -1])[2])

# plot yield curve as before but add time additional surface to plot
plot_yield_all <- plot_ly(showscale = FALSE) %>%
  
  add_surface(x = data$Date,
              y = c(0.25, 1, 5, 10, 15),
              z = data[, -1] %>%
                data.matrix() %>%
                t()) %>%
  
  add_surface(x = data$Date,
              y = c(0.25, 1, 5, 10, 15),
              z = zero_surface %>%
                data.matrix() %>%
                t(),
              colorscale = list(c(0,1),c("rgb(255,0,0)","rgb(255,0,0)"))) %>%
  layout(
    title = title_plot,
    scene = list(
      xaxis = list(title = "date"),
      yaxis = list(title = "maturity"),
      zaxis = list(title = "yield")
    ))

# show result
plot_yield_all

# store result as html widget
htmlwidgets::saveWidget(as_widget(plot_yield_all), 
                        "../Output/yield_curve_ita_compare_zero.html")

################################################################################