################################################################################
# Test GARCH Fit
################################################################################

library(ggplot2)
library(gridExtra)
library(MTS)
library(rugarch)
library(zoo)
library(quantmod)

data(sp500ret)
spec <- ugarchspec(variance.model = list(model = 'eGARCH', garchOrder = c(1, 1)), distribution = 'std')
fit <- ugarchfit(spec, sp500ret[1:1000, , drop = FALSE], solver = 'hybrid')
plot(fit, which=9)

plot.ts(sigma(fit)*250^0.5, ylab="sigma(t)", col="blue")

#######################################

# App 2: Fit model on squared returns

fit2 <- ugarchfit(spec, sp500ret[1:1000, , drop = FALSE]^2, solver = 'hybrid')
plot(fit2, which=9)

plot.ts(sigma(fit2)*250^0.5, ylab="sigma(t)", col="blue")


plot.ts(abs(sp500ret[1:1000, , drop = FALSE]))

##################

MSFT.garch11.fcst = ugarchforecast(fit,n.ahead=100)

################################################################################
