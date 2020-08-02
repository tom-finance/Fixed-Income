################################################################################
# SHORT SKRIPT ON RETURN BOOTSTRAPPING METHOD
# WORK IN PROGRESS
################################################################################

# packages

library(ggplot2)
library(zoo)
library(quantmod)

################################################################################
# daily DAX

getSymbols("^GDAXI", src="yahoo")


ret_DAX <- diff(log(GDAXI$GDAXI.Close))
ret_DAX <- na.omit(ret_DAX)

#

res <- list()

for (i in 1:100) {
  
  ret_sampled <- sample(ret_DAX, 750)
  
  res[[i]] <- exp(cumsum(ret_sampled))
  
  print(i)
}


all <- do.call(cbind.data.frame, res)

# plot all
matplot(all, type = "l", col = "blue", lty = 1, lwd = 0.5)

extr_quantile <- function(x) {
  
  y <- c(quantile(x, 0.05),
         quantile(x, 0.5),
         quantile(x, 0.95))
  
  return(y)
  
}

test <- t(apply(all, MARGIN = 1, FUN = extr_quantile))


autoplot(zoo(test), facet = NULL) + 
  geom_line(size = 1) +
  theme_minimal()

# mean return
mean(unlist(all[750, ]))^(1/3)-1

# check this
# https://www.burns-stat.com/documents/tutorials/the-statistical-bootstrap-and-other-resampling-methods-2/

################################################################################