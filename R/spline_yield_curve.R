# ------------------------------------------------------------------------------
# USER DEFINED SPLINE FUNCTION
#
# (c) Thomas Ludwig, August 2020.
# ------------------------------------------------------------------------------

# load package to create Excel file
library(openxlsx)

# goal of this skript:
# we want to define a function which allows us to interpolate points on the yield curve.

splining <- function(yield = "yield structure",
                     t = "time structure",
                     n_spline = 30) {
  
  term.spline <- spline(t, yield, n = n_spline, method = c("hyman"))
  
  # Hyman method allows us to preserve monotony!
  
  final <- cbind(term.spline[[1]], term.spline[[2]])
  
  colnames(final) <- c("year", "yield")
  
  return(final)
  
} 

# test function with user input and n_spline = 30:
res <- splining(yield = c(0.45, 0.82, 1.07, 1.2, 1.28), t = c(10, 15, 20, 25, 30))

# visualize results
matplot(res[,1], res[,2], type = "l", 
        col = "blue", main = "Interpolated Curve",
        xlab = "t",
        ylab = "y(t)")

# safe results in output directory as Excel file
write.xlsx(res, 
           file = "../Output/splined_curve.xlsx", 
           row.names = FALSE)
           
# Source:
# https://www.r-bloggers.com/quantitative-finance-applications-in-r-6-constructing-a-term-structure-of-interest-rates-using-r-part-1/

# ------------------------------------------------------------------------------
