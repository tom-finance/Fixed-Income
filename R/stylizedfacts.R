#STYLIZED FACTS OF RETURNS
library('moments')
library('normtest')
library('nortest')
library('ggplot2')
library('gridExtra')
library('quantmod')

a <- 0.05

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


#ABS RETURNS (volatility)
returns_absolute <- abs(returns)


#---------------------------#---------------------------
#---------------------------#---------------------------
#AUTOCORRELATION OF RETURNS / ABSOLUTE RETURNS

type <- 'absolute' ####choose 'absolute' for absolute returns 
if (type == 'regular') {  tmpret <- returns } else {  tmpret <- returns_absolute }

lags <- 42

autocorr <- acf(tmpret, lag=lags)$acf[2:(lags+1)]
autocorr_SE <- sqrt((1-autocorr^2) / (n-2))
autocorr_stat <- autocorr / autocorr_SE
#autocorr_p <- pt(abs(autocorr_stat), df=n-2) #for smaller samples --> use t-distribution
autocorr_p <- pnorm(abs(autocorr_stat), 0, 1)
autocorr_h <- autocorr_p < (1 - (a/2))

autocorr_borders_upper <- qt(a, n-2) * autocorr_SE *-1
autocorr_borders_lower <- qt(a, n-2) * autocorr_SE


#PLOT
g <- ggplot() + ggtitle("AUTOCORRELATION")
g <- g + geom_bar(aes(x=1:lags, y=autocorr[1:(lags)]), stat="identity")
g <- g + geom_line(aes(x=1:lags, y=autocorr_borders_lower[1:lags]), color="darkgoldenrod2", size=2, lty=4)
g <- g + geom_line(aes(x=1:lags, y=autocorr_borders_upper[1:lags]), color="darkgoldenrod2", size=2, lty=4)
g


#LINEAR REGRESSION
lag <- 5

returns_lagged <- matrix(, nrow=n, ncol=lag)

for (i in 1:lag) {
  tmp <- Lag(tmpret, k=i)
  returns_lagged[1:n, i] <- tmp 
}

linreg <- lm(tmpret ~ 1 + returns_lagged) #estimate linear regression with lags as predictors
summary(linreg)


#SIMPLE CORRELATION TO LAGS
correl <- cor(tmpret, y = returns_lagged, use="pairwise.complete.obs")





#---------------------------#---------------------------
#---------------------------#---------------------------
#SYMMETRIC?? SKEWNESS TEST
skew <- skewness(returns) #calculate skewness
skew_SE <- sqrt(6 * n *(n-1) / ((n-2) * (n+1) * (n+3))) #approx. sqrt(6/n)
skew_stat <- (skew - 0) / skew_SE #normal distribution has skewness of 0
skew_p <- 1 - pnorm(abs(skew_stat)) #find p value in standard normal distribution as skew_stat is ~normally dist.
skew_h <- skew_p < (1 - (a/2)) #hypothesis, FALSE = fail to reject h0 of differing skewness
skew_ci_lower <- skew - qnorm(1-a/2) * skew_SE #confidence interval lower
skew_ci_upper <- skew + qnorm(1-a/2) * skew_SE #confidence interval upper

#DIFFERING SHAPE?? KURTOSIS TEST
kurt <- kurtosis(returns) #calculate kurtosis
kurt_SE <- sqrt(24 * n *(n-1)^2 / ((n-3) * (n-2) * (n+3) * (n+5))) #approx. sqrt(24/n)
kurt_stat <- (kurt - 3) / kurt_SE #normal distribution has kurtosis of 3
kurt_p <- 1 - pnorm(abs(kurt_stat)) #find p value in standard normal distribution as kurt_stat is ~normally dist.
kurt_h <- kurt_p < (1 - (a/2)) #hypothesis, FALSE = fail to reject h0 of differing kurtosis
kurt_ci_lower <- kurt - qnorm(1-(a/2)) * kurt_SE #confidence interval lower
kurt_ci_upper <- kurt + qnorm(1-(a/2)) * kurt_SE #confidence interval upper

#DISTRIBUTION?? KOLMOGOROW SMIRNOFF TEST, JARQUE BERA TEST, ANDERSON DARLING TEST, CRAMER VON MISES TEST, ...
dist_ks <- ks.test(returns, y="pnorm")
dist_jb <- ajb.norm.test(returns)
dist_ad <- ad.test(returns)
dist_cvm <- cvm.test(returns)



#---------------------------#---------------------------
#---------------------------#---------------------------
#VOLATILITY CLUSTERS
windowlength <- 42
window_mean_volatility <- numeric()
winseq <- seq(from=1, to=n, by=windowlength)

j <- 1
for (i in winseq) {
  window_mean_volatility[j] <- mean(returns_absolute[i:(i+windowlength)], na.rm=TRUE)
  j <- j+1
}

g <- ggplot() + xlim(1,n) #set up plot
g <- g + geom_line(aes(x=1:n, y=returns), color="royalblue2") #draw returns


#tmp <- data.frame(x1=c(0, 500, 1000), y1=c(1, -1, 0.5), x2=c(200, 700, 2500), y2=c(1, -1, 0.5))
h <- ggplot() + xlim(1,n)#set up plot
h <- h + geom_area(aes(x=1:n, y=returns_absolute), fill="springgreen2") #draw absolute returns as "area" plot
h <- h + geom_segment(aes(x=winseq, xend=winseq+windowlength, y=window_mean_volatility, yend=window_mean_volatility), colour="black", size=2)


#layout for group-plot
lay <- rbind(c(1,1), c(2,2))
grid.arrange(g, h, layout_matrix = lay)




#---------------------------#---------------------------
#---------------------------#---------------------------
#OPTICAL APPROACHES DISTRIBUTION
rge <- seq(from=-0.2, to=0.2, by=0.001)
g <- ggplot()
g <- g + geom_histogram(aes(returns, ..density..), binwidth=0.001, fill='gray55') #histogram scaled appropriately
g <- g + geom_line(aes(x = rge, y = dnorm(rge, mu, sigma)), color='seagreen2', size=1.5) #add normal distribution
g <- g + geom_density(aes(returns), color='darkorchid2', size=1) # 
g

h <- g + xlim(-0.2, -0.03)
k <- g + xlim(-0.03, 0.03)


#layout for group-plot
lay <- rbind(c(1,2))
grid.arrange(h, k, layout_matrix = lay)


