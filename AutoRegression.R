install.packages("tseries")
library(package="tseries")



set.seed(0);
n=10000;
a1=rnorm(n,mean=0,sd=1);
a2=rnorm(n,mean=0,sd=2);
X=a1+2*a2;
Y=2*a1+a2
plot(X,Y, xlim=c(-15,15), ylim=c(-15,15))
cor(X,Y)
lm(Y~X)

#Letting T vary, plot the ACF and comment on the impact of T on the ACF ?
T<-5 #10,30,60
x <- rnorm(100) + sin(2*pi*1:100/T)

#plot ACF of x
acf(x,type = c("correlation"),
    plot = TRUE)

data <- read.table("priceM.dat", header = FALSE, sep="", col.names = c('time','bid','ask'))

#• Plot the evolution of the temporal serie.
plot(data$time, data$ask, ylim=c(1.2,1.4))
plot(data$time, data$bid, ylim=c(1.2,1.4))

#is it stationary?
adf.test(data$ask)


#Spread calculation
spread <- data$ask- data$bid
spread
#midpoint between the ask and bid
mid <- data$bid+ spread * 0.5

mid

#Returns Calculation
Returns <- vector()
for (i in 1:(length(mid)-1))
{
  Returns[i] <-(mid[i+1]- mid[i])/mid[i]
}

Returns
#returns plot
plot(Returns, type = 'l')

#is the returns stationary?
adf.test(Returns)

#Compute and plot ACF of returns
acf(Returns, type = c("correlation", "covariance", "partial"),
    plot = TRUE, na.action = na.fail, demean = TRUE, ylim=c(-1,1))

#AR(1)
set.seed(0);
n=1000;
rand=rnorm(n,mean=0,sd=1)
X <- vector(length = n)
X[1] <- 0
for (i in 2:length(X))
{
  X[i] <- X[i-1] + rand[i]
}
plot(X, type = 'l')
ar(X,order.max = 3)


 
t<-1325437052
as.POSIXct(t , origin="1970/01/01")

data <- read.table("EUR_USD-20120101-20120301.dat", header = FALSE, sep="", col.names = c('time','EUR','USD'))
timestamp <- as.POSIXct(data$time , origin="1970/01/01 00:00:00")

# set Filters from Jan to March 2012
timestamp <- timestamp[timestamp> "2011-12-31" & timestamp < "2012-02-29"]
timestamp

#Some trials with Histograms
hist(timestamp, breaks = seq(min(timestamp), max(timestamp)+3600, by='15 mins'), freq = TRUE)

hist(timestamp, breaks = seq(min(timestamp), max(timestamp)+3600*24, 'days'), freq = TRUE)

hist(timestamp, breaks = seq(min(timestamp), max(timestamp)+3600*24*7, 'weeks'), freq = TRUE)

#Extract Date
Date <- as.Date(timestamp)
Date

#Days of week
dwka <- format(Date , "%a")
dwka

table(dwka) # shows the daily distribution of  ticks 

#days of week as numbers from 1-7, 1 for sunday and 7 for saturday
dwkn <- as.numeric( format(Date , "%w") ) +1
dwkn

#Histogram for Daily Distibution of Ticks
hist( dwkn , breaks= -.5+1:8, labels= unique(dwka[order(dwkn)]), freq = TRUE)

#Extracts Date Number from Date
d <-as.numeric(format(Date , "%d"))
#Extracts Week Number from Date Number
weekNo <- ceiling(d/7) 

table(weekNo)

hist( weekNo , breaks= 1:5, freq = TRUE)





######################################################################################################################
# AR Model

set.seed(0);
n=1000
rand=rnorm(n,mean=0,sd=1)
X <- vector(length = n)
X[1] <- 0
delta.AR<- 0.98 #variation with -1,-0.5, 0.5, 0
Const.AR<- 0
for (i in 2:length(X))
{
  X[i] <- Const.AR+ delta.AR*X[i-1] + rand[i]
}
plot(X, type = 'l')
ar(X,aic = FALSE,order.max = 10, plot = TRUE)
adf.test(X)
acf(X)

