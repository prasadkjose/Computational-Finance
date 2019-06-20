install.packages("TTR")
library(package="TTR")
library(forecast)

set.seed(0);
x=100+cumsum(0.5-runif(1000))
x.sma <- SMA(x, n = 100)
plot(x, type = 'l')
lines(x.sma, col="red") #0.1
legend(104,legend=c("x","MA"),
       col=c("black", "red"), lty=1:2, cex=0.7)

ema <- function(alpha)
{ x.ema <-vector(length=1000)
  x.ema[1] <- x[1] 
  x.ema[1000] <- x.sma[1000]
  for (i in 2:999)
  {
    x.ema[i] <- alpha*x[i] + ((1-alpha)* x.ema[i-1])
  }
  return(x.ema)
}




plot(x, type = 'l',ylim=c(90, 110),xlab="t",ylab="X values")
lines(x.sma, col="yellow")

lines(ema(0.1), col="red") #0.1
lines(ema(0.01), col="green") #0.01
lines(ema(2/101), col="blue") #2/n+1
legend(110,legend=c("x","MA", "EMA - Alpha = 0.1", "EMA - Alpha = 0.01", "EMA - Alpha = 2/N+1"),
       col=c("black", "yellow","red", "green", "blue"), lty=1:2, cex=0.7)
x.ema

######################################################################################
#scaling Law
#try scale with 0.001 to 0.01

data <- read.table("EUR_USD-20120101-20120301.dat", header = FALSE, sep="", col.names = c('time','EUR','USD'))

data$time <- as.POSIXct(data$time , origin="1970/01/01 00:00:00")
# set Filters from Jan to March 2012

data <- subset(data,data$time> "2011-12-31" & data$time < "2012-02-29")
data

 #Spread calculation
spread <- data$EUR- data$USD

#midpoint between the ask and bid
x <- data$EUR+ (spread * 0.5)



scaling <- function(x, lambda)
{
  x.ext <- x[1]
  mode = 1
  count = 0
  for (i in 1:length(x)) {
    if(mode == 0){
      if(x[i]>x.ext) x.ext<-x[i]
      else if((x.ext - x[i])/x.ext >= lambda) {
        x.ext <- x[i]
        mode = 1
        count <- count +1
      }
    }
    else if (mode == 1){
      if(x[i] < x.ext){
        x.ext <- x[i]
      }
      else if ((x[i] - x.ext)/x.ext >= lambda){
        x.ext <- x[i]
        mode = 0
        count<- count+1
      }
    }
  }
  return(count)
}

scaling(x, 0.001)
scale<- seq(0.001, 0.01, by=0.001)
scale

plot.scale <- c()
for(i in scale)
{
  plot.scale<- c(plot.scale,scaling(x,i))
} 
plot.scale
plot(scale,plot.scale, log="xy", ylab = "Count of DC")
lines(scale,plot.scale, log="xy", col="red")
