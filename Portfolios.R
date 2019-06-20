#Read the table. 
data<-read.table("closes.dat",header = T,sep="", col.names = c('c1','c2','c3','c4','c5','c6','c7'))
data_len = length(data$c1)

#calculate returns
#MCD
r1 <- (data$c1[-data_len] - data$c1[-1]) / (data$c1[-1])
#BoA
r2 <- (data$c2[-data_len] - data$c2[-1]) / (data$c2[-1])
#IBM
r3 <- (data$c3[-data_len] - data$c3[-1]) / (data$c3[-1])
#Chevron
r4 <- (data$c4[-data_len] - data$c4[-1]) / (data$c4[-1])
#Coke
r5 <- (data$c5[-data_len] - data$c5[-1]) / (data$c5[-1])
#novartis
r6<- (data$c6[-data_len] - data$c6[-1]) / (data$c6[-1])
#AT&T
r7 <- (data$c7[-data_len] - data$c7[-1]) / (data$c7[-1])
# calculate daily and annual returns of the stocks
returns_daily <- data.frame(r1,r2,r3,r4,r5,r6,r7)
Expected.returns <- colMeans(returns_daily)  #Expected returns

# get daily and covariance of returns of the stock
cov_daily = cov(returns_daily)
cov_daily

P_weights <- data.frame(r1=0,r2=0,r3=0,r4=0,r5=0,r6=0,r7=0)
P_returns<-vector()
P_vol <- vector()

# find the inverse
cov.inv <- solve(cov_daily)
#make vector of 1s, one for each stock 
ones <- rep(1, 7)
# find the hyperbola parameters 
B <- ( t(ones) %*% cov.inv %*% Expected.returns )[1]
C <- ( t(Expected.returns) %*% (cov.inv) %*% Expected.returns )[1]
A <- ( t(ones) %*% (cov.inv) %*% ones )[1]
D <- C*A - B^2
# set the number of combinations for imaginary portfolios Monty carlo
num_assets = 7
num_portfolios = 2000
i<-0

for (single_portfolio in 1:num_portfolios) {
  i<-i+1
  print(i)
  x<-runif(num_assets)
  weights <- x/sum(x)
  returns <- weights %*% Expected.returns
  
  volatility = ((t(weights) %*% (cov_daily %*% weights)))^0.5
  P_returns <- c(P_returns, returns)
  P_vol<- c(P_vol, volatility)
  P_weights<-rbind(P_weights,weights)
 
  }
P_weights <- P_weights[-1,]

#analytical 
# efficient frontier:
minvar <- 1/A
minE <- C/A

mu <- seq(-0.0005, 0.0005,by=0.0000001)
#Lambda
y1 <- ( C-(B*mu) )/D
y2 <- ( (A*mu) -B)/D

sigma <- y1 + (y2*mu)
sigma <- sqrt(abs(sigma))
min(sigma)
#plot the Monte Carlo poins
plot(sigma,mu, col= 'RED', type = 'l', main = "Portfolio possibilities curve (Without Shortsell)", xlab = "Risk",
     ylab = "Expected Return",pch=20, xlim = c(0.004,0.010), ylim = c(-0.0002,0.0008))
#plot the frontier
points(P_vol,P_returns, col= 'BLACK')
# plot the minimum risk portfolio:
points(sqrt(1/A), B/A, pch=19, col='GREEN')

#Minimum Risk with monte carlo
x_min.MC <- min(P_vol)  #min risk
x_min.MC.index <- which.min(P_vol) 
y_min.MC <- P_returns[x_min.MC.index] # min Returns
x_min.MC.weights <- P_weights[x_min.MC.index,]#min risk weights
points(x_min.MC, y_min.MC, pch=19, col='BLUE') #plot Monte Carlo min risk portfolio

legend(0.004,0.0008, legend=c("Efficiency frontier","Monte Carlo Portfolios", "Least Risky- Analytics", "Least Risky- Monte Carlo"), col=c("RED","BLACK", "GREEN", "BLUE"), lty=1:1, cex=0.7)

# find composition of minimum risk in the efficiency frontier
x_min <- cov.inv %*% as.matrix((C-B*(B/A))/D*ones + ((A*(B/A)-B)/D * (B/A)))
x_min6 <- cov.inv %*% ones / ( t(ones) %*% cov.inv %*% ones )[1]
x_min6
#portfolio return
port_return <- sum(x_min6 * Expected.returns)

#Evolution of prices plot

plot(data$c1, type = 'l', ylim = c(0,200), main = "Evolution of Prices of stocks",xlab = "Timestep",ylab = "Prices $")
lines(data$c2, col="RED")
lines(data$c3, col="BLUE")
lines(data$c4, col="GREEN")
lines(data$c5, col="PINK")
lines(data$c6, col="ORANGE")
lines(data$c7, col="YELLOW")
legend(x=200, legend=c("McDonalds","BoA","IBM","Chevron","Coca-cola","Novartis","AT&T"), col=c("BLACK","RED", "BLUE", "GREEN","PINK","ORANGE","YELLOW"), lty=1:2, cex=0.7)
