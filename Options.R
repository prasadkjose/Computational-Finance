############################# Black Scholes Formula ##########################
S.0 <- 100
T.maturity <- 1
K <- 120
sigma <- .2
r <- .05

d.1 <- (log(S.0/K) + (r+(sigma^2)/2)*T.maturity) / sigma*sqrt(T.maturity)
d.2 <- (log(S.0/K) + (r-(sigma^2)/2)*T.maturity) / sigma*sqrt(T.maturity)

Call <- (S.0 * pnorm(d.1)) - K * exp(-r*T.maturity) * pnorm(d.2)
#Put <- (K*exp(-r*T.maturity)*pnorm(-d.2)) - (S.0 * pnorm(-d.1))

pnorm(0.9)
################################ Binomial Tree Options #################################################################
#Risk-Neutral Probabilities 
q_prob = function(r, delta_t, sigma) {
  u = exp(sigma*sqrt(delta_t))
  d = exp(-sigma*sqrt(delta_t))
  
  return((exp(r*delta_t) - d)/(u-d))
}
#Build portfolio stock value tree
portfolioValueTree = function(S, sigma, delta_t, N) {
  tree = matrix(0, nrow=N+1, ncol=N+1)
  
  u = exp(sigma*sqrt(delta_t))
  d = exp(-sigma*sqrt(delta_t))
  #grid of stock prices
  for (i in 1:(N+1)) {
    for (j in 1:i) {
      tree[i,j] = S * u^(j-1) * d^((i-1)-(j-1))
    }
  }
  return(tree)
}

#Build Option tree 'f'
f_binomial_option = function(tree, sigma, delta_t, r, X, type) {
  q = q_prob(r, delta_t, sigma)
  option_tree = matrix(0, nrow=nrow(tree), ncol=ncol(tree))
  #put
  if(type == 'put') {
    option_tree[nrow(option_tree),] = pmax(X - tree[nrow(tree),], 0)
  } 
  #call
  else {
    option_tree[nrow(option_tree),] = pmax(tree[nrow(tree),] - X, 0)
  }
  for (i in (nrow(tree)-1):1) {
    for(j in 1:i) {
      #calculate f
      option_tree[i, j] = ((1-q)*option_tree[i+1,j] + q*option_tree[i+1,j+1])/exp(r*delta_t)
    }
  }
  return(option_tree)
}

binomial_option = function(type, sigma, T, r, X, S, N) {
  q = q_prob(r=r, delta_t=T/N, sigma=sigma)
  tree = portfolioValueTree(S=S, sigma=sigma, delta_t=T/N, N=N)
  option = f_binomial_option(tree, sigma=sigma, delta_t=T/N, r=r, X=X, type=type)
  delta = (option[2,2]-option[2,1])/(tree[2,2]-tree[2,1])
  return(list(q=q, stock=tree, option=option, price=option[1,1], delta=delta))
}

bi <- binomial_option(type='call', sigma, T.maturity, r, K, S.0, N=2)
bi$price
Call

#######################PLot of tree depth - stock price ##############################################################
binomial.price <- vector()
for (i in 1:15) {
print(i)
bi <- binomial_option(type='call', sigma, T.maturity, r, K, S.0, N=i)
binomial.price <- c(binomial.price, bi$price)
}

plot(1:50,binomial.price, type = 'l', main = "Plot of call vs tree depth", ylab = "Price using binomial tree", xlab = "Tree Depth")


###############################################

T.maturity <- 1
K <- 120
sigma <- .2
r <- .05
Put <- vector()
Delta<- vector()
Gamma <- vector()
for (S.0 in 1:200) {
  d.1 <- (log(S.0/K) + (r+(sigma^2)/2)*T.maturity) / (sigma*sqrt(T.maturity))
  d.2 <- (log(S.0/K) + (r-(sigma^2)/2)*T.maturity) / (sigma*sqrt(T.maturity))
  Put <-c(Put,(K*exp(-r*T.maturity)*pnorm(-d.2)) - (S.0 * pnorm(-d.1)))
  Delta <- c(Delta, pnorm(d.1) - 1)
  Gamma <- c(Gamma, ((exp(((-d.1^2)/2))/ 2*3.14)/(S.0 * sigma * sqrt(T.maturity))))
}

plot(Put,type = 'l', main = "Plot of Inital Assert Price Vs Put", xlab = "Inital Price")
lines(rep(c(120),each=200),seq(0, 150, length.out = 200), col="Red")
legend(150,100,legend=c("Put", "Strike"),
       col=c("Black","Red"), lty=1:1, cex=0.7)

plot(Delta,type = 'l', main = "Plot of Inital Assert Price Vs the Delta", xlab = "Inital Price", ylim = c(-1,0.1))
lines(Gamma, col= "blue")
lines(rep(c(120),each=200),seq(-1.5, 0.5, length.out = 200), col="Red")
lines(rep(c(which.max(Gamma)),each=200),seq(-1.5, 0.5, length.out = 200), col="Green")
legend(0.1,legend=c("Gamma","Delta","Strike", "Max(Gamma)"),
       col=c("Blue", "Black","Red", "Green"), lty=1:1, cex=0.7)

Delta[95]
Put[95]
