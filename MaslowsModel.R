InitVal = c(101,99,100)

Market <- data.frame("Ask" = InitVal[1], "Bid" = InitVal[2], "Market"= InitVal[3])
AskOB<-vector()
BidOB<-vector()

AskOB[1]=InitVal[1]
BidOB[1]=InitVal[2]
############################ Maslows Model#################################
for(t in 1:1000){
  rand<-runif(1)
  #buy
  if(rand<0.5){
    rand<-runif(1)
    #MO
    if(rand<0.5){
      if(length(AskOB)==0){
        Market<-rbind(Market,c(Market$Ask[t-1],Market$Bid[t-1],Market$Market[t-1]))
      }
      else{
        best<-min(AskOB)
        AskOB<-AskOB[-(which.min(AskOB))]
        Market<-rbind(Market,c(Market$Market[t-1],Market$Bid[t-1],best))
        }
    }
    #LO
    else{
      Market<-rbind(Market,c(Market$Ask[t-1],Market$Bid[t-1],Market$Market[t-1]))
      BidOB=c(BidOB, (Market$Market[t-1]-1) )
    }
  }  
  #ASK
  else{
    rand<-runif(1)
    #MO
    if(rand<0.5){
      if( length(BidOB)==0 ){
        Market<-rbind(Market,c(Market$Ask[t-1],Market$Bid[t-1],Market$Market[t-1]))
       }
      else{
        best<-max(BidOB)
        BidOB<-BidOB[-(which.max(BidOB))]
        Market<-rbind(Market,c(Market$Ask[t-1],Market$Market[t-1],best))
        }
    }
    #LO
    else{
      Market<-rbind(Market,c(Market$Ask[t-1],Market$Bid[t-1],Market$Market[t-1]))
      AskOB=c(AskOB, (Market$Market[t]+1) )
    }
  }
}

Market

plot(Market$Market, type ='l', xlab = "TimeSteps" , ylab = "$")
lines(Market$Ask, col="RED")
lines(Market$Bid, col="BLUE")
legend(102,legend=c("Market","Ask","Bid"),
       col=c("black", "red","blue"), lty=1:2, cex=0.7)
##############################Returns######################################
Returns <-  data.frame("Ask" = c(0), "Bid" = c(0), "Market"= c(0))

for(i in 1:999) { 
a<-(Market$Ask[i+1]-Market$Ask[i])/Market$Ask[i]
b<-(Market$Bid[i+1]-Market$Bid[i])/Market$Bid[i]
m<-(Market$Market[i+1]-Market$Market[i])/Market$Market[i]
Returns<-rbind(Returns,c(a,b,m))
}
Returns

plot(Returns$Market, type ='l', xlab = "TimeSteps" , ylab = "Returns",main = 'Returns')
lines(Returns$Ask, col="RED")
lines(Returns$Bid, col="BLUE")
legend(0.06,legend=c("Market","Ask","Bid"),
       col=c("black", "red","blue"), lty=1:2, cex=0.7)
acf(Returns$Market, plot = TRUE)
tail(Returns)

#MID point

spread <- Market$Ask - Market$Bid

mid <- Market$Bid+ spread * 0.5

plot(Market$Market, type = 'l', main = 'Market Price vs Mid price',ylab = '$', xlab = 'Time step')
lines(mid , col="RED")
legend(110,legend=c("Market","Mid Price"),
       col=c("black", "red"), lty=1:2, cex=0.7)

#Normal Distribution 
set.seed(0);
n=1000;
a1=rnorm(n,mean=0,sd=0.02);
plot(a1, type = 'l')
lines(Returns$Market, col='RED')
acf(a1, plot = TRUE)
