library(reshape2)
#Data frames and Variable declarations
#Timestamp formated data
data <- read.table("EUR_USD-20120101-20120301.dat", header = FALSE, sep="", col.names = c('time','EUR','USD'))
data$time <- as.POSIXct(data$time , origin="1970/01/01 00:00:00")
#not formatted timestmap data
data1 <- read.table("EUR_USD-20120101-20120301.dat", header = FALSE, sep="", col.names = c('time','EUR','USD'))


#Reset these arrays for each sesssion
WAP.arrays <- data.frame("TWAP" = 0, "VWAP" = 0, "DecisionPrice"= 0, "Algorithm results" = 0)
WAP.arrays.quantity  <- data.frame("TWAP" = 0, "VWAP" = 0, "DecisionPrice"= 0, "Algorithm results" = 0)
WAP.difference <- data.frame("TWAP" = 0, "VWAP" = 0,"Algorithm results" = 0)

#Function Definitions
################################################### TWAP #####################################################
TWAP.calc<- function(InitVal){
  #15 min interval
  TWAP <- data.frame("Timestamp" = 0, "buyPrice" = 0, "Shares"= 0)
  
  start <- as.POSIXct(InitVal$time)
  end <- start + as.difftime(1, units="days")
  slices<-seq(from=start, by=15*60, to=end)
  for(i in slices){
    c<-0
    while (is.na(data1[min(which(data1$time == i+c)),2])) {
      if(is.na(data1[min(which(data1$time == i+c)),2])) {
        c<-c+1
        next
      } else break
    }
    TWAP<-rbind(TWAP, c(i,data1[min(which(data1$time == i+c)),2],2))
  }
  TWAP$Timestamp <- as.POSIXct(TWAP$Timestamp , origin="1970/01/01 00:00:00")
  TWAP<- TWAP[-1,]  
  
  TWAP$Shares=1000000*TWAP$buyPrice
  print("TWAP FUnction: Done")
  
  return(TWAP[1:12,])
}

#################################### VWAP ######################################################################
VWAP.calc <- function(InitVal, TWAP){
  W <- 12000000
  # select historical values from the day before the init date 
  histStart<-as.POSIXct(as.numeric(InitVal$time)-86400 , origin="1970/01/01 00:00:00")
  histTimestamp <- data$time[data$time>histStart & data$time < InitVal$time]
  #find the daily distribtuion of ticks  till the initial date 
  histo <- hist(histTimestamp, breaks = seq(min(histTimestamp), max(histTimestamp)+3600, by='15 mins'), freq = TRUE)
  breaks.time<-as.POSIXct(as.numeric(histo$breaks), origin="1970/01/01 00:00:00")
  #vi = volume the previous day during the same interval
  
  hpercent <- (histo$counts[1:12]/sum(histo$counts[1:12])) * 100
  hpercent<- (hpercent*W)/100
  
  VWAP <- data.frame("time" = breaks.time[1:12], "executionPrice"=TWAP$buyPrice[1:12], "volume"=hpercent ,"histVol"=histo$counts[1:12])
  
  print("VWAP FUnction: Done")
  return(VWAP)
}

 ###################################Algorithm#############################################################

#Function TWAP with scaling law
scaling <- function(x, lambda)
{
  #init time t0 , total quantity to be traded, 
  W = 12000000
  #Order book (type(BUY/SELL), Execution price ,timestamp)
  OB <- data.frame("type" = character(1),"executionPrice"= 0 , "timestamp"=0, stringsAsFactors=FALSE)
  x.ext <- x$USD[1]
  mode = 1
  count = 0
  for (i in 1:length(x$USD)) {
    if(mode == 0 &&  W!= 0){
      #down
      if(x$USD[i]>x.ext) x.ext<-x$USD[i]
      else if((x.ext - x$USD[i])/x.ext >= lambda) {
        x.ext <- x$USD[i]
        mode = 1
        OB <- rbind(OB, c("BUY", x$USD[i], x$time[i] ))
        count <- count +1
        W<- W-1000000
      }
    }
    else if (mode == 1 && W != 0){
      #up
      if(x$USD[i] < x.ext){
        x.ext <- x$USD[i]
      }
      else if ((x$USD[i] - x.ext)/x.ext >= lambda){
        x.ext <- x$USD[i]
        mode = 0
        #SELL is not counted as a trade. 
        #OB <- rbind(OB, c("SELL", x$USD[i], x$time[i] ))
        count<- count+1
       
      }
    }
    else break
  }
  return(OB)
}


#Find the time difference between the Init time T0 and end of execution time T1
difftime(OrderBook$timestamp[12], OrderBook$timestamp[1])
OrderBook$executionPrice<- as.numeric(OrderBook$executionPrice)

#############################################################################

#for trying with multiple random initial time. 
#Each Iteration should take no longer than 10 seconds. If it takes longer, Kindly restart the
#loop. The sucessful results are recorded. 
for (variable in 1:5)
{
  print(variable)

  InitVal<- data[sample(1400000,1),]# random time selection. 
  
  t<- TWAP.calc(InitVal) #TWAP 

  v<- VWAP.calc(InitVal, t) #VWAP

  x <- subset(data,data$time> InitVal$time)
  OrderBook <- scaling(x, 0.0009)  #TWAP Algorithm 
  OrderBook$executionPrice<- as.numeric(OrderBook$executionPrice)
  OrderBook$timestamp <- as.POSIXct(as.numeric(OrderBook$timestamp), origin="1970/01/01 00:00:00")
  OrderBook<- OrderBook[-1,] #remove the initial row(0,0,0)
  OrderBook
  #plot Vol. evolution of VWAP. 
    #cumsum(v$volume)
    #plot(v$time, cumsum(v$volume), type = 'l', xlab = "Time", ylab = "volume", main = "Plot of evolution of volume vs time")
 
   #TWAP and VWAP exec. prices array.
  WAP.arrays<- rbind(WAP.arrays, c(sum(t[1:12,2])/12,sum(v$executionPrice*v$volume)/sum(v$volume),InitVal$USD, mean(OrderBook$executionPrice)))
  WAP.arrays.quantity <- rbind(WAP.arrays.quantity, c(sum(t$Shares), sum(v$volume*v$executionPrice), sum(InitVal$USD*12000000), sum(OrderBook$executionPrice*1000000)))
  WAP.difference <- rbind(WAP.difference,c(sum(InitVal$USD*12000000)-sum(t$Shares),sum(InitVal$USD*12000000)-sum(v$volume*v$executionPrice),sum(InitVal$USD*12000000)-sum(OrderBook$executionPrice*1000000)))
}

#The results 
WAP.arrays
WAP.arrays.quantity
WAP.difference
WAP.arrays<- WAP.arrays[-1,] #remove the initial row(0,0,0)
WAP.arrays.quantity<- WAP.arrays.quantity[-1,] #remove the initial row(0,0,0)
WAP.difference<- WAP.difference[-1,] #remove the initial row(0,0,0)


#Bar plot of TWAP, VWAP and decision prices.
d <- do.call(rbind, WAP.arrays)
barplot(d, beside = TRUE, legend.text = rownames(d), 
        args.legend = list(x = "topright", bty="n"), xlab = "Iterations", ylab = "Prices")
d1 <- do.call(rbind, WAP.arrays.quantity)
barplot(d1, beside = TRUE, legend.text = rownames(d), 
        args.legend = list(x = "topright", bty="n"), xlab = "Iterations", ylab = "Total ")

d2 <- do.call(rbind, WAP.difference)
barplot(d2, beside = TRUE, legend.text = rownames(d), 
        args.legend = list(x = "topright", bty="n"), xlab = "Iterations", ylab = "Difference - exec. and dec. prices. ")

plot(WAP.difference$TWAP, ylab = "Price Difference", xlab = "Iterations")
points(WAP.difference$VWAP, col='RED')
points(WAP.difference$Algorithm.results, col= 'BLUE')
legend(30000,legend=c("TWAP","VWAP","Price Evolution"), col=c("BLACK","RED", "BLUE"), lty=1:2, cex=0.7)
