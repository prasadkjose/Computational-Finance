library(package="quantmod")
########################################## Ques 1 #####################################################################################


month <- data.frame("month"=c("Jan","Feb","Mar","April","May","June","July","Aug","Sept","Oct","Nov","Dec"), "Returns"=c(101,102,103,104,105,106,107,108,109,110,111,112))

portInit <- 100

monthlyReturn <- function(month, portInit=100)
{
  monthlyR <-  c((month[1,2] - portInit)/portInit )
  for(i in 2:length(month[,2]))
  {
    monthlyR<-c(monthlyR,(month[i,2]-month[i-1,2])/month[i-1,2])
  }
  return(monthlyR)
}
monthlyReturns <-monthlyReturn(month)# monthly return - vector
monthlyReturns # Monthly Returns
sum(monthlyReturns) * 100 #sum of monthly returns
(prod(monthlyReturns+1)-1) *100 # Annual Return

#average of monthly return
(sum(monthlyReturns)/length(month[,2]))*100
#average Monthly Return
(1.12)^(1/12) - 1 
  
############################################## Ques 2##############################################################################

NoStocks<- c(10,10) #[Microsoft, Starbucks]
Ptm1 <- c(85,30) # Pt-1
# Calculate Inital Value of Portfolio
InitVal <- 0
for (i in 1:length(NoStocks))
{
  InitVal <- InitVal + NoStocks[i]*Ptm1[i]
}

InitVal

# Portfolio Share Weight alpha
Alpha <- vector()
for (i in 1:length(NoStocks))
{
  Alpha[i] <- Ptm1[i]/InitVal
}

Alpha<-Alpha*10

#Compute the one-period return of Microsoft and Stabuck stocks
Pt1 <- c(90,28) #Pt
Returns <- vector()
for (i in 1:length(NoStocks))
{
  Returns[i] <-(Pt1[i]- Ptm1[i])/Ptm1[i]
}

      Returns

ReturnPortfolio <- 0
for(i in 1:length(NoStocks))
{
  ReturnPortfolio <- ReturnPortfolio + Alpha[i]*Returns[i]
}
ReturnPortfolio

#compute the return of the portfolio and its value at the end of month t.
Vt <- 0
for (i in 1:length(NoStocks))
{
  Vt <- InitVal*(1+ReturnPortfolio)
}

Vt

############################################## Ques 3 ##############################################################################
#Bivariate Prob. Distribution 


x.probs = matrix(c(0.2,0.2,0.2,0.4))
y.probs = matrix(c(0.6,0.4))     
y.probs
xy.jProb = matrix(c(0.2,0,
                   0.1,0.1,
                   0.0,0.2,
                   0.3,0.1),2,4)
xy.jProb = matrix(c(0.2,0.1,0,0.3,
                    0,0.1,0.2,0.1),4,2)
x.vals <- 0:3
y.vals <- 0:1
x.vals

# E[X] 
x.E <- sum(x.vals * x.probs)
x.E

#E[X|Y=0]
xy0.E <- sum(x.vals * xy.jProb[,1])
xy.jProb[,1]
xy0.E

#Var[X|Y = 0]
xy0.Var <- sum(((x.vals - xy0.E)^2) * xy.jProb[,1] )
xy0.Var
