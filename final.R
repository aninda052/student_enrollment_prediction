library(tseries)
library(forecast)


#Data Reading
mydata<-read.table(file="fulldata.csv",header = T,sep=",")
attach(mydata)
true_value<- StudentYt
diff_value <- diff(true_value)

#Over view of the Time Series
plot(true_value,ylab="Student No",xlab="Semester",las=1,ylim=c(0,300),las=1,main="Main data")
lines(true_value)
plot(diff_value,ylab="Difference",xlab="Semester",las=1,ylim=c(-200,200),las=1,main="Difference Between Two Semester")
lines(diff_value)


#check serial correlation (Ljung-Box test)
#Null Hypothesis : There is no serial correlation upto lag 2
Box.test(diff_value,lag = 2,type = "Ljung-Box")
# p-value almost close to zero,so we can reject Null Hypothesis
#similarly for true_value
Box.test(true_value,lag = 2,type = "Ljung-Box")


#Stationarity Checking ( Augmented Dickey–Fuller test )
#Null Hypothesis : Ther is a unit root
adf.test(true_value,alternative = "stationary",k=2)
adf.test(diff_value,alternative = "stationary",k=2)
#in both cases the value of Dickey-Fuller test is negetive,so we can reject Null Hypothesis

#More Stationarity Checking 
acf(true_value,main="ACF of Orginal Series")
pacf(true_value,main="PACF of Orginal Series")
acf(diffy,main="ACF of Difference Series")
pacf(diffy,main="PACF of Difference Series")
#acf of orgianl series shows slow decay,whis is a indication of non-stationarity,on the other hand
#difference seriec seems stationary

#testing different ARIMA models
arima(diff_value,order=c(1,0,1)) #arima(1,1,1)
arima(diff_value,order=c(1,0,2)) #arima(1,1,2)
arima(diff_value,order=c(2,0,1)) #arima(2,1,1)
arima(diff_value,order=c(2,0,2)) # arima(2,1,2)

#From analysis ARIMA(2,1,1) best fitted model for this time series
Forcast.arima211 <- arima(true_value,order=c(2,1,1))
pred5 <- predict(Forcast.arima211,n.ahead = 5)


Result <- function(x){
up<-c()
low<-c()
X<-c()
for(i in c(1:5)){
x=pred5$pred[i]
X[i+45]=x
cat ("Predicted  ",x,"\n")
low[i+45]=x-pred5$se[i]
up[i+45]=x+pred5$se[i]
cat("Prediction with Standard Error : ",low[i+45]," - ",up[i+45],"\n")}

jpeg("prediction.jpeg")
plot(true_value,ylim = c(0,350),xlim = c(0,55),xlab="Semester No",ylab="Student No",col="black")
lines(true_value,col="blue")
par(new=T)
plot(X,ylim = c(0,350),xlim = c(0,55),xlab="Semester No",ylab="Student No",col="black")
lines(X,col="green")
lines(low,col="red")
lines(up,col="red")
dev.off()
}

options(digits = 2)
Result(true_value[45])

