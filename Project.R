library(tseries)
library(forecast)
library(TSA)
library(FactoMineR)
library(psych)
library(factoextra)
library(TSPred)
library(CombMSC)
data<-read.csv("Tourism_y.csv") #Tourist arrivals from 2005
y<-data$y #Tourist arrivals
y_ts<-ts(y,start = 2005,frequency = 4) #Time series data
plot(y_ts,main="Tourist Arrivals")
adf.test(y_ts) #Not stationary
kpss.test(y_ts)
diff_y<-diff(y_ts, lag=1, differences = 1) #1st Order Non Seasonal Difference
plot(diff_y, main= "1st Order Non Seasonal Difference")
adf.test(diff_y)
kpss.test(diff_y)
diff_y_1<-diff(diff_y, lag=4, differences = 1) #1st Order Seasonal Difference
plot(diff_y_1,main="1st Order Seasonal Difference")
adf.test(diff_y_1)
kpss.test(diff_y_1) #Thus, stationary
acf(diff_y_1,main="ACF for SS") 
pacf(diff_y_1, main="PACF for SS") 
fit<-auto.arima(diff_y_1, seasonal = FALSE, stationary = TRUE)
summary(fit) #ARIMA(0,0,1) for stationary series
#Now for original series y
holt<-hw(y_ts)
holt
summary(holt)
plot(holt) #AIC 1297.85
#since we have ARIMA(0,0,1) for stationary series and there is no 
#seasonal AR and seasonal MA dependence, Stationary series is 
#obtained by first order non seasonal and first order seasonal 
#difference, we get ARIMA (p, d, q) (p', d', q') for original series
#as ARIMA (0,1,1)(0,1,0) 
arima<-arima(y_ts,order = c(0,1,1),seasonal=list(order=c(0,1,0),period=4))
arima #AIC 1109.19
summary(arima)
plot(arima,type="l",col="blue")
#Thus ARIMA model has the least AIC followed
#by Holt Winters
de<-read.csv("Tourism_rearranged.csv")
de_ts<-ts(de,start = 2005,frequency = 4)
pca<-de_ts[,-c(1,2)] #preparing data for PCA
pca.de<-PCA(pca,scale.unit = T) #PCA 
summary(pca.de)
#KMO is sampling adequacy test. If value < 0.5, sample is not ideal to perform analysis
#Barlett's test of sphericity,
#Ho: Rho=I
#We want to reject the null hypothesis, p-value < 5%
#Both conditions satisfied, then only we can go ahead with Factor Analysis
KMO(pca) #P-value is 0.5 
cortest.bartlett(pca) #P-value is 0
#THus, we perform FA
fviz_eig(pca.de) #Scree plot, select 2 factors 
cor_data<-cor(pca) #Correlation matrix
cor<-data.frame(cor_data)
cory<-cor$y
tail(sort(cory))
principal(cor_data)
fa_data<-fa(cor_data, nfactors = 2, n.obs = 42, rotate = "varimax", scores = "regression")
print(fa_data, sort=TRUE) 
fa_data$factors #Number of factors extracted
fa_data$loadings 
fa_data$e.values 
fa_data$values
fac_scor<-factor.scores(pca, fa_data)
fac_scor 
round(cor(fac_scor$scores),1) 
#2 variables having the highest correlation with y are f13 f41
#Thus arimax model with f13 and f41 as xregs and y is as follows 
#1. by splitting into train and test
f13_ts<-ts(de$f13,start = 2005,frequency = 4)
f41_ts<-ts(de$f41,start = 2005,frequency = 4)
f<-cbind(f13_ts,f41_ts)
split_y<-splitTrainTest(y_ts)
trainy<-split_y$train
testy<-split_y$test
split_f13<-splitTrainTest(f13_ts)
split_f41<-splitTrainTest(f41_ts)
trainx<-cbind(split_f13$train,split_f41$train)
testx<-cbind(split_f13$test,split_f41$test)
arima_x<-Arima(trainy, order = c(0,1,1),seasonal = list(order=c(0,1,0),period=4),xreg = trainx)
summary(arima_x) #AIC=846.47
plotarimapred(y_ts,arima_x,xreg=testx,xlim = c(2005,2017))
#2. by forecasting x
k13<-forecast(f13_ts,h=20)
b<-k13$mean     
b         
k41<-forecast(f41_ts,h=20)
c<-k41$mean     
c           
x1<-as.vector(b)         
x1         
x2<-as.vector(c)         
x2         
x<-cbind(x1,x2)         
arimax<-Arima(y_ts,order = c(0,1,1),seasonal=list(order=c(0,1,0),period=4),xreg=f)
summary(arimax) #AIC=1093.42 
plotarimapred(y_ts,arimax,xreg = x,xlim = c(2005,2022))
predict(arimax,n.ahead = 20,newxreg = x)
predict(arima_x,n.ahead = 30,newxreg = testx)











