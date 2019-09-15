#install.packages("openxlsx")
#install.packages("goftest")
#install.packages("fitdistrplus")
#install.packages("GoFKernel")
#install.packages("NormalLaplace")
#install.packages("metRology")
library(openxlsx)
library(fitdistrplus)
library(goftest)
library(GoFKernel)
library(NormalLaplace)
library(MASS)

#1
data_google <- read.csv("C:/Users/rohit/Documents/NCSU/ST501/Final Yr Project/GOOGL.csv", header = TRUE)
data_amazon <- read.csv("C:/Users/rohit/Documents/NCSU/ST501/Final Yr Project/AMZN.csv", header = TRUE)
a <- data_google[,2]
len_g <- nrow(data_google) - 1
returns_g <- {}
for(i in 1:len_g)
{
  returns_g[i] <- log(a[i+1]/a[i])
}

#1.a
Rg_5 <- quantile(returns_g,c(0.05))
m_Rg_5 <- {}
for (y in 1:len_g){
  if(returns_g[y] < Rg_5){
    m_Rg_5[y] <- returns_g[y]
  }
}
mean_Rg5 <- mean(m_Rg_5, na.rm = TRUE)

#1.b
plotdist(returns_g, histo = TRUE, demp = TRUE)
descdist(returns_g, discrete = FALSE, boot = 500)

#1.c
#1. logistic distribution , p-value = 0.3967
fit_1 <- fitdist(returns_g,"logis")
summary(fit_1)
ad.test(returns_g,"plogis",location = 0.000941299, scale = 0.008714979)

#2. Differentiating post log of the series - p-value = 0.9585
fit_2 <- fitdist(diff(returns_g),"logis")
summary(fit_2)
ad.test(diff(returns_g),"plogis",location = -0.0002411304, scale = 0.0122526418)

#3. return+ returns^2  - p-value = 0.502
fit_3 <- fitdist((returns_g+(returns_g)^2),"logis")
summary(fit_3)
ad.test((returns_g+(returns_g)^2),"plogis",location =  0.001030878, scale = 0.008685456)

#4. Gamma distribution (log returns^2) p-value = 0.3041
fit_4 <- fitdist((returns_g)^2,"gamma")
summary(fit_4)
ad.test((returns_g)^2,"pgamma",shape = 0.3857552,rate =1444.6790894)

#5. Beta distribution (log returns ^2) p-value = 0.3019
fit_5<- fitdist((returns_g)^2,"beta")
summary(fit_5)
ad.test((returns_g)^2,"pbeta",shape1 = 0.3858614,shape2 =1444.4539927)

#6. Cauchy Distribution -p-value = 0.06303
fit_6<- fitdist((returns_g),"cauchy")
summary(fit_6)
ad.test((returns_g),"pcauchy",location = 0.001415183,scale =0.008169676)

#7.T Distribution - p-value = 0.5939 
library("metRology")
fit_7<- fitdist(returns_g,"t.scaled", start=list(df=3,mean=mean(returns_g),sd=sd(returns_g)))
summary(fit_7)
ad.test((returns_g),"pt.scaled",df =4.005144196, mean = 0.001240968, sd = 0.012100976 )

#1.d
c_g=qt.scaled(0.05,df=fit_7$estimate[1],mean=fit_7$estimate[2],sd=fit_7$estimate[3])

ES_integral=function(x)
{
  x*dt.scaled(x,df=fit_7$estimate[1],mean=fit_7$estimate[2],sd=fit_7$estimate[3])
}

G_ES = integrate(ES_integral,lower=-Inf,upper = c_g)

# Fr(c) = 0.05 Result = G_ES/ 0.05
G_ES$value/0.05

#2
#data_amazon <- read.xlsx("C:/Users/msdhotra/Documents/AMZN.xlsx",1, header = TRUE)
c <- data_amazon[,2]
len_a <- nrow(data_amazon) - 1
returns_a <- {}
for(i in 1:len_a)
{
  returns_a[i] <- log(c[i+1]/c[i])
}

#2.a
Ra_5 <- quantile(returns_a,c(0.05))
m_Ra_5 <- {}
for (z in 1:len_a){
  if(returns_a[z] < Ra_5){
    m_Ra_5[z] <- returns_a[z]
  }
}
mean_Ra5 <- mean(m_Ra_5, na.rm = TRUE)


#2.b
plotdist(returns_a, histo = TRUE, demp = TRUE)
descdist(returns_a, discrete = FALSE, boot = 500)

#2.c
#1. logistic distribution , p-value = 0.2098
fit_a <- fitdist(returns_a,"logis")
summary(fit_a)
ad.test(returns_a,"plogis",location = 0.002561909, scale = 0.009902025)

#2. Differentiating post log of the series - p-value = 0.3929
fit_2_a <- fitdist(diff(returns_a),"logis")
summary(fit_2_a)
ad.test(diff(returns_a),"plogis",location = -0.0002572264, scale = 0.0142063325)

#3.T Distribution - p-value = 0.7502
library("metRology")
fit_3_a<- fitdist(returns_a,"t.scaled", start=list(df=3,mean=mean(returns_g),sd=sd(returns_g)))
summary(fit_3_a)
ad.test((returns_a),"pt.scaled",df =2.740838595, mean = 0.003113185, sd = 0.012043242 )


#2.d
c_a=qt.scaled(0.05,df=fit_3_a$estimate[1],mean=fit_3_a$estimate[2],sd=fit_3_a$estimate[3])

ES_integral_a=function(x)
{
  x*dt.scaled(x,df=fit_3_a$estimate[1],mean=fit_3_a$estimate[2],sd=fit_3_a$estimate[3])
}
A_ES = integrate(ES_integral_a,lower=-Inf,upper = c_a)

# Fr(c) = 0.05 Result = A_ES/ 0.05
A_ES$value/0.05


#3
data_g<-data_google[,2]
data_a<-data_amazon[,2]

# calculating Pt(w)
data_Port <- {}
abc <- {}
z <- 0
interval <- 0.02*(0:50)
for (k in 1:length(interval)){
  w <- (interval[k])
  for (n in 1:len_g){
    z <- z+1
    abc[z] <- w*data_g[n] + (1-w)*data_a[n]
  }
}
data_Port <- matrix(abc, nrow = len_g, ncol = length(interval))

# calculating the Rt(w)
returns_Port <- matrix(1:12699, nrow = 249, ncol = 51)
for (k in 1:length(interval)){
  for (n in 1:(len_g - 1)){
    returns_Port[n,k] <- log(data_Port[n+1,k]/data_Port[n,k])
  }
}

#3.a
Rport_5 <- {}
for(y in 1:length(interval)){
  Rport_5[y] <- quantile(returns_Port[,y],c(0.05))
}
plot(interval, Rport_5)


#3.b
ESport_5 <- {}
test <- {}
# Calculating ES(w)
for (i in 1:length(interval)){
  test[i] = quantile(returns_Port[1:249,i], probs = 0.05, na.rm = FALSE)
}
mean_port <- {} 
for (i in 1:length(interval)){
  ESport_5 = subset(returns_Port[,i], returns_Port[,i]<test[i])
  mean_port[i]=mean(ESport_5)
}
plot(interval,mean_port)

#3.c
# w corresponding to maximum ES(w)  max= 0.88
w_max <- interval[which.max(mean_port)]
# w corresponding to minimum ES(w) - min = 0 
w_min <- interval[which.min(mean_port)]
abline(v=c(w_max,w_min), col="red")

