setwd("/Users/dorotheespuhler/Dropbox\ (Personal)/PHD\ Dropbox/1\ MODELLING/R/Appropriateness/")

#------------------------------------------------------------------------------------------------------------------------
#PORBABILITY DENSITIES


# 4 figures arranged in 4 rows and 2 columns
par(mfrow=c(4, 2))

#uniform
#curve(dunif(x, min = 0, max = 1, log = FALSE))
curve(dunif(x, min = 0, max = 1), from=-1, to=5, add=NA, ylab=NA, xlab = NA, main="dunif(x, min = 0, max = 1)", ylim=c(0,1))
curve(punif(x, min = 0, max = 1), from=-1, to=5, col="blue", add=T, ylab=NA, xlab = NA, ylim=c(0,1))

#traingular
library(triangle)
#plot(function(x) dtriangle(x, a=1, b=4, c=1.1), 0, 10)
curve(dtriangle(x, a=1, b=4, c=1.8), from=-1, to=5, add=NA, ylab=NA, xlab = NA, main="dtriangle(x, a=1, b=4, c=1.8)", ylim=c(0,1))
curve(ptriangle(x, a=1, b=4, c=1.8), from=-1, to=5, col="blue", add=T, ylab=NA, xlab = NA, ylim=c(0,1))


#normal
#plot(function(x) dnorm(x, mean = 0, sd = 1, log = FALSE), -1, 1, main="normal, mean=0, sd=1")
#par(new=T)
#plot(function(x) dnorm(x, mean = 0, sd = 0.25, log = FALSE), -1, 1, main="normal, mean=0, sd=1",xlab='', ylab='', axes=F)
#par(new=T)
#plot(function(x) dnorm(x, mean = 0, sd = 100, log = FALSE), -1, 1, main="normal, mean=0, sd=1",xlab='', ylab='', axes=F)
curve(dnorm(x, mean = 0, sd = 0.6), from=-2, to=5, add=NA, ylab=NA, xlab = NA, main="dnorm(x, mean = 0, sd = 0.6)", ylim=c(0,1))
curve(pnorm(x, mean = 0, sd = 0.6), from=-2, to=5, col="blue", add=T, ylab=NA, xlab = NA, ylim=c(0,1))


#lognorm
#plot(function(x) dlnorm(x, mean = 0, sd = 1, log = FALSE), 0, 10, main="lognormal, mean=0, sd=1")
curve(dlnorm(x, mean = 2, sd = 1), from=-1, to=40, add=NA, ylab=NA, xlab = NA, main="dlnorm(x, mean = 1, sd = 1)", ylim=c(0,1))
curve(plnorm(x, mean = 2, sd = 1), from=-1, to=40, col="blue", add=T, ylab=NA, xlab = NA, ylim=c(0,1))

#logistic
#plot(function(x) dlogis(x, location = 0, scale = 1), -10, 10, main="logistic, location=0, scale=1")
curve(dlogis(x, location = 1, scale = 0.25), from=-2, to=5, add=NA, ylab=NA, xlab = NA, main="dlogis(x, location = 1, scale = 0.25)", ylim=c(0,1))
curve(plogis(x, location = 1, scale = 0.25), from=-2, to=5, col="blue", add=T, ylab=NA, xlab = NA, ylim=c(0,1))

#gamma
#plot(function(x) dgamma(x, shape=1, log = FALSE))
curve(dgamma(x, shape=1, scale=1), from=-1, to=5, add=NA, ylab=NA, xlab = NA, main="dgamma(x, shape=1, scale=1)", ylim=c(0,1))
#rate=1/scale
curve(pgamma(x, shape=1, scale=1), from=-1, to=5, col="blue", add=T, ylab=NA, ylim=c(0,1))

#beta
#plot(function(x) dbeta(x, shape1=1, shape2=2, ncp = 0, log = FALSE))
curve(dbeta(x, shape1=2, shape2=3, ncp = 0), from=-1, to=5, add=NA, ylab=NA, xlab = NA, main="dbeta(x, shape1=1, shape2=2, ncp = 0)", ylim=c(0,1.75))
curve(pbeta(x, shape1=2, shape2=3, ncp = 0), from=-1, to=5, col="blue", add=T, ylab=NA, xlab = NA, ylim=c(0,1.75))

#weibull
curve(dweibull(x, shape = 1.5, scale = 1, log = FALSE), from=-1, to=5, add=NA, ylab=NA, xlab = NA, main="dweibull(x, shape = 1.5, scale = 1, log = FALSE)", ylim=c(0,1))
curve(pweibull(x, shape = 1.5, scale = 1, log = FALSE), from=-1, to=5, col="blue", add=T, ylab=NA, xlab = NA, ylim=c(0,1))


par(mfrow=c(1, 1))


#------------------------------------------------------------------------------------------------------------------------
#INTERPOLATIONS

#linear interpolation
y1<-c(0,0.5,1,1,0.5,0)
x1<-c(5,10,25,30,50,60)
f1<-approxfun(x=x1,y=y1, method = "linear", yleft=0, yright=0, rule = 1:1)
curve(f1, from=-5, to=80, add=NA, ylab=NA, xlab = NA, main="approxfun(x=x1,y=y1, method = linear, yleft=0, yright=0, rule = 1:1)", ylim=c(0,1))

#spline interpolation
y1<-c(0,0.5,1,1,0.5,0)
x1<-c(5,10,25,30,50,60)
f1<-splinefun(x=x1,y=y1, method = c("fmm", "periodic", "natural", "monoH.FC", "hyman"), ties = mean)
curve(f1, from=-5, to=80, add=T, ylab=NA, xlab = NA, main="approxfun(x=x1,y=y1, method = linear, yleft=0, yright=0, rule = 1:1)")
#------------------------------------------------------------------------------------------------------------------------
#FITTING
x.fit<- c(c_tmin, c_tmax, c_tmean)
x
hist(x.fit,main="Histogram of observed data")
plot(density(x.fit),main="Density estimate of data")
plot(ecdf(x.fit), main="Empirical cumulative distribution function")



#------------------------------------------------------------------------------------------------------------------------
#NOTES
my.data<-rnorm(n=200, m=10, sd=2)
hist(my.data, main="hist of my.data")
par(new=T)
plot(density(my.data), main="desnity estimation of my data")
plot(ecdf(my.data), main="empircal cumulative distribution functions of my.data")
sd(my.data)
my.data.norm<-(my.data-mean(my.data))/sd(my.data)
qqnorm(my.data.norm)
abline(0,1)

#------------------------------------------------------------------------------------------------------------------------

test<-curve(dnorm(x, mean = 2, sd = 1), from=-1, to=5)
test

auc(test, min = -1, max = 5)


require(sfsmisc)
integrate.xy(curve(dbeta(x, shape1=2, shape2=3, ncp = 0), from=-1, to=5, add=NA, ylab=NA, xlab = NA, main="dbeta(x, shape1=1, shape2=2, ncp = 0)", ylim=c(0,2)))


x1<-rlnorm(50)
x1
NORMCURVE<-dlnorm(x1, mean = 2, sd = 1)
plot(NORMCURVE)
auc(NORMCURVE)
auc(dlnorm(x1, mean = 2, sd = 1))
test<-curve(dnorm(x, mean = 2, sd = 1), from=-1, to=5)

data(churn)
churn$predictions
sensitivity(churn$predictions,churn$labels)
auc(sensitivity(churn$predictions,churn$labels))



x=c(0,1,2,3,4,5,6,7,8,9,10)
y<-dnorm(function(x), mean = 0, sd = 2)
plot(x,y)
auc(x,y)
auc(y)




