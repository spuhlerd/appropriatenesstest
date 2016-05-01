# A set of functions used to define the requirement and capacity ranges for the technology/case attributes

## ==============================================================================================
# Range function

prange <- Vectorize (function(x, lower=-Inf, upper=Inf){
  # a uniform function being 1 for lower>x>upper, else 0
  ifelse(x>=lower & x<=upper, 1, 0)
}, 'x')

drange <- Vectorize (function(x, lower=-Inf, upper=Inf){
  ifelse(x>=lower & x<=upper, 1, 0)
}, 'x')

rrange <- Vectorize (function(x, lower=-Inf, upper=Inf){
  ifelse(x>=lower & x<=upper, 1, 0)
}, 'x')

# old
req.range <- Vectorize (function(x, lower=-Inf, upper=Inf){
  # a uniform function being 1 for lower>x>upper, else 0
  ifelse(x>=lower & x<=upper, 1, 0)
}, 'x')

## ==============================================================================================
# Trapez function

ptrapez <- Vectorize (function(x, a, b=(d-a)/2+a, c=b, d){
  # a trapezoidal function of max 1 starting at a, ending at d and having mode1=b, mode2=c
  #if c is not defined, it is equal to a traiangual function
  #if either b or c are defined, it defines a triangular distribution around the mean
  if (c==d){d=d+0.0001}
  if (a==b){b=b+0.0001}
  approx(x=c(a,b,c,d), y=c(0,1,1,0), xout=x, rule=2)$y
}, 'x')

dtrapez <- Vectorize (function(x, a, b=(d-a)/2+a, c=b, d){
  if (c==d){d=d+0.0001}
  if (a==b){b=b+0.0001}
  approx(x=c(a,b,c,d), y=c(0,1,1,0), xout=x, rule=2)$y
}, 'x')

rtrapez <- Vectorize (function(x, a, b=(d-a)/2+a, c=b, d){
  if (c==d){d=d+0.0001}
  if (a==b){b=b+0.0001}
  approx(x=c(a,b,c,d), y=c(0,1,1,0), xout=x, rule=2)$y
}, 'x')

#old
req.trapez <- Vectorize (function(x, a, b=(d-a)/2+a, c=b, d){
  # a trapezoidal function of max 1 starting at a, ending at d and having mode1=b, mode2=c
  #if c is not defined, it is equal to a traiangual function
  #if either b or c are defined, it defines a triangular distribution around the mean
  if (c==d){d=d+0.0001}
  if (a==b){b=b+0.0001}
  approx(x=c(a,b,c,d), y=c(0,1,1,0), xout=x, rule=2)$y
}, 'x')

## ==============================================================================================
# Category function, for instance for has electricity / has not
dcat <- Vectorize (function (x, probs) {
  # probs is the vector of categories and respective probabilities. E.g. c(no=0.4,yes=0.6)
  # the som of probs has to be =1
  # x is any of the categories o probs
  stopifnot(sum(probs) == 1)
  probs <- probs/sum(probs)
  probs[x]
}, 'x')

pcat <- Vectorize (function (x, probs) {
  stopifnot(probs <= 1) 
  probs[x]
}, 'x')

rcat <- Vectorize (function (x, probs) {
  sample(names(probs), x, replace=TRUE, prob=probs)
}, 'x')
