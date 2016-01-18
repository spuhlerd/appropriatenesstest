# A set of functions used to define the requirement and capacity ranges for the technology/case attributes

req.range <- Vectorize (function(x, lower=-Inf, upper=Inf){
  # a uniform function being 1 for lower>x>upper, else 0
  ifelse(x>=lower & x<=upper, 1, 0)
  }, 'x')

req.trapez <- Vectorize (function(x, a, b=(d-a)/2+a, c=b, d){
  # a trapezoidal function of max 1 starting at a, ending at d and having mode1=b, mode2=c
  #if c is not defined, it is equal to a traiangual function
  #if either b or c are defined, it defines a triangular distribution around the mean
  approx(x=c(a,b,c,d), y=c(0,1,1,0), xout=x, rule=2)$y
}, 'x')

