app.profile = function(tech, case){
  # Description
  # This function alculates the integral of the product (called 'score') of the tech and case function pairs for each attribute
  # Usage
  # app.profile(technology,case)
  # Input:
  # tech: a technology from the technology list, e.g. techlist$septic.tank
  # case: a case from the case list, e.g. arbaminch
  # Output:
  # app.profile
  
  profile = c()
  
  n.app.fun=length(names(tech$app.fun))
  par(mfrow=c(n.app.fun/2, 2))
  
  leg1=deparse(substitute(tech))
  leg2=deparse(substitute(case))
  leg3='Integral'
  
  for(attr in names(tech$app.fun)){
    f1 = Vectorize(tech$app.fun[[attr]])
    f2 = Vectorize(case$app.fun[[attr]])
    fint = function(x) {f1(x) * f2(x)}
    attr.app.score = integrate(fint, -Inf, Inf)$value
    profile = c(profile, attr.app.score)
    plot(tech$app.fun[[attr]], ylab=attr, xlim=c(0,2000), ylim=c(0,1.2), col=12, add=F)
    #par(new=T)
    plot(case$app.fun[[attr]], ylab=attr, xlim=c(0,2000), ylim=c(0,1.2), col=26, add=T)
    plot(fint, ylab=attr, xlim=c(0,2000), ylim=c(0,1.2), col=33,lty=2, add=T)
    legend("topright",legend=c(leg1,leg2,leg3), col=c(12,26,33))
  }
  names(profile) = names(tech$app.fun)
  profile
  #par(new=F)
}

  