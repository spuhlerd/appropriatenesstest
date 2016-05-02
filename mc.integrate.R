mc.integrate<-function(case.app.fun, tech.app.fun, n.sample=10000){
  # This functions computes a monte carlo integration (integration by sampling) of two continous functions
    # it samples the density function and then computes the probability funcitons with the sampled values
    # the return value is score=mean(probability functions of (samples))
  # case.app.fun: the attribute specific appropriateness functions for a case, e.g. funciton(x){dtriangle(x, a = 0, b = 6, c = 2)}
  # tech.app.fun: same as above for technologies
  # sample.fun: function that replaces all density functions d... 
  #     (e.g. dtriangle) with random sample generation functions
  # score: appropriateness score calculated not with integration but with with sampling
  
  sample.fun <- function(f.string,n.sample) {
    substring(f.string, 1, 1) <- "r"      # replace first letter with "r"
                                          # this create a r function instead of a density functon
                                          # e.g. dtriangle to rtriangle : generates n random deviates
                                          # Will return x values with frequency proportional to the density function values
    x<-n.sample                           # this will set the number of evaluation of the following eval statement (assumes the function is define with x) 
    eval(parse(text = f.string))          # Evaluate the function x times
  }
  
  ff1=deparse(case.app.fun)[3] # isolate the function string from the app.fun
  ff2=deparse(tech.app.fun)[3]
  ff1<-sub("[[:space:]]+", "",ff1) # remove spaces from the string
  ff2<-sub("[[:space:]]+", "",ff2)
  if (substring(ff1, 1, 1)=="d"){ # check wich of the functions is the density one
    samples<-sample.fun(ff1,n.sample) # make samples from the density function
    score=mean(tech.app.fun(samples)) # 
  }else{
    samples<-sample.fun(ff2,n.sample) 
    score=mean(case.app.fun(samples))}
  return(score)
}

