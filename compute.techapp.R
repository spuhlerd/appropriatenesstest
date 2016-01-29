compute.techapp= function(case, tech, lshowplot=FALSE){
  # Description
  # Attr: attribute
  # This function computes:
  # attr.app.score for all attr: the integral of tech$app.fun*case.app.fun
  # tech.app.profile: all the attr.app.core of a given tech & case
  # tech.app.score: product of tech.app.profile(i)
  # Usage
  # compute.techapp(technology,case, [lshowplot=FALSE])
  # Input:
  # tech: a technology from the technology list, e.g. techlist$septic.tank
  # case: a case from the case list, e.g. arbaminch
  # Output:
  # app.data: list containing tech, case, tech.app.score, tech.app.profile (containing names(tech$app.fun), values)
  
  tech.app.profile = c() # create empty vector to store intermediat result
  
  n.app.fun=length(names(tech$app.fun)) # number of attributes
  
  ## Prepare plot
  if(lshowplot){
    # Create multiple plot table
    n.plot.row=3 #number of rows
    par(mfcol=c(n.plot.row, n.app.fun)) # dimensions of plot, the col number is equal
    #   to the number of appropriateness functions
    par(mar = c(1, 4, 2, 1), oma = c(2, 1, 2, 1))
    ylabel1='Tech' 
    ylabel2='Case'
    ylabel3='Product'
  }
  
  for(attr in names(tech$app.fun)){
   
     # Calculate app.score
    f1 = tech$app.fun[[attr]]
    f2 = case$app.fun[[attr]]
    fprod = function(x) {f1(x) * f2(x)}
    #attr.app.score = integrate(fprod, -Inf, Inf)$value
    attr.app.score = mc.integrate(f1,f2)
    tech.app.profile = c(tech.app.profile, attr.app.score)
    
    # Plot tech.app.profile
    if(lshowplot){
      plot(tech$app.fun[[attr]], main=attr, xlab="", ylab=ylabel1, xlim=c(0,2000), col=12)
      plot(case$app.fun[[attr]], xlab="", ylab=ylabel2, xlim=c(0,2000), col=475)
      plot(fprod, xlab="", ylab=ylabel3, xlim=c(0,2000), col=642)
      # Erase ylabel o that they do not appear for the other attr
      ylabel1=''
      ylabel2=''
      ylabel3=''
      
    }
  }
  
  ## Get tech name and case name
  # From the list if it exists
  if ( ("casename" %in% names(case)) && ("techname" %in% names(tech) ) ){
    techname=tech$techname
    casename=case$casename
  }else{
  # Form the function call argument otherwise (assuming call with case$casename)
    techname=unlist(strsplit(deparse(substitute(tech)),"\\$"))
    techname=techname[2]
    casename=unlist(strsplit(deparse(substitute(case)),"\\$"))
    casename=casename[2]
  }
  
  ## Make graph title
  if(lshowplot) mtext(paste(techname,", ",casename), outer = TRUE )
  
  # Compute total score
  l=length(tech.app.profile)
  tech.app.score=(prod(tech.app.profile))^(1/l)
  
  ## Create datalist
  tech.app.profile=setNames(tech.app.profile,names(tech$app.fun))
  app.data=list(case=casename, tech=techname, tech.app.score=tech.app.score, tech.app.profile=as.list(tech.app.profile))
  app.data
}

