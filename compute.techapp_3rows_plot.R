compute.techapp= function(case, tech, lshowplot=FALSE){
  # This functions computes the attrapp.scores and the techapp.score for a tech in a given case
  # Usage
  # compute.techapp(case,tech, [lshowplot=FALSE])
  # Input:
  # tech: a technology from the technology list, e.g. techlist$septic.tank
  # case: a case from the case list, e.g. arbaminch
  # app.fun: both tech and cases contain app.fun, functions for each appropriateness attribute
  # Variables:
  # attrapp.score: the mc.intrgrate(tech$app.fun*caseapp.fun)
  # techapp.profile: all the attrapp.core of a given tech & case
  # techapp.score: normalized product of all attrapp.scores
  # lshowplot: 
  # Output:
  # app.data: list containing tech, case, techapp.score, techapp.profile (containing names(tech$app.fun), values)
  
  
  techapp.profile = c() # create empty vector to store intermediat result
  
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
    #attrapp.score = integrate(fprod, -Inf, Inf)$value
    attrapp.score = mc.integrate(f1,f2,1000)
    techapp.profile = c(techapp.profile, attrapp.score)
    
    # Plot techapp.profile
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
  l=length(techapp.profile)
  techapp.score=(prod(techapp.profile))^(1/l) # the normlized product of all attrapp.scores
  
  ## Create datalist
  techapp.profile=setNames(techapp.profile,names(tech$app.fun))
  app.data=list(case=casename, tech=techname, techapp.score=techapp.score, techapp.profile=as.list(techapp.profile))
  app.data
}

