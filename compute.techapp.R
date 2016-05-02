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
  
  # parameter
  techcolor="green"
  casecolor="red"
  
  ## Prepare plot
  if(lshowplot){
    # Create multiple plot table
    par(mfcol=c(n.app.fun,1)) # dimensions of plot, the raw number is equal
    #   to the number of appropriateness functions
    par(mar = c(4, 4, 4, 2), oma = c(2, 1, 2, 4))
  }
  
  for(attr in names(tech$app.fun)){
   
     # Calculate app.score
    f1 = tech$app.fun[[attr]]
    f2 = case$app.fun[[attr]]
    attrapp.score = mc.integrate(f1,f2)
    techapp.profile = c(techapp.profile, attrapp.score)
    
    # Plot techapp.profile
    if(lshowplot){
      # define plot xlim using max value
      maxxlim=10000 #max possible vaule
      xval=c(1:maxxlim)
      techval=tech$app.fun[[attr]](xval)
      caseval=case$app.fun[[attr]](xval)
      Xmaxtech=max(which(techval>0))   #find the largest x absisse corresponding to the max
      if (Xmaxtech==maxxlim) Xmaxtech=1 # special treatment for step function const until inf
      Xmaxcase=max(which(caseval>0))   #find the largest x absisse corresponding to the max
      if (Xmaxcase==maxxlim) Xmaxcase=1 # special treatment for step function const until inf
      Xmaxplot=max(Xmaxtech,Xmaxcase)+10
      
      plot(tech$app.fun[[attr]], main=attr, xlab="x", ylab="tech.app.fun", xlim=c(0,Xmaxplot), col=techcolor)
      cex_label= par("cex")*par("cex.lab")
      par(new = T)
      plot(case$app.fun[[attr]], col=casecolor, axes = FALSE, xlab = "x", ylab = "",xlim=c(0,Xmaxplot))
      axis(side = 4)
      mtext("case.app.fun", side = 4, line=3,cex = cex_label)
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
  
  ## Make graph title and legend
  if(lshowplot) {
    mtext(paste(techname,", ",casename), outer = TRUE )
    #place text in the lower right corner of the graphic
    mtext("- tech.app.fun                             ", col=techcolor, outer=TRUE, side=1, adj=1, line=-1, cex=cex_label)
    mtext("- case.app.fun", col=casecolor, outer=TRUE, side=1, adj=1, line=-1, cex=cex_label)
   }
  # Compute total score
  l=length(techapp.profile)
  techapp.score=(prod(techapp.profile))^(1/l) # the normlized product of all attrapp.scores
  
  ## Create datalist
  techapp.profile=setNames(techapp.profile,names(tech$app.fun))
  app.data=list(case=casename, tech=techname, techapp.score=techapp.score, techapp.profile=as.list(techapp.profile))
  app.data
}

