compute.techapp= function(case, tech, lshowplot=FALSE){
  # This functions computes the attrapp.scores and the techapp.score for a tech in a given case
  # Usage
  # compute.techapp(case, tech, [lshowplot=FALSE])
  # Input:
  # tech: a technology from the technology list, e.g. techlist$septic.tank
  # case: a case from the case list, e.g. caselist$arbaminch
  # app.fun: both tech and cases contain app.fun, functions for each appropriateness attribute
  # Variables:
  # attrapp.score: the mc.intrgrate(tech$app.fun, case$app.fun)
  # techapp.profile: all the attrapp.core of a given tech & case
  # techapp.score: normalized product of all attrapp.scores
  # lshowplot: if TRUE plots are generated to illustrate the overall of the case and tech$app.fun
  # Output:
  # tech.app.data: list containing tech, case, techapp.score, techapp.profile (containing names(tech$app.fun), values)
  
  
  techapp.profile  = c() # create empty vector to store intermediat result
  attr.names = c() # create empty vectore to store names of used attributes
  n.tech.app.fun=length(names(tech$app.fun)) # number of attributes
  n.case.app.fun=length(names(case$app.fun)) # number of attributes
  n.app.fun=min(n.tech.app.fun,n.case.app.fun)
  
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
  
  # in case there are no attribute, n.app.fun is 0.
  # in this case the function retruns a score of 1
  if (n.app.fun==0){
    techapp.data=list()
    techapp.data$tech=techname
    techapp.data$case=casename
    techapp.data$techapp.score=1
    techapp.data$techapp.profile=as.list(c("attr"="empty"))
    return(techapp.data)
  }
  
  # parameter
  techcolor="darksalmon"
  casecolor="darkgreen"
  
  ## Prepare plot
  if(lshowplot){
    # Create multiple plot table
    par(mfcol=c(n.app.fun,1)) # dimensions of plot, the raw number is equal
                               #   to the number of appropriateness functions
    par(mar = c(4, 4, 4, 2), oma = c(2, 1, 2, 4)) # margins for plot window
  }
  
  for(attr in names(tech$app.fun)){
    #check that this attribute also exist in case$app.fun
    if (attr %in% names(case$app.fun)){
      # Store attribute names
      attr.names=c(attr.names,attr)
      # Calculate app.score
      f1 = tech$app.fun[[attr]]
      f2 = case$app.fun[[attr]]
      attrapp.score = mc.integrate(f1,f2)
      techapp.profile = c(techapp.profile, attrapp.score)
      
      # Plots
      if(lshowplot){
        # define plot xlim using max value
        maxxlim=40000 # max possible value
        xval=seq(0,maxxlim,1) # vector of values to evaluate the last non-zero point in the intevall 1:maxxlim
        # only used for plotting as not precise because uses only integer values
        # for functions with max smaller than 1, put 0.1 as intervall in seq
        techval=tech$app.fun[[attr]](xval) # computing tech$app.fun for xval
        caseval=case$app.fun[[attr]](xval) # computing case$app.fun for xval
        Xmaxtech=xval[max(which(techval>0))]   # find the highest xval for which techval is nonzero
        # in case of warning message "In max(which(techval > 0)) : no non-missing arguments to max; returning -Inf", you need to increase maxlim
        if (Xmaxtech>=maxxlim) Xmaxtech=0.0001 # if tech$app.fun is const until Inf set Xmaxtech to very small in order not to be considered
        Xmaxcase=xval[max(which(caseval>0))]   # find the highest xval for which caseval is nonzero
        # in case of warning message "In max(which(caseval > 0)) : no non-missing arguments to max; returning -Inf", you need to increase maxlim
        if (Xmaxcase>=maxxlim) Xmaxcase=0.0001 # if tech$app.fun is const until Inf set Xmaxcase to very small in order not to be considered
        Xmaxplot=max(Xmaxtech,Xmaxcase)+1 # set the higher of Xmaxcase and Xmactech as Xmax for plots
        # plot
        plot(tech$app.fun[[attr]], main=attr, xlab="x", ylab="tech.app.fun", xlim=c(0,Xmaxplot), col=techcolor)
        cex_label= par("cex")*par("cex.lab")
        par(new = T)
        plot(case$app.fun[[attr]], col=casecolor, axes = FALSE, xlab = "x", ylab = "",xlim=c(0,Xmaxplot))
        axis(side = 4)
        mtext("case.app.fun", side = 4, line=3,cex = cex_label)
      }
    }
  }
  
  ## Make multiplot title and legend
  if(lshowplot) {
    mtext(paste(techname,", ",casename), outer = TRUE )
    #place text in the lower right corner of the graphic ("legend")
    mtext("- tech.app.fun                             ", col=techcolor, outer=TRUE, side=1, adj=1, line=-1, cex=cex_label)
    mtext("- case.app.fun", col=casecolor, outer=TRUE, side=1, adj=1, line=-1, cex=cex_label)
  }
  
  # Compute total score
  l=length(techapp.profile)
  techapp.score=(prod(techapp.profile))^(1/l) # the normlized product of all attrapp.scores
  
  ## Create datalist
  techapp.profile=setNames(techapp.profile,attr.names)
  techapp.data=list(case=casename, tech=techname, techapp.score=techapp.score, techapp.profile=as.list(techapp.profile))
  techapp.data
}

