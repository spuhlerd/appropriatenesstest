compute.applist= function(caselist, techlist, lsort=FALSE){
  # Description
  # This function computes for each pair of caselist$case and techlist$case:
  # attr.app.score for all attr: the integral of tech$app.fun*case.app.fun
  # tech.app.profile: all the attr.app.core of a given tech & case
  # tech.app.score: product of tech.app.profile(i)
  # Usage
  # compute.applist(caselist,techlist, [lshowplot=FALSE])
  # Input:
  # caselist: a list of cases with inforows and app.funcitons for each attributes
  # techlist: a list of potential sanitation technologies with inforows and app.functions for each attribute
  # lsort : logical, if TRUE result is sorted by score
  # Output:
  # applist: list containing case,tech, tech.app.score, tech.app.profile (containing names(tech$app.fun), values)
  
  # Create empty list
  applist=list()
  # llop over all case and tech
  for (casename in names(caselist)){
    case=caselist[[casename]]
    case$casename=casename #add casename to the list
    appsublist=list() #store all tech for a given case in a list
    for (techname in names(techlist)){
      tech=techlist[[techname]]
      tech$techname=techname #add techname to the list
      app.item.tmp <- compute.app(case, tech)
      appsublist=append(appsublist,list(app.item.tmp))
    }
    if (lsort){
      appsublist=list.sort(appsublist,(tech.app.score)) # parenthesis are telling to sort in descending order
    }
    applist=append(applist,appsublist)
  }
  #return applist
  applist
  }