compute.sysappscore.by.product= function(syslist_demo,applist_demo){

#get cases names  
    cases=names(caselist)
#start a loop over each case  
    ll=list() 
    for(g in 1:length(sapply(names(caselist),length))){
#initiate vektors and lists which are filled during the loop   
    sysapp.score=c()
    techapp.score=c()
    systechapp.score=c()
    sysattrapp.score=c()
    sysapp.score.tot=c()
    ll.sys=list()
    ll.case=list()
    ll.sys2=list()
    ll.case=cases[g]
#start a loop over all systems enbeded in syslist
    for(j in 1:length(sapply(syslist_demo,length))){
#initiate vektor which are filled during the loop     
    sysattrapp.profile=c() #vektor containing the system atrribute appropriatness scores
    sysattrapp.profile.summary=list()
    systechapp.profile.summary=list()
###calculating the system attribute appropriatness score for each system attribute   
    for(attr in names(syslist_demo[[j]]$app.fun)){
     if(attr %in% names(caselist[[g]]$app.fun)){
      f1 = syslist_demo[[j]]$app.fun[[attr]]
      f2 = caselist[[g]]$app.fun[[attr]]
      attrapp.score = mc.integrate(f1,f2)
      sysattrapp.profile = c(sysattrapp.profile, attrapp.score)
      sysattrapp.profile.summary[[attr]]=attrapp.score
     }
    }
#calculating the total system attribute appropriatness score by product  
    l.sysattr=length(sysattrapp.profile)
    sysattrapp.score=(prod(sysattrapp.profile))^(1/l.sysattr)
    
###calculating the technology appropriatness score    
    for(i in 1:length(sapply(applist_demo,length))){ 
  
    if(applist_demo[[i]]$case == cases[[g]] &  
    applist_demo[[i]]$tech %in% names(syslist_demo[[j]]$techs)){

    techapp.score[i]=applist_demo[[i]]$techapp.score  #gives you the techapp.score calculated my campute.techapp 
                                                        # in a vektor (product of attr scores) 
    systechapp.profile.summary[[applist_demo[[i]]$tech]]=techapp.score[i]
    }
    }
    systechappscore.profile=techapp.score[!is.na(techapp.score)]  #deletes empty (NA) cells from vector
    #calculating the system technology appropriatness score (summarizes all technology appropriatnes scores (by product) into one score)  
    l.systech=length(systechappscore.profile)
    systechapp.score=(prod(systechappscore.profile))^(1/l.systech)
#combining system technology appropriatness score and system attribute appropriatness score inro one score by product
     sysapp.score[[j]]=sysattrapp.score*systechapp.score   
#ceating a list containing the system appropriatnes scores for each system and case (one list for each case)  
    ll.sys2$Case=cases[g]
    ll.sys2$System=names(syslist_demo)[[j]]
    ll.sys2$sysappscore=sysapp.score[[j]]
    ll.sys2$sysattrapp.profile=sysattrapp.profile.summary
    ll.sys2$systechapp.profile=systechapp.profile.summary
    ll.sys[[j]]=ll.sys2
    ll[[ll.case]]=ll.sys #writing the list of each case into a global list containing all cases
    }
#creating a pdf plot 
    pdf(file=paste0(getwd(),"/plots/","System appropriatnes score (by prod.)  ",cases[[g]],".pdf"))
    barplot(sysapp.score, main=cases[g], ylim= c(0:1),names.arg=names(syslist_demo),
          ylab="System appropriatnes score")
    dev.off()
    
    }
    ll
    }