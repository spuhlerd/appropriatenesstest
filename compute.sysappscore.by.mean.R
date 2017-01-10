compute.sysappscore.by.mean= function(syslist_demo,applist_demo){
  
#get cases names  
  cases=names(caselist)
#start a loop over each case  
  ll=list() 
  for(g in 1:length(sapply(names(caselist),length))){
#initiate vektors which are filled during the loop   
    sysapp.score=c()
    systechapp.score=c()
    sysattrapp.score=c()
    sysapp.score.tot=c()
    ll.sys=list()
    ll.case=list()
    ll.sys2=list()
    ll.case=cases[g]
    #start a loop over all systems enbeded in syslist
    for(j in 1:length(sapply(syslist_demo,length))){
      
##################################   system attribute appropriatness score
            
#initiate vektors which are filled during the loop     
     sysattrapp.profile=c() #vektor containing the system atrribute appropriatness scores
     sysattrapp.profile.summary=list()
     systechapp.profile.summary=list()
#calculating the system attribute appropriatness score for each system attribute   
      for(attr in names(syslist_demo[[j]]$app.fun)){
        if(attr %in% names(caselist[[g]]$app.fun)){
          
          f1 = syslist_demo[[j]]$app.fun[[attr]]
          f2 = caselist[[g]]$app.fun[[attr]]
          attrapp.score = mc.integrate(f1,f2)
          sysattrapp.profile = c(sysattrapp.profile, attrapp.score)
          sysattrapp.profile.summary[[attr]]=attrapp.score
        }
      }
#calculating the total system attribute appropriatness score by mean
      sysattrapp.score.by.mean=mean(sysattrapp.profile)
      
################################  technology appropriatness score 
      
      techapp.score.by.mean=c()
# calculating the technology appropriatness score    
      for(i in 1:length(sapply(applist_demo,length))){ 
        
        if(applist_demo[[i]]$case == cases[[g]] &                                     #guckt ob der case der gleich ist
           applist_demo[[i]]$tech %in% names(syslist_demo[[j]]$techs)){              #guckt, ob die technologie im system vorhanden ist

          techattrapp.profile=c()
          
          for(attr in names(techlist[[applist_demo[[i]]$tech]]$app.fun)){               #sucht attribute der technologie 
# Check that this attribute also exist in case$app.fun, otherwise skip
            if (attr %in% names(caselist[[applist_demo[[i]]$case]]$app.fun)){           #guckt, ob dieses attribut auch im case file vorhanden ist
# Calculate app.score
              f1 = techlist[[applist_demo[[i]]$tech]]$app.fun[[attr]]
              f2 = caselist[[applist_demo[[i]]$case]]$app.fun[[attr]]
              attrapp.score = mc.integrate(f1,f2)
              techattrapp.profile = c(techattrapp.profile, attrapp.score)
            }
          }
# Compute total technology appropriatness score 
          techapp.score.by.mean[i]=mean(techattrapp.profile) # the mean of all attrapp.scores is the techapp.score
           systechapp.profile.summary[[applist_demo[[i]]$tech]]=techapp.score.by.mean[i]
           techapp.score.by.mean <- techapp.score.by.mean[!is.na(techapp.score.by.mean)] 
          }
      }
      systechapp.score.by.mean=mean(techapp.score.by.mean)

###################################      
      
# calculating the total system attribute appropriatness score by mean
    sysapp.score[[j]]=sysattrapp.score.by.mean*systechapp.score.by.mean   #combining system technology appropriatness score and system 
                                                          #attribute appropriatness score inro one score by product
#ceating a list containing the system appropriatnes scores for each system and case (one list for each case)  
    ll.sys2$Case=cases[g]
    ll.sys2$System=names(syslist_demo)[[j]]
    ll.sys2$sysappscore=sysapp.score[[j]]
    ll.sys2$sysattrapp.profile=sysattrapp.profile.summary
    ll.sys2$systechapp.profile=systechapp.profile.summary
    ll.sys[[j]]=ll.sys2
    ll[[ll.case]]=ll.sys #writing the list of each case into a global list containing all cases
    }
    # creating a pdf plot 
    pdf(file=paste0(getwd(),"/plots/","System appropriatnes score (by mean)  ",cases[[g]],".pdf"))
    barplot(sysapp.score, main=cases[g], ylim= c(0:1),names.arg=names(syslist_demo),
            ylab="System appropriatnes score")
    dev.off()  
  }
  ll
}