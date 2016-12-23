compute.sysapp.by.mean= function(sysliste_demo,applist_demo){
  
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
    #start a loop over all systems enbeded in syslist
    for(j in 1:length(sapply(sysliste_demo,length))){
      #initiate vektors which are filled during the loop     
      sysattrapp.profile=c() #vektor containing the system atrribute appropriatness scores
      #calculating the system attribute appropriatness score for each attribute   
      for(attr in names(sysliste_demo[[j]]$app.fun)){
        if(attr %in% names(caselist[[g]]$app.fun)){
          
          f1 = sysliste_demo[[j]]$app.fun[[attr]]
          f2 = caselist[[g]]$app.fun[[attr]]
          attrapp.score = mc.integrate(f1,f2)
          sysattrapp.profile = c(sysattrapp.profile, attrapp.score)
        }
      }
      #calculating the total system attribute appropriatness score by mean
      sysattrapp.score=mean(sysattrapp.profile)
      #calculating the technology appropriatness score    
      for(i in 1:length(sapply(applist_demo,length))){ 
        
        if(applist_demo[[i]]$case == cases[[g]] &  
           applist_demo[[i]]$tech %in% names(sysliste_demo[[j]]$techs)){
          
          systechapp.score[i]=applist_demo[[i]]$techapp.score  #gives you the techapp.score calculated my campute.techapp 
                                                              # in a vektor (product of attr scores)   
          ###!!!ACHTUNG !!!! techap.score wird eingelesen und daher wie in compute.techapp als product der tech attr berechnet
        }
      }
      systechapp.score2=systechapp.score[!is.na(systechapp.score)]  #deletes empty (NA) cells from vector
      #calculating the system technology appropriatness score (summarizes all technology appropriatnes scores (by mean) into one score)  
      systechapp.score=mean(systechapp.score2)
      
      sysapp.score[[j]]=(sysattrapp.score+systechapp.score)/2   #combining system technology appropriatness score and system 
                                                                #attribute appropriatness score inro one score by product
    }
    #creating a pdf plot 
    pdf(file=paste("/Users/dorotheespuhler/Dropbox\ (Personal)/PHD\ Dropbox/1\ MODELLING/R/Appropriateness/plots/","System appropriatnes score (by prod.)  ",cases[[g]],".pdf"))
    barplot(sysapp.score, main=cases[g], ylim= c(0:1),names.arg=names(sysliste_demo),
            ylab="System appropriatnes score")
    dev.off()  
    
    #ceating a list containing the system appropriatnes scores for each system and case (one list for each case)  
    ll.sys=list()
    ll.case=list()
    ll.case=cases[g]
    for(e in 1:length(sapply(sysliste_demo,length))){
      ll.sys[[e]]=paste(names(sysliste_demo)[[e]],"appscore=",sysapp.score[[e]])
    }
    ll[[ll.case]]=ll.sys #writing the list of each case into a global list containing all cases
  }
  ll
}
