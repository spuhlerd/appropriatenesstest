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
      
##################################   system attribute appropriatness score
            
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
      sysattrapp.score.by.mean=mean(sysattrapp.profile)
      
################################  technology appropriatness score 
      
      techapp.score=c()
# calculating the technology appropriatness score    
      for(i in 1:length(sapply(applist_demo,length))){ 
        
        if(applist_demo[[i]]$case == cases[[g]] &                                     #guckt ob der case der gleich ist
           applist_demo[[i]]$tech %in% names(sysliste_demo[[j]]$techs)){              #guckt, ob die technologie im system vorhanden ist

          techapp.profile=c()
          for(attr in names(techlist[[applist_demo[[i]]$tech]]$app.fun)){               #sucht attribute der technologie 
# Check that this attribute also exist in case$app.fun, otherwise skip
            if (attr %in% names(caselist[[applist_demo[[i]]$case]]$app.fun)){           #guckt, ob dieses attribut auch im case file vorhanden ist
# Calculate app.score
              f1 = techlist[[applist_demo[[i]]$tech]]$app.fun[[attr]]
              f2 = caselist[[applist_demo[[i]]$case]]$app.fun[[attr]]
              attrapp.score = mc.integrate(f1,f2)
              techapp.profile = c(techapp.profile, attrapp.score)
            }
          }
# Compute total technology appropriatness score 
          techapp.score.by.mean=c(techapp.score, mean(techapp.profile)) # the mean of all attrapp.scores is the techapp.score
        }
      }
      systechapp.score.by.mean=mean(techapp.score.by.mean)
      
###################################      
      
# calculating the total system attribute appropriatness score by mean
    sysapp.score[[j]]=sysattrapp.score.by.mean*systechapp.score.by.mean   #combining system technology appropriatness score and system 
                                                          #attribute appropriatness score inro one score by product
    }
# creating a pdf plot 
    pdf(file=paste("C:/Users/Joel/Desktop/Joel model/Plots/","System appropriatnes score (by mean)  ",cases[[g]],".pdf"))
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
