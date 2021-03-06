sysapplist.write=function(sysapplist_demo.prod, listsep=" ", filename=""){
  # Description
  # Write applist to the screen or file
  # Usage
  # applist.write(applist,listsep=" ",filename="")
  # Input:
  # applist: list of appropriatness technologies as generated by app.compute.R 
  # listsep: separato, default=" "
  # filename: output file, if empty string (""), write to standard output, default=""
  
  # auxiliar values
  profilesep=", " #separator for sysapp.profile
  ndigit=2     #number of digits

  #Set output mode
  if (filename!="") sink(file=filename, append=FALSE) #redirect cat to file
  
  #Write header
  header=paste("Case     ", listsep, "System ",listsep, "sysappscore", listsep, "sysattrapp.profile                           ", listsep, "systechapp.profile","\n")
  cat(header)
  
  
  #Loop over all element
  for(g in names(caselist)){
  for (id in 1:length(sysapplist_demo.prod[[g]])){
    elem=sysapplist_demo.prod[[g]][[id]]

    #prepare sysattrapp.profile list
    profile1=elem$sysattrapp.profile
    profilestr1=""     #string to write techapp.profile
    for (pid in 1:length(profile1)){
      if (pid==1){
        profilestr1=paste(names(profile1[pid]),"=",format(profile1[pid],digits=ndigit)) #no separtor for first element
      }else{
        profilestr1=paste(profilestr1,profilesep,names(profile1[pid]),"=",format(profile1[pid],digits=ndigit))
      }
    }
    #prepare systechapp.profile list
    profile2=elem$systechapp.profile
    profilestr2=""     #string to write techapp.profile
    for (pid in 1:length(profile2)){
      if (pid==1){
        profilestr2=paste(names(profile2[pid]),"=",format(profile2[pid],digits=ndigit)) #no separtor for first element
      }else{
        profilestr2=paste(profilestr2,profilesep,names(profile2[pid]),"=",format(profile2[pid],digits=ndigit))
      }
    }
    
    
    #Combine print
    lineout=paste(format(elem$Case,width=10),listsep,
                  format(elem$System,width=4),listsep,
                  format(elem$sysappscore,digits=ndigit,width=10),listsep,
                  format(profilestr1,digits=ndigit,width=45),listsep,
                  format(profilestr2,digits=ndigit,width=0),listsep,
                  
             "\n")
    cat(lineout)
  }
  }
  if (filename!="") sink(file=NULL) #stop redirection
}
