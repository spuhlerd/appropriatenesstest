techapplist.write=function(techapplist, listsep=" ", filename=""){
  # Description
  # Write techapplist to the screen or file
  # Usage
  # techapplist.write(techapplist,listsep=" ",filename="")
  # Input:
  # techapplist: list of appropriatness technologies as generated by app.compute.R 
  # listsep: separato, default=" "
  # filename: output file, if empty string (""), write to standard output, default=""
  
  # auxiliar values
  profilesep=", " #separator for techapp.profile
  ndigit=5     #number of digits
  
  #Set output mode
  if (filename!="") sink(file=filename, append=FALSE) #redirect cat to file
  
  #Write header
  header=paste("case        ", listsep, "tech             ",listsep, "techapp.score", listsep, "techapp.profile","\n")
  cat(header)
  
  
  #Loop over all element
  for (id in 1:length(techapplist)){
    elem=techapplist[[id]]  #list element
    
    #prepare profile list
    profile=elem$techapp.profile
    profilestr=""     #string to write techapp.profile
    for (pid in 1:length(profile)){
      if (pid==1){
        profilestr=paste(names(profile[pid]),"=",format(profile[pid],digits=ndigit)) #no separtor for first element
      }else{
        profilestr=paste(profilestr,profilesep,names(profile[pid]),"=",format(profile[pid],digits=ndigit))
      }
    }
    
    #Combine print
    lineout=paste(format(elem$case,width=12),
                  listsep,format(elem$tech,width=20),
                  listsep,format(elem$techapp.score,digits=ndigit,width=10),
                  listsep,profilestr,"\n")
    cat(lineout)
  }
  if (filename!="") sink(file=NULL) #stop redirection
}
