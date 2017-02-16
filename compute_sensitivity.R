
compute_sensitivity <- function(num_of_runs,aggmethod,case, n.sample){

# den code n mal laufen lassen und die relevanten ergebnisse jeden runs in einer liste speichern
data_all_runs <- list()
for(i in 1:num_of_runs){
techapplist<-compute.techapplist(caselist,techlist,lsort=TRUE,lshowplot=F,lpdfplot=F,aggmethod, n.sample)
data_for_one_run <- list()
  for(j in 1:length(techapplist)){
    if(techapplist[[j]]$case==case){
    data_for_one_run[[techapplist[[j]]$tech]] <- techapplist[[j]]$techapp.score
    }
  }
 data_all_runs[[i]] <-data_for_one_run 
}

#daten in einer matrix zusammenfassen (für weiterverwendung) -> columns=techapp.scores of each run ; rows= techs
Sensitivity_data <- matrix(nrow=length(data_for_one_run), ncol=num_of_runs+2)
rownames(Sensitivity_data) <- names(data_for_one_run)
for(j in 1:length(data_for_one_run)){
  tech=techapplist[[j]]$tech
for(i in 1:num_of_runs){
  Sensitivity_data[tech,i] <- data_all_runs[[i]][[tech]]
}
}

#  compute statistic moments of each technology and add them to the matrix
for(j in 1:nrow(Sensitivity_data)){
  colnames(Sensitivity_data) <- c(1:num_of_runs,"mean","std")
Sensitivity_data[j,num_of_runs+1] <- mean(Sensitivity_data[j,1:num_of_runs])
Sensitivity_data[j,num_of_runs+2] <- sd(Sensitivity_data[j,1:num_of_runs])
}
Sensitivity_data
}
