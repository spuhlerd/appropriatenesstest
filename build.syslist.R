build.syslist <- function(filename,n.info.row=NULL){ 
  # This function reads the technology and case input data stored in a csv file...
  # filename: csv name containing input data for technologies/cases
  # n.info.row: Number of information rows in 'data.csv'.
  #    A information row is one with no attribute, but other information in the beginning (e.g. 'functional.group').
  #   First line containing the 'names' of the technologies is not counted in n.info.row.
  # info.rows have one item per row, attributes have three rows: name, function, and function variables
  
  dat <- read.table(filename, sep=";", stringsAsFactors=FALSE)
  
  ll <- list()                          # list of technologies or cases
  
  # determine n.inf.row if not specified as argument
  if (is.null(n.info.row)){
    if ("techs" %in% dat[,1]){
      n.info.row=which(dat[,1] == "techs")-2 #number of info line is the number of line until techs minus 1, we substract again minus 1 for the title line 
    }else{
      stop("No techs string in first column")
    }
  }
    if ("attr1" %in% dat[,1]){
      n.info.tech.row=which(dat[,1] == "attr1")-2 #number of info line is the number of line until attr1 minus 1, we substract again minus 1 for the title line 
    }else{
      stop("No attr1 string in first column")
    }
  

  ## loop over all systems
  for(i in 2:ncol(dat)){
    
    ll.i = list()  
    for(j in 1:n.info.row) {
      ll.i[[dat[j+1,1]]] = unlist(strsplit(sub(" ", "", dat[j+1,i]),","))
    }
    
    ## loop over all techs
    ll.i$techs  <- list()
    
    for(tech in n.info.row+3:n.info.tech.row-1){
      
     if(!is.na(dat[tech,i]) & dat[tech,i]!="") {
    ll.i$techs[[dat[tech,i]]] <- dat[tech,i]
    }
    }
    
    ## loop over all attributes
    ll.i$app.fun  <- list()
    for(att in 1:((nrow(dat)-n.info.row-1)/3)){
      
      if(!is.na(dat[3*att-1+n.info.tech.row,i]) & dat[3*att-1+n.info.tech.row,i]!="") {
        f.string <- paste0("function(x){", dat[3*att+n.info.tech.row,i],
                           "(x, ", dat[3*att+n.info.tech.row+1,i], ")}")
        ll.i$app.fun[[dat[3*att-1+n.info.tech.row,i]]] <- eval(parse(text=f.string))
      }
    }
    
    ll[[dat[1,i]]] <- ll.i
  }
  ll
  
}
