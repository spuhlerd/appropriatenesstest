build.list <- function(filename,n.info.row){ 
  # This function reads the technology and case input data stored in a csv file...
  # filename: csv name containing input data for technologies/cases
  # n.info.row: Number of information rows in 'data.csv'.
  #    A information row is one with no attribute, but other information in the beginning (e.g. 'functional.group').
  #   First line containing the 'names' of the technologies is not counted in n.info.row.
  # info.rows have one item per row, attributes have three rows: name, function, and function variables
  
  dat <- read.table(filename, sep=";", stringsAsFactors=FALSE)
  
  ll <- list()                          # list of technologies or cases
  
  ## loop over all tech/cases
  for(i in 2:ncol(dat)){
    
    ll.i = list()  
    for(j in 1:n.info.row) {
      ll.i[[dat[j+1,1]]] = unlist(strsplit(sub(" ", "", dat[j+1,i]), ","))
    }
    
    ## loop over all attributes
    ll.i$app.fun  <- list()
    for(att in 1:((nrow(dat)-n.info.row-1)/3)){
      
      if(!is.na(dat[3*att-1+n.info.row,i]) & dat[3*att-1+n.info.row,i]!="") {
        f.string <- paste0("function(x){", dat[3*att+n.info.row,i],
                           "(x, ", dat[3*att+n.info.row+1,i], ")}")
        ll.i$app.fun[[dat[3*att-1+n.info.row,i]]] <- eval(parse(text=f.string))
      }
      
    }
    
    ll[[dat[1,i]]] <- ll.i
  }
  ll
  
}

