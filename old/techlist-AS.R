
tech = list(

  septic.tank = list(
    functional.group = c('S','T'), # U, S, C, T, D
    products.in=c('blackwater'), # urine, excreta, blackwater, sludge, greywater stormwater
    products.out=c('sludge'),
    app.fun = list(
      bod=function(bod) {dtriangle(bod, 40, 1900, 600)},
      water= function (water) {dtrapezoid(water, 8, 10, 5000, 5000, n1 = 2, n3 = 2, alpha = 1)/dtrapezoid(10, 8, 10, 5000, 5000, n1 = 2, n3 = 2, alpha = 1)},
      temp=function(temp) {if(temp>5) {1} else {0}},
      omskil=function(omskil) {{if(omskil>3) {1} else {0}}}
    )
  ),

  single.pit = list(
    functional.group = c('S'), # U, S, C, T, D
    products.in=c('excreta'), # urine, excreta, blackwater, effluent, sludge, greywater stormwater
    products.out=c('effluent', 'sludge'),
    app.fun = list(
      bod=function(bod) {dtriangle(bod, 10, 400, 100)},
      water=function(water) {1},
      temp=function(temp) {1},
      omskil=function(omskil) {if(omskil>1) {1} else {0}}
    )
  )

)



build.list <- function(filename){
  dat <- read.table(filename, sep=",", stringsAsFactors=FALSE)

  ll <- list()                          # list of technologies or cases

  ## loop over all tech/cases
  for(i in 2:ncol(dat)){

    ll.i <- list(
              functional.group = c('S','T'), # U, S, C, T, D
              products.in=c('blackwater'), # urine, excreta, blackwater, sludge, greywater stormwater
              products.out=c('sludge'),
              app.fun = list())

    n.info.col <- 1                     # number of lines containg information *not* about the attributes
    ## loop over all attributes
    for(att in 1:((nrow(dat)-n.info.col)/3)){

      if(!is.na(dat[3*att-2+n.info.col,i]) & dat[3*att-2+n.info.col,i]!="") {
        f.string <- paste0("function(x){", dat[3*att-1+n.info.col,i],
                           "(x, ", dat[3*att+n.info.col,i], ")}")
        ll.i$app.fun[[dat[3*att-2+n.info.col,i]]] <- eval(parse(text=f.string))
      }

      }

    ll[[dat[1,i]]] <- ll.i
  }
  ll

}


tech2 <- build.list("data.csv")
str(tech2)
