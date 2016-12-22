
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

