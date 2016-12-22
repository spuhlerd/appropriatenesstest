case = list(
  arbaminch = list(
    app.fun = list(
      bod=function(bod) {if(bod<1200) {1} else {0}},
      water= function (water) {dtriangle(water,0, 150, 25)},
      temp=function(temp) {dtriangle(temp, 0, 40, 15)},
      omskil=function(omskil) {dtriangle(omskil, 0, 5, 1)}
    )
  )
)
