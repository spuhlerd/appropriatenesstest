par(mfrow=c(3, 2))

t_tempmin
t_tempmax
c_tempmin
c_tempmax
c_tempmean
tech_req_temp <- function (temp) {dunif(temp, min = t_tempmin, max =t_tempmax)/dunif(t_tempmin, min = t_tempmin, max =t_tempmax)}
case_cap_temp <- function (temp) {dtriangle(temp, a = c_tempmin, b = c_tempmax, c = c_tempmean)}
app_function_temp <- function (temp) {tech_req_temp(temp)*case_cap_temp(temp)}
app_score_temp <- integrate(app_function_temp, -Inf, Inf)
app_score_temp
#Trying to see what happens
curve(tech_req_temp(x), xlim=c(-5,100), ylim=c(0, 1), ylab='', main='app_function_temp', add=FALSE)
curve(case_cap_temp(x), col="blue", add=T)
curve(app_function_temp(x), col="red", add=T)

tech$septic.tank$app.fun$temp
case$arbaminch$app.fun$temp
app_function_temp2 <- function (temp) {tech$septic.tank$app.fun$temp(temp)*case$arbaminch$app.fun$temp(temp)}
app_score_temp2 <- integrate(app_function_temp2, -Inf, Inf)
app_score_temp2
#Trying to see what happens
curve(tech$septic.tank$app.fun$temp(x), xlim=c(-5,100), ylim=c(0, 1), ylab='', main='app_function_temp', add=FALSE)
curve(case$arbaminch$app.fun$temp(x), col="blue", add=T)
curve(app_function_temp2(x), col="red", add=T)

f1=function(temp) {if(temp>5) {1} else {0}}
f2=function(temp) {dunif(temp, 5, 1200)/dunif(5, 5, 1200)}

c1 = function(temp) {dtriangle(temp, 0, 40, 15)}

fint1 = function(x) f1(x)*c1(x)
fint2 = function(x) f2(x)*c1(x)

integrate(fint1, -Inf, Inf)
