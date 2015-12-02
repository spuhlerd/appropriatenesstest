# Fucntion to assess temperature appropriateness
#

t_tmin <- 10
t_tmax <- 200
c_tmin <- 5
c_tmax <- 40
c_tmean <- 15

tr_temp <- function (x, t_tmin, t_tmax) {dunif(x, min = t_tmin, max =t_tmax)/dunif(x, min = t_tmin, max =t_tmax)}
cc_temp <- function (x, c_tmin, c_tmax, c_tmean) {dtriangle(x, a = c_tmin, b = c_tmax, c = c_tmean)}
t_app_temp <- function (tr_temp, cc_temp) {tr_temp*cc_temp}
t_app_temp_score <- integrate(app_temp, -Inf, Inf)



curve(tr_temp(x, t_tmin = 10, t_tmax = 200))



# Fucntion to assess temperature appropriateness

x<-1:t_tmax

tr_temp <- curve(dunif(x, min = t_tmin, max =t_tmax)/dunif(x, min = t_tmin, max =t_tmax), from=-5, to=100, add=NA, ylab=NA, xlab = NA, main="tr_temp", ylim=c(0,1))
cc_temp <- curve(dtriangle(x, a = c_tmin, b = c_tmax, c = c_tmean), from=-5, to=100, add=NA, ylab=NA, xlab = NA, main="cc_temp", ylim=c(0,1))

tr_temp <- dunif(x, min = t_tmin, max =t_tmax)/dunif(x, min = t_tmin, max =t_tmax)
cc_temp <- dtriangle(x, a = c_tmin, b = c_tmax, c = c_tmean)
app_temp <- tr_temp*cc_temp
plot(app_temp)
sum(app)
app_temp

app_temp <- integrate((dunif(x, min = t_tmin, max =t_tmax)/dunif(x, min = t_tmin, max =t_tmax))*(dtriangle(x, a = c_tmin, b = c_tmax, c = c_tmean)))
app_temp



tr_temp <- function (t_tmin, t_tmax) {dunif(x, min = t_tmin, max =t_tmax)/dunif(x, min = t_tmin, max =t_tmax)}
cc_temp <- function (c_tmin, c_tmax, c_tmean) {dtriangle(x, a = c_tmin, b = c_tmax, c = c_tmean)}
app_temp <- function (tr_temp, cc_temp) {tr_temp*cc_temp}
app_temp_score <- integrate(app_temp, lower = Inf, upper = Inf)

cc_temp

