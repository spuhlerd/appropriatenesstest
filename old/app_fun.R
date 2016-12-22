setwd("/Users/dorotheespuhler/Dropbox\ (Personal)/PHD\ Dropbox/1\ MODELLING/R/Appropriateness/")
library(triangle)
library (trapezoid)
library (plotrix)
#------------------------------------------------------------------------------------------------------------------------
#Technology Example "t1"
# Default values (to be replaced with input data)
# bod
t1_bodmin <- 40
t1_bodmax <- 1900 
t1_bodmean <- 600
# water
t1_watermin <- 8 
t1_watermode1 <- 10
t1_watermode2 <- 5000
t1_watermax <- 5000
# temp
t1_tempmin <- 5 
t1_tempmax <- 1200 
# omskil for O&M Skills
t1_omskilmin <- 3
t1_omskilmax <- 6

#------------------------------------------------------------------------------------------------------------------------
#Case Example "c1"
# Default values (to be replaced with input data)
#bod
c1_bodmin <-0 
c1_bodmax <-1200 
#water
c1_watermin<-0
c1_watermax<-160
c1_watermean<-25
# temp
c1_tempmin <- 0 
c1_tempmax <- 40 
c1_tempmean <- 15 
# omskil for O&M Skills
c1_omskilmin <- 0 
c1_omskilmax <- 5
c1_omskilmean <- 1 

#------------------------------------------------------------------------------------------------------------------------
par(mfrow=c(3, 2))

#------------------------------------------------------------------------------------------------------------------------
# Fucntion to assess BODMISSIONS appropriateness (mg/L)

# Default values (to be replaced with input data)
# t for TECHNOLOGY variables, bod
t_bodmin <- t1_bodmin
t_bodmax <- t1_bodmax
t_bodmean <- t1_bodmean
# c for CASE variable, bod
c_bodmin <- c1_bodmin
c_bodmax <- c1_bodmax

# CAPacity distributions (TECHNOLOGY)
tech_cap_bod <- function (bod) {dtriangle(bod, a = t_bodmin, b = t_bodmax, c = t_bodmean)}
# REQuirement distributions (CASE)
case_req_bod <- function (bod) {dunif(bod, min = c_bodmin, max =c_bodmax)/dunif(c_bodmin, min = c_bodmin, max =c_bodmax)}

# Combination of requirements and capacities
app_function_bod <- function (bod) {tech_cap_bod(bod)*case_req_bod(bod)}
# Appropriateness score
app_score_bod <- integrate(app_function_bod, -Inf, Inf)
app_score_bod

#Trying to see what happens
curve(tech_cap_bod(x), xlim = c(-10, 2000), xlab='mgBOD/L', ylab='', main='app_function_bod')
curve(case_req_bod(x),  col="blue", add=T)
curve(app_function_bod(x), col="red", add=T)
#------------------------------------------------------------------------------------------------------------------------
# Function to assess WATER requirements appropriateness (L/PE)

# t for TECHNOLOGY variables, water
t_watermin <- t1_watermin
t_watermode1 <- t1_watermode1
t_watermode2 <- t1_watermode2
t_watermax <- t1_watermax

# c for CASE variable, water
c_watermin<- c1_watermin
c_watermax<-c1_watermax
c_watermean<-c1_watermean

# REQuirement distributions (TECHNOLOGY)
tech_req_water <- function (water) {dtrapezoid(water, min = t_watermin, mode1 = t_watermode1, mode2 = t_watermode2, max = t_watermax, n1 = 2, n3 = 2, alpha = 1)/dtrapezoid(t_watermode1, min = t_watermin, mode1 = t_watermode1, mode2 = t_watermode2, max = t_watermax, n1 = 2, n3 = 2, alpha = 1)}

# CAPacity distributions (CASE)
case_cap_water <- function (water) {dtriangle(water, a = c_watermin, b = c_watermax, c = c_watermean)}

# Combination of requirements and capacities
app_function_water <- function (water) {tech_req_water(water)*case_cap_water(water)}
# Appropriateness score
app_score_water <- integrate(app_function_water, -Inf, Inf)
app_score_water

#Trying to see what happens
curve(tech_req_water(x), ylim=c(0,0.02), xlim=c(0,200), ylab='', main='app_function_water')
curve(case_cap_water(x), col="blue", add=T)
curve(app_function_water(x), col="red", add=T)

#------------------------------------------------------------------------------------------------------------------------
# 'Function to assess TEMPERATURE appropriateness (ºC)

# Default values (to be replaced with input data)
# t for TECHNOLOGY variables, temp for temperature
t_tempmin <- t1_tempmin
t_tempmax <- t1_tempmax
# c for CASE variable, temp for temperature
c_tempmin <- c1_tempmin
c_tempmax <- c1_tempmax
c_tempmean <- c1_tempmean

# REQuirement distributions (TECHNOLOGY)
tech_req_temp <- function (temp) {dunif(temp, min = t_tempmin, max =t_tempmax)/dunif(t_tempmin, min = t_tempmin, max =t_tempmax)}
# CAPacity distributions (CASE)
case_cap_temp <- function (temp) {dtriangle(temp, a = c_tempmin, b = c_tempmax, c = c_tempmean)}

# Combination of requirements and capacities
app_function_temp <- function (temp) {tech_req_temp(temp)*case_cap_temp(temp)}
# Appropriateness score
app_score_temp <- integrate(app_function_temp, -Inf, Inf)
app_score_temp

#Trying to see what happens
curve(tech_req_temp(x), xlim=c(-5,100), ylim=c(0, 1), ylab='', main='app_function_temp')
curve(case_cap_temp(x), col="blue", add=T)
curve(app_function_temp(x), col="red", add=T)

#------------------------------------------------------------------------------------------------------------------------
# 'Function to assess O&M Skills (5 ladder scale)

# Default values (to be replaced with input data)
# t for TECHNOLOGY variables, omskil for O&M Skills
t_omskilmin <- t1_omskilmin
t_omskilmax <- t1_omskilmax
# c for CASE variable, omskil for O&M Skills
c_omskilmin <- c1_omskilmin
c_omskilmax <- c1_omskilmax
c_omskilmean <- c1_omskilmean

# REQuirement distributions (TECHNOLOGY)
tech_req_omskil <- function (omskil) {dunif(omskil, min = t_omskilmin, max =t_omskilmax)/dunif(t_omskilmin, min = t_omskilmin, max =t_omskilmax)}
# CAPacity distributions (CASE)
case_cap_omskil <- function (omskil) {dtriangle(omskil, a = c_omskilmin, b = c_omskilmax, c = c_omskilmean)}

# Combination of requirements and capacities
app_function_omskil <- function (omskil) {tech_req_omskil(omskil)*case_cap_omskil(omskil)}
# Appropriateness score
app_score_omskil <- integrate(app_function_omskil, -Inf, Inf)
app_score_omskil

#Trying to see what happensa
curve(tech_req_omskil(x), xlim=c(0,10), ylim=c(0, 1), ylab='', main='app_function_omskil')
curve(case_cap_omskil(x), col="blue", add=T)
curve(app_function_omskil(x), col="red", add=T)
#------------------------------------------------------------------------------------------------------------------------
# Add Info on Plot
app.profile.first = rbind(app_score_bod, app_score_water, app_score_temp, app_score_omskil)
plot(0,0)
addtable2plot(0,0,app.profile[,1:2],lwd=par("lwd"),bty="n",bg=par("bg"),
              cex=1,xjust=0,yjust=1,xpad=0.1,ypad=0.5,box.col=par("fg"),text.col=par("fg"),
              display.colnames=TRUE,display.rownames=TRUE, title='app.profile')

app.profile.first
