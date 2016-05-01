rm(list=ls())
## ==============================================================================================
setwd("/Users/dorotheespuhler/Dropbox\ (Personal)/PHD\ Dropbox/1\ MODELLING/R/Appropriateness/")

# Load required library packages
library(triangle) # extra package for triangular distribution
library (trapezoid) # extra package for trapezoidial distribution
library(rlist)  # extra package to manupulate/filter app list
library(gridExtra)
library(ColorPalette)

# Load required functions
source("build.list.r")   # This function reads the technology and case input data stored in a csv file...
# build.list(filename,n.info.row)
source("appfunctions.r") # contains functions that are not provided in R but can be used to compute attribute values
  # prange(x, lower=-Inf, upper=Inf)
  # drange(x, lower=-Inf, upper=Inf)
  # rrange(x, lower=-Inf, upper=Inf)
  # ptrapez(x, a, b=(d-a)/2+a, c=b, d)
  # dtrapez(x, a, b=(d-a)/2+a, c=b, d)
  # rtrapez(x, a, b=(d-a)/2+a, c=b, d)
  # dcat(x, probs), probs is the vector of categories and respective probabilities. E.g. c(no=0.4,yes=0.6)
    # !!! the som of probs has to be =1
  # rcat(x, probs)
  # pcat(x, probs)
source("mc.integrate.r") # This functions computes a monte carlo integration of two continous functions
  # mc.integrate(case.app.fun, tech.app.fun, n.sample=10000)
source("compute.techapp.r") # Returns app.profile and app.score (aggregated profile)
  # compute.techapp(tech, case,lshowplot=FALSE)
  # plots provide a graphical representaiton of the two functions and the overall
source("compute.techapplist.r") # Returns a list of app.profiles & app.score for all the techs and caes of a techlist and caseplist
  # compute.techapplist(techlist, caselist, listsep=" ", filename="")
source("techapplist.write.r") # writes applist either to screen or to a file if listsep and filename are provided
 # function(applist, listsep=" ", filename="") 

## ==============================================================================================
# SOME GUIDELINES TO FILL IN DATA LIST FILES
#------------------------------------------
# Each data files contains a list of items (either techs or cases in the columns)
# Each items has a few information attributes (info.row).
# This is followed by a list of appropriateness attributes are listed.
#------------------------------------------
# PREDEFINED OPTIONS FOR TECHs
# Functional groups:
  # User interface (U), Collection and Storage (S), Conveyance (C), (Semi-)centralized Treatement (T), Reuse and Disposal (D)
# Products:
  # urine, faeces, excreta, blackwater, greywater, stormwater, storedurine, driedfaeces, pit humus, compost, sludge, effluent, stabilizedsludge, secondaryeffluent, biogas
#------------------------------------------
# APPROPRIATENESS ATTRIBUTES
# Contains three rows:
  # 1 Name of the attributes to be used: bod, water, temp, omskil, etc.
  # 2 Name of function (see below) describing the technology/case requirement/capactiy 
  # 3 Parameters required for this function
# Recommended functions are:
# p or drange(x, lower=-Inf, upper=Inf)
# p or dtrapez(x, a, b, c, d),
# dtriangle(x, a, b, c)
# dunif(x, min, max)
# Other that might work are: dnorm, dlnorm, dbeta, dweibull, dgamma, dlogis, etc.
# Each attriute is described by a pair of functions, one for the case and one for the tech.
# !!! A pair has always to consits of one density function ('d...') and one distribution function ('p...')
# Which of the two functions is used to describe the case or the technology attribute value can vary
# Generally density functions are used to describe probability that the attribute takes a certain value (e.g. temperature)
# ... and distribution functions are used to describe the performance given the attribute (e.g. the performance of a technology given a certain temperature)


## ==============================================================================================
## ==============================================================================================
## EXAMPLES ON HOW TO USE THE CODE
## Compute appropriateness profiles (app.profile(tech, case)) and apppropriateness scores (app.scores(tech, case))

## ==============================================================================================
# Create the list of technology appropriateness functions and the list of case appropriateness functions
caselist<- build.list("casedata.csv",2)
techlist<- build.list("techdata.csv",3)

## ==============================================================================================
# COMPUTE app.proiles FOR A PAIR OF TECH AND CASE (caselist$case, techlist$tech)
# Using compute.techapp

# Create empty list
applist1=list()
# Compute examples
app.item.tmp <- compute.techapp(caselist$arbaminch, techlist$septic.tank,lshowplot = TRUE)
applist1=append(applist1,list(app.item.tmp))
app.item.tmp <- compute.techapp(caselist$thimi, techlist$septic.tank,lshowplot = TRUE)
applist1=append(applist1,list(app.item.tmp))
app.item.tmp <- compute.techapp(caselist$arbaminch, techlist$single.pit, lshowplot = TRUE)
applist1=append(applist1,list(app.item.tmp))
app.item.tmp <- compute.techapp(caselist$arbaminch, techlist$double.pit, lshowplot = TRUE)
applist1=append(applist1,list(app.item.tmp))
# Print examples
print(t(app.item.tmp), digits=4)  #optionally app.septic.tank[1:3] for tech, case, score, can't print the tech.app.profile as it is list in list

## ==============================================================================================
# COMPUTE app.proiles FOR A PAIR OF TECH AND CASE (caselist$case, techlist$tech)
# Using compute.techapplist

applist2<-compute.techapplist(caselist,techlist,lsort=TRUE)

## ==============================================================================================
# WRITE APPLIST
# Using applist.write

# Write to screen
techapplist.write(applist2)

# Write to file
techapplist.write(applist2, listsep=";", filename="app_list2.csv")

## ==============================================================================================
# USE rlist to FILTER

applist<-applist2

# Example only arbaminch
sub.applist=list.filter(applist,case=="arbaminch")
cat("====================== \n")
cat("Only arbaminch  \n")
techapplist.write(sub.applist)

# Example only arbaminch and score > 0
sub.applist=list.filter(applist,case=="arbaminch", tech.app.score>0)
cat("====================== \n")
cat("Only arbaminch and score > 0 \n")
techapplist.write(sub.applist)

# Examples of manipulations
sub.applist=list.filter(applist,case=="arbaminch", tech=="single.pit")
sub.applist[[1]]$tech.app.profile$bod
# using $ instead of list.filter
applist[[1]]$tech.app.profile$bod # to get the bod of an item in the list, 1 ist the list id

## ==============================================================================================
## ==============================================================================================
## APPLICATIONS
## ==============================================================================================
# Test data from Daniel
# Read data
#caselist_daniel<- build.list("casedata_daniel.csv",2)
#techlist_daniel<- build.list("techdata_daniel.csv",3)
caselist_daniel<- build.list("casedata_daniel-250116.csv",2)
techlist_daniel<- build.list("techdata_daniel-250116.csv",3)

# Test one by one
compute.techapp(caselist_daniel$arbaminch,techlist_daniel$pour.flush, lshowplot=TRUE)
compute.techapp(caselist_daniel$arbaminch,techlist_daniel$UDDT, lshowplot=TRUE)
compute.techapp(caselist_daniel$arbaminch,techlist_daniel$septic.tank, lshowplot=TRUE)
compute.techapp(caselist_daniel$arbaminch,techlist_daniel$single.pit, lshowplot=TRUE)
compute.techapp(caselist_daniel$arbaminch,techlist_daniel$wsp, lshowplot=TRUE)
compute.techapp(caselist_daniel$arbaminch,techlist_daniel$surface.flow.CW, lshowplot=TRUE)
# Compute entire list
techapplist_daniel<-compute.techapplist(caselist_daniel,techlist_daniel,lsort=TRUE)
# Write to screen
techapplist.write(techapplist_daniel)
# Write to file
techapplist.write(techapplist_daniel, listsep=";", filename="techapplist_daniel.csv")

## ==============================================================================================
# Testing data
# Read data
caselist_test<- build.list("casedata_ex.csv",2)
techlist_test<- build.list("techdata_ex.csv",3)

# Test one by one
compute.techapp(caselist_test$arbaminch,techlist_test$T1.wsp, lshowplot=TRUE)
compute.techapp(caselist_test$arbaminch,techlist_test$septic.tank, lshowplot=TRUE)

# Compute entire list
techapplist_test<-compute.techapplist(caselist_test,techlist_test,lsort=TRUE)
# Write to screen
techapplist.write(techapplist_test)
# Write to file
techapplist.write(techapplist_test, listsep=";", filename="techapplist_test.csv")

## ==============================================================================================
# Testing yes no
# Read data
caselist_test<- build.list("casedata_ex_yn.csv",2)
techlist_test<- build.list("techdata_ex_yn.csv",3)

# Test one by one
compute.techapp(caselist_test$arbaminch,techlist_test$uddt, lshowplot=FALSE)
compute.techapp(caselist_test$arbaminch,techlist_test$dry.toilet, lshowplot=FALSE)
yesnotest<-compute.techapplist(caselist_test,techlist_test,lsort=FALSE)
techapplist.write(yesnotest)
