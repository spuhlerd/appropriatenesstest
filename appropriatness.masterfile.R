rm(list=ls())

## ==============================================================================================
#TO DOS:
#- allow for attributes not existing in caselist but in techlist (viceversa already works)
#- how to get all the tech.app.scores... e.g. applist_test[[]]$tech.app.score
#- how to make yes/no or a/b functions
#- how to make functions more flexibel, e.g. 25, 50, 75%
#- provide tech options
## ==============================================================================================
setwd("/Users/dorotheespuhler/Dropbox\ (Personal)/PHD\ Dropbox/1\ MODELLING/R/Appropriateness/")
#command: cd Dropbox/PHD\ Dropbox/1\ MODELLING/R/Appropriateness

library(gridExtra)
library(ColorPalette)

# Load required library packages
library(triangle) # extra package for triangular distribution
library (trapezoid) # extra package for trapezoidial distribution
library(rlist)  # extra package to manupulate/filter app list
# Load required functions
source("build.list.r") # reads the csv data files for technologies and cases descriptions
source("pfunctions.r") # contains functions that are not provided in R such as ranges
source("mc.integrate.r") # function(case.app.fun, tech.app.fun, n.sample=10000) used to integrate by sampling
source("compute.techapp.r") # functions(tech, case,lshowplot=FALSE) returning app.profile and app.score
source("compute.techapplist.r") # function(techlist, caselist, listsep=" ", filename="") making all the app profiles for a techlist and applist
source("techapplist.write.r") # function(applist, listsep=" ", filename="") writes applist

## ==============================================================================================
# SOME GUIDELINES TO FILL IN DATA FILES
#------------------------------------------
# Each data files contines a list of items (either techs or cases in the columns)
# Each items has some attributes (lines)
# The first lines (info.row) contain attributes with particular information needed to construct systems or to understand the context.
# Then the appropriateness attributes are listed.
#------------------------------------------
# FOR TECHs
# Functional groups: U, S, C, T1, T2, D
# Products: Urine, Faeces, Excreta, Balckwater, Greywater, ...
# ... Pit humus, Compost, Sludge, Effluent, Stormwater,...
# ... Treated sludge, Treated effluent, Biogas
#------------------------------------------
# FOR CASES
# Descripton: Just a few lines what the case is about
# Inhabitants: Number of inhabitants
#------------------------------------------
# APPROPRIATENESS ATTRIBUTES
# Each appropriateness attribute goes over three lines:
# 1 Name of the attributes to be used: bod, water, temp, omskil, etc.
# 2 Name of function of function (see below) describing the technology/case requirement/capactiy 
# 3 Parameters required for this function
# Recommended functions are:
# prange(x, lower=-Inf, upper=Inf)
# ptrapez(x, a, b, c, d),
# dtriangle(x, a, b, c)
# dtrapezoid(x, min, mode1, mode2, max)
# dunif(x, min, max)
# dnorm, dlnorm, dbeta, dweibull, dgamma, dlogis, etc.
# Each attriute is described by a pair of functions, one for the case and one for the tech.
# You have to make sure, that this pair consists of one density function ('d...') and one distribution function ('p...')
## ==============================================================================================
# Create the list of technology appropriateness functions and the list of case appropriateness functions
caselist<- build.list("casedata.csv",2)
techlist<- build.list("techdata.csv",3)

## ==============================================================================================
# Compute appropriateness profiles (app.profile(tech, case)) and apppropriateness scores (app.scores(tech, case))

# For single examples: a pair of (caselist$case, techlist$tech)
cat("====== Using compute.techapp ================= \n")
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

# For enitre applists and techlists: a pair of (caselist, techlist)
cat("====== Using compute.techapplist ================= \n")
applist2<-compute.techapplist(caselist,techlist,lsort=TRUE)
#applist_daniel<-compute.techapplist(caselist_daniel,techlist_daniel,lsort=TRUE)

## ==============================================================================================
# Applist displays
# Write to screen
techapplist.write(applist2)
# Write to file
techapplist.write(applist2, listsep=";", filename="app_list2.csv")

## ==============================================================================================
# Use rlist to filter
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

