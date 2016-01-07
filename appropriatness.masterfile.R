rm(list=ls())

## ==============================================================================================
#TO DOS:
#- Correct that $tech.app.score is not 1 if tech does not exist... make example...
#- run even if function not known in casedata...
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
source("listread.r") # reads the csv data files for technologies and cases descriptions
source("req.functions.r") # contains functions that are not provided in R such as ranges
source("compute.app.r") # functions(tech, case,lshowplot=FALSE) returning app.profile and app.score
source("compute.applist.r") # function(techlist, caselist, listsep=" ", filename="") making all the app profiles for a techlist and applist
source("applist.write.r") # function(applist, listsep=" ", filename="") writes applist

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
# req.range(x, lower=-Inf, upper=Inf)
# req.trapez(x, a, b, c, d),
# dtriangle(x, a, b, c)
# dtrapezoid(x, min, mode1, mode2, max)
# dunif(x, min, max)
# dnorm, dlnorm, dbeta, dweibull, dgamma, dlogis, etc.

## ==============================================================================================
# Create the list of technology appropriateness functions and the list of case appropriateness functions
caselist<- build.list("casedata.csv",2)
techlist<- build.list("techdata.csv",3)

## ==============================================================================================
# Compute appropriateness profiles (app.profile(tech, case)) and apppropriateness scores (app.scores(tech, case))

# For single examples: a pair of (caselist$case, techlist$tech)
cat("====== Using compute.app ================= \n")
# Create empty list
applist1=list()
# Compute examples
app.item.tmp <- compute.app(caselist$arbaminch, techlist$septic.tank,lshowplot = TRUE)
applist1=append(applist1,list(app.item.tmp))
app.item.tmp <- compute.app(caselist$thimi, techlist$septic.tank,lshowplot = TRUE)
applist1=append(applist1,list(app.item.tmp))
app.item.tmp <- compute.app(caselist$arbaminch, techlist$single.pit, lshowplot = TRUE)
applist1=append(applist1,list(app.item.tmp))
app.item.tmp <- compute.app(caselist$arbaminch, techlist$double.pit, lshowplot = TRUE)
applist1=append(applist1,list(app.item.tmp))
# Print examples
print(t(app.item.tmp), digits=4)  #optionally app.septic.tank[1:3] for tech, case, score, can't print the tech.app.profile as it is list in list

# For enitre applists and techlists: a pair of (caselist, techlist)
cat("====== Using compute.applist ================= \n")
applist2<-compute.applist(caselist,techlist,lsort=TRUE)
#applist_daniel<-compute.applist(caselist_daniel,techlist_daniel,lsort=TRUE)

## ==============================================================================================
# Applist displays
# Write to screen
applist.write(applist2)
# Write to file
applist.write(applist2, listsep=";", filename="app_list2.csv")

## ==============================================================================================
# Use rlist to filter
applist<-applist2
# Example only arbaminch
sub.applist=list.filter(applist,case=="arbaminch")
cat("====================== \n")
cat("Only arbaminch  \n")
applist.write(sub.applist)
# Example only arbaminch and score > 0
sub.applist=list.filter(applist,case=="arbaminch", tech.app.score>0)
cat("====================== \n")
cat("Only arbaminch and score > 0 \n")
applist.write(sub.applist)

# Examples of manipulations
sub.applist=list.filter(applist,case=="arbaminch", tech=="single.pit")
sub.applist[[1]]$tech.app.profile$bod
# using $ instead of list.filter
applist[[1]]$tech.app.profile$bod # to get the bod of an item in the list, 1 ist the list id

## ==============================================================================================
# Test data from Daniel
# Read data
caselist_daniel<- build.list("casedata_daniel.csv",2)
techlist_daniel<- build.list("techdata_daniel.csv",3)
# Test one by one
compute.app(caselist_daniel$arbaminch,techlist_daniel$pour.flush, lshowplot=TRUE)
compute.app(caselist_daniel$arbaminch,techlist_daniel$UDDT, lshowplot=TRUE)
compute.app(caselist_daniel$arbaminch,techlist_daniel$septic.tank, lshowplot=TRUE)
compute.app(caselist_daniel$arbaminch,techlist_daniel$single.pit, lshowplot=TRUE)
compute.app(caselist_daniel$arbaminch,techlist_daniel$wsp, lshowplot=TRUE)
compute.app(caselist_daniel$arbaminch,techlist_daniel$surface.flow.CW, lshowplot=TRUE)
# Compute entire list
applist_daniel<-compute.applist(caselist_daniel,techlist_daniel,lsort=TRUE)
# Write to screen
applist.write(applist_daniel)
# Write to file
applist.write(applist_daniel, listsep=";", filename="applist_daniel.csv")

## ==============================================================================================
# Testing data
# Read data
caselist_test<- build.list("casedata_ex.csv",2)
techlist_test<- build.list("techdata_ex.csv",3)
# Test one by one
techops<-c('uddt','dry.toilet','pour.flush','urine.storagetank','dehydration.vaults','double.vip','septic.tank','motorized.emptying','conventional.sewer','T1.empty','T1.unplanted.dryingbed','T1.wsp','T2.empty','T2.unplanted.dryingbed','T2.wsp','application.urine','application.faeces','application.humus','application.sluldge','irrigation')
for(i in 1:length(techops)){
compute.app(caselist_test$arbaminch,techlist_test$techops[i], lshowplot=TRUE)
compute.app(caselist_test$arbaminch,techlist_test[i], lshowplot=TRUE)
} 
# Compute entire list
applist_test<-compute.applist(caselist_test,techlist_test,lsort=TRUE)
# Write to screen
applist.write(applist_test)
# Write to file
applist.write(applist_test, listsep=";", filename="applist_test.csv")
