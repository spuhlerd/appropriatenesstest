rm(list=ls())
## ==============================================================================================
# To dos:
# - loop for all tech in all caselist
# - include ranking

## ==============================================================================================
setwd("/Users/dorotheespuhler/Dropbox\ (Personal)/PHD\ Dropbox/1\ MODELLING/R/Appropriateness/")
#command: cd Dropbox/PHD\ Dropbox/1\ MODELLING/R/Appropriateness

library(gridExtra)
library(ColorPalette)

# Load required functions
library(triangle)
library (trapezoid)
library(rlist)  #to manupulate/filter app list
source("listread.r") # reads the csv data files for technologies and cases descriptions
source("req.functions.r") # contains functions that are not provided in R such as ranges
source("compute.app.r") # functions(tech, case) returning app.profile and app.score
source("compute.applist.r") # function(techlist, caselist, listsep=" ", filename="") making all the app profiles for a techlist and applist
source("applist.write.r") # function(applist, listsep=" ", filename="") writes applist

## ==============================================================================================
# Create the list of technology appropriateness functions and the list of case appropriateness functions

techlist<- build.list("techdata.csv",3)
techlist_daniel<- build.list("techdata_daniel.csv",3)
#str(techlist)
caselist<- build.list("casedata.csv",2)
caselist_daniel<- build.list("casedata_daniel.csv",2)
#str(caselist)
#-------------------------------------------
# Some guidelines to fill in the data files:
#------------------------------------------
# Each data files contines a list of items (either techs or cases in the columns)
# Each items has some attributes (lines)
# The first attributes are just information, that helps to understand the case or is needed for later on
# Then the appropriateness attributes are listed
# Each attribute goes over three lines:
# 1 Name of the attributes to be used: bod, water, temp, omskil, etc.
# 2 Name of function of function (see below) describing the technology/case requirement/capactiy 
# 3 Parameters required for this function
# Recommended functions are:
# req.range(x, lower=-Inf, upper=Inf)
# req.trapez(x, a, b, c, d),
# dtriangle(x, a, b, c)
# dtrapezoid(x, min, mode1, mode2, max )
# dunif(x, min, max)
# dnorm, dlnorm, dbeta, dweibull, dgamma, dlogis, etc.


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
