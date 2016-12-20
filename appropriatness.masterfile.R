rm(list=ls())
## ==============================================================================================
setwd("C:/Users/gundlajo/Code/appropriateness_joel")

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
  # pcat(x, probs)
  # dcat(x, probs), probs is the vector of categories and respective probabilities. E.g. c(no=0.4,yes=0.6)
    # !!! the sum of probs has to be =1
  # rcat(x, probs)
source("mc.integrate.r") # This functions computes a monte carlo integration of two continous functions
  # mc.integrate(case.app.fun, tech.app.fun, n.sample=10000)
source("compute.techapp.r") # Returns app.profile and app.score (aggregated profile)
  # compute.techapp(tech, case,lshowplot=FALSE)
  # plots provide a graphical representaiton of the two functions and the overall
source("compute.techapplist.r") # Returns a list of app.profiles & app.score for all the techs and caes of a techlist and caseplist
  # compute.techapplist(techlist, caselist, listsep=" ", filename="")
source("techapplist.write.r") # writes applist either to screen or to a file if listsep and filename are provided
 # function(applist, listsep=" ", filename="") 
source("build.syslist_joel.r")   # This function reads the technology and case input data stored in a csv file...

## ==============================================================================================
# MANAGMENT OF THE MODEL
#------------------------------------------
# I use bitbucket to store the model as well as to share it and manage edits.
# The adress is: https://spuhler@bitbucket.org/spuhler/appropriateness.git
#------------------------------------------
# USING BITBUCKET
#...To install
# - Make an account and send the username to Dorothee
# - Get invited by Dorothee to the repository
# - Make fork (under "Actions" in Overview)
# - Go to the copy of the repository in the fork
# - Copy the https string "https:..."
# - Open the terminal
# - Go to the folder where you want to work in the future (e.g. cd User/Modelling)
# - Create a local clone: git clone "https..." -> this will create a folder with all the required documents. This will create a folder with the "originalname" of the repository (e.g. "Appropriateness")
# - Got to the folder: cd originalname
# - Create your branch to work on e.g. git branch newname (e.g. "AppropriatnessJoel")
# - Go to your branch: git checkout  branchnewname

# During work with fork:
# - Open the terminal and go to your working directory (e,g, cd User/Modelling/Appropriatness).
# - Check if there are any updates: git status
# - Commit the changes you made git commit filename -m "message" (-a for "all")
# - Push the changes you made to the repository only git push origin branchnewname (e.g. AppropriatenessJoel)
# - If you would like to update your files to the changes on bitbucket, use: git pull origin branchnewman - This will replace the content of your folder with the one online, so only pull if this is what you want to do


## ==============================================================================================
# SOME GUIDELINES TO FILL IN DATA LIST FILES (techdata and casedata)
#------------------------------------------
# Each data files contains a list of items (either techs or cases in the columns)
# Each items has a few information attributes (info.rows), build.list automatically detects the number of info rows, so you do not need to provide this.
# This is followed by a list of appropriateness attributes (attr1,...., attrn).
# Info rows can be used to provide comments about the case or the technology.
# For Technologies, the info.rows are also used to provide the functional groups and the products which are used to generate sanitation systems by a different model module.
# Predefined functional groups:
  # User interface (U), Collection and Storage (S), Conveyance (C), (Semi-)centralized Treatement (T), Reuse and/or Disposal (D) (see also http://ecompendium.sswm.info)
# Pre-defined poducts:
  # urine, faeces, excreta, blackwater, greywater, stormwater, storedurine, driedfaeces, pit humus, compost, sludge, effluent, stabilizedsludge, secondaryeffluent, biogas
# Appropriateness attributes are defined by three rows:
  # 1 Name of the attributes: e.g. bod, water, temp, omskil, etc.
  # 2 Name of attribute appropriateness function describing the technology/case requirement/capactiy 
  # 3 Parameters required for this function
# Each distinct attriute is described by a pair of functions, one for the case and one for the tech.
# !!! A pair has always to consits of one density function ('d...') and one distribution function ('p...')
# Which of the two functions is used to describe the case or the technology attribute value can vary
# Generally density functions are used to describe probability that the attribute takes a certain value (e.g. temperature)
# ... and distribution functions are used to describe the performance given the attribute (e.g. the performance of a technology given a certain temperature)
# Recommended functions are:
# p or drange(x, lower=-Inf, upper=Inf) 
# p or dtrapez(x, a, b, c, d),
# dtriangle(x, a, b, c)
# dunif(x, min, max)
# dcat(x, probs)  # probs is the vector of categories and respective probabilities. E.g. c(no=0.4,yes=0.6)
# Other that might work are: dnorm, dlnorm, dbeta, dweibull, dgamma, dlogis, etc.

## ==============================================================================================
## ==============================================================================================
## EXAMPLES ON HOW TO USE THE MODEL TO EVALUATE TE APPROPRIATENESS OF TECHNOLOGIES IN SPEC.CASES
## ==============================================================================================
# READ THE DATA INPUT FILES USING Bbuild.list TO GENERATE A TECHNOLOGY AND A CASE LIST
caselist<- build.list("casedata_demo.csv")
techlist<- build.list("techdata_demo.csv")
## ==============================================================================================
# COMPUTE app.proiles FOR A PAIR OF TECH AND CASE (caselist$case, techlist$tech)
# Using compute.techapp
applist_demo=list()
#arbaminch
app.item.tmp <- compute.techapp(caselist$arbaminch, techlist$uddt,lshowplot = TRUE)
applist_demo=append(applist_demo,list(app.item.tmp))
app.item.tmp <- compute.techapp(caselist$arbaminch, techlist$dry.toilet,lshowplot = TRUE)
applist_demo=append(applist_demo,list(app.item.tmp))
app.item.tmp <- compute.techapp(caselist$arbaminch, techlist$pour.flush,lshowplot = TRUE)
applist_demo=append(applist_demo,list(app.item.tmp))
app.item.tmp <- compute.techapp(caselist$arbaminch, techlist$urine.storagetank,lshowplot = TRUE)
applist_demo=append(applist_demo,list(app.item.tmp))
#...
#thimi
app.item.tmp <- compute.techapp(caselist$thimi, techlist$uddt,lshowplot = TRUE)
applist_demo=append(applist_demo,list(app.item.tmp))
app.item.tmp <- compute.techapp(caselist$thimi, techlist$pour.flush,lshowplot = TRUE)
applist_demo=append(applist_demo,list(app.item.tmp))
#...

## ==============================================================================================
# COMPUTE ENTIRE apppropriateness proiles FOR A LIST OF TECHNOLOGIES AND A LIST OF CASES IN ONE GO
# Using compute.techapplist 
applist_demo<-compute.techapplist(caselist,techlist,lsort=TRUE,lshowplot=TRUE) # use lshowplot=F if
#you are only interested in the results; the plots (displaying the appropriateness functions for each attribute) only help to understand what happens

## ==============================================================================================
# WRITE A applist TO THE SCREEN OR A CSV FILE
# Write to screen
techapplist.write(applist_demo)
# Write to file
techapplist.write(applist_demo, listsep=";", filename="app_list_demo.csv") #giving a list separation charachter and a filename creates a csv file with the results

## ==============================================================================================
# USE list TO FILTER FOR SPECIFIC RESULTS OF INTEREST
applist<-applist_demo
# Example only arbaminch: filtering and subssequently writing to screen
sub.applist=list.filter(applist,case=="arbaminch")
cat("====================== \n") # Insert line for better lisibility
cat("Only arbaminch  \n") # Provide a title to the table
techapplist.write(sub.applist) # Write the sublist to the screen with techapplist.write

# Example only arbaminch and score > 0
sub.applist=list.filter(applist,case=="arbaminch", techapp.score>0)
cat("====================== \n")
cat("Only arbaminch and score > 0 \n")
techapplist.write(sub.applist)

# Further examples of possible filter
sub.applist=list.filter(applist,case=="arbaminch", tech=="dry.toilet") # provides only the results
# for dry toilest in arbaminch
sub.applist[[1]]$techapp.profile$temp # [[1]] stands for the first instance of the sublist for dry 
# toilet, however, this sublist is anyhow only contained of one item
sub.applist=list.filter(applist, tech=="dry.toilet") # if we filter for dry toilet only, we get a list of to items:
sub.applist[[1]]$techapp.profile$temp 
sub.applist[[2]]$techapp.profile$temp


## ==============================================================================================
## ==============================================================================================
## ==============================================================================================
## ==============================================================================================
## ==============================================================================================
## ==============================================================================================
## ==============================================================================================
## ==============================================================================================
## ==============================================================================================
## ==============================================================================================
## ==============================================================================================
## ==============================================================================================
## APPLICATIONS

## ==============================================================================================
# TESTING DATA
# Read data
caselist_test<- build.list("casedata_test.csv")
techlist_test<- build.list("techdata_test.csv")
# Test one by one
compute.techapp(caselist_test$arbaminch,techlist_test$uddt, lshowplot=FALSE)
compute.techapp(caselist_test$arbaminch,techlist_test$dry.toilet, lshowplot=FALSE)
# Test entire lists
techapplist_test<-compute.techapplist(caselist_test,techlist_test,lsort=FALSE) # use lshowplot=TRUE for visual analysis of the results
# Write to see
techapplist.write(techapplist_test)
techapplist.write(techapplist_test, listsep=";", filename="techapplist_daniel.csv")

## ==============================================================================================
# Testing data (RANDOM NOTES DOROTHEE)
# Read data
caselist_test<- build.list("casedata_test.csv")
techlist_test<- build.list("techdata_test.csv")

# Test one by one
compute.techapp(caselist_test$arbaminch,techlist_test$T1.wsp, lshowplot=TRUE)
compute.techapp(caselist_test$arbaminch,techlist_test$septic.tank, lshowplot=TRUE)

# Compute entire list
techapplist_test<-compute.techapplist(caselist_test,techlist_test,lsort=TRUE,lshowplot=TRUE)
compute.techapplist(caselist_ex,techlist,lshowplot=TRUE)

# Write to screen
techapplist.write(techapplist_ex)
# Write to file
techapplist.write(techapplist_ex, listsep=";", filename="techapplist_ex.csv")
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



##@@@@@@@@@@@@@@@@@@@@@@@@ joel test
library(triangle) 
library (trapezoid) 
library(rlist)  
library(gridExtra)
library(ColorPalette)
source("build.list.r")   
source("appfunctions.r")
source("mc.integrate.r") 
source("compute.techapp.r") 
source("compute.techapplist.r") 
source("techapplist.write.r") 
source("build.syslist_joel.r")  
caselist<- build.list("casedata_demo.csv")
techlist<- build.list("techdata_demo.csv")
sysliste_demo <- build.syslist("sysdata_demo.csv")
applist_demo=list()
applist_demo<-compute.techapplist(caselist,techlist,lsort=TRUE,lshowplot=TRUE)
source("compute.sysapp.by.product_joel.r")
sysapplist_demo.prod<-compute.sysapp.by.product(sysliste_demo,applist_demo)
source("compute.sysapp.by.mean_joel.r")
sysapplist_demo.mean<-compute.sysapp.by.mean(sysliste_demo,applist_demo)


################   Joel sesitivity
sensitivity=list()
for (i in 1:10){
  sysapplist_demo.prod<-compute.sysapp.by.product(sysliste_demo,applist_demo)
  sensitivity[[i]]=sysapplist_demo.prod
}




