rm(list=ls())
## ==============================================================================================
setwd("/Users/dorotheespuhler/Dropbox\ (Personal)/PHD\ Dropbox/1\ MODELLING/R/AppropriatenessDorothee/Appropriateness/")

# Load required library packages
library(triangle) # extra package for triangular distribution
library (trapezoid) # extra package for trapezoidial distribution
library(rlist)  # extra package to manupulate/filter app list
library(gridExtra)
library(ColorPalette)
library(ggplot2)

# Load required functions
source("build.list.r")   # This function reads the technology and case input data stored in a csv file...
# build.list(filename,n.info.row)
source("build.syslist.r") 
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
source("compute.techappscore.r") # Returns app.profile and app.score (aggregated profile)
  # compute.techappscore(tech, case,lshowplot=FALSE)
  # plots provide a graphical representaiton of the two functions and the overall
source("compute.techapplist.r") # Returns a list of app.profiles & app.score for all the techs and caes of a techlist and caseplist
  # compute.techapplist(techlist, caselist, listsep=" ", filename="")
source("techapplist.write.r") # writes applist either to screen or to a file if listsep and filename are provided
 # function(applist, listsep=" ", filename="") 
source("compute.sysapplist.r")

source("techapplist.frame.R")

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
caselist_demo<- build.list("casedata_demo.csv")
techlist_demo<- build.list("techdata_demo.csv")

## ==============================================================================================
# COMPUTE app.proiles FOR A PAIR OF TECH AND CASE (caselist$case, techlist$tech)
# Using compute.techapp
applist_demo=list()
#arbaminch
app.item.tmp <- compute.techappscore(caselist_demo$arbaminch, techlist_demo$uddt,lshowplot = TRUE)
applist_demo=append(applist_demo,list(app.item.tmp))
app.item.tmp <- compute.techappscore(caselist_demo$arbaminch, techlist_demo$dry.toilet,lshowplot = TRUE)
applist_demo=append(applist_demo,list(app.item.tmp))
app.item.tmp <- compute.techappscore(caselist_demo$arbaminch, techlist_demo$pour.flush,lshowplot = TRUE)
applist_demo=append(applist_demo,list(app.item.tmp))
app.item.tmp <- compute.techappscore(caselist_demo$arbaminch, techlist_demo$urine.storagetank,lshowplot = TRUE)
applist_demo=append(applist_demo,list(app.item.tmp))
app.item.tmp <- compute.techappscore(caselist_demo$arbaminch, techlist_demo$dehydration.vault,lshowplot = TRUE)
applist_demo=append(applist_demo,list(app.item.tmp))
app.item.tmp <- compute.techappscore(caselist_demo$arbaminch, techlist_demo$septic.tank,lshowplot = TRUE)
applist_demo=append(applist_demo,list(app.item.tmp))
app.item.tmp <- compute.techappscore(caselist_demo$arbaminch, techlist_demo$single.pit,lshowplot = TRUE)
applist_demo=append(applist_demo,list(app.item.tmp))
app.item.tmp <- compute.techappscore(caselist_demo$arbaminch, techlist_demo$double.pit,lshowplot = TRUE)
applist_demo=append(applist_demo,list(app.item.tmp))
app.item.tmp <- compute.techappscore(caselist_demo$arbaminch, techlist_demo$application.urine,lshowplot = TRUE)
applist_demo=append(applist_demo,list(app.item.tmp))
app.item.tmp <- compute.techappscore(caselist_demo$arbaminch, techlist_demo$application.faeces,lshowplot = TRUE)
applist_demo=append(applist_demo,list(app.item.tmp))
app.item.tmp <- compute.techappscore(caselist_demo$arbaminch, techlist_demo$testech,lshowplot = TRUE)
applist_demo=append(applist_demo,list(app.item.tmp))
app.item.tmp <- compute.techappscore(caselist_demo$arbaminch, techlist_demo$application.compost,lshowplot = TRUE)
applist_demo=append(applist_demo,list(app.item.tmp))                    
                    
#thimi
app.item.tmp <- compute.techappscore(caselist_demo$thimi, techlist_demo$uddt,lshowplot = TRUE)
applist_demo=append(applist_demo,list(app.item.tmp))
app.item.tmp <- compute.techappscore(caselist_demo$thimi, techlist_demo$dry.toilet,lshowplot = TRUE)
applist_demo=append(applist_demo,list(app.item.tmp))
app.item.tmp <- compute.techappscore(caselist_demo$thimi, techlist_demo$pour.flush,lshowplot = TRUE)
applist_demo=append(applist_demo,list(app.item.tmp))
app.item.tmp <- compute.techappscore(caselist_demo$thimi, techlist_demo$urine.storagetank,lshowplot = TRUE)
applist_demo=append(applist_demo,list(app.item.tmp))
app.item.tmp <- compute.techappscore(caselist_demo$thimi, techlist_demo$dehydration.vault,lshowplot = TRUE)
applist_demo=append(applist_demo,list(app.item.tmp))
app.item.tmp <- compute.techappscore(caselist_demo$thimi, techlist_demo$septic.tank,lshowplot = TRUE)
#provides an error as for the attr electricity thimi has dtriangle; while septic.tank has a pcat function
applist_demo=append(applist_demo,list(app.item.tmp))
app.item.tmp <- compute.techappscore(caselist_demo$thimi, techlist_demo$single.pit,lshowplot = TRUE)
applist_demo=append(applist_demo,list(app.item.tmp))
app.item.tmp <- compute.techappscore(caselist_demo$thimi, techlist_demo$double.pit,lshowplot = TRUE)
applist_demo=append(applist_demo,list(app.item.tmp))
app.item.tmp <- compute.techappscore(caselist_demo$thimi, techlist_demo$application.urine,lshowplot = TRUE)
applist_demo=append(applist_demo,list(app.item.tmp))
app.item.tmp <- compute.techappscore(caselist_demo$thimi, techlist_demo$application.faeces,lshowplot = TRUE)
applist_demo=append(applist_demo,list(app.item.tmp))
app.item.tmp <- compute.techappscore(caselist_demo$thimi, techlist_demo$testech,lshowplot = TRUE)
applist_demo=append(applist_demo,list(app.item.tmp))
app.item.tmp <- compute.techappscore(caselist_demo$thimi, techlist_demo$application.compost,lshowplot = TRUE)
applist_demo=append(applist_demo,list(app.item.tmp))      


## ==============================================================================================
# COMPUTE ENTIRE apppropriateness proiles FOR A LIST OF TECHNOLOGIES AND A LIST OF CASES IN ONE GO
# Using compute.techapplist 
applist_demo<-compute.techapplist(caselist_demo,techlist_demo, lpdfplot = T) # use lshowplot=F if
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
# Compute sysappscores
source("build.syslist.r")  

syslist_demo <- build.syslist("sysdata_demo.csv")

caselist<-caselist_demo
techlist<-techlist_demo

# by prod
sysapplist_demo.prod<-compute.sysapp.by.product(syslist_demo,applist_demo)
# by mean
sysapplist_demo.mean<-compute.sysapp.by.mean(syslist_demo,applist_demo)
#sysapplist.write(sysapplist_demo.mean)


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
compute.techappscore(caselist_test$arbaminch,techlist_test$uddt, lshowplot=FALSE)
compute.techappscore(caselist_test$arbaminch,techlist_test$dry.toilet, lshowplot=FALSE)
# Test entire lists
# Write to see
techapplist.write(techapplist_test)
techapplist.write(techapplist_test, listsep=";", filename="techapplist_daniel.csv")

## ==============================================================================================
# Testing data (RANDOM NOTES DOROTHEE)
# Read data
caselist_test<- build.list("casedata_test.csv")
techlist_test<- build.list("techdata_test.csv")

# Test one by one
compute.techappscore(caselist_test$arbaminch,techlist_test$T1.wsp, lshowplot=TRUE)
compute.techappscore(caselist_test$arbaminch,techlist_test$septic.tank, lshowplot=TRUE)
compute.techappscore(caselist_test$thimi,techlist_test$septic.tank, lshowplot=TRUE)

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
compute.techappscore(caselist_daniel$arbaminch,techlist_daniel$pour.flush, lshowplot=TRUE)
compute.techappscore(caselist_daniel$arbaminch,techlist_daniel$UDDT, lshowplot=TRUE)
compute.techappscore(caselist_daniel$arbaminch,techlist_daniel$septic.tank, lshowplot=TRUE)
compute.techappscore(caselist_daniel$arbaminch,techlist_daniel$single.pit, lshowplot=TRUE)
compute.techappscore(caselist_daniel$arbaminch,techlist_daniel$wsp, lshowplot=TRUE)
compute.techappscore(caselist_daniel$arbaminch,techlist_daniel$surface.flow.CW, lshowplot=TRUE)
# Compute entire list
techapplist_daniel<-compute.techapplist(caselist_daniel,techlist_daniel,lsort=TRUE)
# Write to screen
techapplist.write(techapplist_daniel)
# Write to file
techapplist.write(techapplist_daniel, listsep=";", filename="techapplist_daniel.csv")

## ==============================================================================================
#KATARNYIA

caselist_katarnyia<- build.list("casedata_katarnyia.csv")
techlist_katarnyia<- build.list("techdata_katarnyia.csv")

applist_katarnyia<-compute.techapplist(caselist_katarnyia,techlist_katarnyia, lpdfplot = T) # use lshowplot=F if

techapplist.write(applist_katarnyia)


techapplist.write(applist_katarnyia, listsep=";", filename="app_list_katarnyia.csv") 



## ==============================================================================================
# ARBA MINCH

caselist_arbaminch<- build.list("casedata_arbaminch.csv")
techlist_arbaminch<- build.list("techdata_arbaminch.csv")

applist_arbaminch<-compute.techapplist(caselist_arbaminch,techlist_arbaminch, lpdfplot = F) # use lshowplot=F if

techapplist.write(applist_arbaminch)


techapplist.write(applist_arbaminch, listsep=";", filename="app_list_arbamincha.csv") 

## ==============================================================================================
##@@@@@@@@@@@@@@@@@@@@@@@@ joel test
library(triangle) 
library (trapezoid) 
library(rlist)  
library(gridExtra)
library(ColorPalette)
source("build.list.r")   
source("appfunctions.r")
source("mc.integrate.r") 
source("compute.techappscore.r") 
source("compute.techapplist.r") 
source("techapplist.write.r") 
source("build.syslist.r")
source("sysapplist.write.r")  
caselist<- build.list("AppCase_Maria.csv")
techlist<- build.list("TechOp_Maria.csv")
syslist<- build.syslist("sysdata_demo.csv")
source("compute.sysapplist.r")
sysapplist<-compute.sysapplist(syslist,caselist,techlist,lsort=T,lshowplot=F,lpdfplot=F,aggmethod="mean")
sysapplist.write(sysapplist)

# Write to screen
techapplist<-compute.techapplist(caselist,techlist,lsort=TRUE,lshowplot=F,lpdfplot=F,aggmethod="mean")
source("techapplist.write.r")  
techapplist.write(techapplist, listsep=";", filename="techapplist_maria.csv")


compute.techappscore(caselist$arbaminch,techlist$dry.toilet, lshowplot=F,lpdfplot=F,aggmethod = "product")

################   Joel sesitivity
source("compute_sensitivity.R") 

sensitivity_100 <- compute_sensitivity(num_of_runs=1,aggmethod="mean",case="arbaminch1", n.sample=100)

>>>>>>> d657d32a098ea0d020f79d9604f7c1efc8040ef9


## ==============================================================================================
#KATARNYIA PAPER

techlist_katarnyia_DS<- build.list("input/didac/Techdata_Katarniya_DS-01.csv")
caselist_katarnyia_DS<- build.list("input/didac/Casedata_Katarnyia_DS-small.csv")

#create techapplist
techapplist_katarnyia_DS<-compute.techapplist(caselist_katarnyia_DS,techlist_katarnyia_DS) 
#techapplist_katarnyia_DS<-compute.techapplist(caselist_katarnyia_DS,techlist_katarnyia_DS, lshowplot = T)
#techapplist_katarnyia_DS<-compute.techapplist(caselist_katarnyia_DS,techlist_katarnyia_DS, lpdfplot = T)

#write techapplist to screen or csv
techapplist.write(techapplist_katarnyia_DS)
techapplist.write(techapplist_katarnyia_DS, listsep=";", filename="output/didac/Techapplist_Katarniya_DS-03.csv") 

#convert to dataframe and write to csv
techappframe_katarnyia_DS=techapplist.frame(techapplist_katarnyia_DS,techlist_katarnyia_DS, caselist_katarnyia_DS)
View(techappframe_katarnyia_DS)
write.table(techappframe_katarnyia_DS, file = "output/didac/Techappframe_katarnyia_DS-01.csv", sep = ";",row.names=F)
#techappframe_katarnyia_DS2=read.csv("output/didac/Techappframe_katarnyia_DS-01.csv", sep = ";")
#View(techappframe_katarnyia_DS2)

# plots
## -- define order of functional groups
techappframe_katarnyia_DS$functional.group_f = factor(techappframe_katarnyia_DS$functional.group, levels=c('U','Uadd','S','C','T','D'))
## -- histo per tech of all scores
ggplot(techappframe_katarnyia_DS, aes(x=techappframe_katarnyia_DS[,4:19])) +
  geom_boxplot() +
  facet_wrap( ~ functional.group_f)



par(mfcol=c(2,3))
boxplot(x=techappframe_katarnyia_DS[,4:19],data=techappframe_katarnyia_DS[techappframe_katarnyia_DS$functional.group== "U", ], col="orange", main="U")
boxplot(x=techappframe_katarnyia_DS[,4:19],data=techappframe_katarnyia_DS[techappframe_katarnyia_DS$functional.group== "Uadd", ], col="darkorange", main="Uadd")
boxplot(x=techappframe_katarnyia_DS[,4:19],data=techappframe_katarnyia_DS[techappframe_katarnyia_DS$functional.group== "S", ], col="green", main="S")
boxplot(x=techappframe_katarnyia_DS[,4:19],data=techappframe_katarnyia_DS[techappframe_katarnyia_DS$functional.group== "C", ], col="lightblue", main="C")
boxplot(x=techappframe_katarnyia_DS[,4:19],data=techappframe_katarnyia_DS[techappframe_katarnyia_DS$functional.group== "T", ], col="blue", main="T")
boxplot(x=techappframe_katarnyia_DS[,4:19],data=techappframe_katarnyia_DS[techappframe_katarnyia_DS$functional.group== "D", ], col="pink", main="D")

# -- plot histogram of tech app scores per functional group
ggplot(techappframe_katarnyia_DS, aes(x=techapp.score, fill=functional.group)) + geom_histogram(show.legend=F) + facet_wrap( ~ functional.group_f)
# -- plot histogram of all tech app scores (coloured per functional group)
ggplot(techappframe_katarnyia_DS, aes(x=techapp.score, fill=functional.group_f, order=functional.group_f)) + geom_histogram()
# -- boxplot of scores per functional group
ggplot(techappframe_katarnyia_DS, aes(y=techapp.score, x=functional.group_f, fill=functional.group_f)) +
  geom_boxplot()

# --simple plots
hist(techappframe_katarnyia_DS$techapp.score,xlim=c(0,1)) 
boxplot(techapp.score ~ functional.group_f,data=techappframe_katarnyia_DS)

