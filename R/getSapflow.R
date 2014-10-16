#-- exploratory analysis of sapflow data at EucFACE. Data collected and processed by Teresa Gimeno


#--- load libraries
library(HIEv)
library(data.table)
library(reshape2)
library(plotBy)
library(car)
library(dplyr)
setToken(tokenfile="C:/Repos/wtc3_flux/HIEv_token.txt")
source(file="//ad.uws.edu.au/dfshare/HomesHWK$/30035219/My Documents/Work/R/generic_functions.R")

#--- download data from HIEv
downloadHIEv(searchHIEv("SAP_L2_"),topath="data/")

#------------------------------------------------------------------------------------------------------
#--- read data into memory, process
files <- list.files(pattern="SAP_L2_",path="data/",full.names=T)

dat.l <- dat.m <- c()
for (i in 1:length(files)){
  dat.l[[i]] <- as.data.frame(fread(files[[i]])) # data are in wide format, and need to be transformed to long format
  
  dat.m[[i]] <- reshape2::melt(data=dat.l[[i]],measure.vars=names(dat.l[[i]])[2:ncol(dat.l[[i]])],value.name="Sapflow",variable.name="Tree")
  
}
dat <- do.call(rbind,dat.m)
dat$DateTime <- as.POSIXct(dat$DateTime,format="%Y-%m-%d %X",tz="GMT")
dat$DateTime_hr <- nearestTimeStep(dat$DateTime,nminutes=60,align="floor")
dat$Ring <- as.factor(substr(dat$Tree,start=2,stop=2))
dat$Ctreat <- ifelse(dat$Ring=="1","elevated",
                     ifelse(dat$Ring=="2","ambient",
                            ifelse(dat$Ring=="3","ambient",
                                   ifelse(dat$Ring=="4","elevated",
                                          ifelse(dat$Ring=="5","elevated","ambient")))))  
#------------------------------------------------------------------------------------------------------



#------------------------------------------------------------------------------------------------------
#--- average by ring and then treatment, plot

#- ring averages for each hour
dat.r <- dplyr::summarize(dplyr::group_by(dat,DateTime_hr,Ring,Ctreat),
                          Sapflow = mean(Sapflow,na.rm=T),
                          Sapflow.sum = sum(Sapflow,na.rm=T))

#- treatment mean and se's for each hour
dat.t <- dplyr::summarize(dplyr::group_by(dat.r,DateTime_hr,Ctreat),
                          Sapflow = mean(Sapflow,na.rm=T),
                          Sapflow.se = standard.error(Sapflow,na.rm=T))

#------------------------------------------------------------------------------------------------------
windows(16,12)
xlims <- c(as.POSIXct("2012-9-20 00:00:00",tz="GMT"),as.POSIXct("2013-5-1 00:00:00",tz="GMT"))
plotBy(Sapflow~DateTime_hr|Ctreat,data=dat.t,type="p",xlim=xlims)
