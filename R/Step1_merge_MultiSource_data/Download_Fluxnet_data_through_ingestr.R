##########################################
#Try to download the data from Fluxnet using Beni's R pacakge ingestr:
#refer the website: https://github.com/stineb/ingestr
#At the end, as the function in this package does not run very smoothly, then I switch to 
#Fluxnet to download all the data...
##########################################
#------------------------------------
#(1)explore data avaiable variables in fluxnets
#------------------------------------
#load package
library(data.table)
library(ingestr)
# 
siteinfo_fluxnet2015<-as.data.frame(siteinfo_fluxnet2015)
mysites <- c("BE-Vie", "DE-Tha")
#
pos<-c(match(mysites[1],siteinfo_fluxnet2015$sitename),
       match(mysites[2],siteinfo_fluxnet2015$sitename))
siteinfo_fluxnet2015<-siteinfo_fluxnet2015[pos,]
siteinfo <- siteinfo_fluxnet2015 %>%
  mutate(date_start = lubridate::ymd(paste0(year_start, "-01-01"))) %>%
  mutate(date_end = lubridate::ymd(paste0(year_end, "-12-31")))
