#############################################################
#Download the CRU Data and unzip them-->
############################################################
#-------------------------------------------------------------------
#(1) download the CRU Data from following website:
#https://crudata.uea.ac.uk/cru/data/hrg/
# description for the data and variables: https://catalogue.ceda.ac.uk/uuid/58a8802721c94c66ae45c3baa4d814d0
#-------------------------------------------------------------------
# the downloaded zip files are deposited in following folder-->original file is .gz file
ori.zip.path<-"D:/CES/Data_for_use/CRU/Download_data/"

#-------------------------------------------------------
#(2) After the data, unzip the files-->
#-------------------------------------------------------
#using the gunzip in R.utils R pacakge
library(R.utils)

#---------------------
#unzip the files
#---------------------
ori.zip.path<-"D:/CES/Data_for_use/CRU/Download_data/ts_4.01/"
exdir.path<-'D:/data/cru/ts_4.01/'   
#unzip the sites:
setwd(ori.zip.path)
all.files.name<-list.files()
#all the gz files names
all.gz.files<-all.files.name[grep(".gz",all.files.name)]
dest.files.names<-substr(all.gz.files,1,31)
#unzip the file
for(i in 1:length(all.gz.files)){
  gunzip(filename=paste0(ori.zip.path,all.gz.files[i]),destname=paste0(exdir.path,dest.files.names[i]))
}


