#############################################################
#Download the FLUXNET Data and unzip them-->HH and DD file
############################################################
#-------------------------------------------------------------------
#(1) download the FLUXNET Data from FLUXNET2015(Tier 1:CC-BY-4.0 Data)
#link:https://fluxnet.org/data/download-data/
#-------------------------------------------------------------------
# the downloaded zip files are deposited in following folder:
ori.zip.path<-"D:/CES/Data_for_use/Fluxnet_Data/Download_data/"

#-------------------------------------------------------
#(2) After the data, unzip the files-->save the half-hourly and daily data
#into corresponding folder:
#-------------------------------------------------------
#----------------------
#a. function to unzip the zip file:
#----------------------
unzip_file<-function(ori.zip.path,sel_site,exdir.path){
  # ori.zip.path<-"D:/CES/Data_for_use/Fluxnet_Data/Download_data/"
  # sel_site<-"CH-Oe1"
  # exdir.path<-'D:/data/FLUXNET-2015_Tier1/HH'   #note do not write .../HH/, write ../HH
  
  #1).set the working directory to ori.zip.path
  setwd(ori.zip.path)
  all.files.name<-list.files()
  
  #2).find the zip file going to be unzipped
  pos_site_zip<-grep(sel_site,all.files.name)
  zip.file.name<-all.files.name[pos_site_zip]
  
  #3). only unzip the half-hourly data in the zip file
  files.name.inzip<-unzip(zip.file.name,list=TRUE)
  #select half-hourly(HH) or halfly(HR) csv
  pos_FULL_HH<-c(grep("_HH",files.name.inzip$Name),grep("_HR",files.name.inzip$Name))
  #only unzip the "FULLSET" csv file-->need to very careful with write the right directory of"zipfile" that going to be unzipped
  #also here exdir should end as".." rather than "../"
  unzip(zipfile=paste0(zip.file.name),files = files.name.inzip$Name[pos_FULL_HH],exdir=exdir.path)  
}
#---------------------
#b.unzip the files
#---------------------
ori.zip.path<-"D:/CES/Data_for_use/Fluxnet_Data/Download_data/"
exdir.path<-'D:/data/FLUXNET-2015_Tier1/HH'   #note do not write .../HH/, write ../HH
#unzip the sites:
setwd(ori.zip.path)
all.files.name<-list.files()
#all the zip files names
all.zip.files<-all.files.name[grep(".zip",all.files.name)]
site.names<-substr(all.zip.files,5,10)
for(i in 1:length(site.names)){
  unzip_file(ori.zip.path,site.names[i],exdir.path)
}



