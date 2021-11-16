##########################################
##exploring the r package phenocamapi,refer the website:
#https://mran.microsoft.com/snapshot/2020-03-10/web/packages/phenocamapi/vignettes/phenocam_data_fusion.html and
#https://www.neonscience.org/resources/learning-hub/tutorials/introduction-working-phenocam-images#toggle-0
##########################################
#------------------------------------
#(1)explore data avaiablity
#------------------------------------
#load package
library(data.table)
library(phenocamapi)
# the metadata as data.table
phenos <- get_phenos()

#-------------------------------------------------
#a. Filtering the Phenocam dataset using attributes
#list the sites that have flux tower:
#--------------------------------------------------
phenofluxsites <- phenos[flux_data==TRUE&!is.na(flux_sitenames)&flux_sitenames!='', .(PhenoCam=site, Flux=flux_sitenames)]
#sort the FLUX name in order to easy check the site infomation
phenofluxsites<-phenofluxsites[order(Flux)]

#--------------------------------------------------
#load the EC sites name I am processing 
#--------------------------------------------------
EC.path<-"D:/CES/Data_for_use/Fluxnet_Data/"
# processed.ECsites<-df_events_all[,.(Flux=unique(sitename))] # 23 sites
processed.ECsites<-read.csv(paste0(EC.path,"fluxnet2015_sites_sel_tidy_all.csv"))  #61 sites
#remove one sites from Austrailia(southern heimpshere):AU-Rig
pos_rm<-match("AU-Rig",processed.ECsites$SiteName)
processed.ECsites<-processed.ECsites[-pos_rm,]   #60 sites
#save the infomation:
save.path<-"D:/CES/Data_for_use/Merge_Data/"
write.csv(processed.ECsites,file=paste0(save.path,"ECflux_and_PhenoCam_site_info/ECflux_and_PhenoCam_site_info.csv"))
#--->then I manually add some information in the ECflux_and_PhenoCam_site_info.csv--->
#then I tidied it to ECflux_and_PhenoCam_site_info_add_manually.csv 

#-------------------------------------------------------
#match the processed EC sites with the phenoCam sites name
#-------------------------------------------------------
sort(phenofluxsites$Flux)

#---------------------
#comparing between phenofluxsies and processed.ECsites
#---------------------
#----------------
#For Beyond sites
#at the end, 16 sites of 37 sites have the PhenoCam data, they are:
#PhenoCam US: c(AT-Neu,CA-Gro,CA-Oas,CA-Obs,CA-TP1,CA-TP3,CA-TP4,IT-MBo,US-Ha1,US-MMS,US-NR1)
#PhenoCam Europe:c(BE-Bra,CH-Fru,DE-Lnf,DK-Sor,FR-Fon) http://european-webcam-network.net/
#-----------------
pos_sites<-which(phenos$flux_sitenames %in% c("AT-Neu","CA-Gro","CA-Oas","CA-Obs","CA-TP1","CA-TP3","CA-TP4","IT-MBo","US-Ha1","US-MMS","US-NR1"))
PhenoCam_US_source<-phenos[pos_sites,]
#----------------
#For Beni's sites
#at the end, 12 sites of 23 sites have the PhenoCam data, they are:
#DE-Hai, US-Syv,US-UMB, US-UMd, Us_Wcr,CA-Qfo, FI-Hyy,IT-Tor, BE-Vie, CH-Lae, DE-Gri,DE-Tha
#-----------------
#check this site information, e.g.(for PhenoCam sites in PhenoCam US)
PhenoCam_US_source<-phenos[flux_sitenames=="US-Syv"|flux_sitenames=="US-UMB"|flux_sitenames=="US-UMd"|flux_sitenames=="US-WCr"|
                             flux_sitenames=="CA-Qfo"|flux_sitenames=="IT-Tor"]
