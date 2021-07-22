##########################################
##exploring the r package phenocamapi,refer the website:
#https://mran.microsoft.com/snapshot/2020-03-10/web/packages/phenocamapi/vignettes/phenocam_data_fusion.html and
#https://www.neonscience.org/resources/learning-hub/tutorials/introduction-working-phenocam-images#toggle-0
##########################################
#------------------------------------
#(1)explore data avaiable
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
EC.path<-"C:/Users/yluo/Desktop/CES/Data_for_use/event_length/"
load(paste0(EC.path,"df_events_length.RDA"))
df_events_all<-as.data.table(df_events_all)
processed.ECsites<-df_events_all[,.(Flux=unique(sitename))] # 23 sites
#-------------------------------------------------------
#match the processed EC sites with the phenoCam sites name
#-------------------------------------------------------
sort(phenofluxsites$Flux)
#at the end, 12 sites of 23 sites have the PhenoCam data, they are:
#DE-Hai, US-Syv,US-UMB, US-UMd, Us_Wcr,CA-Qfo, FI-Hyy,IT-Tor, BE-Vie, CH-Lae, DE-Gri,DE-Tha
#check this site information, e.g.
PhenoCam_US_source<-phenos[flux_sitenames=="US-Syv"|flux_sitenames=="US-UMB"|flux_sitenames=="US-UMd"|flux_sitenames=="US-WCr"|
                             flux_sitenames=="CA-Qfo"|flux_sitenames=="IT-Tor"]
