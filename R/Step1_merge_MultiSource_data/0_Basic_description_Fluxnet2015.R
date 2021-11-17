#########################################################
#Basic infos about the analysis: like the sites names...
#########################################################
#------------------------
#Info1: the flux-sites information in Fluxnet2015
#update in 08-06:at the end, used the sites from Info2
#------------------------
#----------------------------------------------
#A.the information directly from fluxnet website:
#https://fluxnet.org/sites/site-list-and-pages/
#----------------------------------------------
path<-"D:/CES/Data_for_use/Fluxnet_Data/"
site.info <- read.csv(paste0(path,"Basic_info_table.csv"))
# site-number:
nrow(site.info)
# select the sites that latitude bigger than 45 or smaller than -45:
sites.sel<-site.info[site.info$LOCATIION_LAT>45|site.info$LOCATIION_LAT<c(-45),]
nrow(sites.sel)
#only keep the grassland and forest sites
unique(sites.sel$IGBP)
sites.sel_YP<-sites.sel[sites.sel$IGBP=="GRA"|sites.sel$IGBP=="MF"|
                          sites.sel$IGBP=="ENF"|sites.sel$IGBP=="DBF"|sites.sel$IGBP=="DNF",]
nrow(sites.sel_YP) #72 sites
#save the information of selected sites:
# save.path<-"D:/CES/Data_for_use/Fluxnet_Data/"
# write.csv(sites.sel_YP,file = paste0(save.path,"selected.sites_YP.csv"))

#----------------------------------------------
#B.the information from Beni's r package ingestr:
#----------------------------------------------
library(ingestr)
siteinfo_fluxnet2015<-siteinfo_fluxnet2015
#only selecting the Koeppen_code is Cf.. or Df...
#(Koppen-Geiger climate regions that in temperate and cold region that without dry season)
#refer the table 2 in Beck et al., https://www.nature.com/articles/sdata2018214/tables/3
sel_kpcode<-c(grep("Cf",siteinfo_fluxnet2015$koeppen_code),
              grep("Df",siteinfo_fluxnet2015$koeppen_code))
siteinfo_Beni_sel<-siteinfo_fluxnet2015[sel_kpcode,]
#only keep the grassland and forest sites
siteinfo_Beni_sel<-subset(siteinfo_Beni_sel,classid=="GRA"|classid=="MF"|classid=="ENF"|
                            classid=="DBF"|classid=="DNF")
unique(siteinfo_Beni_sel$sitename) #64 sites
#save the information of selected sites:
save.path<-"D:/CES/Data_for_use/Fluxnet_Data/"
write.csv(siteinfo_Beni_sel,file = paste0(save.path,"siteinfo_Beni_sel.csv"))

#--------------------------------
#C. compare the differences in two data sources
#--------------------------------
# setdiff(sites.sel_YP$SITE_ID,siteinfo_Beni_sel$sitename)
# setdiff(siteinfo_Beni_sel$sitename,sites.sel_YP$SITE_ID)
#after the comparision, we found the both datasets both have some unique sites.
#|-->I tried to first select the koppen cimate type for each EC sites, then selected the grassland and
#forest ecosystem in Cf..or Df... climate zone
#!!-->see Info2.

#------------------------
#Info2: classify the flux-sites based on the Köppen-Geiger classification maps(Beck et al.,2018)
#------------------------
#download the map:Beck, H. E. et al. Figshare https://doi.org/10.6084/m9.figshare.6396959 (2018)
library(rgdal)
library(raster)
library(ncdf4)
library(lattice)
library(dplyr)
#load the map 
map.path<-"D:/CES/Data_for_use/Koppen-Geiger_Map/Beck_KG_V1/"
setwd(map.path)
#-------------------
#tif to raster data
#-------------------
#for 0.0083 reslutoin at present
KG_0p0083 <- "Beck_KG_V1_present_0p083.tif"
KG_0p0083_d = raster(KG_0p0083)
#-------------
#load the fluxnet2015 sites coordinates-->going to extract the values from those sites 
#-------------
fluxsites.path<-"D:/CES/Data_for_use/Fluxnet_Data/"
sites.info <- read.csv(paste0(fluxsites.path,"Basic_info_table.csv"))
#
map.sites <- sites.info[,c("SITE_ID","SITE_NAME","LOCATIION_LAT","LOCATION_LONG",
             "LOCATION_ELEV","IGBP","MAT","MAP")]
#renames-->corrsponding to Beni's datasets
names(map.sites)<-c("siteID","sitename","lat","lon","elv","IGBP","MAT","MAP")
map.plot<-map.sites
coordinates(map.plot) <- ~ lon + lat
#------------------------------------------------
# Define the coordinate system that will be used-->this step is also quite important
#------------------------------------------------
myCRS1 <- CRS("+init=epsg:4326") # WGS 84
# add Coordinate Reference System (CRS) projection. 
crs(map.plot) <- myCRS1
crs(KG_0p0083_d) <-myCRS1
#-------------------
#make a plot to show which sites are going to be extracted
#-------------------
# raster::plot(nsp_d1)
myCol <- terrain.colors(30)
#reference
#plot(KG_0p0083_d[[1]],col=myCol,main=names(KG_0p0083_d[[1]]),breaks=c(0,200,500,800,1200,1500,2000));
plot(KG_0p0083_d[[1]],col=myCol,main=names(KG_0p0083_d[[1]]));
points(map.plot,cex=1,col="red")
#------------------------
#extract the values from the map-->Köppen-Geiger code
#------------------------
p <- extract(KG_0p0083_d,map.plot,df=TRUE)
finalc <- as.data.frame(cbind(map.sites,p))
#convert the koppen-geiger code to character:
#ref: https://doi.org/10.6084/m9.figshare.6396959 (2018)
library(dplyr)
#select Cf..(14,15,16)and Df(25,26,27,28)...
sel_sites<-subset(finalc,Beck_KG_V1_present_0p083 %in% c(14:16,25:28))
#convert to character
#14-16:Cfa,Cfb,Cfc/ 25-28:Dfa,Dfb,Dfc,Dfd
sel_sites$Koeppen_code<-rep(NA,nrow(sel_sites))
for(i in 1:nrow(sel_sites)){
  if(sel_sites$Beck_KG_V1_present_0p083[i]==14){
    sel_sites$Koeppen_code[i]="Cfa"
  }
  if(sel_sites$Beck_KG_V1_present_0p083[i]==15){
    sel_sites$Koeppen_code[i]="Cfb"
  }
  if(sel_sites$Beck_KG_V1_present_0p083[i]==16){
    sel_sites$Koeppen_code[i]="Cfc"
  }
  if(sel_sites$Beck_KG_V1_present_0p083[i]==25){
    sel_sites$Koeppen_code[i]="Dfa"
  }
  if(sel_sites$Beck_KG_V1_present_0p083[i]==26){
    sel_sites$Koeppen_code[i]="Dfb"
  }
  if(sel_sites$Beck_KG_V1_present_0p083[i]==27){
    sel_sites$Koeppen_code[i]="Dfc"
  }
  if(sel_sites$Beck_KG_V1_present_0p083[i]==28){
    sel_sites$Koeppen_code[i]="Dfd"
  }
}
#only keep the forest and grasslands:
fluxnet2015_sites_sel<-subset(sel_sites,IGBP=="GRA"|IGBP=="MF"|IGBP=="ENF"|
                            IGBP=="DBF"|IGBP=="DNF")
nrow(fluxnet2015_sites_sel)  #92 sites
##save the information of selected sites:
save.path<-"D:/CES/Data_for_use/Fluxnet_Data/"
write.csv(fluxnet2015_sites_sel,file = paste0(save.path,"fluxnet2015_sites_sel.csv"))
#then-->go to "1_Preprocessing_data_from_fluxnet.R" to unzip the csv file data from fluxnet2015
#and processed and get the daily flux data
#-->after done that-->go Info3:

#------------------------
#Info3: how many years data in each site we are interested
#-->this is mainly pointing to the other sites beyond the sites Beni provided to me
#------------------------
data.path<-"D:/CES/Data_for_use/Fluxnet_Data/Preprocessed_data/Preprocessed_data/Other_sites/"
load(paste0(data.path,"Daily_data.RDA"))
#data years in each site
library(plyr)
sites_years<-ddply(df_all_others_sel_daily,.(sitename),summarise,site_years=c(sum(!is.na(GPP_NT_mean))/365))
#the sites that have longer than 3 years data:
sites_years_sel<-sites_years[sites_years$site_years>=3,]
##match with the PFT types:
sites.sel.info<-read.csv(paste0("D:/CES/Data_for_use/Fluxnet_Data/","fluxnet2015_sites_sel.csv"))
pos<-match(sites_years_sel$sitename,sites.sel.info$siteID)
#adding the PFT information
sites_years_sel<-data.frame(sites_years_sel,sites.sel.info[pos,])
#tidy up the site_infomation:
pos_rm<-match(c("X","siteID","ID","Beck_KG_V1_present_0p083"),names(sites_years_sel))
sites_years_sel_tidy<-sites_years_sel[,-pos_rm]
names(sites_years_sel_tidy)<-c("sitename","site_years","site_fullname","lat","lon","elv",
                               "IGBP","MAT","MAP","Koeppen_code")
##save the information of selected sites:
save.path<-"D:/CES/Data_for_use/Fluxnet_Data/"
write.csv(sites_years_sel_tidy,file = paste0(save.path,"fluxnet2015_sites_sel_tidy_beyondBeni.csv"))

#------------------------
#Info4:all the potential analyzed sites(Beni+Beyond)
#------------------------
#1) sites beyond
site_info_beyond<-sites_years_sel_tidy
names(site_info_beyond)<-c("SiteName","Site_years","Site_fullname","Lat.","Long.","ELV.","PFT","MAT","MAP","Clim.")
site_info_beyond$Site_flag<-rep("Beyond",nrow(site_info_beyond))
#2) sites analyzed using Beni sending data
library("readxl")
coord.path<-"D:/CES/Data_for_use/"
coord_Beni<-read_excel(paste0(coord.path,"Info_Table_about_Photocold_project.xlsx"),sheet = "ECsites_withPhenoCam")
#at the end, two sites do not used for analysis because of the data avaiablity:
rm_sites<-c("US-Wi3","RU-Ha1")
pos_rm<-match(rm_sites,coord_Beni$SiteName)
coord_Beni<-coord_Beni[-pos_rm,]
site_info_beni<-as.data.frame(coord_Beni)
#
site_info_beni$Site_flag<-rep("Beni",nrow(site_info_beni))
#merge two data source:
library(dplyr)
site_info_all<-bind_rows(site_info_beyond,site_info_beni) #61 sites
##save the information:
save.path<-"D:/CES/Data_for_use/Fluxnet_Data/"
write.csv(site_info_all,file = paste0(save.path,"fluxnet2015_sites_sel_tidy_all.csv"))
