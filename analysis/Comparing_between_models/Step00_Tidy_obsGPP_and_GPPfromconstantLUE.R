#################################
#Aim: Try to use the constant LUE to estimate the GPP
#constant LUE is estimated by using the data outside the green-up period
#-->to compare if constant LUE also overestimated GPP in the early spring!
#################################
library(dplyr)
library(lubridate)
library(ggplot2)
library(tidyr)
##add robust regression:
library(robustbase)
#--------------
#(1)load the merged fluxes data 
#and also used the phenophases(SOS and POS) extracted in "Using_sites_in_Fluxnet2015" analysis 
#to determine the green-up period 
#--------------
#--load the merged fluxes data
load.path<-"D:/data/photocold_project/Merge_Data/Using_sites_in_Fluxnet2015_constant_LUEmodel/"
load(paste0(load.path,"Merged_Flux_and_PhenoCam.RDA"))
#
df_merge$year<-year(df_merge$date)
df_merge$doy<-yday(df_merge$date)

#--load the phenophases data
phenos.path<-"D:/data/photocold_project/event_length/Using_sites_in_Fluxnet2015/"
load(paste0(phenos.path,"df_events_length.RDA"))
#Over_days_length-->days when gpp overestimated 
df_phenos<-df_events_all[,c("sitename","Year","sos","peak","Over_days_length")]
names(df_phenos)<-c("sitename","year","sos","peak","Over_days_length")

#------merge flux data and phenos data------------------------
df_merge<-left_join(df_merge,df_phenos,by=c("sitename","year"))
#only keep the site-year when sos and peak both available
df_final<-df_merge[!is.na(df_merge$sos)&!is.na(df_merge$peak),]
#calculate the PAR =fapar_itpl *ppdf_fluxnet2015
df_final$APAR<-df_final$fapar_itpl*df_final$ppfd_fluxnet2015

#-------------------------------------
#(2)simulate the constant LUE (based on DOY) for each site
#-------------------------------------
###---------
#First part: for all the available sites
###---------
df_final_all<-df_final
sites<-unique(df_final_all$sitename)
LUE_allsites<-c()
for(i in 1:length(sites)){
  df<-df_final_all %>%
    filter(sitename==sites[i])
  #select the data outside of the green-up period to model the LUE
  #df_out_greenup<-df[df$doy<df$sos|df$doy>df$peak,]
  # set the doy>peak in order to capture GPP peak using constant LUE
  df_out_greenup<-df[df$doy>df$peak,]  
  # #using robust regression:-->some error hence change to simple linear regression
  # lm_rob<-lmrob(gpp_obs~APAR - 1,data=df_out_greenup)  #set the intercept =0
  lm_norm<-lm(gpp_obs~APAR - 1,data=df_out_greenup)  #set the intercept =0
  stats_sum<-summary(lm_norm)
  # cbind(coef(lm_rob),confint(lm_rob, level = 0.95))
  LUE_temp<-data.frame("sitename"=sites[i],"LUE"=stats_sum$coefficients[1],"p-value"=stats_sum$coefficients[4])
  LUE_allsites<-rbind(LUE_allsites,LUE_temp)
}
LUE_final<-LUE_allsites[,c("sitename","LUE")]
names(LUE_final)<-c("sitename","conLUE")
#-----merge to df_final dataset----------
df_final_all<-left_join(df_final_all,LUE_final,by="sitename")

#-----------------------------------------------------
#compare the GPP_obs with GPP model through conLUE
#-----------------------------------------------------
df_final_all$gpp_conLUE<-df_final_all$APAR*df_final_all$conLUE
##save the data:
GPP_conLUE<-df_final_all
#
GPP_conLUE<-GPP_conLUE[,c("sitename","date","doy","sos","peak","Over_days_length","APAR","gpp_obs","gpp_mod_FULL","gpp_conLUE")]
# save the constant LUE GPP:
save.path<-"D:/data/photocold_project/GPP_from_diffSources/site_scale/"
save(GPP_conLUE,file = paste0(save.path,"GPP_conLUE_FLUX2015_daily.RDA"))
