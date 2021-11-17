##########################################
#Try to merge the data from Fluxnet and PhenoCam
##########################################
#------------------------------------
#(1)load the daily flux-sites data
#------------------------------------
library(lubridate)
##load the processed fluxnet sites data done by me:
load.path<-"D:/CES/Data_for_use/Fluxnet_Data/Preprocessed_data/Preprocessed_data/"
load(paste0(load.path,"Daily_data.RDA"))
df_YP_daily<-df_all_sel_daily
df_YP_daily$Date<-ymd(df_YP_daily$Date)
#rename the variables:
names(df_YP_daily)<-c("sitename","date","temp_mean_fluxnet2015","temp_min_fluxnet2015","temp_max_fluxnet2015",
      "SW_IN_fluxnet2015","patm_fluxnet2015","prec_fluxnet2015","ws_fluxnet2015","ppfd_fluxnet2015",
      "nee","gpp_nt","gpp_dt","vpd_day_fluxnet2015",
      "TS_1_fluxnet2015","TS_2_fluxnet2015","TS_3_fluxnet2015","TS_4_fluxnet2015","TS_5_fluxnet2015","TS_6_fluxnet2015",
      "TS_7_fluxnet2015","TS_8_fluxnet2015","TS_9_fluxnet2015",
      "SWC_1_fluxnet2015","SWC_2_fluxnet2015","SWC_3_fluxnet2015","SWC_4_fluxnet2015","SWC_5_fluxnet2015")

##load Beni processed data:
load.path<-"D:/CES/Data_for_use/Data_sent_by_Beni/"
df_Beni<-read.csv(file=paste0(load.path,"ddf_fluxnet2015_pmodel_with_forcings_stocker19gmd.csv"))
#finding the corresponding sites between "df_Beni"and "df_all_sel_daily"
sel_sites<-unique(df_all_sel_daily$sitename)
#
df_Beni_daily<-c()
for(i in 1:length(sel_sites)){
  df_temp<-subset(df_Beni,df_Beni$sitename==sel_sites[i])
  df_Beni_daily<-rbind(df_Beni_daily,df_temp)
}
df_Beni_daily$date<-mdy(df_Beni_daily$date)

#------------------------------------
#(2)merge the daily flux data that from different data source:
#------------------------------------
#select the vars from df_YP_daily that going to be merged with:
sel_variables<-c("sitename","date","temp_min_fluxnet2015","temp_max_fluxnet2015","SW_IN_fluxnet2015","ws_fluxnet2015",
    paste0("TS_",c(1:9),"_fluxnet2015"),paste0("SWC_",c(1:5),"_fluxnet2015"))
df_YP_daily<-df_YP_daily[,sel_variables]
#merge the df_YP_daily and df_Beni_daily:
library(reshape2)
df_flux_merge<-merge(df_Beni_daily,df_YP_daily,by=c("sitename","date"),all.x = T)

#------------------------------------
#(3)load the daily PhenoCam data and merge to df_flux_merge
#------------------------------------
load.path<-"C:/Users/yluo/Desktop/CES/Data_for_use/PhenoCam_Data/Preprocessed_data/"
load(paste0(load.path,"Daily_data.RDA"))
#
df_merge<-merge(df_flux_merge,df.Phenocam_daily,by=c("sitename","date"),all.x = T)
#save the merged data:
save.path<-"C:/Users/yluo/Desktop/CES/Data_for_use/Merge_Data/"
save(df_merge,file=paste0(save.path,"Merged_Flux_and_PhenoCam.RDA"))
write.csv(df_merge,file = paste0(save.path,"Merged_Flux_and_PhenoCam.csv"),sep="\t")
