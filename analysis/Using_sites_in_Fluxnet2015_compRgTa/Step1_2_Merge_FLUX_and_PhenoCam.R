##########################################
#Try to merge the data from Fluxnet and PhenoCam
##########################################
#------------------------------------
#(1)load the daily flux-sites data
#------------------------------------
library(lubridate)
library(dplyr)
##a. load the processed fluxnet sites data done by me:
load.path<-"D:/data/photocold_project/FLUXNET_data/Using_sites_in_Fluxnet2015_compRgTa/"
load(paste0(load.path,"Daily_data.RDA"))
#sites form Fluxnet2015-->correspounding to Beni's datasets
df_YP_daily<-df_all_sel_daily
df_YP_daily$Date<-ymd(df_YP_daily$Date)
#rename the variables:
#"ppfd_fluxnet2015" corresponds to "ppfd_in_fluxnet2015"--> to combine with Beni's datasets, I used the first name
#adopt the "temp_day_fluxnet" name from Beni's dataset to represent mean daily temperature
names(df_YP_daily)<-c("sitename","date","temp_day_fluxnet2015","temp_min_fluxnet2015","temp_max_fluxnet2015",
                      "SW_IN_fullday_mean_fluxnet2015","SW_IN_midday_mean_fluxnet2015","SW_IN_midday_max_fluxnet2015",
                      "SW_OUT_fullday_mean_fluxnet2015","SW_OUT_midday_mean_fluxnet2015","SW_OUT_midday_max_fluxnet2015",
                      "PPFD_IN_fullday_mean_fluxnet2015","PPFD_IN_midday_mean_fluxnet2015","PPFD_IN_midday_max_fluxnet2015",
                      "PPFD_OUT_fullday_mean_fluxnet2015","PPFD_OUT_midday_mean_fluxnet2015","PPFD_OUT_midday_max_fluxnet2015",
                      "nee","gpp_nt","gpp_dt",
                      "TS_1_fluxnet2015","TS_2_fluxnet2015","TS_3_fluxnet2015","TS_4_fluxnet2015","TS_5_fluxnet2015","TS_6_fluxnet2015",
                      "TS_7_fluxnet2015"
                      # "SWC_1_fluxnet2015","SWC_2_fluxnet2015","SWC_3_fluxnet2015","SWC_4_fluxnet2015","SWC_5_fluxnet2015",
                      # "SWC_6_fluxnet2015","SWC_7_fluxnet2015"
                      )

##b. load Beni processed data:
load.path<-"D:/CES/Data_for_use/Data_sent_by_Beni/"
df_Beni<-read.csv(file=paste0(load.path,"ddf_fluxnet2015_pmodel_with_forcings_stocker19gmd.csv"))
##load the selected sites (from Fluxnet2015) info for anlysis 
load.path<-"D:/data/photocold_project/sel_sites_info/Using_sites_in_Fluxnet2015/"
load(paste0(load.path,"df_sites_sel.RDA"))
##check how many sites data available:
pos<-match(df_sites_sel$sitename,unique(df_Beni$sitename))
sel_pos<-pos[!is.na(pos)]
avai.sites<-unique(df_Beni$sitename)[sel_pos]
df_sites_avai<-df_sites_sel %>%
  filter(sitename %in% avai.sites)
#available sites data for analysis:
df_avai<-df_Beni %>%
  filter(sitename %in% avai.sites)
df_avai$date<-mdy(df_avai$date)
#save the available sites info:
# save(df_sites_avai,file=paste0(load.path,"df_sites_avai.RDA"))

#c. keep the variables that Beni's datasets(df_avai) have, and add other variables from df_all_daily
pos<-match(names(df_avai[,-c(1:2)]),names(df_YP_daily[,-c(1:2)]))
pos_del<-pos[!is.na(pos)]+2  #real position should +2
df_YP_daily_temp<-df_YP_daily[,-pos_del]

#get the merged datasets-->merge both df_avai and df_YP_daily_temp
library(reshape2)
df_merge_daily<-merge(df_avai,df_YP_daily_temp,by = c("sitename","date"),all.x = T)

#------------------------------------
#(2)load the daily PhenoCam data and merge to df_flux_merge
#------------------------------------
load.path<-"D:/CES/Data_for_use/PhenoCam_Data/Preprocessed_data/"
load(paste0(load.path,"Daily_data.RDA"))
df.Phenocam_daily$date<-as.Date(df.Phenocam_daily$date)
#
df_merge<-merge(df_merge_daily,df.Phenocam_daily,by=c("sitename","date"),all.x = T)
#save the merged data:
save.path<-"D:/data/photocold_project/Merge_Data/Using_sites_in_Fluxnet2015_compRgTa/"
save(df_merge,file=paste0(save.path,"Merged_Flux_and_PhenoCam.RDA"))

