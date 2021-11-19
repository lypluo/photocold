##########################################
#Try to merge the data from Fluxnet and PhenoCam
##########################################
#------------------------------------
#(1)load the daily flux-sites data
#------------------------------------
library(lubridate)
##load Beni processed data:
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
save(df_sites_avai,file=paste0(load.path,"df_sites_avai.RDA"))
#------------------------------------
#(2)load the daily PhenoCam data and merge to df_flux_merge
#------------------------------------
load.path<-"D:/CES/Data_for_use/PhenoCam_Data/Preprocessed_data/"
load(paste0(load.path,"Daily_data.RDA"))
df.Phenocam_daily$date<-as.Date(df.Phenocam_daily$date)
#
df_merge<-merge(df_avai,df.Phenocam_daily,by=c("sitename","date"),all.x = T)
#save the merged data:
save.path<-"D:/data/photocold_project/Merge_Data/Using_sites_in_Fluxnet2015/"
save(df_merge,file=paste0(save.path,"Merged_Flux_and_PhenoCam.RDA"))

