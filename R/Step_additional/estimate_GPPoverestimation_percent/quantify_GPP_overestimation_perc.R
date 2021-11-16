#############################################################################
#Aim:quantify the event site-years about how much GPP was estimated?
#-------------------------------------------------------------------------
#(1)load the data after aligning and calculating some important variables like LUE...
#-------------------------------------------------------------------------
load.path<-"D:/CES/Data_for_use/Merge_Data/sep_event_data/"
load(paste0(load.path,"df_len5_nonnorm.RDA"))
  #--------------------------
  #calculating more variables
  #--------------------------
  #1)LUE=GPP/fAPAR*ppfd
  #unit-->GPP: umol m-2 s-1; ppdf-->umol m-2 s-1; fAPRA: unitless
  #2)GRVI=(gcc-rcc)/c(gcc+rcc)
  for(i in 1:length(df_len5_nonnorm)){
    #
    df_proc<-df_len5_nonnorm[[i]]
    df_proc$LUE<-df_proc$gpp_obs/c(df_proc$fapar_itpl*df_proc$ppfd_fluxnet2015)
    df_proc$GRVI<-c(df_proc$gcc_90-df_proc$rcc_90)/c(df_proc$gcc_90+df_proc$rcc_90)
    #assign value back:
    df_len5_nonnorm[[i]]<-df_proc
  }
###########################
#!!!only targets the event sites
###########################
df_events<-df_len5_nonnorm$df_dday
#--------------------
#(2)calculate how much %GPP was estimated 
#-------------------------------------------------------------------------
##########
#a.only keep data the dday>0:
##########
df_events_sel<-df_events[df_events$dday>=0,]

#######
#b.calculate the GPP overestimation precent for each site-years
#######
library(plyr)
#keep the same length(observation numbers) for both gpp_obs and gpp_mod_FULL
df_events_sel<-df_events_sel[!is.na(df_events_sel$gpp_obs)&!is.na(df_events_sel$gpp_mod_FULL),]
overyear_cumGPP.df<-ddply(df_events_sel,.(sitename,Year),summarise,cum_gpp_obs=sum(gpp_obs),cum_gpp_mod_FULL=sum(gpp_mod_FULL))

#calculate the overestimation percentage: gpp_mod-gpp_obs/gpp_obs
overyear_cumGPP.df$over_perc<-c(overyear_cumGPP.df$cum_gpp_mod_FULL - overyear_cumGPP.df$cum_gpp_obs)/overyear_cumGPP.df$cum_gpp_obs *100
#remove the 5% and 95% percentiles
q05<-quantile(overyear_cumGPP.df$over_perc,probs = 0.05)
q10<-quantile(overyear_cumGPP.df$over_perc,probs = 0.1)
q90<-quantile(overyear_cumGPP.df$over_perc,probs = 0.9)
q95<-quantile(overyear_cumGPP.df$over_perc,probs = 0.95)

#
over_cum_sel<-overyear_cumGPP.df[overyear_cumGPP.df$over_perc>q05&overyear_cumGPP.df$over_perc<q95,]
median(over_cum_sel$over_perc)
sd(over_cum_sel$over_perc)
