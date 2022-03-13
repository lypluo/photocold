##############################################################
#Aim: To fit the constant LUE for cali_gpp during the green-up
#-->and also assign this constant LUE to derive the gpp(conLUE*APAR) in the green-up period 
############################################################
library(dplyr)
library(tidyr)

#-------------------
#(1)load the data
#-------------------
load.path<-"D:/data/photocold_project/Merge_Data/Comparing_between_models_calibrated_nonGreeenUp/"
load(paste0(load.path,"GPP_from_diffSource_daily_cali.RDA"))

#-------------------
#(2)start to fit the constant LUE for calibrated during the non "green-up" period for each model:
#-------------------
fit_conLUE_fun_model<-function(df,model_name){
  
  # df<-gpp_daily_final
  # model_name<-"pmodel"
  
  #----fit the constant LUE for site(using data outside green-up period)--------
  gpp_name<-paste0("cali_gpp_",model_name)
  sites<-unique(df$sitename)
  LUE_allsites<-c()
  df_sel<-df%>%
    select(sitename,date,doy,sos,peak,APAR,gpp_name)
  Nvars<-length(df_sel)
  names(df_sel)<-c(names(df_sel)[-Nvars],"gpp_cali")
  #select the data outside of the green-up period to model the LUE
  df_sel_out_greenup<-df_sel[df_sel$doy<df_sel$sos|df_sel$doy>df_sel$peak,]
  #!!update from 2022,Feb-->according to Beni's suggestion,
  #using the data above percentile 10-90(here I used the data above perc50)
  perc_values<-quantile(df_sel_out_greenup$gpp_cali,probs = seq(0.1,1,0.1),na.rm = T)
  df_sel_out_greenup<-df_sel_out_greenup[df_sel_out_greenup$gpp_cali>as.numeric(perc_values[5]),]
  
  for (i in 1:length(sites)) {
    #
    df_temp<-df_sel_out_greenup %>%
      filter(sitename==sites[i])
    #linear regression:
    if(length(df_temp$gpp_cali[!is.na(df_temp$gpp_cali)])>5){
      lm_norm<-lm(gpp_cali~0+APAR,data=df_temp) #set the intercept =0
      stats_sum<-summary(lm_norm)
      LUE_temp<-data.frame("sitename"=sites[i],"LUE"=stats_sum$coefficients[1],"p-value"=stats_sum$coefficients[4])
    }
    if(length(df_temp$gpp_cali[!is.na(df_temp$gpp_cali)])==5){
      LUE_temp<-c("sitename"=sites[i],"LUE"=NA,"p-value"=NA)
    }
    #
    LUE_allsites<-rbind(LUE_allsites,LUE_temp)
  }
  #merge the conLUE to df
  LUE_allsites<-LUE_allsites[,c("sitename","LUE")]
  LUE_allsites$LUE<-as.numeric(LUE_allsites$LUE)
  names(LUE_allsites)<-c("sitename","conLUE_outGP")
  #
  df_final<-left_join(df_sel,LUE_allsites,by="sitename")
  #tidy the names of the data.frame
  gpp_names<-c(paste0("conLUE_gpp_",model_name))
  df.conLUE<-df_final %>%
    mutate(gpp_mod_conLUE=APAR*conLUE_outGP) %>%   #calculated conLUE for all the periods
    select(sitename,date,gpp_mod_conLUE)
  #
  Nvars<-length(names(df.conLUE))
  new.names<-c(names(df.conLUE)[-Nvars],
               gpp_names)
  names(df.conLUE)<-new.names
  return(df.conLUE)
}
fit_conLUE_fun_gppObs<-function(df){
  
  # df<-gpp_daily_final
  #----fit the constant LUE for site(using data outside green-up period)--------
  gpp_name<-paste0("gpp_","obs")
  sites<-unique(df$sitename)
  LUE_allsites<-c()
  df_sel<-df%>%
    select(sitename,date,doy,sos,peak,APAR,gpp_name)
  #select the data outside of the green-up period to model the LUE
  df_sel_out_greenup<-df_sel[df_sel$doy<df_sel$sos|df_sel$doy>df_sel$peak,]
  #!!update from 2022,Feb-->according to Beni's suggestion,
  #using the data above percentile 50/25(here I used the data above perc50)
  perc_values<-quantile(df_sel_out_greenup$gpp_obs,probs = seq(0.1,1,0.1),na.rm = T)
  df_sel_out_greenup<-df_sel_out_greenup[df_sel_out_greenup$gpp_obs>as.numeric(perc_values[5]),]
  
  for (i in 1:length(sites)) {
    #
    df_temp<-df_sel_out_greenup %>%
      filter(sitename==sites[i])
    #linear regression:
    if(length(df_temp$gpp_obs[!is.na(df_temp$gpp_obs)])>5){
      lm_norm<-lm(gpp_obs~0+APAR,data=df_temp) #set the intercept =0
      stats_sum<-summary(lm_norm)
      LUE_temp<-data.frame("sitename"=sites[i],"LUE"=stats_sum$coefficients[1],"p-value"=stats_sum$coefficients[4])
    }
    if(length(df_temp$gpp_obs[!is.na(df_temp$gpp_obs)])<=5){
      LUE_temp<-c("sitename"=sites[i],"LUE"=NA,"p-value"=NA)
    }
    #
    LUE_allsites<-rbind(LUE_allsites,LUE_temp)
  }
  #merge the conLUE to df
  LUE_allsites<-LUE_allsites[,c("sitename","LUE")]
  LUE_allsites$LUE<-as.numeric(LUE_allsites$LUE)
  names(LUE_allsites)<-c("sitename","conLUE_outGP")
  #
  df_final<-left_join(df_sel,LUE_allsites,by="sitename")
  #tidy the names of the data.frame
  gpp_names<-c(paste0("conLUE_gpp_","obs"))
  df.conLUE<-df_final %>%
    mutate(gpp_mod_conLUE=APAR*conLUE_outGP) %>%   #calculated conLUE for all the periods
    select(sitename,date,gpp_mod_conLUE)
  #
  Nvars<-length(names(df.conLUE))
  new.names<-c(names(df.conLUE)[-Nvars],
               gpp_names)
  names(df.conLUE)<-new.names
  return(df.conLUE)
}

#---------------------------------------
#(3)tidy the conLUE GPP and save the data
#---------------------------------------
#for bess,rf(same for the Fluxcom),and pmodel 
gpp_bess<-fit_conLUE_fun_model(gpp_daily_final,"bess")
gpp_rf<-fit_conLUE_fun_model(gpp_daily_final,"rf")
gpp_pmodel<-fit_conLUE_fun_model(gpp_daily_final,"pmodel")
#and also for gpp_obs
gpp_obs<-fit_conLUE_fun_gppObs(gpp_daily_final)
#------------------
#merge the datasets with conLUE:
gpp_daily_conLUE<-left_join(gpp_pmodel,
                           gpp_bess,
                           by=c("sitename","date"))
gpp_daily_conLUE<-left_join(gpp_daily_conLUE,
                           gpp_rf,
                           by=c("sitename","date"))
gpp_daily_conLUE<-left_join(gpp_daily_conLUE,
                           gpp_obs,
                           by=c("sitename","date"))



#to merge all the data(both for the data in the green-up and beyond green-up):
gpp_daily_conLUE_final<-left_join(gpp_daily_final,gpp_daily_conLUE,by=c("sitename","date"))
#save the data
save.path<-"D:/data/photocold_project/Merge_Data/Comparing_between_models_calibrated_nonGreeenUp/"
save(gpp_daily_conLUE_final,file = paste0(save.path,"GPP_from_diffSource_daily_conLUE.RDA"))

