##############################################################
#Aim: to calibrate the gpp from diff models for the period beyond the green-up
# the calibration process is done for each site
############################################################
library(dplyr)
library(tidyr)
library(tictoc)#-->record the parameterization time
library(GenSA) #parameterization
#-----------------
#(1)load the data
#----------------
# load.path<-"D:/data/photocold_project/Merge_Data/Comparing_between_models_calibrated_nonGreeenUp/"
load("./data/temp_test_data/GPP_from_diffSource_daily.RDA")
# df_merge<-as.tibble(df_merge)
#----------------
#(2)calibrate the gpp data beyond green-up for each model 
#----------------
#--------------------
# a.construct cost funtion
# returns the MSE(between gpp_mod and gpp_obs)
#--------------------------
# source("D:/Github/gpp_bias_correction/R/model_hardening_byBeni.R")
#retrieve the calibration parameter (linear regression between modelled and observed gpp) for modelled gpp
model_calibration<-function(df.proc){
  lm_res<-lm(gpp_obs ~ 0 + gpp_mod, data=df.proc)
  par_cali<-as.numeric(lm_res$coefficients)
  return(par_cali)
}

#----------------------
#b.return optimilized parameter for each site for each model
#----------------------
# set initial value for parameter
calibrate_fun<-function(df,model_name){
  # df<-df_merge
  # model_name<-"rf"
  
  #---select the variables------
  gpp_name<-paste0("gpp_",model_name)
  df_sel<-df %>%
    select(sitename,date,doy,sos,peak,Over_days_length,APAR,gpp_obs,gpp_name)
  #
  names(df_sel)<-c("sitename","date","doy","sos","peak",
                   "Over_days_length","APAR","gpp_obs","gpp_mod")
  # df_sel<-df_sel[!is.na(df_sel$gpp_mod) & !is.na(df_sel$gpp_obs),]
  
  ###########################
  #---calibrate the modeled gpp compared to obs gpp during "beyond green-up" 
  df_sel_beyondGP<-df_sel[df_sel$doy<df_sel$sos | df_sel$doy>df_sel$peak,]
  df_sel_beyondGP<-df_sel_beyondGP[!is.na(df_sel_beyondGP$gpp_obs) & !is.na(df_sel_beyondGP$gpp_mod),]
  # #!!update from 2022,Feb-->according to Beni's suggestion,
  # #using the data above percentile 10-90(here I used the data above perc70)
  # perc_values<-quantile(df_sel_beyondGP$gpp_mod,probs = seq(0.1,1,0.1),na.rm = T)
  # df_sel_beyondGP<-df_sel_beyondGP[df_sel_beyondGP$gpp_mod>as.numeric(perc_values[7]),]
  
  #-----------
  #calibrate the gpp for each site
  #-----------
  sites<-unique(df_sel_beyondGP$sitename)
  
  ###retrive parameter for each site
  df.cali<-c()
  for (i in 1:length(sites)){
    df.ori<-df_sel %>%
      filter(sitename==sites[i])
    df.proc<-df_sel_beyondGP %>%
      filter(sitename==sites[i])
    #!!update from 2022,Feb-->according to Beni's suggestion,
    #using the data above percentile 10-90(here I used the data above perc70)
    perc_values<-quantile(df.proc$gpp_mod,probs = seq(0.1,1,0.1),na.rm = T)
    df.proc<-df.proc[df.proc$gpp_mod>as.numeric(perc_values[7]),]
    #---------
    #parameter calibration 
    #----------
    if(nrow(df.proc)>5){
      scaling_df <- df.proc %>%
        # group_by(sitename) %>%
        do({
          scaling_factor <-model_calibration(.)
          
          data.frame(
            sitename = sites[i],
            scaling_factor = scaling_factor
          )
        })
      #
      #calibration applies whole site on every date
      df.ori_add <- left_join(df.ori, scaling_df)
      #################
      df.cali<-rbind(df.cali,df.ori_add)
    }
    else
    next
  }
 ##

 ##tidy the names of data.frame
  gpp_names<-c(paste0("cali_gpp_",model_name))
  df.cali<-df.cali %>%
    mutate(gpp_mod_cali=gpp_mod*scaling_factor) %>%
    select(sitename,date,gpp_mod_cali)
  #
  Nvars<-length(names(df.cali))
  new.names<-c(names(df.cali)[-Nvars],
               gpp_names)
  names(df.cali)<-new.names
  return(df.cali)
}

#---------------------------------------
#(3)tidy the calibrated df and save the data
#---------------------------------------
#for bess,rf(same for the Fluxcom),and pmodel 
gpp_bess<-calibrate_fun(df_merge,"bess")
gpp_rf<-calibrate_fun(df_merge,"rf")
gpp_pmodel<-calibrate_fun(df_merge,"pmodel")
#merge the datasets:
gpp_daily_final<-left_join(gpp_pmodel,
                       gpp_bess,
                       by=c("sitename","date"))

gpp_daily_final<-left_join(gpp_daily_final,
                           gpp_rf,
                           by=c("sitename","date"))
#to merge all the data(both for the data in the green-up and beyond green-up):
gpp_daily_final<-left_join(df_merge,gpp_daily_final,by=c("sitename","date"))

save.path<-"D:/data/photocold_project/Merge_Data/Comparing_between_models_calibrated_nonGreeenUp/"
save(gpp_daily_final,file = paste0(save.path,"GPP_from_diffSource_daily_cali.RDA"))

#updated on Mar,09:
#############
#have a look at the performance for all the site-years identified as "overestimated sites"
#for each site or all sites
library(ggplot2)
gpp_daily_final_over<-gpp_daily_final[gpp_daily_final$Over_days_length>20 & !is.na(gpp_daily_final$Over_days_length),]
#
gpp_daily_final_over %>%
  filter(!is.na(gpp_bess))%>%
  group_by(sitename) %>%
  ggplot(aes(x=doy,y=cali_gpp_bess))+
  geom_point()+
  facet_wrap(~sitename)
#
df_allsites<-gpp_daily_final_over %>%
  select(sitename,date,doy,sos,peak,gpp_obs,starts_with("cali_"))%>%
  group_by(doy)%>%
  summarise(obs = mean(gpp_obs, na.rm = TRUE),
            mod_cali_pmodel = mean(cali_gpp_pmodel,na.rm=T),
            mod_cali_bess = mean(cali_gpp_bess,na.rm=T),
            mod_cali_rf = mean(cali_gpp_rf,na.rm=T),
            sos = mean(sos,na.rm=T),
            peak = mean(peak,na.rm=T)
  ) %>%
  pivot_longer(c(obs,mod_cali_pmodel,mod_cali_bess,mod_cali_rf),
               names_to = "Source", values_to = "gpp")
#
df_allsites %>%
  ggplot(aes(doy, gpp, color = Source))+
  geom_point(size=1.2)+
  # scale_color_manual(values = c("mod_conLUE" = "orange",
  #                               "mod_Pmodel"="steelblue2","obs" = "black"),
  #                    labels = c("constant LUE model","P model","Obs.")) +
  geom_vline(aes(xintercept=mean(sos,na.rm=T)),col="blue",lty=2,size=1.1)+
  annotate(geom = "text",x=90,y=10,label="sos",col="blue",size=6)+
  # geom_vline(aes(xintercept=mean(peak)),col="blue",lty=2,size=1.1)+
  labs(y = expression( paste("GPP (g C m"^-2, " d"^-1, ")" ) ),
       x = "Day of year")

