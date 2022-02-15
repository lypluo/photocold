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
#(2)calibrate the gpp data beyong green-up for each model 
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
  par_factor<-rep(par_cali,nrow(df.proc))
  return(par_factor)
}

#----------------------
#b.return optimilized parameter for each site for each model
#----------------------
# set initial value for parameter
calibrate_fun<-function(df,model_name){
  # df<-df_merge
  # model_name<-"bess"
  
  #---select the variables------
  gpp_name<-paste0("gpp_",model_name)
  df_sel<-df %>%
    select(sitename,date,doy,sos,peak,Over_days_length,APAR,gpp_obs,gpp_name)
  #
  names(df_sel)<-c("sitename","date","doy","sos","peak",
                   "Over_days_length","APAR","gpp_obs","gpp_mod")
  df_sel<-df_sel[!is.na(df_sel$gpp_mod) & !is.na(df_sel$gpp_obs),]
  
  ###########################
  #---calibrate the modeled gpp compared to obs gpp during "beyond green-up" 
  df_sel_beyondGP<-df_sel[df_sel$doy<df_sel$sos | df_sel$doy>df_sel$peak,]
  #-----------
  #calibrate the gpp for each site
  #-----------
  sites<-unique(df_sel_beyondGP$sitename)
  
  ###retrive parameter for each site
  df.cali<-c()
  for (i in 1:length(sites)){
    df.proc<-df_sel_beyondGP %>%
      filter(sitename==sites[i])
    # df.proc<-df_merge %>%
    #   filter(sitename==sites[i]) %>%
    #   mutate(gpp_obs=gpp,gpp=NULL)
    #---------
    #parameter calibration 
    #----------
    scaling_df <- df.proc %>%
      # group_by(sitename) %>%
      do({
        scaling_factor <-model_calibration(.)
        
        data.frame(
          sitename = .$sitename,
          date = .$date,
          scaling_factor = scaling_factor
        )
      })
    
    df.proc_add <- left_join(df.proc, scaling_df)
    #################
  df.cali<-rbind(df.cali,df.proc_add)
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

#
save.path<-"D:/data/photocold_project/Merge_Data/Comparing_between_models_calibrated_nonGreeenUp/"
save(gpp_daily_final,file = paste0(save.path,"GPP_from_diffSource_daily_cali.RDA"))
