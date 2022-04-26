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
model_calibration<-function(df.proc,par){
  lm_res<-lm(gpp_obs ~ 0 + gpp_mod, data=df.proc)
  par_cali<-as.numeric(lm_res$coefficients)*par
  par_factor<-rep(par_cali,nrow(df.proc))
  return(par_factor)
}

cost.function <- function(df.proc,par) {
  scaling_df <- df.proc %>%
    # group_by(sitename) %>%
    do({
      scaling_factor <-model_calibration(.,par)

      data.frame(
        sitename = .$sitename,
        date = .$date,
        scaling_factor = scaling_factor
      )
    })

  df.proc_add <- left_join(df.proc, scaling_df)
  #rmse
  # rmse <- sqrt(
  #   sum(
  #     (df.proc$gpp_obs - df.proc$gpp_mod * df.proc$scaling_factor)^2)
  # )/nrow(df)
  #MSE:mean square error
  mse<- mean( ( df.proc_add$gpp_obs - df.proc_add$gpp_mod * df.proc_add$scaling_factor )^2, na.rm = TRUE )

  
  # This visualizes the process,
  # comment out when running for real
  # plot(df.proc$gpp_obs, type = 'p',ylim=c(0,12))
  # lines(df.proc$gpp_mod, col = "red")
  # lines(df.proc$gpp_mod * df.proc$scaling_factor, col = "blue",cex=1.2)
  # Sys.sleep(0.1)
  
  return(mse)
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
  par_sites<-c()
  for (i in 1:length(sites)){
    df.proc<-df_sel_beyondGP %>%
      filter(sitename==sites[i])
    # df.proc<-df_merge %>%
    #   filter(sitename==sites[i]) %>%
    #   mutate(gpp_obs=gpp,gpp=NULL)
    #---------
    #parameter calibration 
    #----------
    par = c(1)
    lower=c(0)
    upper=c(10)
      optim_par <- GenSA::GenSA(
      par = par,
      fn = cost.function,  #cost function from a.
      lower = lower,
      upper = upper,
      data = df.proc,
      control = list(max.call=100))$par
      
    #################
    par_sites[[i]]<-optim_par
    
  }
}
