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

#----------------
#(2)calibrate the gpp data beyong green-up for each model 
#----------------
#--------------------
# a.construct cost funtion
# returns the MSE(between gpp_mod and gpp_obs)
#--------------------------

model_calibration<-function(df.proc,par){
  k=as.numeric(par[1])
  par_cali<-rep(k,nrow(df.proc))
  return(par_cali)
}

cost.function <- function(df.proc,par) {
  
  scaling_factor <- df.proc %>%
    # group_by(sitename) %>%
    do({
      scaling_factor <-model_calibration(.,par)
      
      data.frame(
        sitename = .$sitename,
        date = .$date,
        scaling_factor = scaling_factor
      )
    })
  
  df.proc <- left_join(df.proc, scaling_factor)
  
  #rmse
  # rmse <- sqrt(
  #   sum(
  #     (df.proc$gpp_obs - df.proc$gpp_mod * df.proc$scaling_factor)^2)
  # )/nrow(df)
  #MSE:mean square error
  mse<- mean( ( df.proc$gpp_obs- df.proc$gpp_mod * df.proc$scaling_factor )^2, na.rm = TRUE )

  
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
par = c(1)
lower=c(0)
upper=c(10)

calibrate_fun<-function(df,model_name){
  df<-df_merge
  model_name<-"bess"
  
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
    #---------
    #parameter calibration 
    #----------
    
      optim_par <- GenSA::GenSA(
      par = par,
      fn = cost.function,  #cost function from a.
      data = df.proc,
      lower = lower,
      upper = upper,
      control = list(max.call=100))$par
      print(i)
    #################
    par_sites[[i]]<-optim_par
    
  }

  
}
