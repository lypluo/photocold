#---------------------------------------
#Check the resulting p-model from Beni:
#---------------------------------------
#downloading the p-model output data from:https://zenodo.org/record/3559850#.YQ0IIYj7TlU

#-------------------
#(1)load the data
#-------------------
load.path<-"D:/CES/Data_for_use/p_model_output/"
gpp_p_model<-read.csv(paste0(load.path,"gpp_pmodel_fluxnet2015_stocker19gmd_daily.csv"))
#check the setup
unique(gpp_p_model$setup)
#check the sitenames in "FULL" mode
gpp_FULL<-subset(gpp_p_model,setup=="FULL")
length(unique(gpp_FULL$sitename))   #129

#----------------
#(2).save the p-model modeling daily data
#----------------
save.path<-"D:/CES/Data_for_use/p_model_output/Preprocessed_data/"
save(gpp_FULL,file=paste0(save.path,"GPP_pmodel_daily_data.RDA"))
