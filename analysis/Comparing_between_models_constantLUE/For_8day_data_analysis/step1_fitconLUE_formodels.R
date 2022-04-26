##############################################################
#Aim: to fit the constant LUE for models such as VPM, BESS, and RF model
############################################################
library(dplyr)
library(tidyr)
library(tictoc)#-->record the parameterization time
library(GenSA) #parameterization
#-----------------
#(1)load the data
#----------------
load.path<-"D:/data/photocold_project/Merge_Data/Comparing_between_models_calibrated_nonGreeenUp/"
load(paste0(load.path,"GPP_from_diffSource_8day.RDA"))

#----------------
#(2)Fit constant LUE for models such as VPM, BESS, and RF model
#----------------
