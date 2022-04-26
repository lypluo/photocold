##Aim: Comparing the modelled GPP from different data sources(BESS, VPM,p-model,RF model,and constant LUE model)-->to investigate if modelled GPP are overestimated the GPP in the early spring? 
 specific steps:
  1) firstly to calibrated the GPP beyond green-up from different models with EC-based GPP; 
  2) using the calibrated GPP to fit the constant LUE in each model; 
  3) using the constant LUE to model the GPP for each model in the green-up period and compared the modeled GPP with EC GPP in the green-up period.

Code running order:
1) step0_tidy_GPP_from_diffSources_daily.R
2) step1_calibrate_GPP_beyondGP_diffModels_using_lm.R (at first, I used step1_calibrate_GPP_beyondGP_diffModels_using_GenSA.R-->but the calibration process does not work, 
    hence I use the simple way to calibrate the data,i.e. calibrate model gpp by developing linear regression between obs and mod gpp)
