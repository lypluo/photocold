Aim: this code is used for exploring if there is high light intensity under low temperature in the event sites.

# Running code steps for analysis:
Step0_Basic_description_Fluxnet2015-->
   Step1_1_Preprocessing_data_from_fluxnet
   Step1_2_Merge_FLUX_and_PhenoCam-->
     vignette_photocold_separate_norm_GPPmismatch_period_trs_new_all_overestimation_diff0_3SD_allsites_inFluxnet2015-->
       Step2_overestimation_stats_details_event_length-->      
         Step2_comp_vars_quantile_plot：using all sites--> Step2_comp_vars_quantile_plot_PFTs-->
         Step2_comp_vars_violinplot.R-->Step2_comp_vars_violinplot_PFTs.R-->
         Step2_comp_vars_boxplot.R(using Ta as x axix:_TaasX or using Rg as x axis:_RgasX) --> Step2_comp_vars_boxplot_PFTs.R(_TaasX or _RgasX)
         
          

#additional for the sites distribution map:
     Step3_Sites_distribution_map.R

##！The P-model gpp used here are the original P-model output (consistent with the Stocker et al., 2020)