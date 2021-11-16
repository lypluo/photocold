1. Basic information about the analyzed sites:
1) Basic_description_Fluxnet2015.R (related running code:"Preprocessing_data_from_fluxnet.R")
   -Preprocessing_data_from_fluxnet.R
   --> For fluxnet data:
   preprocessing the data including steps such as:
   a.unzip the zip files
   b.select the interested variables
   c.summary the half-hourly data to daily data
2. used code order to tidy the data:
1) Check_PhenoCam&FLux_Data_Availability.R -->checking the data avaiability of PhenoCam and Flux data-->get a overview;
   -->espeically check if the analyzed EC sites have the PhenoCam data
2) Download_PhenoCam_data.R--> downloading the phenocam data from PhenoCam US or getting the data from Koen for EuroPheno
   Download the flux data from the Fluxnet directly...

3) Preprocessing_data_from_PhenoCam.R/Preprocessing_data_from_p_model_output.R:
   - preprocessing the PhenoCam data and the data from p_model

4) Merge_FLUX_and_PhenoCam.R: merging the Flux data and PhenoCam data

3.Sites_distribution_map.R-->update the map after finished the step 2.

------------------------------------------------------------------------------------------------
After this-->used the merge data to continue the processing in: 
"C:\Users\yluo\Desktop\R_testcode\PhotoCold\Second_round_of_code\RMarkdonw\Step2_identify_events"

----------------------------------------------------------------------------------------------------   
temporaily not used code:
"Download_Fluxnet_data_through_ingestr.R"-->the ingestr package did not run smooth...