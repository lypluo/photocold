used code order:
1) Check_PhenoCam&FLux_Data_Availability.R -->checking the data avaiability of PhenoCam and Flux data-->get a overview;
2) Download_PhenoCam_data.R--> downloading the phenocam data;
   Download the flux data from the Fluxnet
3) Preprocessing_data_from_fluxnet.R/Preprocessing_data_from_PhenoCam.R:
   preprocessing the data including steps such as:
   a.unzip the zip files
   b.select the interested variables
   c.summary the half-hourly data to daily data
4) Merge_FLUX_and_PhenoCam.R: merging the Flux data and PhenoCam data

After this-->used the merge data to continue the processing in: 
"C:\Users\yluo\Desktop\R_testcode\PhotoCold\Second_round_of_code\RMarkdonw\Step2_identify_events"
   
temporaily not used code:
"Download_Fluxnet_data_through_ingestr.R"-->the ingestr package did not run smooth...