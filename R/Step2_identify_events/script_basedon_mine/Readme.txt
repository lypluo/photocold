#R code running order:
1. stats_for_kls_method.R-->R script used to calculate the failure ratio when using the klosterman method...
2. biaes_distribution folder:
    biaes_distribution_outgreenup.R-->the distribution of biaes (GPP_mod-GPP_obs) outside of the green-up period(there are three ranges:1)[peak,265/366]; DoY[1, sos]& DOY[peak,365/366]; [1,sos] & [eos,365/366]-->at the end, I chose the [1,sos] & [eos,365/366] to determine the SD of the biaes)
      ---> this R script was used to test if biases with different range will impact the results (for Beni-flagged "over_estimation" sites)
    biaes_distribution_outgreenup_othersites.R-->similar as biaes_distribution_outgreenup.R but for Beni-flagged "non over-estimation" sites
3. gpp_isevent_stats.R-->comparison of "is_event" distribution that derived from different phenological extraction methods
4. gpp_overestimation_stats.R-->the script used to compare the distribution of different stats,i.e. the range of growing season (GS), GPP over_estimation days (over_estimation), greenup period(greenup),"is_event","is_event" when air temperature is lower than 10 (is_event_less10)
5. overestimation_stats_details_event_length.R-->plot the voilion plot of "event" length.
6. sep_isevent_siteyears_overyears_perc.R-->make a plot to show the percentage of the "GPP overestimation" in each site.
7. sep_isevent_siteyears_comp_vars.R-->make plot to compare each environmental variables between "overestimation" site-years and "non-overestimation" site-years