#############################################################################
#Aim:compare the variables in "event" and "non-event" site years after aligning the data-->specifically for Temp
#In this script, I specifically focus on three selected sites: NL-Loo, US-PFa, and RU-Fyo,
#which both have the "gpp overestimation year" and "non-overestimation year"
#!!need to note: in this script--> I used the updated p-model gpp data:
#-------------------------------------------------------------------------
#(1)load the data that includes the "is_event" information
#-------------------------------------------------------------------------
library(dplyr)
#---------------------
#A.load the event_length data
#---------------------
load.path<-"D:/data/photocold_project/event_length/Using_sites_in_Fluxnet2015_updatePmodel/"
load(paste0(load.path,"df_events_length.RDA"))
#select the sites both have the "overestimated" and "non-overestimated" gpp:
df_events_sel<-df_events_all %>%
  filter(sitename == "NL-Loo" | sitename =="US-PFa" | sitename =="RU-Fyo")
#
df_events_sel_NL_Loo<-df_events_all %>%
  filter(sitename == "NL-Loo")
df_events_sel_US_PFa<-df_events_all %>%
  filter(sitename == "US-PFa")
df_events_sel_RU_Fyo<-df_events_all %>%
  filter(sitename == "RU-Fyo")

# a function to separate out the site-year that event_length higher than some thresholds(e.g. 30 days)
sep_siteyears<-function(df,sep_var,sep_thrs){
  # df<-df_events_all
  # sep_var<-"Over_days_length"
  # sep_thrs<-30
  #
  df_sep<-df[df[,sep_var]>sep_thrs & !is.na(df[,sep_var]),]
  pos_sep<-as.vector(which(df[,sep_var]>sep_thrs))
  #as some site the sep_var==NA, hence using this way to separate the data
  pos_left<-setdiff(as.vector(1:length(df[,sep_var])),pos_sep)
  df_left<-df[pos_left,]
  df_new<-list(df_sep,df_left)
  names(df_new)<-c("event_siteyears","noevent_siteyears")
  return(df_new)
}
##separate the site-year when the over_days_length > 20 days
df.sep20_NL_Loo<-sep_siteyears(df_events_sel_NL_Loo,"Over_days_length",20)
df.sep20_US_PFa<-sep_siteyears(df_events_sel_US_PFa,"Over_days_length",20)
df.sep20_RU_Fyo<-sep_siteyears(df_events_sel_RU_Fyo,"Over_days_length",20)
#---------------------
#B.load the data 
#---------------------
load.path<-"D:/data/photocold_project/Merge_Data/Using_sites_in_Fluxnet2015_updatePmodel/"
#from new method:
load(paste0(load.path,"ddf_labeled_norm_trs_newmethod_all_overestimation_Fluxnet2015_sites.RDA"))
df_all_sites<-ddf_labeled;rm(ddf_labeled) 
df_norm_all<-df_all_sites
#for selected sites:
df_norm_NL_Loo<-df_norm_all %>% filter(sitename=="NL-Loo")
df_norm_US_PFa<-df_norm_all %>% filter(sitename=="US-PFa")
df_norm_RU_Fyo<-df_norm_all %>% filter(sitename=="RU-Fyo")

#-------------------------------------------------------------------------
#(2)start to align the data according to Beni's functions of "align_events" and "get_consecutive"
#-------------------------------------------------------------------------
#---------------------------------
#source the functions to detect the consecutive events
#---------------------------------
fun.path<-"D:/Github/photocold/R/Step2_identify_events/Functions/functions_from_beni/"
#source get_consecutive.R
source(paste0(fun.path,"get_consecutive.R"))
#source get_consecutive_greenup.R
source(paste0(fun.path,"get_consecutive_greenup.R"))

#source align_event.R -->for event site years
source(paste0(fun.path,"align_events_df.R")) 
#source align_nonevent.R -->for non-event site years
source(paste0(fun.path,"align_nonevents_df.R"))
library(dplyr)
sep_siteyears_data<-function(df.data,dovars,df.sep,leng_threshold,before,after,nbins,do_norm){
  # df.data<-df_norm_NL_Loo
  # dovars<-c("gpp")
  # df.sep<-df.sep20_NL_Loo
  # leng_threshold<-5
  # before=60
  # after=0
  # nbins=10
  # do_norm=FALSE
  
  #-------------------------------------------------------------------------
  #A.separating the datasets:"event" and "nonevent" site years
  #-------------------------------------------------------------------------
  N<-nrow(df.sep$event_siteyears)
  df_events.years<-c()    #target
  df_nonevents.years<-c() #target
  #for is.event years
  pos.isevent<-c()
  for(i in 1:N){
    df.temp<-subset(df.data,sitename==df.sep$event_siteyears$sitename[i] & Year==df.sep$event_siteyears$Year[i])
    pos.temp<-c(which(df.data$sitename==df.temp$sitename & df.data$Year==df.temp$Year))
    
    df_events.years<-rbind(df_events.years,df.temp)
    pos.isevent<-c(pos.isevent,pos.temp)
  }
  #for no is.event years
  pos.non_isevent<-setdiff(c(1:nrow(df.data)),pos.isevent)
  df_nonevents.years<-df.data[pos.non_isevent,] #target
  
  #---------------------------------
  #B.start to align different events(for event site years) and green-up(for non-event site years) in each site-year
  #---------------------------------
  # df<-df_norm_all
  # leng_threshold<-20
  # before=30
  # after=30
  # nbins=10
  # do_norm=FALSE
  
  #function format
  # align_events <- function( df, leng_threshold, before, after, nbins, do_norm=FALSE )
  #at this stage-->bins do not used(now set default value as 10)
  #-------------
  #set different length_threshold-->5 days(consecutive 5 days overestimation will be an event)
  #-------------
  #and select the 30 days before the events 
  df_len_events<-align_events(df_events.years,dovars,leng_threshold = leng_threshold,before = before,after = after,
                                nbins = nbins,do_norm = do_norm)
  print("ok")
  df_len_nonevents<-align_nonevents(df_nonevents.years,dovars,leng_threshold = leng_threshold,before = before,after = after,
                                    nbins = nbins,do_norm = do_norm)
  #---------------------------------
  #C.separate the df_event and df_nonevent;
  #---------------------------------
  df_all<-c()
  #for event site years
  df_dday<-df_len_events$df_dday
  #for non_event site years, take the doy belongs to green-up period:
  df_noevent_dday<-df_len_nonevents$df_dday
  #
  df_all<-list(df_dday=df_dday,df_noevent_dday=df_noevent_dday)
  return(df_all)
}
#!!important step:leng_threshold=5-->merge the events(consecutive days are over 5 days)
#do_vars-->the variables that are going to be processed(normalized)-->actually do not use at the moment
names(df_norm_all)
do_vars<-c("gpp_obs","fapar_itpl","fapar_spl",paste0(c("ppfd","temp_day","temp_min","temp_max",
                                                       "vpd_day","prec","patm","SW_IN","ws",paste0("TS_",1:9),paste0("SWC_",1:5)),"_fluxnet2015"),
           "gcc_90","rcc_90")
#set the before events days from 30 days to 60 days
df_len5_nonnorm_NL_Loo<-sep_siteyears_data(df_norm_NL_Loo,do_vars,df.sep20_NL_Loo,5,60,0,10,FALSE)
df_len5_nonnorm_US_PFa<-sep_siteyears_data(df_norm_US_PFa,do_vars,df.sep20_US_PFa,5,60,0,10,FALSE)
df_len5_nonnorm_RU_Fyo<-sep_siteyears_data(df_norm_RU_Fyo,dovars,df.sep20_RU_Fyo,5,60,0,10,FALSE)

#-------------------------------------------------------------------------
#(3)calculating some important variables like LUE...
#-------------------------------------------------------------------------
#--------------------------
#calculating more variables
#--------------------------
#1)LUE=GPP/fAPAR*ppfd
#unit-->GPP: umol m-2 s-1; ppdf-->umol m-2 s-1; fAPRA: unitless
#2)GRVI=(gcc-rcc)/c(gcc+rcc)
Cal_add_vars<-function(df){
  # df<-df_len5_nonnorm_NL_Loo
  for(i in 1:length(df)){
    #
    df_proc<-df[[i]]
    df_proc$LUE<-df_proc$gpp/c(df_proc$fapar_itpl*df_proc$ppfd_fluxnet2015)
    df_proc$GRVI<-c(df_proc$gcc_90-df_proc$rcc_90)/c(df_proc$gcc_90+df_proc$rcc_90)
    #assign value back:
    df[[i]]<-df_proc
  }
  return(df)
}
#
df_len5_nonnorm_NL_Loo<-Cal_add_vars(df_len5_nonnorm_NL_Loo)
df_len5_nonnorm_US_PFa<-Cal_add_vars(df_len5_nonnorm_US_PFa)
df_len5_nonnorm_RU_Fyo<-Cal_add_vars(df_len5_nonnorm_RU_Fyo)
#-------------------------------------------------------------------------
#(4)going to compare the "event" and "non-event" site
#-------------------------------------------------------------------------
  #-------------------------------------------------------------------------
  #start plotting
  #-------------------------------------------------------------------------
  library(plyr)  
  library(ggplot2)
  library(cowplot)
  library(grid)
  
#function used to compare the "event" and "non_event" sites using geom_ribbon 
#source the comparsion test written by me:
fun.path<-"D:/Github/photocold/R/Step2_identify_events/Functions/functions_from_YP/"
source(paste0(fun.path,"Difference_test_for_2Classes.R"))

plot_2groups<-function(df,comp_var,var_unit,do_norm,do_legend){
    # df<-df_len5_nonnorm_NL_Loo
    # comp_var<-"temp_min_fluxnet2015"
    # var_unit<-"()"
    # do_norm<-FALSE
    # do_legend<-FALSE

    #
    df.event<-df$df_dday
    df.noevent<-df$df_noevent_dday
    #------------------------------------
    #remove the sites that have the point numbers smaller than 10
    #------------------------------------
    N_value<-ddply(df.noevent,.(sitename),summarise,N=length(date))
    rm_sites<-subset(N_value,N<10)
    if(nrow(rm_sites)>=1){
      for(i in nrow(rm_sites)){
        rm_site_name<-rm_sites[i]
        df.noevent<-df.noevent[df.noevent$sitename!=as.character(rm_site_name),]
      }  
    }
    df.noevent<-df.noevent
    #--------------------------------
    #selected the most relevant vars
    #for the comparison,select the original variable if "do_norm"==FALSE,otherwise "do_norm"==TRUE
    #--------------------------------
    if(do_norm==FALSE){
      df.event_sel<-df.event[,c("sitename","Year","date","dday",comp_var)]
      names(df.event_sel)<-c("sitename","Year","date","dday","comp_var")
      df.nonevent_sel<-df.noevent[,c("sitename","Year","date","dday",comp_var)]
      names(df.nonevent_sel)<-c("sitename","Year","date","dday","comp_var")
    }
    if(do_norm==TRUE){
      comp_var_norm<-paste0("ds",comp_var)
      df.event_sel<-df.event[,c("sitename","Year","date","dday",comp_var_norm)]
      names(df.event_sel)<-c("sitename","Year","date","dday","comp_var")
      #
      df.nonevent_sel<-df.noevent[,c("sitename","Year","date","dday",comp_var_norm)]
      names(df.nonevent_sel)<-c("sitename","Year","date","dday","comp_var")
      #also make var_unit()==""
      var_unit<-c()
    }
    #--------------------------------------
    #plotting
    #--------------------------------------
    # Create a text
    grob_event <- grobTree(textGrob("Event site-year", x=0.6,  y=0.9, hjust=0,
                                    gp=gpar(col="red", fontsize=18, fontface="italic")))
    # grob_event_name <- grobTree(textGrob(df_name, x=0.8,  y=0.1, hjust=0,
    #                                      gp=gpar(col="red", fontsize=18, fontface="italic")))
    grob_nonevent <- grobTree(textGrob("Non-Event site-year", x=0.6,  y=0.9, hjust=0,
                                       gp=gpar(col="green4", fontsize=18, fontface="italic")))
    # grob_nonevent_name<-grobTree(textGrob(df_name, x=0.8,  y=0.1, hjust=0,
    #                                       gp=gpar(col="red", fontsize=18, fontface="italic")))
    #y axis range:
    ymin<-min(range(df.event_sel$comp_var,na.rm = T),range(df.nonevent_sel$comp_var,na.rm = T))
    ymax<-max(range(df.event_sel$comp_var,na.rm = T),range(df.nonevent_sel$comp_var,na.rm = T))
    #x axis range
    # x_range_event<-range(df.event_sel$doy)
    # x_range_nonevent<-range(df.nonevent_sel$doy)
    ###start to make the quantiile plot:
    df.event_sel$flag<-rep("GPP overestimated years",nrow(df.event_sel))
    df.nonevent_sel$flag<-rep("GPP non-overestimated years",nrow(df.nonevent_sel))
    #!one thing need to pay attention-->use q90 to limit the dday range in sites as the sites diff a lot
    #to ensure the results do not impact by one specific site
    dday_range_event<-ddply(df.event_sel,.(sitename),summarize,min_dday=min(dday),max_dday=max(dday))
    dday_range_nonevent<-ddply(df.nonevent_sel,.(sitename),summarize,min_dday=min(dday),max_dday=max(dday))
    sel_dday_event<-floor(quantile(dday_range_event$max_dday,0.75))
    sel_dday_nonevent<-floor(quantile(dday_range_nonevent$max_dday,0.75))
    #
    df.event_sel<-df.event_sel[df.event_sel$dday<=sel_dday_event,]
    df.nonevent_sel<-df.nonevent_sel[df.nonevent_sel$dday<=sel_dday_nonevent,]
    ##merge and classify 
    df.all<-rbind(df.event_sel,df.nonevent_sel)
    df.all$flag<-factor(df.all$flag,levels = c("GPP overestimated years","GPP non-overestimated years"))
    ##
    #merge the different sites in "event" and "non-event" sites->calculate the quantiles at the same time
    df.all_q<-ddply(df.all,.(flag,dday),summarize,q10=quantile(comp_var,0.10,na.rm = T),q25=quantile(comp_var,0.25,na.rm = T),
          q50=quantile(comp_var,0.5,na.rm = T),q75=quantile(comp_var,0.75,na.rm = T),q90=quantile(comp_var,0.9,na.rm = T))
    #-----------------------
    #add a additional test-->to compare if the vars are significant different(q50) in two categories
    #----------------------
    compare_diff<-function(df_comp,sel_perc,flag){
      # df_comp<-df.all
      # sel_perc<-1             #which period (percentage of the minimum days) used to compare between vars in two category
      # flag<-"all"

      df_comp_event<-df_comp[df_comp$flag=="GPP overestimated years",]
      df_comp_nonevent<-df_comp[df_comp$flag=="GPP non-overestimated years",]
      #------
      #select the data
      #------
      range_dday_event<-range(df_comp_event$dday)
      range_dday_nonevent<-range(df_comp_nonevent$dday)
      range_sel<-c(min(range_dday_event[1],range_dday_nonevent[1]),
                   floor(sel_perc*min(range_dday_event[2],range_dday_nonevent[2])))
      #
      df_comp_event_sel<-subset(df_comp_event,dday>=range_sel[1]&dday<=range_sel[2])
      df_comp_nonevent_sel<-subset(df_comp_nonevent,dday>=range_sel[1]&dday<=range_sel[2])
      #additional-->for before events(b0):
      df_comp_event_b0<-subset(df_comp_event,dday>=range_sel[1]&dday<=0)
      df_comp_nonevent_b0<-subset(df_comp_nonevent,dday>=range_sel[1]&dday<=0)
      #make the statistical comparision:
      if(flag=="q50"){
        stat.test_1<-difftest_function(df_comp_event_sel$q50,df_comp_nonevent_sel$q50)
        stat.test_2<-difftest_function(df_comp_event_b0$q50,df_comp_nonevent_b0$q50)
        stat.test<-cbind(stat.test_1,stat.test_2)
        names(stat.test)<-c("All selecte period","Before events")
      }
      if(flag=="all"){
        stat.test_1<-difftest_function(df_comp_event_sel$comp_var,df_comp_nonevent_sel$comp_var)
        stat.test_2<-difftest_function(df_comp_event_b0$comp_var,df_comp_nonevent_b0$comp_var)
        stat.test<-cbind(stat.test_1,stat.test_2)
        names(stat.test)<-c("All selecte period","Before events")
      }
      return(stat.test)
    }
    #
    stat.alldata_for2<-compare_diff(df.all,1,"all")
    stat.q50_for2<-compare_diff(df.all_q,1,"q50")
    #making the quantile plot using ribbon function:
    p_plot<-ggplot(df.all_q)+
      # annotate("rect",xmin=0,xmax=max(df.all_q$dday),ymin = -Inf,ymax = Inf,alpha=0.2)+
      #some changes here
      annotate("rect",xmin=0,xmax=70,ymin = -Inf,ymax = Inf,alpha=0.2)+
      geom_line(aes(x=dday,y=q50,col=flag),size=1.05)+
      scale_color_manual("",values = c("GPP overestimated years"="red","GPP non-overestimated years"="green4"))+
      # geom_ribbon(aes(x=dday,ymin=q10,ymax=q90,fill=flag),alpha=0.15)+
      geom_ribbon(aes(x=dday,ymin=q25,ymax=q75,fill=flag),alpha=0.4)+
      scale_fill_manual("",values = c("GPP overestimated years"="red","GPP non-overestimated years"="green4"))+
      ylab(paste0(comp_var," ",var_unit))+
      theme_classic()+
      theme(legend.position = c(0.35,0.9),legend.background = element_blank(),
            legend.text = element_text(size=20),
            axis.title = element_text(size=24),
            axis.text = element_text(size = 20))+
      xlim(-60,70)  #add x range in 2021-09-25
    #legend
    if(do_legend==FALSE){
    p_plot<-p_plot+
      theme(legend.position = "none")
    }
    # print(p_plot)
    #returun object
    out<-c()
    out$stats_q50<-stat.q50_for2
    out$stats_alldata<-stat.alldata_for2
    out$plot<-p_plot
    
    return(out)
  }

#-------------------------------------------------------------------------
#(5) officially to compare the vars in two different group ("event" and "non_event")
#-------------------------------------------------------------------------
  ###potentially variables need to be checked:
  #fAPAR-itpl;fAPRA_spl
  #ppfd
  #temp_day
  #vpd_day
  #prec
  #patm
  #gpp_obs,gpp_mod_full,gpp_obs_norm,gpp_mod_norm,gpp_res

#2021-09-25:check the temp_min,temp_max,temp_day, and LUE
#update in 2021-11-16:update the gpp_obs, gpp_biaes
analysis_fun<-function(df_len5_nonnorm,site){
  # df_len5_nonnorm<-df_len5_nonnorm_NL_Loo
  # out_all<-c()
  # site<-"NL-Loo"
  #--------------
  #I.GPP and LUE
  #-------------
  #gpp_obs
  p_gpp_obs_len5_b60<-plot_2groups(df_len5_nonnorm,"gpp","(umol m-2 s-1)",do_norm = FALSE,do_legend = TRUE)
  #gpp biaes
  p_gpp_biaes_len5_b60<-plot_2groups(df_len5_nonnorm,"gpp_res","(umol m-2 s-1)",do_norm = FALSE,FALSE)
  #LUE
  p_LUE_len5_b60<-plot_2groups(df_len5_nonnorm,"LUE","",do_norm = FALSE,FALSE)
  #for ppfd
  p_ppfd_len5_b60<-plot_2groups(df_len5_nonnorm,"ppfd_fluxnet2015","(u mol m-2 s-1)",do_norm = FALSE,do_legend = FALSE)
  #fapar_spl and fapar_itpl
  p_fapar_itpl_len5_b60<-plot_2groups(df_len5_nonnorm,"fapar_itpl","",do_norm = FALSE,do_legend = FALSE)
  #some modifying in the plot:
  p_ppfd_len5_b60$plot<-p_ppfd_len5_b60$plot+
    ylab("ppfd (u mol m-2 s-1)")
  p_fapar_itpl_len5_b60$plot<-p_fapar_itpl_len5_b60$plot+
    ylab("fapar")
  #--------------
  #II.Environment variables
  #note by YP 2021-11-21:
  #-->there is no temp_min/temp_max,SW_IN,TS_1,SWC_1 data for original Fluxnet2015 data tidied from Beni
  #-------------
  #temp_day
  p_temp_day_len5_b60<-plot_2groups(df_len5_nonnorm,"temp_day_fluxnet2015","(degreeC)",do_norm = FALSE,FALSE)
  #temp_min
  p_temp_min_len5_b60<-plot_2groups(df_len5_nonnorm,"temp_min_fluxnet2015","(degreeC)",do_norm = FALSE,do_legend = TRUE)
  #temp_max
  p_temp_max_len5_b60<-plot_2groups(df_len5_nonnorm,"temp_max_fluxnet2015","(degreeC)",do_norm = FALSE,FALSE)
  #for prec
  p_prec_len5_b60<-plot_2groups(df_len5_nonnorm,"prec_fluxnet2015","(mm)",do_norm = FALSE,do_legend = FALSE)
  #vpd_day
  p_vpd_day_len5_b60<-plot_2groups(df_len5_nonnorm,"vpd_day_fluxnet2015","(Pa)",do_norm = FALSE,do_legend = FALSE)
  #SW_IN
  p_SW_IN_len5_b60<-plot_2groups(df_len5_nonnorm,"SW_IN_fluxnet2015","(W m-2)",do_norm = FALSE,FALSE)
  # TS_1-->first layer soil temperature
  if(site!="US-PFa"){
    p_TS_1_len5_b60<-plot_2groups(df_len5_nonnorm,"TS_1_fluxnet2015","(degreeC)",do_norm = FALSE,FALSE)
  }
  if(site=="US-PFa"){
  p_TS_1_len5_b60<-c()   #US-PFa did not have the TS measurements
  }
  #SWC_1-->first layer soil mosture
  p_SWC_1_len5_b60<-plot_2groups(df_len5_nonnorm,"SWC_1_fluxnet2015","(%)",do_norm = FALSE,FALSE)
  
  #some modifying in the plot:
  p_temp_min_len5_b60$plot<-p_temp_min_len5_b60$plot+
    ylab("Minimum Ta (ºC)")+
    ylim(-30,25)
  p_temp_day_len5_b60$plot<-p_temp_day_len5_b60$plot+
    ylab("Mean Ta (ºC)")+
    ylim(-30,25)
  p_temp_max_len5_b60$plot<-p_temp_max_len5_b60$plot+
    ylab("Maximum Ta (ºC)")+
    ylim(-30,25)
  p_prec_len5_b60$plot<-p_prec_len5_b60$plot+
    ylab("prec (mm)")
  p_vpd_day_len5_b60$plot<-p_vpd_day_len5_b60$plot+
    ylab("vpd_day (Pa)")
  p_SW_IN_len5_b60$plot<-p_SW_IN_len5_b60$plot+
    ylab("SW_IN (W m-2)")
  if(site!="US-PFa"){
  p_TS_1_len5_b60$plot<-p_TS_1_len5_b60$plot+
    ylab("TS (ºC)")}
  if(site=="US-PFa"){
    p_TS_1_len5_b60<-c()}
  p_SWC_1_len5_b60$plot<-p_SWC_1_len5_b60$plot+
    ylab("SWC (ºC)")
  #--------------
  #III.PhenoCam VIs
  #-------------
  if(length(df_len5_nonnorm$df_dday$gcc_90[!is.na(df_len5_nonnorm$df_dday$gcc_90)])>10){
  #gcc_90
  p_gcc_90_len5_b60<-plot_2groups(df_len5_nonnorm,"gcc_90","",do_norm = FALSE,TRUE)
  #rcc_90
  p_rcc_90_len5_b60<-plot_2groups(df_len5_nonnorm,"rcc_90","",do_norm = FALSE,FALSE)
  #GRVI:
  p_GRVI_len5_b60<-plot_2groups(df_len5_nonnorm,"GRVI","",do_norm = FALSE,FALSE)
  }
  if(length(df_len5_nonnorm$df_dday$gcc_90[!is.na(df_len5_nonnorm$df_dday$gcc_90)])<=10){
    #gcc_90
    p_gcc_90_len5_b60<-c()
    #rcc_90
    p_rcc_90_len5_b60<-c()
    #GRVI:
    p_GRVI_len5_b60<-c()
  }
  #merge all the outputs:
  out_all<-list(p_gpp_obs_len5_b60=p_gpp_obs_len5_b60,
               p_ppfd_len5_b60=p_ppfd_len5_b60,
               p_fapar_itpl_len5_b60=p_fapar_itpl_len5_b60,
               p_LUE_len5_b60=p_LUE_len5_b60,
               p_gpp_biaes_len5_b60=p_gpp_biaes_len5_b60, #GPP and LUE
               p_temp_min_len5_b60=p_temp_min_len5_b60,
               p_temp_day_len5_b60=p_temp_day_len5_b60,
               p_temp_max_len5_b60=p_temp_max_len5_b60,
               p_SW_IN_len5_b60=p_SW_IN_len5_b60,
               p_prec_len5_b60=p_prec_len5_b60,
               p_vpd_day_len5_b60=p_vpd_day_len5_b60, 
               p_SWC_1_len5_b60=p_SWC_1_len5_b60,
               p_TS_1_len5_b60=p_TS_1_len5_b60,#Environmental factors
               p_gcc_90_len5_b60=p_gcc_90_len5_b60,
               p_rcc_90_len5_b60=p_rcc_90_len5_b60,
               p_GRVI_len5_b60=p_GRVI_len5_b60  #VIs
               )
return(out_all)
}

#-------------------------------------------------------------------------
#(6) save the plot
#-------------------------------------------------------------------------
save.path<-"D:/plots/photocold_project/Using_sites_in_Fluxnet2015_updatePmodel/quantile_plot/Forsites_both_have_over_and_nonover_gpp_estimation/"
#for different sites:
out_NL_Loo<-analysis_fun(df_len5_nonnorm_NL_Loo,"NL-Loo")
out_US_PFa<-analysis_fun(df_len5_nonnorm_US_PFa,"US-PFa")
out_RU_Fyo<-analysis_fun(df_len5_nonnorm_RU_Fyo,"RU-Fyo")

#merge plots
merge_plot_fun<-function(out_all,save.path,site){
  # out_all<-out_NL_Loo
  # save.path<-save.path
  # site<-"NL-Loo"
  #--------------
  #I.GPP and LUE
  #-------------
  p_merge_Cflux<-plot_grid(out_all$p_gpp_obs_len5_b60$plot,
                           out_all$p_ppfd_len5_b60$plot,
                           out_all$p_fapar_itpl_len5_b60$plot,
                           out_all$p_LUE_len5_b60$plot,
                           out_all$p_gpp_biaes_len5_b60$plot,
                           labels = "auto",nrow=2,label_size = 18,align = "hv")
  # print(p_merge_Cflux)
  ggsave(paste0(save.path,site,"_","p_Cflux.png"),p_merge_Cflux,width = 20,height = 15)
  #--------------
  #II.Environment variables
  #-------------
  p_merge_EnviroVars<-plot_grid(
    out_all$p_temp_min_len5_b60$plot,out_all$p_temp_day_len5_b60$plot,out_all$p_temp_max_len5_b60$plot,
    out_all$p_SW_IN_len5_b60$plot,
    out_all$p_prec_len5_b60$plot,
    out_all$p_vpd_day_len5_b60$plot,
    out_all$p_SWC_1_len5_b60$plot,
    out_all$p_TS_1_len5_b60$plot,
    labels = "auto",nrow=2,label_size = 18,align = "hv")
  # p_merge_EnviroVars<-plot_grid(
  #   out_all$p_temp_day_len5_b60$plot,
  #   out_all$p_prec_len5_b60$plot,
  #   out_all$p_vpd_day_len5_b60$plot,
  #   labels = "auto",nrow=2,label_size = 18,align = "hv")
  # print(p_merge_EnviroVars)
  ggsave(paste0(save.path,site,"_","p_EnviroVars.png"),p_merge_EnviroVars,width = 28,height = 15)
  #--------------
  #III.PhenoCam VIs
  #-------------
  p_merge_VIs<-plot_grid(out_all$p_gcc_90_len5_b60$plot,
                         out_all$p_rcc_90_len5_b60$plot,
                         out_all$p_GRVI_len5_b60$plot,
                         labels = "auto",nrow=2,label_size = 18,align = "hv")
  # print(p_merge_VIs)
  ggsave(paste0(save.path,site,"_","p_VIs.png"),p_merge_VIs,width = 15,height = 10)
}

#
merge_plot_fun(out_NL_Loo,save.path,"NL-Loo")
merge_plot_fun(out_US_PFa,save.path,"US-PFa")
merge_plot_fun(out_RU_Fyo,save.path,"RU-Fyo")

###################################backup code#############################
#--------------
#IV.stats-->difference test
#-------------
# stats_merge_Cflux<-rbind(gpp_obs=p_gpp_obs_len5_b60$stats_q50[1,],
#                          ppdf=p_ppfd_len5_b60$stats_q50[1,],
#                          fapar=p_fapar_itpl_len5_b60$stats_q50[1,],
#                          LUE=p_LUE_len5_b60$stats_q50[1,],
#                          gpp_biases=p_gpp_biaes_len5_b60$stats_q50[1,])
# stats_merge_EnviroVars<-rbind( 
#   temp_min=p_temp_min_len5_b60$stats_q50[1,],
#   temp_day=p_temp_day_len5_b60$stats_q50[1,],
#   temp_max=p_temp_max_len5_b60$stats_q50[1,],
#   SW_IN=p_SW_IN_len5_b60$stats_q50[1,],
#   prec=p_prec_len5_b60$stats_q50[1,],
#   vpd=p_vpd_day_len5_b60$stats_q50[1,],
#   SWC_1=p_SWC_1_len5_b60$stats_q50[1,],
#   TS_1=p_TS_1_len5_b60$stats_q50[1,])
# stats_merge_VIs<-rbind(gcc_90=p_gcc_90_len5_b60$stats_q50[1,],
#                        rcc_90=p_rcc_90_len5_b60$stats_q50[1,],
#                        GRVI=p_GRVI_len5_b60$stats_q50[1,])
# #
# stats_merge_Cflux$flag<-rep("Cflux",nrow(stats_merge_Cflux))
# stats_merge_EnviroVars$flag<-rep("EnviroVars",nrow(stats_merge_EnviroVars))
# stats_merge_VIs$flag<-rep("VIs",nrow(stats_merge_VIs))
# stats_all<-rbind(stats_merge_Cflux,stats_merge_EnviroVars,stats_merge_VIs)
