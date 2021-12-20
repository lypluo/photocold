#############################################################################
#Aim:compare the variables in "event" and "non-event" site years after aligning the data-->specifically for Temp
#-------------------------------------------------------------------------
#(1)load the data that includes the "is_event" information
#-------------------------------------------------------------------------
#---------------------
#A.load the event_length data
#---------------------
load.path<-"D:/data/photocold_project/event_length/Using_sites_in_Fluxnet2015_compRgTa/"
load(paste0(load.path,"df_events_length.RDA"))
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
df.sep20<-sep_siteyears(df_events_all,"Over_days_length",20)

#---------------------
#B.load the data 
#---------------------
load.path<-"D:/data/photocold_project/Merge_Data/Using_sites_in_Fluxnet2015_compRgTa/"
#from new method:
load(paste0(load.path,"ddf_labeled_norm_trs_newmethod_all_overestimation_Fluxnet2015_sites.RDA"))
df_all_sites<-ddf_labeled;rm(ddf_labeled) 
df_norm_all<-df_all_sites
  
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

sep_siteyears_data<-function(df.data,dovars,df.sep,leng_threshold,before,after,nbins,do_norm){
  # df.data<-df_norm_all
  # dovars<-c("gpp_obs")
  # df.sep<-df.sep20
  # leng_threshold<-5
  # before=30
  # after=0
  # nbins=10
  # do_norm=TRUE
  
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
#do_vars-->the variables that are going to be processed(normalized)-->actually do not processed in this process 
# names(df_norm_all)
# do_vars<-c("gpp_obs","fapar_itpl","fapar_spl",paste0(c("ppfd","temp_day","temp_min","temp_max",
#                                                        "vpd_day","prec","patm","SW_IN","ws",paste0("TS_",1:7),paste0("SWC_",1:5)),"_fluxnet2015"),
#            "gcc_90","rcc_90")
#set the before events days from 30 days to 60 days
df_len5_nonnorm<-sep_siteyears_data(df_norm_all,do_vars,df.sep20,5,60,0,10,FALSE)

#-------------------------------------------------------------------------
#(3)calculating some important variables like LUE...
#-------------------------------------------------------------------------
#--------------------------
#calculating more variables
#--------------------------
#1)LUE=GPP/fAPAR*ppfd
#unit-->GPP: umol m-2 s-1; ppdf-->umol m-2 s-1; fAPRA: unitless
#2)GRVI=(gcc-rcc)/c(gcc+rcc)
#3)albedo:alpha_SW<-SW_OUT/SW_IN; alpha_ppdf<-PPFD_IN/PPFD_OUT

for(i in 1:length(df_len5_nonnorm)){
  #
  df_proc<-df_len5_nonnorm[[i]]
  df_proc$LUE<-df_proc$gpp_obs/c(df_proc$fapar_itpl*df_proc$ppfd_fluxnet2015)
  df_proc$GRVI<-c(df_proc$gcc_90-df_proc$rcc_90)/c(df_proc$gcc_90+df_proc$rcc_90)
  df_proc$alpha_SW<-df_proc$SW_OUT_fullday_mean_fluxnet2015/df_proc$SW_IN_fullday_mean_fluxnet2015
  df_proc$alpha_PPFD<-df_proc$PPFD_OUT_fullday_mean_fluxnet2015/df_proc$PPFD_IN_fullday_mean_fluxnet2015
  #assign value back:
  df_len5_nonnorm[[i]]<-df_proc
}
#-------------------------------------------------------------------------
#(4)going to compare the "event" and "non-event" site withboxplot
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

comp_boxplot<-function(df,comp_vars,var_units,do_legend,end_xylab){
  # df<-df_len5_nonnorm
  # comp_vars<-c("temp_min_fluxnet2015","SW_IN_fullday_mean_fluxnet2015")
  # var_units<-c("(degreeC)","")
  # do_legend<-FALSE
  # end_xylab<-c("Minimum Ta (ºC)","SW_IN full-dday mean (W m-2)")
  
  #
  df.event<-df$df_dday
  df.noevent<-df$df_noevent_dday
  #------------------------------------
  #plotting 
  #------------------------------------
  df.event$flag<-rep("GPP overestimated sites",nrow(df.event))
  df.noevent$flag<-rep("GPP non-overestimated sites",nrow(df.noevent))
  df.tidy<-rbind(df.event,df.noevent)
  df.tidy$flag<-factor(df.tidy$flag,levels = c("GPP overestimated sites","GPP non-overestimated sites"))
  #only select the data dday between -60 to 70:
  df.tidy<-df.tidy[df.tidy$dday>=c(-60) & df.tidy$dday<=70,]
  ##
  df.tidy<-df.tidy[,c("sitename","date",comp_vars,"flag")]
  names(df.tidy)<-c("sitename","date","comp_varx","comp_vary","flag")
  ##
  p_plot_main<-ggplot(df.tidy,aes(x = comp_varx, y = comp_vary,fill=flag))+
    # annotate("rect",xmin=0,xmax=70,ymin = -Inf,ymax = Inf,alpha=0.2)+  #
    geom_violin(trim=TRUE)+
    # geom_boxplot(width=0.1)
    # scale_color_manual("",values = c("GPP overestimated sites"="red","GPP non-overestimated sites"="cyan"))+
    scale_fill_manual("",values = c("GPP overestimated sites"=adjustcolor("tomato",0.3),"GPP non-overestimated sites"=adjustcolor("cyan",0.5)))+
    xlab(paste0(comp_vars[1]," ",var_units[1]))+
    ylab(paste0(comp_vars[2]," ",var_units[2]))+
    theme(legend.position = c(0.15,0.9),legend.background = element_blank(),
          legend.text = element_text(size=20),
          axis.title = element_text(size=20),
          axis.text = element_text(size = 20),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())+
    xlab(end_xylab[1])+
    ylab(end_xylab[2])
    
  ifelse(comp_vars[2]!="alpha_SW",
    y_median<-round(median(df.tidy[df.tidy$flag=="GPP overestimated sites",]$comp_vary,na.rm = T),0),
    y_median<-round(median(df.tidy[df.tidy$flag=="GPP overestimated sites",]$comp_vary,na.rm = T),2))

    p_plot_y<-ggplot(df.tidy,aes(x = flag, y = comp_vary,fill=flag))+
    geom_boxplot(width=0.2,position = position_dodge(width = 0.1))+
    geom_hline(yintercept = y_median,lty=2,size=1.1,col="blue")+
    annotate(geom = "text",x=0.6,y=y_median+y_median*0.5,label=y_median,size=4)+
    scale_fill_manual("",values = c("GPP overestimated sites"=adjustcolor("tomato",0.3),"GPP non-overestimated sites"=adjustcolor("cyan",0.5)))+
    theme(legend.position = "none",
          legend.background = element_blank(),
          legend.text = element_blank(),
          axis.title = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_text(size = 18),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          # panel.border = element_blank(),
          rect=element_rect(fill="transparent"))
 
  x_median<-round(median(df.tidy[df.tidy$flag=="GPP overestimated sites",]$comp_varx,na.rm = T),0)
  p_plot_x<-ggplot(df.tidy,aes(x = flag, y = comp_varx,fill=flag))+
    geom_boxplot(width=0.2,position = position_dodge(width = 0.1))+
    geom_hline(yintercept = x_median,lty=2,size=1.1,col="blue")+
    annotate(geom = "text",x=0.6,y=x_median+3,label=x_median,size=4)+
    coord_flip()+
    scale_fill_manual("",values = c("GPP overestimated sites"=adjustcolor("tomato",0.3),"GPP non-overestimated sites"=adjustcolor("cyan",0.5)))+
    theme(legend.position = "none",
          legend.background = element_blank(),
          legend.text = element_blank(),
          axis.title = element_blank(),
          axis.text.x = element_text(size = 18),
          axis.text.y = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          # panel.border = element_blank(),
          rect=element_rect(fill="transparent"))
   ##variation range:
  if(comp_vars[2]!="alpha_SW"){
    p_plot_main<-p_plot_main+
      xlim(-55,20)+
      ylim(-300,1000)
    # xlim(-60,70)  #add x range in 2021-09-25
  }
  if(comp_vars[2]=="alpha_SW"){
    p_plot_main<-p_plot_main+
      xlim(-55,20)+
      ylim(-0.4,1)
    # xlim(-60,70)  #add x range in 2021-09-25
    p_plot_y<-p_plot_y+
      ylim(0,1)
  }
  #legend
  if(do_legend==FALSE){
    p_plot_main<-p_plot_main+
      theme(legend.position = "none")
  }
  
  plot.with.insert<-
    ggdraw()+
    draw_plot(p_plot_main)+
    draw_plot(p_plot_y,x=0.08,y=0.5,width =0.2,height = 0.3)+
    draw_plot(p_plot_x,x=0.4,y=0.1,width =0.3,height = 0.2)
  #
  return(plot.with.insert)
}


#-------------------------------------------------------------------------
#(5) officially to compare the vars in two different group ("event" and "non_event")
#-------------------------------------------------------------------------
##mainly compare between the SW and Ta
# df<-df_len5_nonnorm
# comp_vars<-c("temp_min_fluxnet2015","SW_IN_midday_mean_fluxnet2015")
# var_units<-c("(degreeC)","(W m-2)")
# do_legend<-FALSE

#--------------
#Environment variables
#-------------

#tmin and SW_midday_mean
p_tmin_SW_fullday_mean<-comp_boxplot(df_len5_nonnorm,c("temp_min_fluxnet2015","SW_IN_fullday_mean_fluxnet2015"),
                                    c("(degreeC)","(W m-2)"),TRUE,c("Minimum Ta (ºC)","SW_IN full-dday mean (W m-2)"))
#tmin and SW_midday_mean
p_tmin_SW_midday_mean<-comp_boxplot(df_len5_nonnorm,c("temp_min_fluxnet2015","SW_IN_midday_mean_fluxnet2015"),
             c("(degreeC)","(W m-2)"),FALSE,c("Minimum Ta (ºC)","SW_IN mid-dday mean (W m-2)"))
#tmin and SW_midday_max
p_tmin_SW_midday_max<-comp_boxplot(df_len5_nonnorm,c("temp_min_fluxnet2015","SW_IN_midday_max_fluxnet2015"),
                                    c("(degreeC)","(W m-2)"),FALSE,c("Minimum Ta (ºC)","SW_IN mid-dday maximum (W m-2)"))
#tmean and SW_midday_max
p_tmean_SW_midday_max<-comp_boxplot(df_len5_nonnorm,c("temp_day_fluxnet2015","SW_IN_midday_max_fluxnet2015"),
                              c("(degreeC)","(W m-2)"),FALSE,c("mean Ta (ºC)","SW_IN mid-dday mean (W m-2)"))
#tmin and SW_midday_max
p_tmin_PPFD_midday_max<-comp_boxplot(df_len5_nonnorm,c("temp_min_fluxnet2015","PPFD_IN_midday_max_fluxnet2015"),
                                   c("(degreeC)","(umol m-2 s-1)"),FALSE,c("Minimum Ta (ºC)","PPFD_IN mid-dday max (W m-2)"))
#tmin and alpha_SW
p_tmin_alpha_SW<-comp_boxplot(df_len5_nonnorm,c("temp_min_fluxnet2015","alpha_SW"),
                              c("(degreeC)",""),FALSE,c("Minimum Ta (ºC)","alpha_SW"))
#-------------------------------------------------------------------------
#(6) save the plot
#-------------------------------------------------------------------------
save.path<-"D:/plots/photocold_project/Using_sites_in_Fluxnet2015_compRgTa/violin_plot/"
#merge plots
#--------------
#I.GPP and LUE
#-------------
# p_merge_Cflux<-plot_grid(p_gpp_obs_len5_b60$plot,
#                          p_ppfd_len5_b60$plot,
#                          p_fapar_itpl_len5_b60$plot,
#                          p_LUE_len5_b60$plot,
#                          p_gpp_biaes_len5_b60$plot,
#                          labels = "auto",nrow=2,label_size = 18,align = "hv")
# ggsave(paste0(save.path,"p_Cflux.png"),p_merge_Cflux,width = 20,height = 15)
#--------------
#II.Environment variables
#-------------
p_merge_EnviroVars<-plot_grid(p_tmin_SW_fullday_mean,
                              p_tmin_SW_midday_mean,p_tmin_SW_midday_max,p_tmean_SW_midday_max,
                              p_tmin_PPFD_midday_max,p_tmin_alpha_SW,
  labels = "auto",ncol=2,label_size = 18,align = "hv")

ggsave(paste0(save.path,"p_EnviroVars.png"),p_merge_EnviroVars,width = 22,height = 22)
#--------------
#III.PhenoCam VIs
#-------------
# p_merge_VIs<-plot_grid(p_gcc_90_len5_b60$plot,
#                        p_rcc_90_len5_b60$plot,
#                        p_GRVI_len5_b60$plot,
#                        labels = "auto",nrow=2,label_size = 18,align = "hv")
# ggsave(paste0(save.path,"p_VIs.png"),p_merge_VIs,width = 15,height = 10)
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
