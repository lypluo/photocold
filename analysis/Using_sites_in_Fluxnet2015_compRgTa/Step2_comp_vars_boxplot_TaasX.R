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
#(4)going to compare the "event" and "non-event" site with boxplot
#-------------------------------------------------------------------------
#--------------
#first to classify the "df_dday" and "df_noevent_dday" into different N classes
#--------------
library(plyr)  
library(ggplot2)
library(cowplot)
library(grid)
Classify_Nbins_andPlot<-function(df,class_dday_range,class_var,N,do_manual_class){
  # df<-df_len5_nonnorm
  # class_dday_range<-c(-60,70)
  # class_var<-"temp_min_fluxnet2015"
  # N<-10
  # do_manual_class<-TRUE
  #----------------
  #do classification:
  #----------------
  df.event<-df$df_dday
  df.noevent<-df$df_noevent_dday
  #merge df.event and df.noevent
  df.event$year_flag<-rep("GPP overestimated sites",nrow(df.event))
  df.noevent$year_flag<-rep("GPP non-overestimated sites",nrow(df.noevent))
  #
  df.all<-rbind(df.event,df.noevent)
  df.all$year_flag<-factor(df.all$year_flag,levels = c("GPP overestimated sites","GPP non-overestimated sites"))
  #only select the data dday between -60 to 70:
  df.all<-df.all[df.all$dday>=class_dday_range[1] & df.all$dday<=class_dday_range[2],]
  
  # Bins for different variables
  xmin<-round(min(df.all[,class_var],na.rm = T),1)
  xmax<-round(max(df.all[,class_var],na.rm = T),1)         
  bins  <- seq( from=xmin, 
                to=xmax, by=(xmax-xmin)/N )
  if(do_manual_class==TRUE){
    xmin<-c(-40)
    xmax<-30
    bins  <- seq( from=xmin, 
                  to=xmax, by=(xmax-xmin)/N )
  }
  
  #
  df.all<-df.all %>% mutate(inbin=cut( as.numeric(df.all[,class_var]), breaks = bins ))
  #
  return(df.all)
}

##classify the df with the minimum temperature
# df_bins<-Classify_Nbins_andPlot(df_len5_nonnorm,c(-60,70),"temp_min_fluxnet2015",10,TRUE)

#-----------------
#start plotting
#----------------
comp_boxplot<-function(df,comp_yvar,do_legend,end_xylab){
  # df<-df_bins_Ta
  # comp_yvar<-c("SW_IN_fullday_mean_fluxnet2015")
  # do_legend<-FALSE
  # end_xylab<-c("Minium Ta (degreeC)","")

  #------------------------------------
  #plotting 
  #------------------------------------
  ##
  df.tidy<-df[,c("sitename","date",comp_yvar,"inbin","year_flag")]
  names(df.tidy)<-c("sitename","date","comp_vary","inbin","year_flag")
  #remove the rows when inbin ==NA
  df.tidy<-df.tidy[!is.na(df.tidy$inbin),]
  ##
  p_plot_main<-ggplot(df.tidy,aes(x = inbin, y = comp_vary,fill=year_flag))+
    geom_boxplot()+
    # annotate("rect",xmin=0,xmax=70,ymin = -Inf,ymax = Inf,alpha=0.2)+  #
    scale_fill_manual("",values = c("GPP overestimated sites"=adjustcolor("tomato",1),
                    "GPP non-overestimated sites"=adjustcolor("cyan3",1)))+
    theme(legend.position = c(0.25,0.9),legend.background = element_blank(),
          legend.text = element_text(size=20),
          axis.title = element_text(size=20),
          axis.text = element_text(size = 20),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())+
    xlab(end_xylab[1])+
    ylab(end_xylab[2])
  if(comp_yvar=="alpha_SW"|comp_yvar=="alpha_PPFD"){
    p_plot_main<-p_plot_main+
      ylim(0,1)
  }
  #legend
  if(do_legend==FALSE){
    p_plot_main<-p_plot_main+
      theme(legend.position = "none")
  }
   #
  return(p_plot_main)
}
##
# df<-df_bins
# comp_yvar<-c("SW_IN_fullday_mean_fluxnet2015")
# do_legend<-FALSE
# end_xylab<-c("Minium Ta (degreeC)","SW_IN full-dday mean (W m-2)")
# comp_boxplot(df_bins,c("SW_IN_fullday_mean_fluxnet2015"),TRUE,c("Minium Ta (degreeC)","SW_IN full-dday mean (W m-2)"))
#-------------------------------------------------------------------------
#(5) officially to compare the vars in two different group ("event" and "non_event")
#-------------------------------------------------------------------------
##mainly compare between the SW and Ta
#--------------
#classify based on the tmin(x),and plot others
#-------------
#1)classification:
df_bins_Ta<-Classify_Nbins_andPlot(df_len5_nonnorm,c(-60,70),"temp_min_fluxnet2015",10,TRUE)

#2)plotting
#tmin and SW_midday_mean
p_tmin_SW_fullday_mean<-comp_boxplot(df_bins_Ta,c("SW_IN_fullday_mean_fluxnet2015"),TRUE,
                                   c("Minimum Ta (ºC)","SW_IN full-dday mean (W m-2)"))
#tmin and SW_midday_mean
p_tmin_SW_midday_mean<-comp_boxplot(df_bins_Ta,c("SW_IN_midday_mean_fluxnet2015"),FALSE,
                                    c("Minimum Ta (ºC)","SW_IN middday mean (W m-2)"))
#tmin and SW_midday_max
p_tmin_SW_midday_max<-comp_boxplot(df_bins_Ta,c("SW_IN_midday_max_fluxnet2015"),FALSE,
                                   c("Minimum Ta (ºC)","SW_IN middday maximum (W m-2)"))
#tmin and PPFD_IN_midday_mean
p_tmin_PPFD_midday_mean<-comp_boxplot(df_bins_Ta,c("PPFD_IN_midday_mean_fluxnet2015"),FALSE,
                                     c("Minimum Ta (ºC)","PPFD_IN mid-dday (max umol m-2 s-1)"))
#tmin and PPFD_IN_midday_max
p_tmin_PPFD_midday_max<-comp_boxplot(df_bins_Ta,c("PPFD_IN_midday_max_fluxnet2015"),FALSE,
                                     c("Minimum Ta (ºC)","PPFD_IN mid-dday max (umol m-2 s-1)"))
#tmin and alpha_SW
p_tmin_alpha_SW<-comp_boxplot(df_bins_Ta,c("alpha_SW"),FALSE,c("Minimum Ta (ºC)","alpha_SW"))
#tmin and alpha_PPFD
p_tmin_alpha_PPFD<-comp_boxplot(df_bins_Ta,c("alpha_PPFD"),FALSE,c("Minimum Ta (ºC)","alpha_PPFD"))

#-------------------------------------------------------------------------
#(6) save the plot
#-------------------------------------------------------------------------
save.path<-"D:/plots/photocold_project/Using_sites_in_Fluxnet2015_compRgTa/boxplot/"
#merge plots
#--------------
#I.Merge Ta and other variables
#-------------
p_merge_TaandOther<-plot_grid(p_tmin_SW_fullday_mean,p_tmin_SW_midday_mean,p_tmin_SW_midday_max,
                              p_tmin_PPFD_midday_mean,p_tmin_PPFD_midday_max,
                              p_tmin_alpha_SW,
  labels = "auto",ncol=2,label_size = 18,align = "hv")

ggsave(paste0(save.path,"p_Taandother.png"),p_merge_TaandOther,width = 22,height = 22)

