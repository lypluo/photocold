#############################################################################
#Aim:tidy the site-years that have the "is_event":the dates when GPP is overestimation.
#-------------------------------------------------------------------------
#(1)load the data of the "is_event" length
#-------------------------------------------------------------------------
library(plyr)
# library(dplyr)
##load the event_length data
load.path<-"C:/Users/yluo/Desktop/CES/Data_for_use/event_length/"
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
##separate the site-year when the over_days_length > 30 days
df.sep20<-sep_siteyears(df_events_all,"Over_days_length",20)
df.sep30<-sep_siteyears(df_events_all,"Over_days_length",30)
df.sep45<-sep_siteyears(df_events_all,"Over_days_length",45)
df.sep60<-sep_siteyears(df_events_all,"Over_days_length",60)

#-------------------------------------------------------------------------
#(2)load the all the site-years data and separate the data between the site with "is_event" and "no is_event(length<=30)"
#-------------------------------------------------------------------------
load.path<-"C:/Users/yluo/Desktop/CES/Data_for_use/Data_sent_by_Beni/"
#from new method:
load(paste0(load.path,"ddf_labeled_norm_trs_newmethod_all_overestimation.RDA"))
df_norm_trs_newM_oversites<-ddf_labeled;rm(ddf_labeled)  #sites flagged as Beni that have the gpp overestimation in the spring 
load(paste0(load.path,"ddf_labeled_norm_trs_newmethod_sites_flag_without_overestimation.RDA"))
df_norm_trs_newM_no_oversites<-ddf_labeled;rm(ddf_labeled)  #sites flagged as Beni that do not have the gpp overestimation in the spring
#merge two datasets
df_norm_all<-rbind(df_norm_trs_newM_no_oversites,df_norm_trs_newM_oversites)
##write a function to separate the datasets into two parts: GPP overestimation and no GPP overestimation
sep_siteyears_data<-function(df.data,df.sep,sep_method){
  # df.data<-df_norm_all
  # df.sep<-df.sep30
  # sep_method<-"EVENT"
  # here I defined 3 methods to separate the datasets:
  #1-GS:based on growing season; 2-Event:based on the overestimation period;3-Event10:based on overstimation and lower than 10 degree
  
  #-------------------------------------------------------------------------
  #1.separating the datasets:"event" and "nonevent" site years
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
  
  #-------------------------------------------------------------------------
  #2.separating the "is_event" period from "event" site-years
  #-------------------------------------------------------------------------
  ##A.for sep_method=="GS"
  #-------------------------------------------------------------------------
  if(sep_method=="GS"){
    ###for growing season days in "event" site-year
    df_isevents.sep<-c()
    for(i in 1:N){
      df.temp<-subset(df_events.years,sitename==df.sep$event_siteyears$sitename[i] & Year==df.sep$event_siteyears$Year[i] & 
                        doy>=df.sep$event_siteyears$sos & doy<=df.sep$event_siteyears$peak)
      df_isevents.sep<-rbind(df_isevents.sep,df.temp)
    }
    
    ###for growing season days in "non-event" site-year
    df_nonevents.sep<-df_nonevents.years[df_nonevents.years$greenup=="yes",]
  }
  #-------------------------------------------------------------------------
  ##B.for sep_method=="Event"
  #-------------------------------------------------------------------------
  if(sep_method=="EVENT"){
    ###for growing season days in "event" site-year
    df_isevents.sep<-c()
    for(i in 1:N){
      df.temp<-subset(df_events.years,sitename==df.sep$event_siteyears$sitename[i] & Year==df.sep$event_siteyears$Year[i] & 
                        doy>=df.sep$event_siteyears$Over_start & doy<=df.sep$event_siteyears$Over_end)
      df_isevents.sep<-rbind(df_isevents.sep,df.temp)
    }
    
    ###for growing season days in "non-event" site-year
    #tips-->using the quantile 10 and 90 of Over_start and Over_end respectively as the separate data
    doy.start.q<-quantile(df.sep$event_siteyears$Over_start,probs = seq(0,1,0.1),na.rm = T)
    doy.end.q<-quantile(df.sep$event_siteyears$Over_end,probs = seq(0,1,0.1),na.rm = T)
    doy.q<-round(as.numeric(c(doy.start.q[2],doy.end.q[10])),0)  #10% Over_start and 90% Over_end
    
    summary_siteyear<-function(df){
      #df<-df_nonevents.years
      
      df_greenup<-df[df$greenup=="yes",]
      df_greenup_sum<-ddply(df_greenup,.(sitename,Year),summarise,sos=min(doy),peak=max(doy))
      return(df_greenup_sum)
    }
    list.siteyears<-summary_siteyear(df_nonevents.years)
    #start to separat the data in the range of doy.q
    df_nonevents.sep<-c()
    for(i in 1:nrow(list.siteyears)){
      df.temp<-subset(df_nonevents.years,sitename==list.siteyears$sitename[i] & Year==list.siteyears$Year[i] & 
                        doy>=list.siteyears$sos[i] & doy<=list.siteyears$peak[i] & doy>=doy.q[1] & doy<=doy.q[2])
    ##using the range of doy.q to determine the range of "non-event" site year
      df_nonevents.sep<-rbind(df_nonevents.sep,df.temp)
    }
  }
  #-------------------------------------------------------------------------
  ##C.for sep_method=="Event10"
  #-------------------------------------------------------------------------
  if(sep_method=="EVENT10"){
    ###for growing season days in "event" site-year and when T<10
    df_isevents.sep<-c()
    for(i in 1:N){
      df.temp<-subset(df_events.years,sitename==df.sep$event_siteyears$sitename[i] & Year==df.sep$event_siteyears$Year[i] & 
                        doy>=df.sep$event_siteyears$Over_start_T10 & doy<=df.sep$event_siteyears$Over_end_T10)
      df_isevents.sep<-rbind(df_isevents.sep,df.temp)
    }
    
    ###for growing season days in "non-event" site-year
    #tips-->using the quantile 10 and 90 of Over_start and Over_end respectively as the separate data
    doy.start.q<-quantile(df.sep$event_siteyears$Over_start_T10,probs = seq(0,1,0.1),na.rm = T)
    doy.end.q<-quantile(df.sep$event_siteyears$Over_end_T10,probs = seq(0,1,0.1),na.rm = T)
    doy.q<-round(as.numeric(c(doy.start.q[2],doy.end.q[10])),0)  #10% Over_start and 90% Over_end
    
    summary_siteyear<-function(df){
      # df<-df_nonevents.years
      
      df_greenup<-df[df$greenup=="yes",]
      df_greenup_sum<-ddply(df_greenup,.(sitename,Year),summarise,sos=min(doy),peak=max(doy))
      return(df_greenup_sum)
    }
    list.siteyears<-summary_siteyear(df_nonevents.years)
    #start to separat the data in the range of doy.q
    df_nonevents.sep<-c()
    for(i in 1:nrow(list.siteyears)){
      df.temp<-subset(df_nonevents.years,sitename==list.siteyears$sitename[i] & Year==list.siteyears$Year[i] & 
                        doy>=list.siteyears$sos[i] & doy<=list.siteyears$peak[i] & doy>=doy.q[1] & doy<=doy.q[2])
      ##using the range of doy.q to determine the range of "non-event" site year
      df_nonevents.sep<-rbind(df_nonevents.sep,df.temp)
    }
  }
  
  #-------------------------------------------------------------------------
  #3.return the datasets
  #-------------------------------------------------------------------------
  df.final<-list(df_isevents.sep,df_nonevents.sep)
  names(df.final)<-c("isevent","nonevent")
  return(df.final)
}

#-------------------------------------------------------------------------
#(3)calculate the stats of df_isevents.seq and df_nonevents.seq and making plots
#-------------------------------------------------------------------------
#I.Taking df.sep30 as an example-------------------------
  #------------
  #A.separate the "is_event" days using different separate methods
  #-------------
  #a1. sep_method<-"GS"  
  df.sep20_GS<-sep_siteyears_data(df_norm_all,df.sep20,"GS")
  df.sep30_GS<-sep_siteyears_data(df_norm_all,df.sep30,"GS")
  df.sep45_GS<-sep_siteyears_data(df_norm_all,df.sep45,"GS")
  df.sep60_GS<-sep_siteyears_data(df_norm_all,df.sep60,"GS")

  #a2. sep_method<-"EVENT"  
  df.sep20_EVENT<-sep_siteyears_data(df_norm_all,df.sep20,"EVENT")
  df.sep30_EVENT<-sep_siteyears_data(df_norm_all,df.sep30,"EVENT")
  df.sep45_EVENT<-sep_siteyears_data(df_norm_all,df.sep45,"EVENT")
  df.sep60_EVENT<-sep_siteyears_data(df_norm_all,df.sep60,"EVENT")
  
  #a3. sep_method<-"EVENT10"  
  df.sep20_EVENT10<-sep_siteyears_data(df_norm_all,df.sep20,"EVENT10")
  df.sep30_EVENT10<-sep_siteyears_data(df_norm_all,df.sep30,"EVENT10")
  df.sep45_EVENT10<-sep_siteyears_data(df_norm_all,df.sep45,"EVENT10")
  df.sep60_EVENT10<-sep_siteyears_data(df_norm_all,df.sep60,"EVENT10")
  #------------
  #B.making plots
  #-------------
  library(ggplot2)
  library(cowplot)
  library(grid)
  comp_2groups<-function(df,comp_var,var_unit,legend_flag,df_name){
    # df<-df.sep45_EVENT
    # comp_var<-"gpp_obs"
    # # sel_var<-"gpp_obs_roll20"
    # var_unit<-"(g C m-2)"
    # legend_flag<-TRUE
    # df_name<-"EVENT"
    
    #
    df.event<-df$isevent
    df.noevent<-df$nonevent
    #selected the most relevant vars
    df.event_sel<-df.event[,c("sitename","Year","date","doy",comp_var)]
    names(df.event_sel)<-c("sitename","Year","date","doy","comp_var")
    df.nonevent_sel<-df.noevent[,c("sitename","Year","date","doy",comp_var)]
    names(df.nonevent_sel)<-c("sitename","Year","date","doy","comp_var")
    #plotting
    #adding the fitting lines in each sites:
    # Create a text
    grob_event <- grobTree(textGrob("Event Sites", x=0.7,  y=0.9, hjust=0,
                              gp=gpar(col="red", fontsize=18, fontface="italic")))
    grob_event_name <- grobTree(textGrob(df_name, x=0.8,  y=0.1, hjust=0,
                                                     gp=gpar(col="red", fontsize=18, fontface="italic")))
    grob_nonevent <- grobTree(textGrob("Non-Event Sites", x=0.7,  y=0.9, hjust=0,
                                    gp=gpar(col="red", fontsize=18, fontface="italic")))
    grob_nonevent_name<-grobTree(textGrob(df_name, x=0.8,  y=0.1, hjust=0,
                                                     gp=gpar(col="red", fontsize=18, fontface="italic")))
    #y axis range:
    ymin<-min(range(df.event_sel$comp_var,na.rm = T),range(df.nonevent_sel$comp_var,na.rm = T))
    ymax<-max(range(df.event_sel$comp_var,na.rm = T),range(df.nonevent_sel$comp_var,na.rm = T))
    #x axis range
    # x_range_event<-range(df.event_sel$doy)
    # x_range_nonevent<-range(df.nonevent_sel$doy)
    p_event<-ggplot()+
      geom_point(data=df.event_sel,aes(x=doy,y=comp_var,col=sitename),alpha=0.3,pch=16)+
      # geom_line(data=df.event_sel,aes(x=doy,y=sel_var),col="black")+
      geom_smooth(data=df.event_sel,aes(x=doy,y=comp_var,col=sitename),method = "lm",
                  formula = y ~ poly(x,4) ,se=FALSE)+
      ylab(paste0(comp_var," ",var_unit))+
      ylim(ymin,ymax)+
      xlim(0,250)+
      annotation_custom(grob = grob_event)+
      annotation_custom(grob=grob_event_name)+
      theme_classic()+
      theme(axis.title = element_text(size=18),
        axis.text = element_text(size = 16))
    p_nonevent<-ggplot()+
      geom_point(data=df.nonevent_sel,aes(x=doy,y=comp_var,col=sitename),alpha=0.3,pch=16)+
      geom_smooth(data=df.nonevent_sel,aes(x=doy,y=comp_var,col=sitename),method = "lm",
                  formula = y ~ poly(x,4) ,se=FALSE)+
      ylab(paste0(comp_var," ",var_unit))+
      ylim(ymin,ymax)+
      xlim(0,250)+
      annotation_custom(grob = grob_nonevent)+
      annotation_custom(grob = grob_nonevent_name)+
      theme_classic()+
      theme(axis.title = element_text(size=18),
            axis.text = element_text(size = 16))
    if(legend_flag==TRUE){
      p_event<-p_event+
        theme(legend.position = c(0.2,0.7),legend.background = element_blank())+
        guides(col = guide_legend(ncol=2))
      p_nonevent<-p_nonevent+
        theme(legend.position = c(0.2,0.7),legend.background = element_blank())+
        guides(col = guide_legend(ncol=2))
    }
    if(legend_flag==FALSE){
      p_event<-p_event+
        theme(legend.position = "none")
      p_nonevent<-p_nonevent+
        theme(legend.position = "none")
    }
    
    #merge plots
    p_merge<-plot_grid(p_event,p_nonevent,
                       labels = "auto",ncol=2,label_size = 12,align = "hv")
    # print(p_merge)
    return(p_merge)
  }
  ###potentially variables need to be checked:
  #fAPAR-itpl;fAPRA_spl
  #ppfd
  #temp_day
  #vpd_day
  #prec
  #patm
  #gpp_obs,gpp_mod_full,gpp_obs_norm,gpp_mod_norm,gpp_res
plot_2groups<-function(df.GS,df.EVENT,df.EVENT10,comp_var,var_unit){
    # df.GS<-df.sep30_GS
    # df.EVENT<-df.sep30_EVENT
    # df.EVENT10<-df.sep30_EVENT10
    # comp_var<-"gpp_obs"
    # var_unit<-"(gC m-2)"
    
    p1<-comp_2groups(df.GS,comp_var,var_unit,TRUE,"GS")
    p2<-comp_2groups(df.EVENT,comp_var,var_unit,TRUE,"EVENT")
    # p3<-comp_2groups(df.EVENT10,comp_var,var_unit,TRUE,"EVENT10")
    #only merge growing season and event
    p_merge<-plot_grid(p1,p2,
                               labels = "auto",nrow=2,label_size = 12,align = "hv")
    return(p_merge)
}
#save the plot
save.path<-"C:/Users/yluo/Desktop/R_testcode/PhotoCold/Second_round_of_code/plot/comp_vars/"
#b1.check for the gpp_obs
#gpp_obs_sep20
p_gpp_obs_sep20<-plot_2groups(df.sep20_GS,df.sep20_EVENT,df.sep20_EVENT10,"gpp_obs","(gC m-2)")
ggsave(paste0(save.path,"gpp_obs_sep20.png"),p_gpp_obs_sep20,width = 15,height = 12)
#gpp_obs_sep30
p_gpp_obs_sep30<-plot_2groups(df.sep30_GS,df.sep30_EVENT,df.sep30_EVENT10,"gpp_obs","(gC m-2)")
ggsave(paste0(save.path,"gpp_obs_sep30.png"),p_gpp_obs_sep30,width = 15,height = 12)
#gpp_obs_sep45
p_gpp_obs_sep45<-plot_2groups(df.sep45_GS,df.sep45_EVENT,df.sep45_EVENT10,"gpp_obs","(gC m-2)")
ggsave(paste0(save.path,"gpp_obs_sep45.png"),p_gpp_obs_sep45,width = 15,height = 12)
#gpp_obs_sep60
p_gpp_obs_sep60<-plot_2groups(df.sep60_GS,df.sep60_EVENT,df.sep60_EVENT10,"gpp_obs","(gC m-2)")
ggsave(paste0(save.path,"gpp_obs_sep60.png"),p_gpp_obs_sep60,width = 15,height = 12)

#b2.check for the temp_day
#temp_day
p_temp_day_sep20<-plot_2groups(df.sep30_GS,df.sep20_EVENT,df.sep20_EVENT10,"temp_day_fluxnet2015","(degreeC)")
ggsave(paste0(save.path,"temp_day_sep20.png"),p_temp_day_sep20,width = 15,height = 12)
#
p_temp_day_sep30<-plot_2groups(df.sep30_GS,df.sep30_EVENT,df.sep30_EVENT10,"temp_day_fluxnet2015","(degreeC)")
ggsave(paste0(save.path,"temp_day_sep30.png"),p_temp_day_sep30,width = 15,height = 12)
#
p_temp_day_sep45<-plot_2groups(df.sep45_GS,df.sep45_EVENT,df.sep45_EVENT10,"temp_day_fluxnet2015","(degreeC)")
ggsave(paste0(save.path,"temp_day_sep45.png"),p_temp_day_sep45,width = 15,height = 12)
#
p_temp_day_sep60<-plot_2groups(df.sep60_GS,df.sep60_EVENT,df.sep60_EVENT10,"temp_day_fluxnet2015","(degreeC)")
ggsave(paste0(save.path,"temp_day_sep60.png"),p_temp_day_sep60,width = 15,height = 12)

#b3.check for the VPD_day
#vpd_day
p_vpd_day_sep30<-plot_2groups(df.sep30_GS,df.sep30_EVENT,df.sep30_EVENT10,"vpd_day_fluxnet2015","(Pa)")
ggsave(paste0(save.path,"vpd_day_sep30.png"),p_vpd_day_sep30,width = 15,height = 12)
#
p_vpd_day_sep45<-plot_2groups(df.sep45_GS,df.sep45_EVENT,df.sep45_EVENT10,"vpd_day_fluxnet2015","(Pa)")
ggsave(paste0(save.path,"vpd_day_sep45.png"),p_vpd_day_sep45,width = 15,height = 12)
#
p_vpd_day_sep60<-plot_2groups(df.sep60_GS,df.sep60_EVENT,df.sep60_EVENT10,"vpd_day_fluxnet2015","(Pa)")
ggsave(paste0(save.path,"vpd_day_sep60.png"),p_vpd_day_sep60,width = 15,height = 12)

#b4.check for the perc
p_prec_sep30<-plot_2groups(df.sep30_GS,df.sep30_EVENT,df.sep30_EVENT10,"prec_fluxnet2015","(mm)")
ggsave(paste0(save.path,"prec_sep30.png"),p_prec_sep30,width = 15,height = 12)
#
p_prec_sep45<-plot_2groups(df.sep45_GS,df.sep45_EVENT,df.sep45_EVENT10,"prec_fluxnet2015","(mm)")
ggsave(paste0(save.path,"prec_sep45.png"),p_prec_sep45,width = 15,height = 12)
#
p_prec_sep60<-plot_2groups(df.sep60_GS,df.sep60_EVENT,df.sep60_EVENT10,"prec_fluxnet2015","(mm)")
ggsave(paste0(save.path,"prec_sep60.png"),p_prec_sep60,width = 15,height = 12)

#b5.check for the patm
p_patm_sep30<-plot_2groups(df.sep30_GS,df.sep30_EVENT,df.sep30_EVENT10,"patm_fluxnet2015","(Pa)")
ggsave(paste0(save.path,"patm_sep30.png"),p_patm_sep30,width = 15,height = 12)
#
p_patm_sep45<-plot_2groups(df.sep45_GS,df.sep45_EVENT,df.sep45_EVENT10,"patm_fluxnet2015","(Pa)")
ggsave(paste0(save.path,"patm_sep45.png"),p_patm_sep45,width = 15,height = 12)
#
p_patm_sep60<-plot_2groups(df.sep60_GS,df.sep60_EVENT,df.sep60_EVENT10,"patm_fluxnet2015","(Pa)")
ggsave(paste0(save.path,"patm_sep60.png"),p_patm_sep60,width = 15,height = 12)

#b5.check for the ppfd
p_ppfd_sep30<-plot_2groups(df.sep30_GS,df.sep30_EVENT,df.sep30_EVENT10,"ppfd_fluxnet2015","(u mol m-2 s-1)")
ggsave(paste0(save.path,"ppfd_sep30.png"),p_ppfd_sep30,width = 15,height = 12)
#
p_ppfd_sep45<-plot_2groups(df.sep45_GS,df.sep45_EVENT,df.sep45_EVENT10,"ppfd_fluxnet2015","(u mol m-2 s-1)")
ggsave(paste0(save.path,"ppfd_sep45.png"),p_ppfd_sep45,width = 15,height = 12)
#
p_ppfd_sep60<-plot_2groups(df.sep60_GS,df.sep60_EVENT,df.sep60_EVENT10,"ppfd_fluxnet2015","(u mol m-2 s-1)")
ggsave(paste0(save.path,"ppfd_sep60.png"),p_ppfd_sep60,width = 15,height = 12)

#b6.fapar_spl and fapar_itpl
p_fapar_itpl_sep20<-plot_2groups(df.sep20_GS,df.sep20_EVENT,df.sep20_EVENT10,"fapar_itpl","")
ggsave(paste0(save.path,"fapar_itpl_sep20.png"),p_fapar_itpl_sep20,width = 15,height = 12)
#
p_fapar_itpl_sep30<-plot_2groups(df.sep30_GS,df.sep30_EVENT,df.sep30_EVENT10,"fapar_itpl","")
ggsave(paste0(save.path,"fapar_itpl_sep30.png"),p_fapar_itpl_sep30,width = 15,height = 12)
#
p_fapar_itpl_sep45<-plot_2groups(df.sep45_GS,df.sep45_EVENT,df.sep45_EVENT10,"fapar_itpl","")
ggsave(paste0(save.path,"fapar_itpl_sep45.png"),p_fapar_itpl_sep45,width = 15,height = 12)
#
p_fapar_itpl_sep60<-plot_2groups(df.sep60_GS,df.sep60_EVENT,df.sep60_EVENT10,"fapar_itpl","")
ggsave(paste0(save.path,"fapar_itpl_sep60.png"),p_fapar_itpl_sep60,width = 15,height = 12)

##
p_fapar_spl_sep20<-plot_2groups(df.sep20_GS,df.sep20_EVENT,df.sep20_EVENT10,"fapar_spl","")
ggsave(paste0(save.path,"fapar_spl_sep20.png"),p_fapar_spl_sep20,width = 15,height = 12)
#
p_fapar_spl_sep30<-plot_2groups(df.sep30_GS,df.sep30_EVENT,df.sep30_EVENT10,"fapar_spl","")
ggsave(paste0(save.path,"fapar_spl_sep30.png"),p_fapar_spl_sep30,width = 15,height = 12)
#
p_fapar_spl_sep45<-plot_2groups(df.sep45_GS,df.sep45_EVENT,df.sep45_EVENT10,"fapar_spl","")
ggsave(paste0(save.path,"fapar_spl_sep45.png"),p_fapar_spl_sep45,width = 15,height = 12)
#
p_fapar_spl_sep60<-plot_2groups(df.sep60_GS,df.sep60_EVENT,df.sep60_EVENT10,"fapar_spl","")
ggsave(paste0(save.path,"fapar_spl_sep60.png"),p_fapar_spl_sep60,width = 15,height = 12)

#additional: add more variables to compare-->July-5
#temp_min
p_temp_min_sep20<-plot_2groups(df.sep30_GS,df.sep20_EVENT,df.sep20_EVENT10,"temp_min_fluxnet2015","(degreeC)")
ggsave(paste0(save.path,"temp_min_sep20.png"),p_temp_min_sep20,width = 15,height = 12)
#temp_max
p_temp_max_sep20<-plot_2groups(df.sep30_GS,df.sep20_EVENT,df.sep20_EVENT10,"temp_max_fluxnet2015","(degreeC)")
ggsave(paste0(save.path,"temp_max_sep20.png"),p_temp_max_sep20,width = 15,height = 12)
#SWC_1
p_swc_1_sep20<-plot_2groups(df.sep30_GS,df.sep20_EVENT,df.sep20_EVENT10,"SWC_1_fluxnet2015","(%)")
ggsave(paste0(save.path,"SWC_1_sep20.png"),p_swc_1_sep20,width = 15,height = 12)
#TS_1
p_ts_1_sep20<-plot_2groups(df.sep30_GS,df.sep20_EVENT,df.sep20_EVENT10,"TS_1_fluxnet2015","(degreeC)")
ggsave(paste0(save.path,"TS_1_sep20.png"),p_ts_1_sep20,width = 15,height = 12)
#for gcc
p_gcc_90_sep20<-plot_2groups(df.sep30_GS,df.sep20_EVENT,df.sep20_EVENT10,"gcc_90","")
ggsave(paste0(save.path,"gcc_90_sep20.png"),p_gcc_90_sep20,width = 15,height = 12)
#for rcc
p_rcc_90_sep20<-plot_2groups(df.sep30_GS,df.sep20_EVENT,df.sep20_EVENT10,"rcc_90","")
ggsave(paste0(save.path,"rcc_90_sep20.png"),p_rcc_90_sep20,width = 15,height = 12)
