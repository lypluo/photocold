#############################################################################
#Aim:using Beni's method to align the data in each site-year
#-------------------------------------------------------------------------
#(1)load the data that includes the "is_event" information
#-------------------------------------------------------------------------
#---------------------
#A.load the event_length data
#---------------------
load.path<-"D:/CES/Data_for_use/event_length/"
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
# df.sep30<-sep_siteyears(df_events_all,"Over_days_length",30)
# df.sep45<-sep_siteyears(df_events_all,"Over_days_length",45)
# df.sep60<-sep_siteyears(df_events_all,"Over_days_length",60)
#---------------------
#B.load the data 
#---------------------
load.path<-"D:/CES/Data_for_use/Data_sent_by_Beni/"
#from new method:
load(paste0(load.path,"ddf_labeled_norm_trs_newmethod_all_overestimation.RDA"))
df_norm_trs_newM_oversites<-ddf_labeled;rm(ddf_labeled)  #sites flagged as Beni that have the gpp overestimation in the spring 
load(paste0(load.path,"ddf_labeled_norm_trs_newmethod_sites_flag_without_overestimation.RDA"))
df_norm_trs_newM_no_oversites<-ddf_labeled;rm(ddf_labeled)  #sites flagged as Beni that do not have the gpp overestimation in the spring
load(paste0(load.path,"ddf_labeled_norm_trs_newmethod_all_overestimation_beyondsites.RDA"))
df_norm_trs_newM_beyond_sites<-ddf_labeled;rm(ddf_labeled)  #sites beyond Beni's datasets
#merge the datasets
df_norm_all<-rbind(rbind(df_norm_trs_newM_no_oversites,df_norm_trs_newM_oversites),df_norm_trs_newM_beyond_sites)

#-------------------------------------------------------------------------
#(2)start to align the data according to Beni's functions of "align_events" and "get_consecutive"
#-------------------------------------------------------------------------
#---------------------------------
#source the functions to detect the consecutive events
#---------------------------------
fun.path<-"D:/CES/R_testcode/PhotoCold/Second_round_of_code/R/Step2_identify_events/Functions/functions_from_beni/"
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

#test not using the information from df.sep20-->check if we can see the differences between sites for the e.g. "Ta"
#results-->yes!
# sep_siteyears_data_1<-function(df.data,leng_threshold,before,after,nbins,do_norm){
#   # df.data<-df_norm_all
#   # leng_threshold<-20
#   # before=30
#   # after=30
#   # nbins=10
#   # do_norm=FALSE
#   #-------------------------------------------------------------------------
#   #A.separating the datasets:"event" and "nonevent" site years
#   #-------------------------------------------------------------------------
#   #start to align different events in each site-year
#   df_events.data<-align_events(df.data,leng_threshold = leng_threshold,before = before,after = after,
#                                nbins = nbins,do_norm = do_norm)
#   #for event site years
#   df_dday<-df_events.data$df_dday
#   #for non_event site years, take the doy belongs to green-up period:
#   df_noevent_data<-df.data[-df_dday$idx_df,]
#   #test:using the 5% and 95% quantile of doy from "event" to select the "non-events" data
#   ##-->this not correct!-->should use the "green-up" to separate the groups
#   # doy_q<-quantile(df_dday$doy,probs=seq(0,1,0.05))
#   # doy_thrs<-doy_q[c(2,20)]
#   # df_noevent_dday<-df_noevent_data[df_noevent_data$doy>as.numeric(doy_thrs[1]) & df_noevent_data$doy<as.numeric(doy_thrs[2]),]
#   #
#   df_noevent_dday<-df_noevent_data[df_noevent_data$GS=="yes",]
#   #
#   df_all<-c()
#   df_all<-list(df_dday=df_dday,df_noevent_dday=df_noevent_dday)
#   return(df_all)
# }
#!!important step:leng_threshold=5-->merge the events(consecutive days are over 5 days)
#do_vars-->the variables that are going to be processed:
names(df_norm_all)
do_vars<-c("gpp_obs","fapar_itpl","fapar_spl",paste0(c("ppfd","temp_day","temp_min","temp_max",
            "vpd_day","prec","patm","SW_IN","ws",paste0("TS_",1:9),paste0("SWC_",1:5)),"_fluxnet2015"),
           "gcc_90","rcc_90")
##!!pay attention-->do not write the variable names wrong!-->always to check
df_len5<-sep_siteyears_data(df_norm_all,do_vars,df.sep20,5,30,0,10,TRUE)
# df_len5<-sep_siteyears_data_1(df_norm_all,5,30,0,10,FALSE)
  #-------------------------------------------------------------------------
  #(C)start plotting
  #-------------------------------------------------------------------------
  library(ggplot2)
  library(cowplot)
  library(grid)
  #I.function used to compare the "event" and "non_evnet" according to doy
  comp_2groups_doy<-function(df,comp_var,var_unit,legend_flag,do_norm){
    # df<-df_len5
    # comp_var<-"gpp_obs"
    # var_unit<-"(g C m-2)"
    # legend_flag<-TRUE
    # do_norm<-TRUE
    
    #
    df.event<-df$df_dday
    df.noevent<-df$df_noevent_dday
    #selected the most relevant vars
    #for the comparison,select the original variable if "do_norm"==FALSE,otherwise "do_norm"==TRUE
    if(do_norm==FALSE){
      df.event_sel<-df.event[,c("sitename","Year","date","doy","dday",comp_var)]
      names(df.event_sel)<-c("sitename","Year","date","doy","dday","comp_var")
      df.nonevent_sel<-df.noevent[,c("sitename","Year","date","doy","dday",comp_var)]
      names(df.nonevent_sel)<-c("sitename","Year","date","doy","dday","comp_var")
    }
    if(do_norm==TRUE){
      comp_var_norm<-paste0("ds",comp_var)
      df.event_sel<-df.event[,c("sitename","Year","date","doy","dday",comp_var_norm)]
      names(df.event_sel)<-c("sitename","Year","date","doy","dday","comp_var")
      #
      df.nonevent_sel<-df.noevent[,c("sitename","Year","date","doy","dday",comp_var_norm)]
      names(df.nonevent_sel)<-c("sitename","Year","date","doy","dday","comp_var")
      #also make var_unit()==""
      var_unit<-c()
    }
    
    #plotting
    #adding the fitting lines in each sites:
    # Create a text
    grob_event <- grobTree(textGrob("Event site-year", x=0.6,  y=0.9, hjust=0,
                                    gp=gpar(col="red", fontsize=18, fontface="italic")))
    # grob_event_name <- grobTree(textGrob(df_name, x=0.8,  y=0.1, hjust=0,
    #                                      gp=gpar(col="red", fontsize=18, fontface="italic")))
    grob_nonevent <- grobTree(textGrob("Non-Event site-year", x=0.6,  y=0.9, hjust=0,
                                       gp=gpar(col="red", fontsize=18, fontface="italic")))
    # grob_nonevent_name<-grobTree(textGrob(df_name, x=0.8,  y=0.1, hjust=0,
    #                                       gp=gpar(col="red", fontsize=18, fontface="italic")))
    #y axis range:
    ymin<-min(range(df.event_sel$comp_var,na.rm = T),range(df.nonevent_sel$comp_var,na.rm = T))
    ymax<-max(range(df.event_sel$comp_var,na.rm = T),range(df.nonevent_sel$comp_var,na.rm = T))
    #x axis range
    # x_range_event<-range(df.event_sel$doy)
    # x_range_nonevent<-range(df.nonevent_sel$doy)
    p_event<-ggplot()+
      geom_point(data=df.event_sel,aes(x=doy,y=comp_var,col=sitename),alpha=0.3,pch=16)+
      geom_point(data=df.event_sel[df.event_sel$dday==0,],aes(x=doy,y=comp_var,col=sitename),size=3,pch=1,stroke=3)+
      # geom_line(data=df.event_sel,aes(x=doy,y=sel_var),col="black")+
      geom_smooth(data=df.event_sel,aes(x=doy,y=comp_var,col=sitename),method = "lm",
                  formula = y ~ poly(x,4) ,se=FALSE)+
      ylab(paste0(comp_var," ",var_unit))+
      xlim(0,250)+
      annotation_custom(grob = grob_event)+
      # annotation_custom(grob=grob_event_name)+
      theme_classic()+
      theme(axis.title = element_text(size=18),
            axis.text = element_text(size = 16))
    p_nonevent<-ggplot()+
      geom_point(data=df.nonevent_sel,aes(x=doy,y=comp_var,col=sitename),alpha=0.3,pch=16)+
      geom_point(data=df.nonevent_sel[df.nonevent_sel$dday==0,],aes(x=doy,y=comp_var,col=sitename),size=3,pch=1,stroke=3)+
      geom_smooth(data=df.nonevent_sel,aes(x=doy,y=comp_var,col=sitename),method = "lm",
                  formula = y ~ poly(x,4) ,se=FALSE)+
      ylab(paste0(comp_var," ",var_unit))+
      xlim(0,250)+
      annotation_custom(grob = grob_nonevent)+
      # annotation_custom(grob = grob_nonevent_name)+
      theme_classic()+
      theme(axis.title = element_text(size=18),
            axis.text = element_text(size = 16))
    #show the sclae range
    if(do_norm==FALSE){
      p_event<-p_event+
        ylim(ymin,ymax)
      p_nonevent<-p_nonevent+
        ylim(ymin,ymax)
      
    }
    if(do_norm==TRUE){
      p_event<-p_event+
        ylab(paste0("norm-",comp_var," ",var_unit))+
        geom_hline(yintercept = 0,lty=2,size=1.1)
        # geom_vline(xintercept = 0,lty=2,size=1.1)
      p_nonevent<-p_nonevent+
        ylab(paste0("norm-",comp_var," ",var_unit))+
        geom_hline(yintercept = 0,lty=2,size=1.1)
        # geom_vline(xintercept = 0,lty=2,size=1.1)
    }
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
  
  #II.function used to compare the "event" and "non_evnet" by start from align date
  comp_2groups_aligndate<-function(df,comp_var,var_unit,legend_flag,do_norm){
    # df<-df_len5
    # comp_var<-"temp_min_fluxnet2015"
    # var_unit<-"(degreeC)"
    # legend_flag<-TRUE
    # do_norm<-TRUE
    
    #
    df.event<-df$df_dday
    df.noevent<-df$df_noevent_dday
    ##in order to compare more objectively between event and non-event
    ##first to check the distribution of doy(events) and calculate its quantiles
    ##only select qunatile 5% and 95% as the select range for the non-event
    # ggplot(df.event, aes(x=doy))+
    #   # geom_histogram(aes(y=..density..),binwidth = 0.01,position="identity", alpha=0.3,fill="white",col="green3",)+
    #   geom_density(col="forestgreen",size=1.05)
    # doy.q<-quantile(df.event$doy,probs = seq(0,1,0.05))
    # #select the df.noevent at the doy between [5%,95%] percentiles 
    # df.noevent<-df.noevent[df.noevent$doy>=doy.q[2] & df.noevent$doy<=doy.q[20],]
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
    #selected the most relevant vars
    #for the comparison,select the original variable if "do_norm"==FALSE,otherwise "do_norm"==TRUE
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
    
    #plotting
    #adding the fitting lines in each sites:
    # Create a text
    grob_event <- grobTree(textGrob("Event site-year", x=0.6,  y=0.9, hjust=0,
                                    gp=gpar(col="red", fontsize=18, fontface="italic")))
    # grob_event_name <- grobTree(textGrob(df_name, x=0.8,  y=0.1, hjust=0,
    #                                      gp=gpar(col="red", fontsize=18, fontface="italic")))
    grob_nonevent <- grobTree(textGrob("Non-Event site-year", x=0.6,  y=0.9, hjust=0,
                                       gp=gpar(col="red", fontsize=18, fontface="italic")))
    # grob_nonevent_name<-grobTree(textGrob(df_name, x=0.8,  y=0.1, hjust=0,
    #                                       gp=gpar(col="red", fontsize=18, fontface="italic")))
    #y axis range:
    ymin<-min(range(df.event_sel$comp_var,na.rm = T),range(df.nonevent_sel$comp_var,na.rm = T))
    ymax<-max(range(df.event_sel$comp_var,na.rm = T),range(df.nonevent_sel$comp_var,na.rm = T))
    #x axis range
    # x_range_event<-range(df.event_sel$doy)
    # x_range_nonevent<-range(df.nonevent_sel$doy)
    p_event<-ggplot()+
      geom_point(data=df.event_sel,aes(x=dday,y=comp_var,col=sitename),alpha=0.3,pch=16)+
      # geom_line(data=df.event_sel,aes(x=doy,y=sel_var),col="black")+
      geom_smooth(data=df.event_sel,aes(x=dday,y=comp_var,col=sitename),method = "lm",
                  formula = y ~ poly(x,4) ,se=FALSE)+
      ylab(paste0(comp_var," ",var_unit))+
      xlim(-50,200)+
      annotation_custom(grob = grob_event)+
      # annotation_custom(grob=grob_event_name)+
      geom_vline(xintercept = 0,lty=2,size=1.2)+
      theme_classic()+
      theme(axis.title = element_text(size=18),
            axis.text = element_text(size = 16))
    p_nonevent<-ggplot()+
      geom_point(data=df.nonevent_sel,aes(x=dday,y=comp_var,col=sitename),alpha=0.3,pch=16)+
      geom_smooth(data=df.nonevent_sel,aes(x=dday,y=comp_var,col=sitename),method = "lm",
                  formula = y ~ poly(x,4) ,se=FALSE)+
      ylab(paste0(comp_var," ",var_unit))+
      xlim(-50,200)+
      annotation_custom(grob = grob_nonevent)+
      # annotation_custom(grob = grob_nonevent_name)+
      geom_vline(xintercept = 0,lty=2,size=1.2)+
      theme_classic()+
      theme(axis.title = element_text(size=18),
            axis.text = element_text(size = 16))
    #######
    #show the sclae range
    if(do_norm==FALSE){
      p_event<-p_event+
        ylim(ymin,ymax)+
        geom_hline(yintercept = 0,lty=2,size=1.1)+
        geom_vline(xintercept = 0,lty=2,size=1.1)
      p_nonevent<-p_nonevent+
        ylim(ymin,ymax)+
        geom_hline(yintercept = 0,lty=2,size=1.1)+
        geom_vline(xintercept = 0,lty=2,size=1.1)
      
    }
    if(do_norm==TRUE){
      p_event<-p_event+
        ylab(paste0("norm-",comp_var," ",var_unit))+
      geom_hline(yintercept = 0,lty=2,size=1.1)+
      geom_vline(xintercept = 0,lty=2,size=1.1)
      p_nonevent<-p_nonevent+
        ylab(paste0("norm-",comp_var," ",var_unit))+
      geom_hline(yintercept = 0,lty=2,size=1.1)+
      geom_vline(xintercept = 0,lty=2,size=1.1)
    }
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

#-------------------------------------------------------------------------
#(3) officially to compare the vars in two different group ("event" and "non_event")
#-------------------------------------------------------------------------
  ###potentially variables need to be checked:
  #fAPAR-itpl;fAPRA_spl
  #ppfd
  #temp_day
  #vpd_day
  #prec
  #patm
  #gpp_obs,gpp_mod_full,gpp_obs_norm,gpp_mod_norm,gpp_res

plot_2groups<-function(df,comp_var,var_unit,do_norm){
    # df<-df_len5
    # comp_var<-"temp_day_fluxnet2015"
    # var_unit<-"(degreeC)"
    # do_norm<-TRUE
    
    p1<-comp_2groups_doy(df,comp_var,var_unit,TRUE,do_norm)
    p2<-comp_2groups_aligndate(df,comp_var,var_unit,TRUE,do_norm)
    #only merge growing season and event
    p_merge<-plot_grid(p1,p2,
                       labels = "auto",nrow=2,label_size = 12,align = "hv")
    return(p_merge)
  }
#save the plot
save.path<-"C:/Users/yluo/Desktop/R_testcode/PhotoCold/Second_round_of_code/plot/comp_vars/based_Beni/update_results/"
#------------------------------
#(A)first using df_len20-->using event date from [-30,end of event]
#------------------------------ 
# sep_siteyears_data<-function(df.data,do.vars,df.sep,leng_threshold,before,after,nbins,do_norm){
# df_len5<-sep_siteyears_data_1(df_norm_all,dovars,5,30,0,10,FALSE)
#
do_vars<-c("gpp_obs","fapar_itpl","fapar_spl",paste0(c("ppfd","temp_day","temp_min","temp_max",
          "vpd_day","prec","patm","SW_IN","ws",paste0("TS_",1:9),paste0("SWC_",1:5)),"_fluxnet2015"),
           "gcc_90","rcc_90")
#-------------------------
#Test....
#update by YP:2021-07-16:
#first test by by normalization and no normalizaiton-->do_norm="FALSE" or "TRUE"
#-------------------------
df_len5_nonnorm<-sep_siteyears_data(df_norm_all,do_vars,df.sep20,5,30,0,10,FALSE)
df_len5_norm<-sep_siteyears_data(df_norm_all,do_vars,df.sep20,5,30,0,10,TRUE)
#-------------
#save the data
#-------------
data.save.path<-"D:/CES/Data_for_use/Merge_Data/sep_event_data/"
save(df_len5_nonnorm,file=paste0(data.save.path,"df_len5_nonnorm.RDA"))

#test with "temp_day_fluxnet2015" and "temp_min_fluxnet2015"
#
p_temp_day_len5_b30_nonnorm<-plot_2groups(df_len5_nonnorm,"temp_day_fluxnet2015","(degreeC)",FALSE)
ggsave(paste0(save.path,"temp_day_len5_b30_nonnorm.png"),p_temp_day_len5_b30_nonnorm,width = 15,height = 12)
p_temp_day_len5_b30_norm<-plot_2groups(df_len5_norm,"temp_day_fluxnet2015","(degreeC)",TRUE)
ggsave(paste0(save.path,"temp_day_len5_b30_merge_norm.png"),p_temp_day_len5_b30_norm,width = 15,height = 12)
#
p_temp_min_len5_b30_nonnorm<-plot_2groups(df_len5_nonnorm,"temp_min_fluxnet2015","(degreeC)",FALSE)
ggsave(paste0(save.path,"temp_min_len5_b30_nonnorm.png"),p_temp_min_len5_b30_nonnorm,width = 15,height = 12)
p_temp_min_len5_b30_norm<-plot_2groups(df_len5_norm,"temp_min_fluxnet2015","(degreeC)",TRUE)
ggsave(paste0(save.path,"temp_min_len5_b30_merge_norm.png"),p_temp_min_len5_b30_norm,width = 15,height = 12)
#-------------------------
#After the test, it seems use the original data (not normalized data will be more intutive for the more varied variables such as GPP, Ta)
#Hence,update the results with no-normlization for following variables:gpp,temp
#-------------------------
save.path<-"C:/Users/yluo/Desktop/R_testcode/PhotoCold/Second_round_of_code/plot/comp_vars/based_Beni/update_results/lines_plot/"
#a1.check for the gpp_obs
#b_30 stands for 30 days before event
#gpp_obs_len5
p_gpp_obs_len5_b30<-plot_2groups(df_len5_nonnorm,"gpp_obs","(gC m-2)",do_norm = FALSE)
ggsave(paste0(save.path,"gpp_obs_len5_b30.png"),p_gpp_obs_len5_b30,width = 15,height = 12)
# #gpp_obs_len20
# p_gpp_obs_len20_b30<-plot_2groups(df_len20,"gpp_obs","(gC m-2)")
# ggsave(paste0(save.path,"gpp_obs_len20_b30.png"),p_gpp_obs_len20_b30,width = 15,height = 12)
# #gpp_obs_len30
# p_gpp_obs_len30_b30<-plot_2groups(df_len30,"gpp_obs","(gC m-2)")
# ggsave(paste0(save.path,"gpp_obs_len30_b30.png"),p_gpp_obs_len30_b30,width = 15,height = 12)
# #gpp_obs_len40
# p_gpp_obs_len40_b30<-plot_2groups(df_len40,"gpp_obs","(gC m-2)")
# ggsave(paste0(save.path,"gpp_obs_len40_b30.png"),p_gpp_obs_len40_b30,width = 15,height = 12)

#a2.check for the temp_day,temp_min,and temp_max
#temp_day
p_temp_day_len5_b30<-plot_2groups(df_len5_nonnorm,"temp_day_fluxnet2015","(degreeC)",do_norm = FALSE)
ggsave(paste0(save.path,"temp_day_len5_b30.png"),p_temp_day_len5_b30,width = 15,height = 12)

#
# p_temp_day_len20_b30<-plot_2groups(df_len20,"temp_day_fluxnet2015","(degreeC)")
# ggsave(paste0(save.path,"temp_day_len20_b30.png"),p_temp_day_len20_b30,width = 15,height = 12)
# #
# p_temp_day_len30_b30<-plot_2groups(df_len30,"temp_day_fluxnet2015","(degreeC)")
# ggsave(paste0(save.path,"temp_day_len30_b30.png"),p_temp_day_len30_b30,width = 15,height = 12)
# #
# p_temp_day_len40_b30<-plot_2groups(df_len40,"temp_day_fluxnet2015","(degreeC)")
# ggsave(paste0(save.path,"temp_day_len40_b30.png"),p_temp_day_len40_b30,width = 15,height = 12)

#a3.fapar_spl and fapar_itpl
p_fapar_itpl_len5_b30<-plot_2groups(df_len5_nonnorm,"fapar_itpl","",do_norm = FALSE)
ggsave(paste0(save.path,"fapar_itpl_len5_b30.png"),p_fapar_itpl_len5_b30,width = 15,height = 12)
#
# p_fapar_itpl_len30_b30<-plot_2groups(df_len30,"fapar_itpl","")
# ggsave(paste0(save.path,"fapar_itpl_len30_b30.png"),p_fapar_itpl_len30_b30,width = 15,height = 12)
# #
# p_fapar_itpl_len40_b30<-plot_2groups(df_len40,"fapar_itpl","")
# ggsave(paste0(save.path,"fapar_itpl_len40_b30.png"),p_fapar_itpl_len40_b30,width = 15,height = 12)
# 
# ##
# p_fapar_spl_len20_b30<-plot_2groups(df_len20,"fapar_spl","")
# ggsave(paste0(save.path,"fapar_spl_len20_b30.png"),p_fapar_spl_len20_b30,width = 15,height = 12)
# #
# p_fapar_spl_len30_b30<-plot_2groups(df_len30,"fapar_spl","")
# ggsave(paste0(save.path,"fapar_spl_len30_b30.png"),p_fapar_spl_len30_b30,width = 15,height = 12)
# # 
# p_fapar_spl_len40_b30<-plot_2groups(df_len40,"fapar_spl","")
# ggsave(paste0(save.path,"fapar_spl_len40_b30.png"),p_fapar_spl_len40_b30,width = 15,height = 12)

#a4.for VPD
#vpd_day
p_vpd_day_len5_b30<-plot_2groups(df_len5_nonnorm,"vpd_day_fluxnet2015","(Pa)",do_norm = FALSE)
ggsave(paste0(save.path,"vpd_day_len5_b30.png"),p_vpd_day_len5_b30,width = 15,height = 12)

#a5.for prec
p_prec_len5_b30<-plot_2groups(df_len5_nonnorm,"prec_fluxnet2015","(mm)",do_norm = FALSE)
ggsave(paste0(save.path,"prec_len5_b30.png"),p_prec_len5_b30,width = 15,height = 12)

#a5.for ppfd
p_ppfd_len5_b30<-plot_2groups(df_len5_nonnorm,"ppfd_fluxnet2015","(u mol m-2 s-1)",do_norm = FALSE)
ggsave(paste0(save.path,"ppfd_len5_b30.png"),p_ppfd_len5_b30,width = 15,height = 12)

#additional: add more variables to compare
#temp_min
p_temp_min_len5_b30<-plot_2groups(df_len5_nonnorm,"temp_min_fluxnet2015","(degreeC)",do_norm = FALSE)
ggsave(paste0(save.path,"temp_min_len5_b30.png"),p_temp_min_len5_b30,width = 15,height = 12)
#temp_max
p_temp_max_len5_b30<-plot_2groups(df_len5_nonnorm,"temp_max_fluxnet2015","(degreeC)",do_norm = FALSE)
ggsave(paste0(save.path,"temp_max_len5_b30.png"),p_temp_max_len5_b30,width = 15,height = 12)

###for gcc and rcc-->both using nonnorm and norm data
#normalized
#gcc_90
p_gcc_90_len5_b30<-plot_2groups(df_len5_nonnorm,"gcc_90","",do_norm = FALSE)
ggsave(paste0(save.path,"gcc_90_len5_b30_nonnorm.png"),p_gcc_90_len5_b30,width = 15,height = 12)
#rcc_90
p_rcc_90_len5_b30<-plot_2groups(df_len5_nonnorm,"rcc_90","",do_norm = FALSE)
ggsave(paste0(save.path,"rcc_90_len5_b30_nonnorm.png"),p_rcc_90_len5_b30,width = 15,height = 12)

#####
#gcc_90
p_gcc_90_len5_b30<-plot_2groups(df_len5_norm,"gcc_90","",do_norm = TRUE)
ggsave(paste0(save.path,"gcc_90_len5_b30_norm.png"),p_gcc_90_len5_b30,width = 15,height = 12)
#rcc_90
p_rcc_90_len5_b30<-plot_2groups(df_len5_norm,"rcc_90","",do_norm = TRUE)
ggsave(paste0(save.path,"rcc_90_len5_b30_norm.png"),p_rcc_90_len5_b30,width = 15,height = 12)

