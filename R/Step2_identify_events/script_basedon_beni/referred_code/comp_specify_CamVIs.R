#############################################################################
#Aim:compare the variables in "event" and "non-event" site years after aligning the data
#in this script, focus on the PhenoCam VIs such as the GCC and RCC
#-------------------------------------------------------------------------
#(1)load the data after aligning and calculating some important variables like LUE...
#-------------------------------------------------------------------------
load.path<-"D:/CES/Data_for_use/Merge_Data/sep_event_data/"
load(paste0(load.path,"df_len5_nonnorm.RDA"))
  #--------------------------
  #calculating more variables
  #--------------------------
  #1)LUE=GPP/fAPAR*ppfd
  #unit-->GPP: umol m-2 s-1; ppdf-->umol m-2 s-1; fAPRA: unitless
  #2)GRVI=(gcc-rcc)/c(gcc+rcc)
  for(i in 1:length(df_len5_nonnorm)){
    #
    df_proc<-df_len5_nonnorm[[i]]
    df_proc$LUE<-df_proc$gpp_obs/c(df_proc$fapar_itpl*df_proc$ppfd_fluxnet2015)
    df_proc$GRVI<-c(df_proc$gcc_90-df_proc$rcc_90)/c(df_proc$gcc_90+df_proc$rcc_90)
    #assign value back:
    df_len5_nonnorm[[i]]<-df_proc
  }

#-------------------------------------------------------------------------
#(2)tidy the infos about the site info in "event" and "non-event" sites
#-------------------------------------------------------------------------
  df.event<-df_len5_nonnorm$df_dday
  df.noevent<-df_len5_nonnorm$df_noevent_dday
  # site-years in df.event and df.noevent
  library(plyr)
  CamVIs_tidy<-function(df){
    # df<-df.event
    df<-df[!is.na(df$gcc_90),]
    #take the green-up period:
    df<-df[df$greenup=="yes",]
    df_out<-ddply(df,.(sitename,Year),summarize,gcc_90_mean=mean(gcc_90,na.rm = T),
                  rcc_90_mean=mean(rcc_90,na.rm = T),
                  GRVI_mean=mean(GRVI,na.rm = T))
    return(df_out)
  }
  #
  stats.event<-CamVIs_tidy(df.event)
  stats.nonevent<-CamVIs_tidy(df.noevent)
  #sites info:
  unique(stats.event$sitename)
  #"CA-Qfo" "DE-Hai" "DK-Sor" "IT-Tor" "US-MMS" "US-NR1" "US-UMB" "US-UMd" "US-WCr"
  unique(stats.nonevent$sitename)
  #"DE-Tha" "US-NR1"
#-------------------------------------------------------------------------
#(3)going to compare the "event" and "non-event" site regrading the Cam-VIs
#-------------------------------------------------------------------------
  #from (2), I used all the data from "CA-Qfo" "DE-Hai" "DK-Sor" "IT-Tor" "US-MMS"  "US-UMB" "US-UMd" "US-WCr" as the data from event sites
  #data from "DE-Tha" as the data from non-event sites
  #---------------
  #load the CamVI data
  #---------------
  load.path<-"D:/CES/Data_for_use/PhenoCam_Data/Preprocessed_data/"
  load(paste0(load.path,"Daily_data.RDA"))
  #
  event.sites<-c("CA-Qfo","DE-Hai","DK-Sor","IT-Tor","US-MMS","US-UMB","US-UMd","US-WCr")
  event.VI<-subset(df.Phenocam_daily,sitename %in% event.sites)
  nonevent.sites<-"DE-Tha"
  nonevent.VI<-subset(df.Phenocam_daily,sitename %in% nonevent.sites)
  #
  df.VI<-c()
  df.VI[[1]]<-event.VI;df.VI[[2]]<-nonevent.VI
  names(df.VI)<-c("df_dday","df_noevent_dday")
  #only for ENF
  df.VI_ENF<-c()
  df.VI_ENF[[1]]<-event.VI[event.VI$PhenoCam_PFT=="EN",];unique(event.VI[event.VI$PhenoCam_PFT=="EN",]$sitename)
  df.VI_ENF[[2]]<-nonevent.VI[nonevent.VI$PhenoCam_PFT=="EN",]
  names(df.VI_ENF)<-c("df_dday","df_noevent_dday")
  #-------------------------------------------------------------------------
  #start plotting
  #-------------------------------------------------------------------------
  library(plyr)  
  library(ggplot2)
  library(cowplot)
  library(grid)
  
#function used to compare the "event" and "non_event" sites using geom_ribbon 
#source the comparsion test written by me:
fun.path<-"C:/Users/yluo/Desktop/R_testcode/PhotoCold/Second_round_of_code/R/Step2_identify_events/Functions/functions_from_YP/"
source(paste0(fun.path,"Difference_test_for_2Classes.R"))

plot_2groups<-function(df,comp_var,var_unit,do_norm,do_legend){
    # df<-df.VI_ENF
    # comp_var<-"gcc_90"
    # var_unit<-""
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
      df.event_sel<-df.event[,c("sitename","year","date","doy",comp_var)]
      names(df.event_sel)<-c("sitename","Year","date","doy","comp_var")
      df.nonevent_sel<-df.noevent[,c("sitename","year","date","doy",comp_var)]
      names(df.nonevent_sel)<-c("sitename","Year","date","doy","comp_var")
    #
      df.event_sel$date<-as.Date(df.event_sel$date)
      df.nonevent_sel$date<-as.Date(df.nonevent_sel$date)
    #--------------------------------------
    #plotting
    #--------------------------------------
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
    ###start to make the quantiile plot:
    df.event_sel$flag<-rep("event-sites",nrow(df.event_sel))
    df.nonevent_sel$flag<-rep("nonevent-sites",nrow(df.nonevent_sel))
    #----------------------------------------
    #merge the event sites and non-event sites
    #-----------------------------------------
    ##merge and classify 
    df.all<-rbind(df.event_sel,df.nonevent_sel)
    df.all$flag<-factor(df.all$flag,levels = c("event-sites","nonevent-sites"))
      
    #merge the different sites in "event" and "non-event" sites->calculate the quantiles at the same time
    df.all_q<-ddply(df.all,.(flag,doy),summarize,q10=quantile(comp_var,0.10,na.rm = T),q25=quantile(comp_var,0.25,na.rm = T),
          q50=quantile(comp_var,0.5,na.rm = T),q75=quantile(comp_var,0.75,na.rm = T),q90=quantile(comp_var,0.9,na.rm = T))
    #----------------------------------------------
    #making the quantile plot using ribbon function:
    #-----------------------------------------------
    p_plot<-ggplot(df.all_q)+
      # geom_line(data=df.all_q[df.all_q$flag=="event-sites",],aes(x=doy,y=q50,col=flag),size=1.05)+
      # geom_point(data=df.all_q[df.all_q$flag=="nonevent-sites",],aes(x=doy,y=q50,col=flag),size=1.05)+
      geom_line(aes(x=doy,y=q50,col=flag),size=1.05,pch=1)+
      geom_point(aes(x=doy,y=q50,col=flag),size=1.05,pch=1)+
      annotate("rect",xmin=0,xmax=max(df.all_q$doy),ymin = -Inf,ymax = Inf,alpha=0.2)+
      geom_line(aes(x=doy,y=q50,col=flag),size=1.05)+
      scale_color_manual("",values = c("event-sites"="red","nonevent-sites"="green4"))+
      geom_ribbon(aes(x=doy,ymin=q10,ymax=q90,fill=flag),alpha=0.15)+
      geom_ribbon(aes(x=doy,ymin=q25,ymax=q75,fill=flag),alpha=0.4)+
      scale_fill_manual("",values = c("event-sites"="red","nonevent-sites"="green4"))+
      ylab(paste0(comp_var," ",var_unit))+
      theme_classic()+
      theme(legend.position = c(0.35,0.9),legend.background = element_blank(),
            legend.text = element_text(size=18),
            axis.title = element_text(size=18),
            axis.text = element_text(size = 16))
    #legend
    if(do_legend==FALSE){
    p_plot<-p_plot+
      theme(legend.position = "none")
    }
    # print(p_plot)
    #returun object
    out<-c()
    out$plot<-p_plot
    
    return(out)
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

#Here only check the VIs:


##greenness...
#gcc_90
p_gcc_90_len5_b30<-plot_2groups(df.VI,"gcc_90","",do_norm = FALSE,TRUE)
#rcc_90
p_rcc_90_len5_b30<-plot_2groups(df.VI,"rcc_90","",do_norm = FALSE,FALSE)
#GRVI
p_GRVI_len5_b30<-plot_2groups(df.VI,"GRVI","",do_norm = FALSE,FALSE)

#-------------------------------------------------------------------------
#(4) save the plot
#-------------------------------------------------------------------------
save.path<-"C:/Users/yluo/Desktop/R_testcode/PhotoCold/Second_round_of_code/plot/comp_vars/based_Beni/update_results/quantile_plot/All_sites/"
#merge plots
#--------------
#PhenoCam VIs-->using all data
#-------------
p_merge_VIs<-plot_grid(p_gcc_90_len5_b30$plot,
                       p_rcc_90_len5_b30$plot,
                       p_GRVI_len5_b30$plot,
                       labels = "auto",nrow=2,label_size = 12,align = "hv")
ggsave(paste0(save.path,"p_VIs_usingalldata.png"),p_merge_VIs,width = 15,height = 10)
#--------------
#PhenoCam VIs-->only using ENF data
#-------------
#gcc_90
p_gcc_90_len5_b30_ENF<-plot_2groups(df.VI_ENF,"gcc_90","",do_norm = FALSE,TRUE)
#rcc_90
p_rcc_90_len5_b30_ENF<-plot_2groups(df.VI_ENF,"rcc_90","",do_norm = FALSE,FALSE)
#GRVI
p_GRVI_len5_b30_ENF<-plot_2groups(df.VI_ENF,"GRVI","",do_norm = FALSE,FALSE)

p_merge_VIs_ENF<-plot_grid(p_gcc_90_len5_b30_ENF$plot,
                       p_rcc_90_len5_b30_ENF$plot,
                       p_GRVI_len5_b30_ENF$plot,
                       labels = "auto",nrow=2,label_size = 12,align = "hv")
ggsave(paste0(save.path,"p_VIs_usingENFdata.png"),p_merge_VIs_ENF,width = 15,height = 10)

