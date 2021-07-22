#############################################################################
#Aim:compare the variables in "event" and "non-event" site years after aligning the data
#-------------------------------------------------------------------------
#(1)load the data after aligning
#-------------------------------------------------------------------------
load.path<-"C:/Users/yluo/Desktop/CES/Data_for_use/Merge_Data/sep_event_data/"
load(paste0(load.path,"df_len5_nonnorm.RDA"))

#-------------------------------------------------------------------------
#(2)going to compare the "event" and "non-event" site
#-------------------------------------------------------------------------
  #-------------------------------------------------------------------------
  #start plotting
  #-------------------------------------------------------------------------
  library(ggplot2)
  library(cowplot)
  library(grid)
  
  #function used to compare the "event" and "non_evnet" by plotting simple box-plot
plot_2groups<-function(df,comp_var,var_unit,do_norm){
    # df<-df_len5_nonnorm
    # comp_var<-"temp_min_fluxnet2015"
    # var_unit<-"(degreeC)"
    # do_norm<-FALSE
    
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
    ###start to make the box plot:
    df.event_sel$flag<-rep("event-sites",nrow(df.event_sel))
    df.nonevent_sel$flag<-rep("nonevent-sites",nrow(df.nonevent_sel))
    #merge and classify 
    df.all<-rbind(df.event_sel,df.nonevent_sel)
    df.all$flag<-factor(df.all$flag,levels = c("event-sites","nonevent-sites"))
    df.all_before<-df.all[df.all$dday<0,]
    df.all_after<-df.all[df.all$dday>=0,]
    
    plot_boxplot<-function(df.plot,data_flag){
      # df.plot<-df.all
      # data_flag<-"all"
      #
      p_plot<-ggplot(df.plot,aes(x=flag,y=comp_var,col=flag))+
        geom_violin(trim=TRUE)+
        geom_boxplot(width=0.1)+
        ylab(paste0(comp_var," ",var_unit))+
        xlab(data_flag)+
        theme(axis.title = element_text(size=18),
                              axis.text = element_text(size = 16))
      if(data_flag=="all"|data_flag=="before events"){
        p_plot<-p_plot+
          theme(legend.position = "none")
      }
      if(data_flag=="within events"){
        p_plot<-p_plot+
        theme(legend.position = c(0.7,0.1),legend.background = element_blank(),
              legend.text = element_text(size=16),
              legend.title = element_blank())
      }
      
      return(p_plot)
    }
    #
    p_all<-plot_boxplot(df.all,"all")
    p_before<-plot_boxplot(df.all_before,"before events")
    p_after<-plot_boxplot(df.all_after,"within events")
    
    #merge plots
    p_merge<-plot_grid(p_all,p_before,p_after,
                       labels = "auto",ncol=3,label_size = 12,align = "hv")
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

#save the plot
save.path<-"C:/Users/yluo/Desktop/R_testcode/PhotoCold/Second_round_of_code/plot/comp_vars/based_Beni/update_results/box_plot/"
#a1.check for the gpp_obs
#b_30 stands for 30 days before event
#gpp_obs_len5
p_gpp_obs_len5_b30<-plot_2groups(df_len5_nonnorm,"gpp_obs","(gC m-2)",do_norm = FALSE)
ggsave(paste0(save.path,"gpp_obs_len5_b30.png"),p_gpp_obs_len5_b30,width = 15,height = 12)

#a2.check for the temp_day,temp_min,and temp_max
#temp_day
p_temp_day_len5_b30<-plot_2groups(df_len5_nonnorm,"temp_day_fluxnet2015","(degreeC)",do_norm = FALSE)
ggsave(paste0(save.path,"temp_day_len5_b30.png"),p_temp_day_len5_b30,width = 15,height = 12)

#a3.fapar_spl and fapar_itpl
p_fapar_itpl_len5_b30<-plot_2groups(df_len5_nonnorm,"fapar_itpl","",do_norm = FALSE)
ggsave(paste0(save.path,"fapar_itpl_len5_b30.png"),p_fapar_itpl_len5_b30,width = 15,height = 12)
#

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

#additional: add more variables to compare-->July-5
#temp_min
p_temp_min_len5_b30<-plot_2groups(df_len5_nonnorm,"temp_min_fluxnet2015","(degreeC)",do_norm = FALSE)
ggsave(paste0(save.path,"temp_min_len5_b30.png"),p_temp_min_len5_b30,width = 15,height = 12)
#temp_max
p_temp_max_len5_b30<-plot_2groups(df_len5_nonnorm,"temp_max_fluxnet2015","(degreeC)",do_norm = FALSE)
ggsave(paste0(save.path,"temp_max_len5_b30.png"),p_temp_max_len5_b30,width = 15,height = 12)
#gcc_90
p_gcc_90_len5_b30<-plot_2groups(df_len5_nonnorm,"gcc_90","",do_norm = FALSE)
ggsave(paste0(save.path,"gcc_90_len5_b30_merge_smallevents.png"),p_gcc_90_len5_b30,width = 15,height = 12)
#rcc_90
p_rcc_90_len5_b30<-plot_2groups(df_len5_nonnorm,"rcc_90","",do_norm = FALSE)
ggsave(paste0(save.path,"rcc_90_len5_b30_merge_smallevents.png"),p_rcc_90_len5_b30,width = 15,height = 12)

