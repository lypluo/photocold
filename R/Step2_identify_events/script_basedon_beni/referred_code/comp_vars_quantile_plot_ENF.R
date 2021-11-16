#############################################################################
#Aim:compare the variables in "event" and "non-event" site years after aligning the data
#----->only target at the ENF
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
###########################
#!!!only select the ENF
###########################
df_events<-df_len5_nonnorm$df_dday[df_len5_nonnorm$df_dday$fluxnet_PFT=="ENF",]
df_nonevents<-df_len5_nonnorm$df_noevent_dday[df_len5_nonnorm$df_noevent_dday$fluxnet_PFT=="ENF",]
#
df_len5_nonnorm$df_dday<-df_events
df_len5_nonnorm$df_noevent_dday<-df_nonevents
#-------------------------------------------------------------------------
#(2)going to compare the "event" and "non-event" site
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
fun.path<-"C:/Users/yluo/Desktop/R_testcode/PhotoCold/Second_round_of_code/R/Step2_identify_events/Functions/functions_from_YP/"
source(paste0(fun.path,"Difference_test_for_2Classes.R"))

plot_2groups<-function(df,comp_var,var_unit,do_norm,do_legend){
    # df<-df_len5_nonnorm
    # comp_var<-"gpp_obs"
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
    df.all$flag<-factor(df.all$flag,levels = c("event-sites","nonevent-sites"))
      
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

      df_comp_event<-df_comp[df_comp$flag=="event-sites",]
      df_comp_nonevent<-df_comp[df_comp$flag=="nonevent-sites",]
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
      annotate("rect",xmin=0,xmax=max(df.all_q$dday),ymin = -Inf,ymax = Inf,alpha=0.2)+
      geom_line(aes(x=dday,y=q50,col=flag),size=1.05)+
      scale_color_manual("",values = c("event-sites"="red","nonevent-sites"="green4"))+
      geom_ribbon(aes(x=dday,ymin=q10,ymax=q90,fill=flag),alpha=0.15)+
      geom_ribbon(aes(x=dday,ymin=q25,ymax=q75,fill=flag),alpha=0.4)+
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
    out$stats_q50<-stat.q50_for2
    out$stats_alldata<-stat.alldata_for2
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

#
#a1.check for the gpp_obs
#b_30 stands for 30 days before event
#gpp_obs_len5
p_gpp_obs_len5_b30<-plot_2groups(df_len5_nonnorm,"gpp_obs","(umol m-2 s-1)",do_norm = FALSE,do_legend = TRUE)

#a2.check for the temp_day,temp_min,and temp_max
#temp_day
p_temp_day_len5_b30<-plot_2groups(df_len5_nonnorm,"temp_day_fluxnet2015","(degreeC)",do_norm = FALSE,do_legend = FALSE)

#a3.fapar_spl and fapar_itpl
p_fapar_itpl_len5_b30<-plot_2groups(df_len5_nonnorm,"fapar_itpl","",do_norm = FALSE,do_legend = FALSE)

#a4.for VPD
#vpd_day
p_vpd_day_len5_b30<-plot_2groups(df_len5_nonnorm,"vpd_day_fluxnet2015","(Pa)",do_norm = FALSE,do_legend = FALSE)

#a5.for prec
p_prec_len5_b30<-plot_2groups(df_len5_nonnorm,"prec_fluxnet2015","(mm)",do_norm = FALSE,do_legend = FALSE)

#a5.for ppfd
p_ppfd_len5_b30<-plot_2groups(df_len5_nonnorm,"ppfd_fluxnet2015","(u mol m-2 s-1)",do_norm = FALSE,do_legend = FALSE)

#additional: add more variables to compare-->July-5
#temp_min
p_temp_min_len5_b30<-plot_2groups(df_len5_nonnorm,"temp_min_fluxnet2015","(degreeC)",do_norm = FALSE,TRUE)
#temp_max
p_temp_max_len5_b30<-plot_2groups(df_len5_nonnorm,"temp_max_fluxnet2015","(degreeC)",do_norm = FALSE,FALSE)
#SW_IN
p_SW_IN_len5_b30<-plot_2groups(df_len5_nonnorm,"SW_IN_fluxnet2015","(W m-2)",do_norm = FALSE,FALSE)
#TS_1-->first layer soil temperature
p_TS_1_len5_b30<-plot_2groups(df_len5_nonnorm,"TS_1_fluxnet2015","(degreeC)",do_norm = FALSE,FALSE)
#SWC_1-->first layer soil mosture
p_SWC_1_len5_b30<-plot_2groups(df_len5_nonnorm,"SWC_1_fluxnet2015","(%)",do_norm = FALSE,FALSE)
#!gpp biaes
p_gpp_biaes_len5_b30<-plot_2groups(df_len5_nonnorm,"gpp_res","(umol m-2 s-1)",do_norm = FALSE,FALSE)
#LUE
p_LUE_len5_b30<-plot_2groups(df_len5_nonnorm,"LUE","",do_norm = FALSE,FALSE)

##greenness...
#gcc_90
p_gcc_90_len5_b30<-plot_2groups(df_len5_nonnorm,"gcc_90","",do_norm = FALSE,TRUE)
#rcc_90
p_rcc_90_len5_b30<-plot_2groups(df_len5_nonnorm,"rcc_90","",do_norm = FALSE,FALSE)
#GRVI:
p_GRVI_len5_b30<-plot_2groups(df_len5_nonnorm,"GRVI","",do_norm = FALSE,FALSE)

#-------------------------------------------------------------------------
#(4) save the plot
#-------------------------------------------------------------------------
save.path<-"C:/Users/yluo/Desktop/R_testcode/PhotoCold/Second_round_of_code/plot/comp_vars/based_Beni/update_results/quantile_plot/All_sites/onlyENF/"
#merge plots
#--------------
#I.GPP and LUE
#-------------
p_merge_Cflux<-plot_grid(p_gpp_obs_len5_b30$plot,
                         p_ppfd_len5_b30$plot,
                         p_fapar_itpl_len5_b30$plot,
                         p_LUE_len5_b30$plot,
                         p_gpp_biaes_len5_b30$plot,
                         labels = "auto",nrow=2,label_size = 12,align = "hv")
ggsave(paste0(save.path,"p_Cflux.png"),p_merge_Cflux,width = 20,height = 15)
#--------------
#II.Environment variables
#-------------
p_merge_EnviroVars<-plot_grid( 
                   p_temp_min_len5_b30$plot,p_temp_day_len5_b30$plot,p_temp_max_len5_b30$plot,
                   p_SW_IN_len5_b30$plot,
                   p_prec_len5_b30$plot,
                   p_vpd_day_len5_b30$plot,
                   p_SWC_1_len5_b30$plot,
                   p_TS_1_len5_b30$plot,
                   labels = "auto",nrow=2,label_size = 12,align = "hv")
ggsave(paste0(save.path,"p_EnviroVars.png"),p_merge_EnviroVars,width = 20,height = 15)
#--------------
#III.PhenoCam VIs
#-------------
p_merge_VIs<-plot_grid(p_gcc_90_len5_b30$plot,
                       p_rcc_90_len5_b30$plot,
                       p_GRVI_len5_b30$plot,
                       labels = "auto",nrow=2,label_size = 12,align = "hv")
ggsave(paste0(save.path,"p_VIs.png"),p_merge_VIs,width = 15,height = 10)
#--------------
#IV.stats-->difference test
#-------------
stats_merge_Cflux<-rbind(gpp_obs=p_gpp_obs_len5_b30$stats_q50[1,],
                         ppdf=p_ppfd_len5_b30$stats_q50[1,],
                         fapar=p_fapar_itpl_len5_b30$stats_q50[1,],
                         LUE=p_LUE_len5_b30$stats_q50[1,],
                         gpp_biases=p_gpp_biaes_len5_b30$stats_q50[1,])
stats_merge_EnviroVars<-rbind( 
  temp_min=p_temp_min_len5_b30$stats_q50[1,],
  temp_day=p_temp_day_len5_b30$stats_q50[1,],
  temp_max=p_temp_max_len5_b30$stats_q50[1,],
  SW_IN=p_SW_IN_len5_b30$stats_q50[1,],
  prec=p_prec_len5_b30$stats_q50[1,],
  vpd=p_vpd_day_len5_b30$stats_q50[1,],
  SWC_1=p_SWC_1_len5_b30$stats_q50[1,],
  TS_1=p_TS_1_len5_b30$stats_q50[1,])
stats_merge_VIs<-rbind(gcc_90=p_gcc_90_len5_b30$stats_q50[1,],
                       rcc_90=p_rcc_90_len5_b30$stats_q50[1,],
                       GRVI=p_GRVI_len5_b30$stats_q50[1,])
#
stats_merge_Cflux$flag<-rep("Cflux",nrow(stats_merge_Cflux))
stats_merge_EnviroVars$flag<-rep("EnviroVars",nrow(stats_merge_EnviroVars))
stats_merge_VIs$flag<-rep("VIs",nrow(stats_merge_VIs))
stats_all<-rbind(stats_merge_Cflux,stats_merge_EnviroVars,stats_merge_VIs)
#-------------------------------------------------------------------------
#(5) one additional plot: SWC,VPD, GPP_biaes-->density plot
#-->need to update the results once normalizing the SWC
#-------------------------------------------------------------------------
library(dplyr)
library(LSD)
#source Beni's function to plot heatmap
source.path<-"C:/Users/yluo/Desktop/R_testcode/PhotoCold/Second_round_of_code/R/Step2_identify_events/Functions/functions_from_beni/"
source(paste0(source.path,"LSD.heatscatter.R"))
##
save.path<-"C:/Users/yluo/Desktop/R_testcode/PhotoCold/Second_round_of_code/plot/comp_vars/based_Beni/update_results/density_plot/onlyENF/"
##
  df<-df_len5_nonnorm
  #event sites and non-event sites
  df_event<-df$df_dday
  df_nonevent<-df$df_noevent_dday
  #---------------------
  #density plot
  #---------------------
  #pay attention: remember to source the function LSD.heatscatter.R
  gg_event_mean <- heatscatter(df_event$temp_day_fluxnet2015,df_event$ppfd_fluxnet2015, xlab = "Mean Ta (degreeC)", ylab = "ppfd (umol m-2 s-1)", 
         ggplot = TRUE)+
         xlim(-30,25)+
         geom_vline(xintercept = 0,lty=2,lwd=1.2)+
         labs(title = "Event sites")
  gg_nonevent_mean <- heatscatter(df_nonevent$temp_day_fluxnet2015,df_nonevent$ppfd_fluxnet2015, xlab = "Mean Ta (degreeC)", ylab = "ppfd (umol m-2 s-1)", 
         ggplot = TRUE)+
         xlim(-30,25)+
         geom_vline(xintercept = 0,lty=2,lwd=1.2)+
         labs(title = "Non-event sites")
  gg_event_min <- heatscatter(df_event$temp_min_fluxnet2015,df_event$ppfd_fluxnet2015, xlab = "Mininum Ta (degreeC)", ylab = "ppfd (umol m-2 s-1)", 
        ggplot = TRUE)+
        xlim(-30,25)+
        geom_vline(xintercept = 0,lty=2,lwd=1.2)+
        labs(title = "Event sites")
  gg_nonevent_min <- heatscatter(df_nonevent$temp_min_fluxnet2015,df_nonevent$ppfd_fluxnet2015, xlab = "Mininum Ta (degreeC)", ylab = "ppfd (umol m-2 s-1)", 
        ggplot = TRUE)+
        xlim(-30,25)+
        geom_vline(xintercept = 0,lty=2,lwd=1.2)+
        labs(title = "Non-event sites")
  
  #
  gg_density<-plot_grid(gg_event_mean,gg_event_min,
                        gg_nonevent_mean,gg_nonevent_min,
                        labels = "auto",nrow =2,label_size = 12,align = "hv")
  ggsave(paste0(save.path,"Ta_ppfd_density.png"),gg_density,width = 15,height = 10)
  #------------------------
  #add gpp_res as the color
  #------------------------
  colpal = colorpalette(colpal="heat",nrcol=30,simulate = FALSE,daltonize = FALSE,cvd = "p",alpha = NULL,rev = FALSE)

  # p_SWC_VPD<-ggplot(df_nonevent,aes(x=SWC_1_fluxnet2015,y=vpd_day_fluxnet2015,col=gpp_res))+
  #   geom_point()
  #event and non-event sites
  p_temp_ppfd_event_mean<-ggplot(df_event,aes(x=temp_day_fluxnet2015,y=ppfd_fluxnet2015,col=gpp_res))+
    geom_point()+
    xlab( "Mean Ta (degreeC)")+
    ylab("ppfd (umol m-2 s-1)")+
    scale_color_continuous(type="viridis")+
    labs(title = "Event sites") 
  p_temp_ppfd_nonevent_mean<-ggplot(df_nonevent,aes(x=temp_day_fluxnet2015,y=ppfd_fluxnet2015,col=gpp_res))+
    geom_point()+
    xlab( "Mean Ta (degreeC)")+
    ylab("ppfd (umol m-2 s-1)")+
    scale_color_continuous(type="viridis")+
    labs(title = "Non-event sites")
  p_temp_ppfd_event_min<-ggplot(df_event,aes(x=temp_min_fluxnet2015,y=ppfd_fluxnet2015,col=gpp_res))+
    geom_point()+
    xlab( "Mininum Ta (degreeC)")+
    ylab("ppfd (umol m-2 s-1)")+
    scale_color_continuous(type="viridis")+
    labs(title = "Event sites") 
  p_temp_ppfd_nonevent_min<-ggplot(df_nonevent,aes(x=temp_min_fluxnet2015,y=ppfd_fluxnet2015,col=gpp_res))+
    geom_point()+
    xlab( "Mininum Ta (degreeC)")+
    ylab("ppfd (umol m-2 s-1)")+
    scale_color_continuous(type="viridis")+
    labs(title = "Non-event sites")
  #
  gg_biaes<-plot_grid(p_temp_ppfd_event_mean,p_temp_ppfd_event_min,
                      p_temp_ppfd_nonevent_mean,p_temp_ppfd_nonevent_min,
                      labels = "auto",nrow =2,label_size = 12,align = "hv")

ggsave(paste0(save.path,"Ta_ppfd_gpp_biaes.png"),gg_biaes,width = 15,height = 10)
