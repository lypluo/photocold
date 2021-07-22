#############################################################################
#Aim:tidy the site-years that have the "is_event":the dates when GPP is overestimation.
#-------------------------------------------------------------------------
#(1)load the data of the "is_event" length
#-------------------------------------------------------------------------
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
# df.sep15<-sep_siteyears(df_events_all,"Over_days_length",15)
df.sep20<-sep_siteyears(df_events_all,"Over_days_length",20)
df.sep30<-sep_siteyears(df_events_all,"Over_days_length",30)
df.sep45<-sep_siteyears(df_events_all,"Over_days_length",45)
df.sep60<-sep_siteyears(df_events_all,"Over_days_length",60)
#------------------------------------------
#additional:to quantify %years are overstimation in one site
#------------------------------------------
library(plyr)
quantify_over_perc<-function(df){
  # df<-df.sep30
  
  #
  overyear_num.df<-ddply(df$event_siteyears,.(sitename),summarise,Over_year_num=length(Year))
  # overyear_num.df$flag<-rep("Beni_flag_oversites",length(overyear_num.df$sitename))
  nonoveryear_num.df<-ddply(df$noevent_siteyears,.(sitename),summarise,Nonover_year_num=length(Year))
  # nonoveryear_num.df$flag<-rep("Beni_flag_nonoversites",length(nonoveryear_num.df$sitename))
  #
  allyears_num<-merge(overyear_num.df,nonoveryear_num.df,by="sitename",all = T)
  Overyear_perc<-100*allyears_num$Over_year_num/rowSums(allyears_num[,c("Over_year_num","Nonover_year_num")],na.rm = T)
  allyears_num$Overyear_perc<-Overyear_perc
  #set NA==0 if  Overyear_perc=NA
  allyears_num$Overyear_perc[is.na(allyears_num$Overyear_perc)]<-0
  return(allyears_num)
}
over_perc_sep20<-quantify_over_perc(df.sep20)
over_perc_sep30<-quantify_over_perc(df.sep30)
over_perc_sep45<-quantify_over_perc(df.sep45)
over_perc_sep60<-quantify_over_perc(df.sep60)

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
##write a function to calculate the avg Temp and avg Prec in different period: annual and green-up period
cal_sites_meteos<-function(df.data,df.perc,period){
  # df.data<-df_norm_all
  # df.perc<-over_perc_sep30
  # period<-"annual"
  #-------------------------------------------------------------------------
  #1.separating the datasets according to different period
  #-------------------------------------------------------------------------
  sitenames<-unique(df.data$sitename)
  df.sites<-c()
  #for annual
  if(period=="annual"){
   for(i in 1:length(sitenames)){
     df.temp<-subset(df.data,sitename==sitenames[i])
     df.sites[[i]]<-df.temp
   }
  names(df.sites)<-sitenames
  }
  #for green-up
  if(period=="greenup"){
    for(i in 1:length(sitenames)){
      df.temp<-subset(df.data,sitename==sitenames[i])
      df.temp<-df.temp[df.temp$greenup=="yes",]
      df.sites[[i]]<-df.temp
    }
    names(df.sites)<-sitenames
  }
  
  #-------------------------------------------------------------------------
  #2.calculate the mean and sd of the Ta  and Prec
  #-------------------------------------------------------------------------
  df.meteo<-c()
  for(i in 1:length(df.sites)){
    df.run<-df.sites[[i]]
    Ta.temp<-ddply(df.run,.(sitename),summarise,Ta.mean=mean(temp_day_fluxnet2015,na.rm = T),
          Ta.sd=sd(temp_day_fluxnet2015,na.rm = T),
          Ta.min=quantile(temp_day_fluxnet2015,probs = 0.05,na.rm=T),
          Ta.max=quantile(temp_day_fluxnet2015,probs = 0.95,na.rm = T))
    Prec.temp.Year<-ddply(df.run, .(sitename,Year),summarise,Prec=sum(prec_fluxnet2015))
    Prec.temp<-ddply(Prec.temp.Year,.(sitename),summarize,prec.mean=mean(Prec),prec.sd=sd(Prec))
    temp<-cbind(Ta.temp,Prec.temp[,2:3])
    df.meteo<-rbind(df.meteo,temp)
  }
  #-------------------------------------------------------------------------
  #3.merge the df.meteo and df.perc and return the merge datasets
  #-------------------------------------------------------------------------
  df.merge<-merge(df.meteo,df.perc,by="sitename",all.x = T)
  #sepate the df.merege
  if(period=="annual"){
    df.merge$flag<-rep(NA,nrow(df.merge))
    df.merge[df.merge$prec.mean>=800,]$flag<-"rainfall: >= 800 mm"
    df.merge[df.merge$prec.mean<800,]$flag<-"rainfall: < 800 mm"
  }
  if(period=="greenup"){
    df.merge$flag<-rep(NA,nrow(df.merge))
    df.merge[df.merge$Ta.mean>=10,]$flag<-"Ta_warm: >=10 degree"
    df.merge[df.merge$Ta.mean<10,]$flag<-"Ta_cool: <10 degree"
  }
    
  return(df.merge)
}

#-------------------------------------------------------------------------
#(3)making plots
#-------------------------------------------------------------------------
#I.Taking df.sep30 as an example-------------------------
  #------------
  #A.separate the "is_event" days using different separate methods
  #-------------
  df.annual_20<-cal_sites_meteos(df_norm_all,over_perc_sep20,"annual")
  df.greenup_20<-cal_sites_meteos(df_norm_all,over_perc_sep20,"greenup")
  
  df.annual_30<-cal_sites_meteos(df_norm_all,over_perc_sep30,"annual")
  df.greenup_30<-cal_sites_meteos(df_norm_all,over_perc_sep30,"greenup")
  
  df.annual_45<-cal_sites_meteos(df_norm_all,over_perc_sep45,"annual")
  df.greenup_45<-cal_sites_meteos(df_norm_all,over_perc_sep45,"greenup")
  
  df.annual_60<-cal_sites_meteos(df_norm_all,over_perc_sep60,"annual")
  df.greenup_60<-cal_sites_meteos(df_norm_all,over_perc_sep60,"greenup")
  
  #------------
  #B.making plots
  #-------------
  library(ggplot2)
  library(cowplot)
  library(grid)
  library(ggrepel)
  library(ggpubr)
  comp_sites<-function(df,period){
    # df<-df.greenup_30
    # period<-"greenup"
    
    #scatter plot
    df.plot1<-ggplot(df,aes(x=prec.mean,y=Ta.mean,col=Overyear_perc))+
      geom_point(size=4,pch=16)+
      # geom_errorbarh(aes(xmax = prec.sum+prec.sd, xmin = prec.sum-prec.sd))+
      geom_pointrange(aes(xmin = prec.mean-prec.sd, xmax = prec.mean+prec.sd))+  #prec.sd should have some probelm as I used sd for each day
      geom_pointrange(aes(ymin=Ta.mean-Ta.sd,ymax=Ta.mean+Ta.sd))+
      xlab("Prec (mm)")+ylab("Avg Ta (degreeC)")+
      scale_color_gradientn(colours = rev(terrain.colors(10)))+
      theme_classic()+
      theme(axis.title = element_text(size=18),
            axis.text = element_text(size=16),
            legend.title = element_text(size=16),
            legend.text = element_text(size=12))
    df.plot2<-ggplot(df,aes(x=prec.mean,y=Ta.min,col=Overyear_perc))+
      geom_point(size=4,pch=16)+
      # geom_errorbarh(aes(xmax = prec.sum+prec.sd, xmin = prec.sum-prec.sd))+
      geom_pointrange(aes(xmin = prec.mean-prec.sd, xmax = prec.mean+prec.sd))+  #prec.sd should have some probelm as I used sd for each day
      # geom_pointrange(aes(ymin=Ta.mean-Ta.sd,ymax=Ta.mean+Ta.sd))+
      xlab("Prec (mm)")+ylab("Minum Ta (degreeC)")+
      scale_color_gradientn(colours = rev(terrain.colors(10)))+
      theme_classic()+
      theme(axis.title = element_text(size=18),
            axis.text = element_text(size=16),
            legend.title = element_text(size=16),
            legend.text = element_text(size=12))
    #
    grob_annual <- grobTree(textGrob("Annual", x=0.7,  y=0.1, hjust=0,
                                    gp=gpar(col="black", fontsize=18, fontface="italic")))
    grob_greenup <- grobTree(textGrob("Green-up", x=0.5,  y=0.1, hjust=0,
                                     gp=gpar(col="black", fontsize=18, fontface="italic")))
    
    if(period=="annual"){
    df.plot1<-df.plot1+
      geom_hline(yintercept = 10,lty=2,size=1.2)+
      geom_vline(xintercept = 800,lty=2,size=1.2)+
      annotation_custom(grob = grob_annual)+
      geom_label_repel(aes(label = sitename,   #add site names
                            fill = factor(flag)), color = 'grey10',size = 3.5) +
      scale_fill_manual(values = c("rainfall: >= 800 mm"=adjustcolor("cyan",0.5),
                                   "rainfall: < 800 mm"=adjustcolor("tomato",0.5)))+
      theme(legend.position = c(0.8,0.2))
    
    df.plot2<-df.plot2+
      geom_hline(yintercept = 0,lty=2,size=1.2)+
      geom_vline(xintercept = 800,lty=2,size=1.2)+
      annotation_custom(grob = grob_annual)+
      geom_label_repel(aes(label = sitename,  #add sites name
                           fill = factor(flag)), color = 'grey10',size = 3.5) +
      scale_fill_manual(values = c("rainfall: >= 800 mm"=adjustcolor("cyan",0.5),
                                   "rainfall: < 800 mm"=adjustcolor("tomato",0.5)))+
      theme(legend.position = c(0.8,0.2))
    }
    if(period=="greenup"){
      df.plot1<-df.plot1+
        geom_hline(yintercept = 10,lty=2,size=1.2)+
        annotation_custom(grob = grob_greenup)+
        geom_label_repel(aes(label = sitename,   #add site names
                             fill = factor(flag)), color = 'grey10',size = 3.5) +
        scale_fill_manual(values = c("Ta_cool: <10 degree"=adjustcolor("cyan",0.5),
                                     "Ta_warm: >=10 degree"=adjustcolor("tomato",0.5)))+
        theme(legend.position = c(0.8,0.2))
      df.plot2<-df.plot2+
        geom_hline(yintercept = 0,lty=2,size=1.2)+
        annotation_custom(grob = grob_greenup)+
        geom_label_repel(aes(label = sitename,   #add site names
                             fill = factor(flag)), color = 'grey10',size = 3.5) +
        scale_fill_manual(values = c("Ta_cool: <10 degree"=adjustcolor("cyan",0.5),
                                     "Ta_warm: >=10 degree"=adjustcolor("tomato",0.5)))+
        theme(legend.position = c(0.8,0.2))
    }
    
    #
    df.plot<-ggarrange(df.plot1,df.plot2,ncol=2,align = "hv",common.legend = TRUE)
    # df.plot<-plot_grid(df.plot1,df.plot2,
    #                    labels = "auto",ncol=2,label_size = 12,align = "hv")
    # print(df.plot)
    return(df.plot)
    
  }
#-------------------------------------------------------------------------
#(3)save plots
#-------------------------------------------------------------------------
#save the plot
save.path<-"C:/Users/yluo/Desktop/R_testcode/PhotoCold/Second_round_of_code/plot/comp_sites/"
#b1.annual year
#gpp_obs_sep20
p_annual_sep20<-comp_sites(df.annual_20,"annual")
#gpp_obs_sep30
p_annual_sep30<-comp_sites(df.annual_30,"annual")
#gpp_obs_sep45
p_annual_sep45<-comp_sites(df.annual_45,"annual")
#gpp_obs_sep60
p_annual_sep60<-comp_sites(df.annual_60,"annual")

#b1.greenup
#gpp_obs_sep20
p_greenup_sep20<-comp_sites(df.greenup_20,"greenup")
#gpp_obs_sep30
p_greenup_sep30<-comp_sites(df.greenup_30,"greenup")
#gpp_obs_sep45
p_greenup_sep45<-comp_sites(df.greenup_45,"greenup")
#gpp_obs_sep60
p_greenup_sep60<-comp_sites(df.greenup_60,"greenup")


#put anunal and greenup results together
p_sep20<-ggarrange(p_annual_sep20,p_greenup_sep20,nrow = 2,align = "hv",common.legend = TRUE)
ggsave(paste0(save.path,"annual_sep20.png"),p_sep20,width = 12,height = 12)

p_sep30<-ggarrange(p_annual_sep30,p_greenup_sep30,nrow = 2,align = "hv",common.legend = TRUE)
ggsave(paste0(save.path,"annual_sep30.png"),p_sep30,width = 12,height = 12)

p_sep45<-ggarrange(p_annual_sep45,p_greenup_sep45,nrow = 2,align = "hv",common.legend = TRUE)
ggsave(paste0(save.path,"annual_sep45.png"),p_sep45,width = 12,height = 12)

p_sep60<-ggarrange(p_annual_sep60,p_greenup_sep60,nrow = 2,align = "hv",common.legend = TRUE)
ggsave(paste0(save.path,"annual_sep60.png"),p_sep60,width = 12,height = 12)

