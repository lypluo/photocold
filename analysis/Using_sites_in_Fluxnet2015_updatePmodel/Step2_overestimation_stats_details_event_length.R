#############################################################################
#Aim:tidy the stats from the "is_event":the dates when GPP is overestimation.
#-------------------------------------------------------------------------
#(1)load the data
#-------------------------------------------------------------------------
load.path<-"D:/data/photocold_project/Merge_Data/Using_sites_in_Fluxnet2015_updatePmodel/"
#from new method:
load(paste0(load.path,"ddf_labeled_norm_trs_newmethod_all_overestimation_Fluxnet2015_sites.RDA"))
df_all_sites<-ddf_labeled;rm(ddf_labeled) 
#------------------------------------------------------------------------
#(2)select the stats such as "is_event"
#------------------------------------------------------------------------
df_all_sites_sel<-df_all_sites[,c("sitename","date","doy","Year","greenup","is_event","is_event_less10")]
#------------------------------------------------------------------------
#(3)summary the events stats:
#------------------------------------------------------------------------
#the stats that were summarized:
#A.sos,peak
#B.Over_start,Over_end,Over_period_length,Over_days_length(for GPP overestmation)
#C.Over_start_T10,Over_end_T10,Over_period_length_T10,Over_days_length_T10(for GPP overestmation)
library(plyr)
summary_fun<-function(df){
  # df<-df_all_sites_sel
  
  df_greenup<-df[df$greenup=="yes",]
  df_greenup_sum<-ddply(df_greenup,.(sitename,Year),summarise,sos=min(doy),peak=max(doy))
  
  df_event<-df[df$is_event=="yes",]
  df_event_sum<-ddply(df_event,.(sitename,Year),summarise,Over_start=min(doy),Over_end=max(doy),
                                    Over_period_length=max(doy)-min(doy)+1,Over_days_length=length(is_event))
  df_event_T10<-df[df$is_event_less10=="yes",]
  df_event_T10_sum<-ddply(df_event_T10,.(sitename,Year),summarise,Over_start_T10=min(doy),Over_end_T10=max(doy),
                                        Over_period_length_T10=max(doy)-min(doy)+1,Over_days_length_T10=length(is_event))
  #
  df_event_agg_temp<-merge(df_greenup_sum,df_event_sum,by = c("sitename","Year"),all.x = T)
  df_event_agg<-merge(df_event_agg_temp,df_event_T10_sum,by= c("sitename","Year"),all.x = T)
  return(df_event_agg)
}
##
df_allsites_events<-summary_fun(df_all_sites_sel)
df_events_all<-df_allsites_events #if the value equals NA-->means the sites do not have overestimation
#------------------------------------------------------------------------
#(4)plotting
#---------------------------------
##violin plot for is_event
#---------------------------------
library(ggplot2)
library(plyr)
library(reshape2)
#
df.temp<-df_events_all[,c("sitename","Year","Over_period_length","Over_days_length","Over_period_length_T10","Over_days_length_T10")]
df.events_length<-melt(df.temp,id.var=c("sitename","Year"))
names(df.events_length)<-c("sitename","Year","length_type","length")
#working here
p_length<-ggplot(data=df.events_length,aes(x=length_type,y=length,fill=length_type))+
  geom_violin(trim=TRUE)+
  geom_boxplot(width=0.1)+ 
  geom_hline(yintercept = c(15,30,45,60,75),col="gray",lty=2,lwd=1.2)+
  xlab("")+
  theme_classic()+
  theme(legend.position = "none")
# p_length_flag<-ggplot()+
#   geom_violin(data=df.events_length,aes(x=length_type,y=length,fill=length_type,col=flag),trim=TRUE, position = position_dodge(0.9) )+
#   geom_boxplot(data=df.events_length,aes(x=length_type,y=length,fill=length_type,col=flag), width=0.1, position = position_dodge(0.9) )+ 
#   scale_color_manual(values = c("Oversites"="black", "No_oversites"="gray40","Beyondsites"="gray80"))+
#   geom_hline(yintercept = c(15,30,45,60,75),col="gray",lty=2,lwd=1.2)+
#   theme_classic()+
#   theme(legend.position = c(0.8,0.8),
#         legend.background = element_blank())+
#   guides(fill = guide_legend(nrow = 2, byrow = TRUE))

#library(cowplot)
library(ggpubr)
library(gridExtra)
#p_merge<-ggarrange(p_length,p_length_flag,nrow=2,align = "hv")
#save the plot-->need to check why the plot cannot save:Nov.17,2021
save.path<-"D:/plots/photocold_project/Using_sites_in_Fluxnet2015_updatePmodel/"
plot(p_length)
ggsave(file=paste0(save.path,"Events_lengths_comparison.png"),p_length,dev="png",width = 10,height=10)
#---------------------------------
##(5)save the df.events_length(information of site-years)
#---------------------------------
save.path<-"D:/data/photocold_project/event_length/Using_sites_in_Fluxnet2015_updatePmodel/"
save(df_events_all,file=paste0(save.path,"df_events_length.RDA"))
