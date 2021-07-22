#############################################################################
#Aim:tidy the stats from the "is_event":the dates when GPP is overestimation.
#-------------------------------------------------------------------------
#(1)load the data
#-------------------------------------------------------------------------
load.path<-"C:/Users/yluo/Desktop/CES/Data_for_use/Data_sent_by_Beni/"
#old method before
load(paste0(load.path,"ddf_labeled_ori_trs.RDA"))
df_ori_trs_oldmethod<-ddf_labeled;rm(ddf_labeled)

load(paste0(load.path,"ddf_labeled_norm_trs.RDA"))
df_norm_trs_oldmethod<-ddf_labeled;rm(ddf_labeled)

load(paste0(load.path,"ddf_labeled_norm_kls.RDA"))
df_norm_kls_oldmethod<-ddf_labeled;rm(ddf_labeled)
#from new method:
load(paste0(load.path,"ddf_labeled_norm_trs_newmethod.RDA"))
df_norm_trs_newmethod<-ddf_labeled;rm(ddf_labeled)

load(paste0(load.path,"ddf_labeled_norm_kls_newmethod.RDA"))
df_norm_kls_newmethod<-ddf_labeled;rm(ddf_labeled)

#(2)merge three sources of DoYs
df_ori_trs_oldM_sel<-df_ori_trs_oldmethod[,c("sitename","date","doy","is_event")]
df_norm_trs_oldM_sel<-df_norm_trs_oldmethod[,c("sitename","date","doy","is_event")]
df_norm_kls_oldM_sel<-df_norm_kls_oldmethod[,c("sitename","date","doy","is_event")]
df_norm_trs_newM_sel<-df_norm_trs_newmethod[,c("sitename","date","doy","is_event_less10")]
df_norm_kls_newM_sel<-df_norm_kls_newmethod[,c("sitename","date","doy","is_event_less10")]

#
names(df_ori_trs_oldM_sel)<-c("sitename","date","doy","ori_trs_oldM")
names(df_norm_trs_oldM_sel)<-c("sitename","date","doy","norm_trs_oldM")
names(df_norm_kls_oldM_sel)<-c("sitename","date","doy","norm_kls_oldM")
names(df_norm_trs_newM_sel)<-c("sitename","date","doy","norm_trs_newM")
names(df_norm_kls_newM_sel)<-c("sitename","date","doy","norm_kls_newM")

#merge
df_ori_trs_oldM_sel$date<-as.POSIXct(df_ori_trs_oldM_sel$date)
df_norm_trs_oldM_sel$date<-as.POSIXct(df_norm_trs_oldM_sel$date)
df_norm_kls_oldM_sel$date<-as.POSIXct(df_norm_kls_oldM_sel$date)
#
pos_match<-match(df_norm_trs_newM_sel$date,df_norm_trs_oldM_sel$date)
df_ori_trs_oldM_sel<-df_ori_trs_oldM_sel[pos_match,]
df_norm_trs_oldM_sel<-df_norm_trs_oldM_sel[pos_match,]
df_norm_kls_oldM_sel<-df_norm_kls_oldM_sel[pos_match,]
library(reshape2)
df_all<-cbind(df_ori_trs_oldM_sel,df_norm_trs_oldM_sel[,"norm_trs_oldM"],df_norm_kls_oldM_sel[,"norm_kls_oldM"],
              df_norm_trs_newM_sel[,"norm_trs_newM"],df_norm_kls_newM_sel[,"norm_kls_newM"])
names(df_all)<-c("sitename","date","doy","ori_trs_oldM","norm_trs_oldM","norm_kls_oldM","norm_trs_newM","norm_kls_newM")
df.event<-melt(df_all,id.vars = c("sitename","date","doy"))
names(df.event)<-c("sitename","date","doy","pheno_source","is_event")
#------------------------------------------------------------------------
#(2)plotting
#---------------------------------
##histogram for is_event
#---------------------------------
library(ggplot2)
library(plyr)
#calculate the mean of doy
df.plot<-df.event[df.event$is_event=="yes",]
df.plot$pheno_source<-factor(df.plot$pheno_source,levels = c("ori_trs_oldM","norm_trs_oldM","norm_kls_oldM","norm_trs_newM","norm_kls_newM"))
mu <- ddply(df.plot, "pheno_source", summarise, grp.mean=mean(doy))
p_hist<-ggplot(df.plot, aes(x=doy, col=pheno_source))+
  geom_histogram(binwidth = 1,fill="white",
                 position="identity")+
  # geom_histogram(aes(y=..density..), alpha=0.5,binwidth = 5,
  #                position="identity")+
  geom_vline(data=mu, aes(xintercept=grp.mean, color=pheno_source),
             linetype="dashed",size=1.2)+
  scale_color_manual(values=c("ori_trs_oldM"="tomato","norm_trs_oldM"="green3","norm_kls_oldM"="skyblue",
                              "norm_trs_newM"="grey","norm_kls_newM"="orange"))+
  theme_minimal()

p_density<-ggplot(df.plot, aes(x=doy, col=pheno_source,fill=pheno_source))+
  geom_vline(data=mu, aes(xintercept=grp.mean, color=pheno_source),
             linetype="dashed",size=1.2)+
  # geom_histogram(aes(y=..density..), alpha=0.5, 
  #                position="identity")+
  geom_density(alpha=.4,size=1.2)+
  scale_color_manual(values=c("ori_trs_oldM"="tomato","norm_trs_oldM"="green3","norm_kls_oldM"="skyblue",
                              "norm_trs_newM"="grey","norm_kls_newM"="orange"))+
  scale_fill_manual(values=c("ori_trs_oldM"="tomato","norm_trs_oldM"="green3","norm_kls_oldM"="skyblue",
                             "norm_trs_newM"="grey","norm_kls_newM"="orange"))+
  theme_minimal()
#
#library(cowplot)
library(ggpubr)
library(gridExtra)
p_merge<-ggarrange(p_hist,p_density,ncol=2,align = "hv",common.legend=TRUE)
# p_merge<-plot_grid(p_hist,p_density,
#                    labels = "auto",ncol=2,label_size = 12,align = "hv")
#save the plot
save.path<-"C:/Users/yluo/Documents/GitHub/photocold/plot/"
# pdf(paste0(save.path,"is_event(doy).pdf"),width = 8,height=5)
# print(p_merge)
# dev.off()
plot(p_merge)
ggsave(file=paste0(save.path,"is_event(doy).png"),p_merge,dev="png",width = 10,height=5)

