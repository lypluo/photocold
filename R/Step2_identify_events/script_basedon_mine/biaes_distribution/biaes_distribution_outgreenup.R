#############################################################################
#Aim:To check the biases(GPPmod-GPPobs) distribution outside of "green-up" period
#-------------------------------------------------------------------------
#(1)load the data 
#-------------------------------------------------------------------------
load.path<-"C:/Users/yluo/Desktop/CES/Data_for_use/Data_sent_by_Beni/"
load(paste0(load.path,"df_all_results/df_all_norm_kls_newmethod.RDA"))
#-------------------------------------------------------------------------
#(2)merge the biases distribution plot from different sites:
#-------------------------------------------------------------------------
library(cowplot)
library(ggplot2)
sites<-names(df_all)
plot_biaes<-c()
for(i in 1:length(sites)){
  plot_temp<-df_all[[i]]$p_den_outgreenup
  plot_temp<-plot_temp+
    ylim(0,10)+xlim(-0.6,0.4)+
    geom_vline(xintercept = 0,col="red",lwd=1.2)+
    annotate(geom = "text",-0.4,8,label=sites[i],size=6)
plot_biaes[[i]]<-plot_temp
}
#merge the plots:
p_merge<-plot_grid(plot_biaes[[1]],plot_biaes[[2]],plot_biaes[[3]],
                   plot_biaes[[4]],plot_biaes[[5]],plot_biaes[[6]],
                   plot_biaes[[7]],plot_biaes[[8]],plot_biaes[[9]],
                   plot_biaes[[10]],plot_biaes[[11]],plot_biaes[[12]],
                   labels = "auto",ncol=3,label_size = 12,align = "hv")
#save the plot
save.path<-"C:/Users/yluo/Documents/GitHub/photocold/plot/"
plot(p_merge)
ggsave(file=paste0(save.path,"biaes_distribution_outgreenup.png"),p_merge,dev="png",width = 10,height=10)
