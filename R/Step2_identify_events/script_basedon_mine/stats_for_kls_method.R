#############################################################################
#Aim:to summary how many years that "klosterman" method doesn't work
#-------------------------------------------------------------------------
#(1)load the data
#-------------------------------------------------------------------------
load.path<-"C:/Users/yluo/Desktop/CES/Data_for_use/Data_sent_by_Beni/"
load(paste0(load.path,"df_all_results/df_all_norm_kls_newmethod.RDA"))
#-------------------------------------------------------------------------
#(2)calculate how many years and percentage that the klsosterman method is failed
#-------------------------------------------------------------------------
kls_failure<-c()
for(i in 1:length(df_all)){
  kls_temp<-df_all[[i]]
  method_labels<-kls_temp$pos_agg$method
  kls_failure_number<-length(which(method_labels=="thrs"))
  kls_failure_percent<-length(which(method_labels=="thrs"))/length(method_labels)
  kls_failure_temp<-c(kls_failure_number,kls_failure_percent)
  kls_failure<-rbind(kls_failure,kls_failure_temp)
}
kls_failure<-data.frame(kls_failure)
names(kls_failure)<-c("number","percentage")
row.names(kls_failure)<-names(df_all)
