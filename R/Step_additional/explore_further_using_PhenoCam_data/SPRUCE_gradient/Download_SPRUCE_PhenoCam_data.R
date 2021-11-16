##########################################
#download the PhenoCam data from PhenoCam US (specifically for the SPRUCE experimental sites)
# https://phenocam.sr.unh.edu/webcam/
##########################################
#------------------------------------
#(1).check the data for SPRUCE experiment
#------------------------------------
#load package
library(data.table)
library(phenocamapi)
# the metadata as data.table
phenos <- get_phenos()

#-------------------------------------------------
#a. Filtering the Phenocam dataset using attributes
#list the sites with "spruce.."
#--------------------------------------------------
sel_sites<-phenos[grep("spruce",phenos$site),]
#-------------------------------------------------
#b. further filtering the sites: group=="SPRUCE"
#and active==TRUE
#--------------------------------------------------
sel_sites<-sel_sites[sel_sites$group=="SPRUCE" & sel_sites$active==TRUE,]

#------------------------------------
#(2).confirm the meaning of the names to prepare the further analysis:
#------------------------------------
#referring the details in Andrew's paper:https://www.nature.com/articles/s41586-018-0399-1
#especially the experimental plot in Hanson et al., 2016(https://bg.copernicus.org/preprints/bg-2016-449/bg-2016-449.pdf)
#a.here I only select the treatments that plants within the constructed enclousres.
df_final<-sel_sites[-c(1:6),]
#for the treatments-->here is the meaning of the acroyoms
#e.g. P06 and P19E-->plot 06 and plot 19 are the chambered temperature control and E means elevated
#separate the Tree and shrub(underground)
#Shurb
df_final_Shurb<-df_final[df_final$primary_veg_type=="SH",]
#Trees
pos<-match(df_final_Shurb$site,df_final$site)
pos_tree<-setdiff(c(1:length(df_final$site)),pos)
df_final_Tree<-df_final[pos_tree,]

#----------------------------------------
#(3).download the data from the PhenoCam USA
#----------------------------------------
Meta.info_Tree<-as.data.frame(df_final_Tree)
Meta.info_Shurb<-as.data.frame(df_final_Shurb)
#download the data through the R package phenocamapi
library(phenocamapi)
#a.write the function to download the PhenoCam data
ts_download<-function(Site,VegType,RoiID,Type){
  # Site="spruceT0P06"
  # VegType="EN"
  # RoiID=c(1000)
  # Type="1day"
  
  ##download the data
  data_df<-c()
  df_names<-paste0(VegType,"_",RoiID)
  for(i in 1:length(RoiID)){
    data_temp<-get_pheno_ts(site=Site,vegType = VegType,roiID = RoiID[i],type = Type)
    #add PFT and ROI_name
    data_temp$PFT<-rep(VegType,nrow(data_temp))
    data_temp$RoiID<-RoiID[i]
    data_df<-rbind(data_df,data_temp)
  }
  return(data_df)
}
#start to downloading the phenocam data
#
save.path<-"D:/CES/Data_for_use/PhenoCam_Data/Through_PhenoCam_USA/VIs/SPRUCE/"
#b.download 3-day time resolution data
########
#for the trees
########
#need to confirm the PFT in each treatment and select the ENF ROIs for analysis:
#T0P06:select EN_1000; T0P19E: select EN_1000
#T2P20:select EN_1000; T2P11E:select EN_1000
#T4P13:select EN_1000; T4P04E: select EN_1000
#T6P08:select EN_1000 T6P16E:select EN_1000
#T9P17 and T9P10E:select EN_1000(2015-2018;looks kind of abnormal) and EN_2000(2018-present)
#
SPRUCE_T0ACO2_EN<-ts_download("spruceT0P06","EN",c(1000),"3day")
SPRUCE_T0ECO2_EN<-ts_download("spruceT0P19E","EN",c(1000),"3day")
SPRUCE_T2ACO2_EN<-ts_download("spruceT2P20","EN",c(1000),"3day")
SPRUCE_T2ECO2_EN<-ts_download("spruceT2P11E","EN",c(1000),"3day")
SPRUCE_T4ACO2_EN<-ts_download("spruceT4P13","EN",c(1000),"3day")
SPRUCE_T4ECO2_EN<-ts_download("spruceT4P04E","EN",c(1000),"3day")
SPRUCE_T6ACO2_EN<-ts_download("spruceT6P08","EN",c(1000),"3day")
SPRUCE_T6ECO2_EN<-ts_download("spruceT6P16E","EN",c(1000),"3day")
SPRUCE_T9ACO2_EN<-ts_download("spruceT9P17","EN",c(1000,2000),"3day")
SPRUCE_T9ECO2_EN<-ts_download("spruceT9P10E","EN",c(1000,2000),"3day")
#----------------------------
##test for T9 treatments-->determine which data are better
#----------------------------
SPRUCE_T9ACO2_EN$date<-as.POSIXct(SPRUCE_T9ACO2_EN$date)
SPRUCE_T9ECO2_EN$date<-as.POSIXct(SPRUCE_T9ECO2_EN$date)
ggplot(data=SPRUCE_T9ACO2_EN,aes(x=date,y=gcc_90,col=RoiID))+
  geom_point() #data from ROI 1000 are smaller than ROI 2000
#
t1<-SPRUCE_T9ACO2_EN[SPRUCE_T9ACO2_EN$date>=as.POSIXct("2018-01-01")&SPRUCE_T9ACO2_EN$date<=as.POSIXct("2018-03-31")]
ggplot(data=t1,aes(x=date,y=gcc_90,col=RoiID))+
  geom_point()
#
t1_1000<-t1[RoiID=="1000",]
t1_2000<-t1[RoiID=="2000",]
lm_gcc90<-lm(t1_1000$gcc_90~t1_2000$gcc_90)
lm_rcc90<-lm(t1_1000$rcc_90~t1_2000$rcc_90)

ggplot(data=SPRUCE_T9ECO2_EN,aes(x=date,y=gcc_90,col=RoiID))+
  geom_point() #data between ROI 1000 and ROI2000 are consistent
  #-->data before 2018 using the ROI 1000,the rest of period using data from ROI2000
#----------------------------
##adjust the data in T9ACO2 and T9ECO2
#----------------------------
#I.for T9ACO2-->(,2018-03-31],using the data from ROI1000
#[2018-04-01,...]-->using the data from ROI2000-->gcc_90 and rcc_90 adjusted according to the test above:
SPRUCE_T9ACO2_EN$date<-as.POSIXct(SPRUCE_T9ACO2_EN$date)
data_seg1<-SPRUCE_T9ACO2_EN[SPRUCE_T9ACO2_EN$RoiID==1000 & SPRUCE_T9ACO2_EN$date<=as.POSIXct("2018-03-31")]
data_seg2<-SPRUCE_T9ACO2_EN[SPRUCE_T9ACO2_EN$RoiID==2000 & SPRUCE_T9ACO2_EN$date>as.POSIXct("2018-03-31")]
#adjust gcc_90 and rcc_90:
data_seg2$gcc_90<-coef(lm_gcc90)[1]+data_seg2$gcc_90*coef(lm_gcc90)[2]
data_seg2$rcc_90<-coef(lm_rcc90)[1]+data_seg2$rcc_90*coef(lm_rcc90)[2]
#
data_merge<-rbind(data_seg1,data_seg2)
ggplot(data=data_merge,aes(x=date,y=rcc_90,col=RoiID))+
  geom_point()

ggplot(data=SPRUCE_T9ACO2_EN,aes(x=date,y=rcc_90,col=RoiID))+
  geom_point() 
#it seems the Camera after 2018 has some problem(degaraded?)-->hence do not use the data after 2018-03-31
data_new<-data_seg1
SPRUCE_T9ACO2_EN<-data_new

#I.for T9ECO2-->(,2018-03-31],using the data from ROI1000
#[2018-04-01,...]-->using the data from ROI2000
data_seg1<-SPRUCE_T9ECO2_EN[SPRUCE_T9ECO2_EN$RoiID==1000 & SPRUCE_T9ECO2_EN$date<=as.POSIXct("2018-03-31")]
data_seg2<-SPRUCE_T9ECO2_EN[SPRUCE_T9ECO2_EN$RoiID==2000 & SPRUCE_T9ECO2_EN$date>as.POSIXct("2018-03-31")]
data_merge<-rbind(data_seg1,data_seg2)
ggplot(data=data_merge,aes(x=date,y=gcc_90,col=RoiID))+
  geom_point()
SPRUCE_T9ECO2_EN<-data_merge

########
#for the Shurb
########
#need to confirm the PFT in each treatment and select the ENF ROIs for analysis:
#T0P06SH, T0P19ESH-->SH_1000
#T2P20SH, T2P11ESH-->SH_1000
#T4P13SH, T4P04ESH-->SH_1000
#T6P08SH, T6P16ESH-->SH_1000
#T9P17SH, T9P10ESH-->SH_1000
SPRUCE_T0ACO2_SH<-ts_download("spruceT0P06","SH",c(1000),"3day")
SPRUCE_T0ECO2_SH<-ts_download("spruceT0P19E","SH",c(1000),"3day")
SPRUCE_T2ACO2_SH<-ts_download("spruceT2P20","SH",c(1000),"3day")
SPRUCE_T2ECO2_SH<-ts_download("spruceT2P11E","SH",c(1000),"3day")
SPRUCE_T4ACO2_SH<-ts_download("spruceT4P13","SH",c(1000),"3day")
SPRUCE_T4ECO2_SH<-ts_download("spruceT4P04E","SH",c(1000),"3day")
SPRUCE_T6ACO2_SH<-ts_download("spruceT6P08","SH",c(1000),"3day")
SPRUCE_T6ECO2_SH<-ts_download("spruceT6P16E","SH",c(1000),"3day")
SPRUCE_T9ACO2_SH<-ts_download("spruceT9P17","SH",c(1000),"3day")
SPRUCE_T9ECO2_SH<-ts_download("spruceT9P10E","SH",c(1000),"3day")

#----------------------
#(4)save the data
#----------------------
####EN
SPRUCE_EN<-list(SPRUCE_T0ACO2_EN,SPRUCE_T0ECO2_EN,SPRUCE_T2ACO2_EN,SPRUCE_T2ECO2_EN,
                SPRUCE_T4ACO2_EN,SPRUCE_T4ECO2_EN,SPRUCE_T6ACO2_EN,SPRUCE_T6ECO2_EN,
                SPRUCE_T9ACO2_EN,SPRUCE_T9ECO2_EN)
treatments<-c("T0ACO2","T0ECO2","T2ACO2","T2ECO2",
              "T4ACO2","T4ECO2","T6ACO2","T6ECO2","T9ACO2","T9ECO2")
SPRUCE_VIs_EN<-c()
for(i in 1: length(SPRUCE_EN)){
  if(i==1){
    temp<-SPRUCE_EN[[i]]
    temp$date<-as.POSIXct(temp$date)
    N_length<-nrow(temp)
    temp$treatment<-rep(treatments[i],N_length)
    SPRUCE_VIs_EN<-temp
  }
  if(i>1){
    temp<-SPRUCE_EN[[i]]
    temp$date<-as.POSIXct(temp$date)
    N_length<-nrow(temp)
    temp$treatment<-rep(treatments[i],N_length)
    SPRUCE_VIs_EN<-rbind(SPRUCE_VIs_EN,temp)
  }
  rm(temp)
}
# write.csv(SPRUCE_VIs_EN,file = paste0(save.path,"Cam_VIs_EN",".csv"))
save(SPRUCE_VIs_EN,file = paste0(save.path,"Cam_VIs_EN",".RDA"))

####SH
SPRUCE_SH<-list(SPRUCE_T0ACO2_SH,SPRUCE_T0ECO2_SH,SPRUCE_T2ACO2_SH,SPRUCE_T2ECO2_SH,
                SPRUCE_T4ACO2_SH,SPRUCE_T4ECO2_SH,SPRUCE_T6ACO2_SH,SPRUCE_T6ECO2_SH,
                SPRUCE_T9ACO2_SH,SPRUCE_T9ECO2_SH)
treatments<-c("T0ACO2","T0ECO2","T2ACO2","T2ECO2",
              "T4ACO2","T4ECO2","T6ACO2","T6ECO2","T9ACO2","T9ECO2")
SPRUCE_VIs_SH<-c()
for(i in 1: length(SPRUCE_SH)){
  if(i==1){
    temp<-SPRUCE_SH[[i]]
    N_length<-nrow(temp)
    temp$treatment<-rep(treatments[i],N_length)
    SPRUCE_VIs_SH<-temp
  }
  if(i>1){
    temp<-SPRUCE_SH[[i]]
    N_length<-nrow(temp)
    temp$treatment<-rep(treatments[i],N_length)
    SPRUCE_VIs_SH<-rbind(SPRUCE_VIs_SH,temp)
  }
  rm(temp)
}
# write.csv(SPRUCE_VIs_EN,file = paste0(save.path,"Cam_VIs_EN",".csv"))
save(SPRUCE_VIs_SH,file = paste0(save.path,"Cam_VIs_SH",".RDA"))
