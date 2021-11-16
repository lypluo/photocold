#----------------------------------------------------------------------
# Aim:label the sites in the on the global maps
#----------------------------------------------------------------------
#load the datasets
#----------------------------------------------------------------------
library(ggplot2)
library(ggmap)
library(rgdal)
library(rgeos)
library(maptools)
library(dplyr)
library(tidyr)
library(tmap)
library(mapdata)
library(rworldmap)
library(rworldxtra)
library(colorRamps)
library(graphics)
library(jpeg)
#(1)load the map 
data(coastsCoarse)
#prepration for map
# newmap <- getMap(resolution = "high")[getMap()$ADMIN!='Antarctica',]
newmap <- getMap(resolution = "high")
#pal_color <- colorRampPalette('skyblue')
#set the map area() --> temperate and tropical region,but remove the Antarctica region 
# plot(newmap,
#      ylim = c(22.3, 90),
#      asp = 2,col="gray",fill=T
# )

#add coordinates of sites
#I.load the EC sites Beni provide to me
library("readxl")
coord.path<-"D:/CES/Data_for_use/"
coord_Beni<-read_excel(paste0(coord.path,"Info_Table_about_Photocold_project.xlsx"),sheet = "ECsites_withPhenoCam")
#at the end, two sites do not used for analysis because of the data avaiablity:
rm_sites<-c("US-Wi3","RU-Ha1")
pos_rm<-match(rm_sites,coord_Beni$SiteName)
coord_Beni<-coord_Beni[-pos_rm,]

#II.load the EC sites beyond Bein provide to me (I tidy again)
load.path<-"D:/CES/Data_for_use/Fluxnet_Data/"
coord_Beyond<-read.csv(paste0(load.path,"fluxnet2015_sites_sel_tidy_beyondBeni.csv"))
#only merge the sitename, lon,lat,classid(PFT),koeppen_code(Clim.) with two datasets
sel_vars<-c("sitename","lon","lat","IGBP","Koeppen_code")
coord_Beyond<-coord_Beyond[,sel_vars]
names(coord_Beyond)<-c("SiteName","Long.","Lat.","PFT","Clim.")
#
coord_Beni_sel<-as.data.frame(coord_Beni)[,c("SiteName","Long.","Lat.","PFT","Clim.")]
#
coord_Beyond$flag<-rep("Beyond",nrow(coord_Beyond))
coord_Beni_sel$flag<-rep("Beni",nrow(coord_Beni_sel))
coord_merge<-rbind(coord_Beni_sel,coord_Beyond)

####set the factors
#for sites from Beni
coord_Beni$Clim.<-factor(coord_Beni$Clim.,levels = c("Cfb","Dfb","Dfc"))
coord_Beni$PFT<-factor(coord_Beni$PFT,levels = c("GRA","DBF","MF","ENF"))
#for merged sites
coord_merge$Clim.<-factor(coord_merge$Clim.,levels = c("Cfa","Cfb","Dfa","Dfb","Dfc"))
coord_merge$PFT<-factor(coord_merge$PFT,levels = c("GRA","DBF","MF","ENF"))
coord_merge$flag<-factor(coord_merge$flag,levels = c("Beni","Beyond"))

#III.selected the final sites that used in the analysis:
load.path<-"D:/CES/Data_for_use/Merge_Data/ECflux_and_PhenoCam_site_info/"
sites_final<-read.csv(paste0(load.path,"ECflux_and_PhenoCam_site_info_add_manually_final.csv"))
pos_final<-match(sites_final$SiteName,coord_merge$SiteName)
coord_final<-coord_merge[pos_final,]

#---------------------------------------------
#(2)plotting
#---------------------------------------------
library(RColorBrewer)
library(grDevices)
############
# map theme
############
#can refer:http://www.sthda.com/english/wiki/ggplot2-themes-and-background-colors-the-3-elements
theme_map <- 
  # theme_dark() +    # theme_minimal()
  theme(
    #add by YP:
    # panel.background = element_rect(fill = "gray60",
    #                                 colour = "gray60",
    #                                 size = 0.5, linetype = "solid"),
    # #add by YP:
    # plot.background = element_rect(fill="gray60"),
    #
    plot.title = element_text(hjust = 0, face="bold", size = 18),
    
    # legend.position = "right", # c(0.07, 0.35), #"left"
    # legend.key.size = unit(c(5, 1), "mm"),
    legend.title=element_text(size=12),
    legend.text=element_text(size=10),
    
    # axis.line = element_blank(),
    # axis.text = element_blank(),
    # axis.title = element_blank(),
    
    # panel.grid.major = element_line(colour="black",size = 0.5,linetype = "solid"),
    panel.grid.minor = element_blank(),
    # plot.margin = unit( c(0, 0, 0, 5) , "mm")
  )

# define labels
lat.labels <- seq(30, 90, 30)
lat.short  <- seq(30, 90, 10)
lon.labels <- seq(-180, 180, 60)
lon.short  <- seq(-180, 180, 10)

a <- sapply( lat.labels, function(x) if (x>0) {parse(text = paste0(x, "*degree ~ N"))} else if (x==0) {parse(text = paste0(x, "*degree"))} else {parse(text = paste0(-x, "*degree ~ S"))} )
b <- sapply( lon.labels, function(x) if (x>0) {parse(text = paste0(x, "*degree ~ E"))} else if (x==0) {parse(text = paste0(x, "*degree"))} else {parse(text = paste0(-x, "*degree ~ W"))} )
#---------------------------------------------
# 1. Create ggplot object
#---------------------------------------------
lonmin=-180
lonmax=180
latmin=30
latmax=90
#group=group-->results in the wrong map background:ask Beni's advices
#something need to be paid attention-->to make sure the plot looks right
#-->should set latmin=-90; latmax=90
#-->and also leave some place for latitude and longtitude-->set the limits in scale_x/y_continous adding or minus some numbers
gg <- ggplot() +
  theme_map+
  # background countries
  # geom_polygon(data=newmap, aes(long, lat, group=group), color=NA, fill='grey75') +
  # Coastline
  geom_path(data=coastsCoarse, aes(long, lat, group=group), color='black',size=1.02) +
  # 
  scale_x_continuous(expand = c(0,0), limits = c(-1+lonmin,lonmax+1), breaks = lon.labels, labels = b) +
  scale_y_continuous(expand = c(0,0), limits = c(-1+latmin,latmax+1), breaks = lat.labels, labels = a) +
  labs( x = "longtitude", y = "latitude")
#---------------------------------------------
# 2. add sites information
#---------------------------------------------
#----------
#plot1:the analyzed sites-->the sites Beni sent to me
#----------
library(ggrepel)  #add the labels
gg_Beni<-gg+
  geom_point(data=coord_Beni,aes(x=Long.,y=Lat.,col=Clim.,shape=PFT),size=3)+
  geom_label_repel(data=coord_Beni,aes(x=Long.,y=Lat.,label = SiteName,fill = Clim.),
                   color = 'black',max.overlaps = 50,label.size = 0.1,arrow = arrow(ends = "first",length = unit(0.05,"inch")),
                   size = 2.5) +
  theme(legend.position = "bottom")

#
save.path<-"C:/Users/yluo/Desktop/R_testcode/PhotoCold/Second_round_of_code/plot/maps/"
plot(gg_Beni)
ggsave(file=paste0(save.path,"sites_distribution_Beni_send.png"),gg_Beni,dev="png",width = 12,height=6)

#----------
#plot2:the analyzed sites-->all the potential sites
#----------
gg_merge<-gg+
  geom_point(data=coord_merge[coord_merge$flag=="Beni",],aes(x=Long.,y=Lat.,col="Beni"),size=2,pch=16)+
  geom_point(data=coord_merge[coord_merge$flag=="Beyond",],aes(x=Long.,y=Lat.,col="Beyond"),size=2,pch=16,)+
  geom_point(data=coord_final,aes(x=Long.,y=Lat.,col="Final"),size=4,pch=1)+
  # geom_label_repel(data=coord_merge,aes(x=Long.,y=Lat.,label = SiteName,fill = Clim.),
  #                  color = 'black',max.overlaps = 50,label.size = 0.1,arrow = arrow(ends = "first",length = unit(0.05,"inch")),
  #                  size = 2.5) +
  scale_color_manual("",values = c("Beni"="red","Beyond"="blue","Final"="green"))+
  theme(legend.position = "bottom")
#
save.path<-"C:/Users/yluo/Desktop/R_testcode/PhotoCold/Second_round_of_code/plot/maps/"
plot(gg_merge)
ggsave(file=paste0(save.path,"sites_distribution_all_potential.png"),gg_merge,dev="png",width = 12,height=6)

#temporaily-->all the potential analysis sites
pp_temp<-gg+
  geom_point(data=coord_merge,aes(x=Long.,y=Lat.),size=2,pch=16,col="blue")+
  geom_label_repel(data=coord_merge,aes(x=Long.,y=Lat.,label = SiteName,fill = Clim.),
                   color = 'black',max.overlaps = 50,label.size = 0.1,arrow = arrow(ends = "first",length = unit(0.05,"inch")),
                   size = 2.5) +
  # scale_color_manual("",values = c("Beni"="red","Beyond"="blue","Final"="green"))+
  theme(legend.position = "bottom")
plot(pp_temp)
ggsave(file=paste0(save.path,"sites_distribution_temporal2.png"),pp_temp,dev="png",width = 12,height=6)
