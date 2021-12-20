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

#(1)load the EC sites used from Fluxnet 2015
library("readxl")
sites.path<-"D:/data/photocold_project/sel_sites_info/Using_sites_in_Fluxnet2015/"
load(paste0(sites.path,"df_sites_sel.RDA"))
ori_coord_sites<-df_sites_sel
#at the end, seveals sites do not used for analysis because of the data availabilty 
#-->no complete spring 
#load the final sites name
load.path<-"D:/data/photocold_project/event_length/Using_sites_in_Fluxnet2015/"
load(paste0(load.path,"df_events_length.RDA"))
final.sites<-unique(df_events_all$sitename)
match.pos<-match(final.sites,ori_coord_sites$sitename)
final_coord_sites<-ori_coord_sites[match.pos,]

#(2)add the flag and set the factors
ori_coord_sites$flag<-rep("Ori-sites",nrow(ori_coord_sites))
final_coord_sites$flag<-rep("Final_sites",nrow(final_coord_sites))

####set the factors
#for sites from ori sites
ori_coord_sites$koeppen_code<-factor(ori_coord_sites$koeppen_code,levels = c("Cfa","Cfb","Dfb","Dfc"))
ori_coord_sites$PFT<-factor(ori_coord_sites$classid,levels = c("DBF","MF","ENF"))
# #for final sites
final_coord_sites$koeppen_code<-factor(final_coord_sites$koeppen_code,levels = c("Cfa","Cfb","Dfb","Dfc"))
final_coord_sites$PFT<-factor(final_coord_sites$classid,levels = c("DBF","MF","ENF"))

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
#plots:the analyzed sites-->all the sites from FLUXNET2015
#----------
library(ggrepel)  #add the labels
gg_merge_no_sitenames<-gg+
  geom_point(data=ori_coord_sites,aes(x=lon,y=lat,col="Original"),size=2,pch=16,)+
  geom_point(data=final_coord_sites,aes(x=lon,y=lat,col="Final"),size=4,pch=1)+
  # geom_label_repel(data=ori_coord_sites,aes(x=lon,y=lat,label = sitename,fill = koeppen_code),
  #                  color = 'black',max.overlaps = 50,label.size = 0.1,arrow = arrow(ends = "first",length = unit(0.05,"inch")),
  #                  size = 2.5) +
  scale_color_manual("",values = c("Original"="red","Final"="green"))+
  theme(legend.position = "bottom")

gg_merge_with_sitenames<-gg+
  geom_point(data=ori_coord_sites,aes(x=lon,y=lat,col="Original"),size=2,pch=16,)+
  geom_point(data=final_coord_sites,aes(x=lon,y=lat,col="Final"),size=4,pch=1)+
  geom_label_repel(data=ori_coord_sites,aes(x=lon,y=lat,label = sitename,fill = koeppen_code),
                   color = 'black',max.overlaps = 50,label.size = 0.1,arrow = arrow(ends = "first",length = unit(0.05,"inch")),
                   size = 2.5) +
  scale_color_manual("",values = c("Original"="red","Final"="green"))+
  theme(legend.position = "bottom")

#
save.path<-"D:/plots/photocold_project/Using_sites_in_Fluxnet2015/sites_map/"
plot(gg_merge_no_sitenames)
ggsave(file=paste0(save.path,"sites_distribution_no_sitenames.png"),gg_merge_no_sitenames,dev="png",width = 12,height=6)
plot(gg_merge_with_sitenames)
ggsave(file=paste0(save.path,"sites_distribution_with_sitenames.png"),gg_merge_with_sitenames,dev="png",width = 12,height=6)

