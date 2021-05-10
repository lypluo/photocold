##################################
####Aim:check the original SIF data
#################################
library(lubridate)

#load the sif data provided by David
load.path<-"C:/Users/yluo/Desktop/Oak_trees/Data_for_use/SIF_data/ori_data/"
load(paste0(load.path,"sif_fluxes_and_meteo.RDA"))
###
df_SIFandMeteo<-sif_fluxes_and_meteo
###checking some variables
#Temp-->temp2 is the temperature outside the flox box
plot(df_SIFandMeteo$`datetime [UTC]`,df_SIFandMeteo$`temp1 [C]`)
plot(df_SIFandMeteo$`datetime [UTC]`,df_SIFandMeteo$`temp2 [C]`)
#Humidity-->both from flox(one inside one outside), but looks strange to me
plot(df_SIFandMeteo$`datetime [UTC]`,df_SIFandMeteo$`Humidity 1 [%]`)
plot(df_SIFandMeteo$`datetime [UTC]`,df_SIFandMeteo$`Humidity 2 [%]`)
#raidance and reflectance
#radiance 
plot(df_SIFandMeteo$`datetime [UTC]`,df_SIFandMeteo$`Incoming at 750nm [W m-2nm-1sr-1]`)
points(df_SIFandMeteo$`datetime [UTC]`,df_SIFandMeteo$`Reflected 760 [W m-2nm-1sr-1]`,col="red")
points(df_SIFandMeteo$`datetime [UTC]`,df_SIFandMeteo$`Reflected 687 [W m-2nm-1sr-1]`,col="blue")
#reflectance
plot(df_SIFandMeteo$`datetime [UTC]`,df_SIFandMeteo$`Reflectance 750 [-]`)
points(df_SIFandMeteo$`datetime [UTC]`,df_SIFandMeteo$`Reflected 760 [W m-2nm-1sr-1]`/df_SIFandMeteo$`Incoming at 750nm [W m-2nm-1sr-1]`,col="red")
#E_stablity-->Percentage difference between WR1 and WR2->normally should smaller than 0.5%
plot(df_SIFandMeteo$`datetime [UTC]`,df_SIFandMeteo$`E_stability [%]`,ylim=c(0,1000))
abline(h=0.5,col='blue',lty=2)
#SIF:
#O2A band:760nm 
plot(df_SIFandMeteo$`datetime [UTC]`,df_SIFandMeteo$`SIF_A_ifld [mW m-2nm-1sr-1]`,ylim=c(-10,20))
#O2B band:687nm
points(df_SIFandMeteo$`datetime [UTC]`,df_SIFandMeteo$`SIF_B_ifld [mW m-2nm-1sr-1]`,ylim=c(-10,20),col="red")

#PAR
#unit:PARd->W m-2 EPAR->W m-2 sr-1,avareged incoming radiance in PRA range(averaged 400-700) 
#LPAR->W m-2 sr-1,avareged reflected radiance in PRA range(averaged 400-700) 
#
plot(df_SIFandMeteo$`datetime [UTC]`,df_SIFandMeteo$PARd,xlab="",ylab=expression("PARd (W m"^-2*")"))

save.path<-"C:/Users/yluo/Desktop/Oak_trees/plots/SIF/check_ori_SIF_data/"
pdf(file=paste0(save.path,"PAR_related.pdf"))
par(mfrow=c(2,1))
plot(df_SIFandMeteo$`datetime [UTC]`,df_SIFandMeteo$EPAR,col="black",xlab="",ylab=expression("EPAR /LPAR (W m"^-2*"sr"^-1*")"))
points(df_SIFandMeteo$`datetime [UTC]`,df_SIFandMeteo$LPAR,col="blue",xlab="",ylab="")
legend("topright",bty='n',legend = c("EPAR","LPAR"),col=c("black","blue"),pch=1)
plot(df_SIFandMeteo$`datetime [UTC]`,df_SIFandMeteo$PARd/c(300*pi),col="red",xlab = "",ylab=expression("PAd/c(300*pi) (W m"^-2*"sr"^-1*")"))
dev.off()

#SIF--O2A band:760nm, O2B band:687nm
pdf(file=paste0(save.path,"Ori_SIF760_680.pdf"))
plot(df_SIFandMeteo$`datetime [UTC]`,df_SIFandMeteo$`SIF_A_ifld [mW m-2nm-1sr-1]`,ylim=c(-1,10),col="black",xlab="",ylab=expression("SIF (mW m"^-2*"nm"^-1*"sr"^-1*")"),xaxt="n")
axis(1,at=pretty(df_SIFandMeteo$`datetime [UTC]`,10), labels = pretty(df_SIFandMeteo$`datetime [UTC]`,10),cex.axis=1.5)
points(df_SIFandMeteo$`datetime [UTC]`,df_SIFandMeteo$`SIF_B_ifld [mW m-2nm-1sr-1]`,ylim=c(-1,10),col="red",xlab="")
legend("topleft",col=c("black","red"),legend = c("SIF760","SIF680"),bty="n")
dev.off()
