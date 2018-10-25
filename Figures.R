library(data.table)
library(seacarb)
library(ggplot2)
library(lubridate)
library(viridis)


setwd("C:/Mueller_Jens_Data/181023_Spring_Summer_2018/data/Finnmaid/Summarized_datasets")

df.area <- data.table(read.csv("df_mean_area_CT.csv"))
df.area$date <- ymd_hms(df.area$date)
df.area$day.date <- as.POSIXct(strptime(paste(2000,df.area$day), format = "%Y %j",tz="GMT"))
setorder(df.area, year, date)


df.dist <- data.table(read.csv("df_mean_distanceHEL.csv"))
df.dist$date <- ymd_hms(df.dist$date)
df.dist$day.date <- as.POSIXct(strptime(paste(2000,df.dist$day), format = "%Y %j",tz="GMT"))
setorder(df.dist, year, date)


df.spring <- data.table(read.csv("df_end_spring.csv"))
setorder(df.spring, year, -dist.Hel.int)


start <- as.POSIXct(strptime("2000/03/01", format = "%Y/%m/%d",tz="GMT"))
end   <- as.POSIXct(strptime("2000/08/31", format = "%Y/%m/%d",tz="GMT"))

shadesOfGreyRed <- c(rev(colorRampPalette(c("grey35", "grey75"))(15)), "red")



Fig <-
ggplot()+
  geom_hline(yintercept=100)+
  geom_path(data=df.area, aes(day.date, mean.pCO2, col=as.factor(year)))+
  scale_color_manual(values = shadesOfGreyRed, name="Year")+
  facet_wrap(~Area)+
  scale_x_datetime(date_labels = "%b", date_breaks = "month",
                   limits = c(start, end), name="Month")+
  labs(y=expression(pCO[2]~(µatm)))+
  coord_cartesian(ylim = c(0,500))

setwd("C:/Mueller_Jens_Data/181023_Spring_Summer_2018/plots")
tiff("MeanPCO2_FacetArea_AllYears.tiff", width = 200, height = 130, units = 'mm', res = 600, compression = 'lzw')
Fig
dev.off()
rm(Fig)


Fig <-
ggplot()+
  geom_hline(yintercept=100)+
  geom_path(data=df.area[Area == "4.EGS"], aes(day.date, mean.pCO2, col=as.factor(year)))+
  scale_color_manual(values = shadesOfGreyRed, name="Year")+
  facet_wrap(~Area)+
  scale_x_datetime(date_labels = "%b", date_breaks = "month",
                   limits = c(start, end), name="Month")+
  labs(y=expression(pCO[2]~(µatm)))+
  coord_cartesian(ylim = c(0,500))

setwd("C:/Mueller_Jens_Data/181023_Spring_Summer_2018/plots")
tiff("MeanPCO2_EGS_AllYears.tiff", width = 200, height = 130, units = 'mm', res = 600, compression = 'lzw')
Fig
dev.off()
rm(Fig)


Fig <-
ggplot()+
  geom_path(data=df.area, aes(day.date, mean.Tem, col=as.factor(year)))+
  scale_color_manual(values = shadesOfGreyRed, name="Year")+
  facet_wrap(~Area)+
  scale_x_datetime(date_labels = "%b", date_breaks = "month",
                   limits = c(start, end), name="Month")+
  labs(y="Tem (°C)")+
  coord_cartesian(ylim = c(0,26))

setwd("C:/Mueller_Jens_Data/181023_Spring_Summer_2018/plots")
tiff("MeanTEM_FacetArea_AllYears.tiff", width = 200, height = 130, units = 'mm', res = 600, compression = 'lzw')
Fig
dev.off()
rm(Fig)

Fig <-
ggplot()+
  geom_path(data=df.area[Area == "4.EGS"], aes(day.date, mean.Tem, col=as.factor(year)))+
  scale_color_manual(values = shadesOfGreyRed, name="Year")+
  facet_wrap(~Area)+
  scale_x_datetime(date_labels = "%b", date_breaks = "month",
                   limits = c(start, end), name="Month")+
  labs(y="Tem (°C)")+
  coord_cartesian(ylim = c(0,26))

setwd("C:/Mueller_Jens_Data/181023_Spring_Summer_2018/plots")
tiff("MeanTEM_EGS_AllYears.tiff", width = 200, height = 130, units = 'mm', res = 600, compression = 'lzw')
Fig
dev.off()
rm(Fig)


Fig <-
ggplot()+
  geom_path(data=df.area, aes(day.date, mean.CT, col=as.factor(year)))+
  scale_color_manual(values = shadesOfGreyRed, name="Year")+
  facet_wrap(~Area, scales = "free_y")+
  scale_x_datetime(date_labels = "%b", date_breaks = "month",
                   limits = c(start, end), name="Month")+
  labs(y=expression(C[T]~"*"~(µmol~kg^{-1})))+
  scale_y_continuous(breaks = seq(1000,2000,50))

setwd("C:/Mueller_Jens_Data/181023_Spring_Summer_2018/plots")
tiff("MeanCT_FacetArea_AllYears.tiff", width = 200, height = 130, units = 'mm', res = 600, compression = 'lzw')
Fig
dev.off()
rm(Fig)

Fig <-
ggplot()+
  geom_path(data=df.area[Area == "4.EGS"], aes(day.date, mean.CT, col=as.factor(year)))+
  scale_color_manual(values = shadesOfGreyRed, name="Year")+
  facet_wrap(~Area, scales = "free_y")+
  scale_x_datetime(date_labels = "%b", date_breaks = "month",
                   limits = c(start, end), name="Month")+
  labs(y=expression(C[T]~"*"~(µmol~kg^{-1})))+
  scale_y_continuous(breaks = seq(1000,2000,50))

setwd("C:/Mueller_Jens_Data/181023_Spring_Summer_2018/plots")
tiff("MeanCT_EGS_AllYears.tiff", width = 200, height = 130, units = 'mm', res = 600, compression = 'lzw')
Fig
dev.off()
rm(Fig)



Fig <-
ggplot()+
  geom_path(data=df.area, aes(mean.Tem, mean.CT, col=as.factor(year)))+
  scale_color_manual(values = shadesOfGreyRed, name="Year")+
  facet_wrap(~Area, scales = "free_y")+
  labs(y=expression(C[T]~"*"~(µmol~kg^{-1})), x="Tem (°C)")+
  scale_y_continuous(breaks = seq(1000,2000,50))

setwd("C:/Mueller_Jens_Data/181023_Spring_Summer_2018/plots")
tiff("MeanCT_vs_mean_Tem_FacetArea_AllYears.tiff", width = 200, height = 130, units = 'mm', res = 600, compression = 'lzw')
Fig
dev.off()
rm(Fig)


Fig <-
ggplot()+
  geom_path(data=df.area[Area == "4.EGS"], aes(mean.Tem, mean.CT, col=as.factor(year)))+
  scale_color_manual(values = shadesOfGreyRed, name="Year")+
  facet_wrap(~Area, scales = "free_y")+
  labs(y=expression(C[T]~"*"~(µmol~kg^{-1})), x="Tem (°C)")+
  scale_y_continuous(breaks = seq(1000,2000,50))

setwd("C:/Mueller_Jens_Data/181023_Spring_Summer_2018/plots")
tiff("MeanCT_vs_mean_Tem_EGS_AllYears.tiff", width = 200, height = 130, units = 'mm', res = 600, compression = 'lzw')
Fig
dev.off()
rm(Fig)



########################################################


df.dist <- df.dist[year > 2000]
df.dist <- df.dist[!is.na(year)]

Fig <-
ggplot()+
  geom_path(data=df.dist[route == "E"], aes(day.date, mean.pCO2, col=as.factor(year)))+
  scale_color_manual(values = shadesOfGreyRed, name="Year")+
  facet_wrap(~dist.Hel.int)+
  scale_x_datetime(date_labels = "%b", date_breaks = "month",
                   limits = c(start, end), name="Month")+
  labs(y=expression(pCO[2]~(µatm)))+
  coord_cartesian(ylim = c(0,500))

setwd("C:/Mueller_Jens_Data/181023_Spring_Summer_2018/plots")
tiff("MeanpCO2_FacetDist_AllYears.tiff", width = 250, height = 170, units = 'mm', res = 600, compression = 'lzw')
Fig
dev.off()
rm(Fig)



########################################################


shadesOfGreyRed <- c(rev(colorRampPalette(c("grey35", "grey75"))(
  length(levels(as.factor(df.spring$year)))-1
  )), "red")


Fig <-
  ggplot()+
  geom_ribbon(data=df.spring, aes(dist.Hel.int, ymin=min.pCO2, ymax=max.pCO2, fill=as.factor(year)), alpha = 0.1)+
  geom_path(data=df.spring, aes(dist.Hel.int, mean.pCO2, col=as.factor(year)))+
  scale_color_manual(values = shadesOfGreyRed, name="Year")+
  scale_fill_manual(values = shadesOfGreyRed, name="Year")+
  labs(y=expression(pCO[2]~(µatm)), x="Distance from Helsinki (km)")+
  coord_cartesian(ylim = c(0,500))

setwd("C:/Mueller_Jens_Data/181023_Spring_Summer_2018/plots")
tiff("pCO2_vs_Dist_AllYears.tiff", width = 200, height = 130, units = 'mm', res = 600, compression = 'lzw')
Fig
dev.off()
rm(Fig)


Fig <-
  ggplot()+
  geom_ribbon(data=df.spring, aes(dist.Hel.int, ymin=min.Tem, ymax=max.Tem, fill=as.factor(year)), alpha = 0.1)+
  geom_path(data=df.spring, aes(dist.Hel.int, mean.Tem, col=as.factor(year)))+
  scale_color_manual(values = shadesOfGreyRed, name="Year")+
  scale_fill_manual(values = shadesOfGreyRed, name="Year")+
  labs(y=expression(pCO[2]~(µatm)), x="Distance from Helsinki (km)")

setwd("C:/Mueller_Jens_Data/181023_Spring_Summer_2018/plots")
tiff("Tem_vs_Dist_AllYears.tiff", width = 200, height = 130, units = 'mm', res = 600, compression = 'lzw')
Fig
dev.off()
rm(Fig)

