library(data.table)
library(seacarb)
library(ggplot2)
library(lubridate)
library(viridis)


setwd("C:/Mueller_Jens_Data/181023_Spring_Summer_2018/data/Finnmaid/Summarized_datasets")

df.area <- data.table(read.csv("df_mean_area_CT.csv"))
df.area$date <- ymd_hms(df.area$date)
df.area$day.date <- as.POSIXct(strptime(paste(2000,df.area$day), format = "%Y %j",tz="GMT"))
setorder(df.area, Area, year, date)


df.dist <- data.table(read.csv("df_mean_distanceHEL.csv"))
df.dist$date <- ymd_hms(df.dist$date)
df.dist$day.date <- as.POSIXct(strptime(paste(2000,df.dist$day), format = "%Y %j",tz="GMT"))
setorder(df.dist, dist.Hel.int, year, date)


start <- as.POSIXct(strptime("2000/03/01", format = "%Y/%m/%d",tz="GMT"))
end   <- as.POSIXct(strptime("2000/08/31", format = "%Y/%m/%d",tz="GMT"))

shadesOfGreyRed <- c(rev(colorRampPalette(c("grey35", "grey75"))(15)), "red")



Fig <-
ggplot()+
  geom_hline(yintercept=100)+
  geom_point(data=df.area, aes(day.date, mean.pCO2, col=as.factor(year)))+
  geom_path(data=df.area, aes(day.date, mean.pCO2, col=as.factor(year)))+
  scale_color_manual(values = shadesOfGreyRed, name="Year")+
  facet_wrap(~Area)+
  scale_x_datetime(date_labels = "%b", date_breaks = "month",
                   limits = c(start, end), name="Month")+
  labs(y=expression(pCO[2]~(µatm)))+
  coord_cartesian(ylim = c(0,500))

setwd("C:/Mueller_Jens_Data/181023_Spring_Summer_2018/plots")
tiff("MeanPCO2_FacetArea_AllYears.tiff", width = 300, height = 200, units = 'mm', res = 600, compression = 'lzw')
Fig
dev.off()
rm(Fig)


Fig <-
ggplot()+
  geom_point(data=df.area, aes(day.date, mean.Tem, col=as.factor(year)))+
  geom_path(data=df.area, aes(day.date, mean.Tem, col=as.factor(year)))+
  scale_color_manual(values = shadesOfGreyRed, name="Year")+
  facet_wrap(~Area)+
  scale_x_datetime(date_labels = "%b", date_breaks = "month",
                   limits = c(start, end), name="Month")+
  labs(y="Tem (°C)")+
  coord_cartesian(ylim = c(0,26))

setwd("C:/Mueller_Jens_Data/181023_Spring_Summer_2018/plots")
tiff("MeanTEM_FacetArea_AllYears.tiff", width = 300, height = 200, units = 'mm', res = 600, compression = 'lzw')
Fig
dev.off()
rm(Fig)


Fig <-
ggplot()+
  geom_point(data=df.area, aes(day.date, mean.CT, col=as.factor(year)))+
  geom_path(data=df.area, aes(day.date, mean.CT, col=as.factor(year)))+
  scale_color_manual(values = shadesOfGreyRed, name="Year")+
  facet_wrap(~Area, scales = "free_y")+
  scale_x_datetime(date_labels = "%b", date_breaks = "month",
                   limits = c(start, end), name="Month")+
  labs(y=expression(C[T]~"*"~(µmol~kg^{-1})))+
  scale_y_continuous(breaks = seq(1000,2000,50))

setwd("C:/Mueller_Jens_Data/181023_Spring_Summer_2018/plots")
tiff("MeanCT_FacetArea_AllYears.tiff", width = 300, height = 200, units = 'mm', res = 600, compression = 'lzw')
Fig
dev.off()
rm(Fig)



Fig <-
ggplot()+
  geom_point(data=df.area, aes(mean.Tem, mean.CT, col=as.factor(year)))+
  geom_path(data=df.area, aes(mean.Tem, mean.CT, col=as.factor(year)))+
  scale_color_manual(values = shadesOfGreyRed, name="Year")+
  facet_wrap(~Area, scales = "free_y")+
  labs(y=expression(C[T]~"*"~(µmol~kg^{-1})), x="Tem (°C)")+
  scale_y_continuous(breaks = seq(1000,2000,50))

setwd("C:/Mueller_Jens_Data/181023_Spring_Summer_2018/plots")
tiff("MeanCT_vs_mean_Tem_AllYears.tiff", width = 300, height = 200, units = 'mm', res = 600, compression = 'lzw')
Fig
dev.off()
rm(Fig)



########################################################


df.dist <- df.dist[year > 2000]
df.dist <- df.dist[!is.na(year)]

Fig <-
ggplot()+
  geom_point(data=df.dist, aes(day.date, mean.pCO2, col=as.factor(year)))+
  geom_path(data=df.dist, aes(day.date, mean.pCO2, col=as.factor(year)))+
  scale_color_manual(values = shadesOfGreyRed, name="Year")+
  facet_wrap(~dist.Hel.int)+
  scale_x_datetime(date_labels = "%b", date_breaks = "month",
                   limits = c(start, end), name="Month")+
  labs(y=expression(pCO[2]~(µatm)))+
  coord_cartesian(ylim = c(0,500))

setwd("C:/Mueller_Jens_Data/181023_Spring_Summer_2018/plots")
tiff("MeanpCO2_FacetDist_AllYears.tiff", width = 300, height = 200, units = 'mm', res = 600, compression = 'lzw')
Fig
dev.off()
rm(Fig)


# Fig <-
# ggplot()+
#   geom_point(data=df.dist, aes(day.date, mean.CT, col=as.factor(year)))+
#   geom_path(data=df.dist, aes(day.date, mean.CT, col=as.factor(year)))+
#   scale_color_manual(values = shadesOfGreyRed, name="Year")+
#   facet_wrap(~dist.Hel.int)+
#   scale_x_datetime(date_labels = "%b", date_breaks = "month",
#                    limits = c(start, end), name="Month")+
#   labs(y=expression(C[T]~"*"~(µmol~kg^{-1})))+
#   scale_y_continuous(breaks = seq(1000,2000,50))
# 
# setwd("C:/Mueller_Jens_Data/181023_Spring_Summer_2018/plots")
# tiff("MeanCT_FacetDist_AllYears.tiff", width = 300, height = 200, units = 'mm', res = 600, compression = 'lzw')
# Fig
# dev.off()
# rm(Fig)

