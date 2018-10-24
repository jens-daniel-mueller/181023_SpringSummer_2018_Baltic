library(data.table)
library(seacarb)
library(ggplot2)
library(dygraphs)
library(xts)
library(lubridate)
library(marelac)
library(geosphere)


setwd("C:/Mueller_Jens_Data/181023_Spring_Summer_2018/data/Finnmaid/Summarized_datasets")

df <- data.table(read.csv("dfall.csv"))
df$date <- ymd_hms(df$date)


#### subset data set by areas defined in Schneider and Müller (2018)


df$Area <- with(df,
    ifelse(Lon>12 & Lon<12.6, "1.MEB",
    ifelse(Lon>13.1 & Lon<14.3, "2.ARK",
    ifelse(Lat>57.5 & Lat<58.5 & route %in% c("E", "G"), "4.EGS",
    ifelse(Lat>56.8 & Lat<57.5 & route=="W", "3.WGS",
    ifelse(Lat>58.5 & Lat<59 & Lon>20, "5.NGS",
    ifelse(Lon>22 & Lon<24, "6.WGF",
    ifelse(Lon>24 & Lon<24.5, "7.HGF", "NaN"))))))))

df.area <- df[,.(
  date = mean(date),
  mean.Sal = mean(Sal, na.rm = TRUE),
  SD.Sal = sd(Sal, na.rm = TRUE),
  mean.Tem = mean(Tem, na.rm = TRUE),
  SD.Tem = sd(Tem, na.rm = TRUE),
  mean.pCO2 = mean(pCO2, na.rm = TRUE),
  SD.pCO2 = sd(pCO2, na.rm = TRUE),
  max.pCO2 = max(pCO2),
  min.pCO2 = min(pCO2),
  mean.cO2 = mean(cO2, na.rm = TRUE),
  SD.cO2 = sd(cO2, na.rm = TRUE),
  mean.patm = mean(patm, na.rm = TRUE),
  nr=.N),
  by=.(Area, ID)] [Area != "NaN"]

 # ggplot(df.area[Area=="5.NGS"], aes(date, mean.pCO2))+
 #   geom_point()+
 #   ylim(0, 400)

df.area$year <- year(df.area$date)
df.area$day <- yday(df.area$date)

setwd("C:/Mueller_Jens_Data/181023_Spring_Summer_2018/data/Finnmaid/Summarized_datasets")
write.csv(df.area, "df_mean_area.csv", row.names = FALSE)

rm(df.area)
df$Area <- NULL


#### subset data set by distance intervals starting from Helsinki

Hel <- c(24.945831, 60.192059)

df$dist.Hel <- distGeo(cbind(df$Lon, df$Lat), Hel)/1e3
df$dist.Hel.int <- cut(df$dist.Hel, seq(0, 1200, 50))

df.dist <- df[,.(
  date = mean(date),
  mean.Sal = mean(Sal, na.rm = TRUE),
  SD.Sal = sd(Sal, na.rm = TRUE),
  mean.Tem = mean(Tem, na.rm = TRUE),
  SD.Tem = sd(Tem, na.rm = TRUE),
  mean.pCO2 = mean(pCO2, na.rm = TRUE),
  SD.pCO2 = sd(pCO2, na.rm = TRUE),
  max.pCO2 = max(pCO2),
  min.pCO2 = min(pCO2),
  mean.cO2 = mean(cO2, na.rm = TRUE),
  SD.cO2 = sd(cO2, na.rm = TRUE),
  mean.patm = mean(patm, na.rm = TRUE),
  nr=.N),
  by=.(dist.Hel.int, route, ID)]


df.dist$year <- year(df.dist$date)
df.dist$day <- yday(df.dist$date)

setwd("C:/Mueller_Jens_Data/181023_Spring_Summer_2018/data/Finnmaid/Summarized_datasets")
write.csv(df.dist, "df_mean_distanceHEL.csv", row.names = FALSE)

rm(df.dist, Hel, df)
