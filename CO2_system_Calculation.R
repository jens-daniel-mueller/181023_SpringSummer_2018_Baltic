library(data.table)
library(seacarb)
library(ggplot2)
library(lubridate)
# library(dygraphs)
# library(xts)
# library(marelac)


setwd("C:/Mueller_Jens_Data/181023_Spring_Summer_2018/data/Finnmaid/Summarized_datasets")

df.area <- data.table(read.csv("df_mean_area.csv"))
df.area$date <- ymd_hms(df.area$date)

df.dist <- data.table(read.csv("df_mean_distanceHEL.csv"))
df.dist$date <- ymd_hms(df.dist$date)


#### CT calculation

df.area$Sal.fix <- as.numeric(
  with(df.area,
  ifelse(Area == "1.MEB", 9.6,
  ifelse(Area == "2.ARK", 7.6,
  ifelse(Area %in% c("4.EGS", "3.WGS", "5.NGS"), 6.7,
  ifelse(Area == "6.WGF", 6.2,
  ifelse(Area == "7.HGF", 5.7, "NaN")))))))

df.area$AT.fix <- as.numeric(
  with(df.area,
  ifelse(Area == "1.MEB", 1741e-6,
  ifelse(Area == "2.ARK", 1694e-6,
  ifelse(Area %in% c("4.EGS", "3.WGS", "5.NGS"), 1665e-6,
  ifelse(Area == "6.WGF", 1580e-6,
  ifelse(Area == "7.HGF", 1513e-6, "NaN")))))))


df.area$mean.CT <- as.numeric(NA)
df.area[!is.na(mean.Tem)]$mean.CT <- 1e6*with(df.area[!is.na(mean.Tem)], carb(flag=24, var1=mean.pCO2, var2=AT.fix, S=Sal.fix, T=mean.Tem, k1k2="m10", kf="dg", pHscale="T"))[,16]

df.area$AT.fix <- df.area$AT.fix*1e6


df.area$eq.CT <- as.numeric(NA)
df.area[!is.na(mean.Tem)]$eq.CT <- 1e6*with(df.area[!is.na(mean.Tem)], carb(flag=24, var1=400, var2=AT.fix*1e-6, S=Sal.fix, T=mean.Tem, k1k2="m10", kf="dg", pHscale="T"))[,16]


# ggplot()+
#   geom_point(data=df.area[year>2010], aes(date, mean.cO2))+
#   facet_wrap(~Area)
# 
# df.area[mean.cO2>520]$mean.cO2 <- NA
# df.area[mean.cO2<200]$mean.cO2 <- NA



write.csv(df.area, "df_mean_area_CT.csv", row.names = FALSE)

# 
# 
# 
# 
# #### interpolation 2009
# 
# 
# df09 <- df[date > as.POSIXct(strptime("2008-10-01 12:00:00",
#                                       format="%Y-%m-%d %H:%M:%S", tz="GMT")) &
#              date < as.POSIXct(strptime("2010-03-10 12:00:00",
#                                         format="%Y-%m-%d %H:%M:%S", tz="GMT"))]
# 
# 
# # ggplot()+
# #   geom_point(data=df09, aes(date, mean.pCO2))+
# #   facet_wrap(~Area)+
# #   geom_vline(xintercept = c(as.numeric(as.POSIXct(strptime("2009-01-01 00:00:00", 
# #                                                            format="%Y-%m-%d %H:%M:%S", tz="GMT"))),
# #                             as.numeric(as.POSIXct(strptime("2009-12-31 24:00:00", 
# #                                                            format="%Y-%m-%d %H:%M:%S", tz="GMT")))
# #   ))
# 
# for (i in levels(df09$Area)){
# 
# pCO2 <- approxfun(df09[Area == i]$date, df09[Area == i]$mean.pCO2)
# CT <- approxfun(df09[Area == i]$date, df09[Area == i]$mean.CT)
# #cO2 <- approxfun(df09[Area == i]$date, df09[Area == i]$mean.cO2)
# Sal <- approxfun(df09[Area == i]$date, df09[Area == i]$mean.Sal)
# Tem <- approxfun(df09[Area == i]$date, df09[Area == i]$mean.Tem)
# patm <- approxfun(df09[Area == i]$date, df09[Area == i]$mean.patm)
# 
# interval <- as.POSIXct(strptime("2009-01-01 12:00:00", format="%Y-%m-%d %H:%M:%S", tz="GMT"))+
#   (days(seq(0,364,1)))
# 
# temp <- cbind(data.table(interval),
#                 data.table(i),
#                 data.table(pCO2(interval)),
#                 data.table(CT(interval)),
#                 #data.table(cO2(interval)),
#                 data.table(Sal(interval)),
#                 data.table(Tem(interval)),
#                 data.table(patm(interval)))
# names(temp) <- c("date", "Area", "int.pCO2", "int.CT", "int.Sal", "int.Tem", "int.patm")
# 
# if (exists("ts.int.09")){
#   ts.int.09 <- rbind (ts.int.09, temp)
# } else{ts.int.09 <- temp}
# 
# rm(temp, interval, pCO2, CT, Sal, Tem, patm)
# 
# }
# 
# 
# setwd("C:/Mueller_Jens_Data/150824_Biogeochemistry_Baltic/Figures/5_Seasons/plots/interpolation")
# 
# 
# reg <- levels(df09$Area)[1]
# 
# for (var in c("pCO2", "CT", "Sal", "Tem", "patm"))
#   for (reg in levels(df09$Area))
# {
# mean.var <- paste("mean", var, sep = ".")
# int.var <- paste("int", var, sep = ".")
# #i<-i+1
# 
# fig <-
# ggplot()+
#   geom_point(data=df09[Area == reg], aes(date, get(mean.var), col="measured"), size=0.2)+
#   geom_line(data=df09[Area == reg], aes(date, get(mean.var), col="measured"), size=0.2)+
#   geom_point(data=ts.int.09[Area == reg], aes(date, get(int.var), col="interpolated"), size=0.2)+
#   scale_color_manual(values = c("black", "red"))+
#   geom_vline(xintercept = c(as.numeric(as.POSIXct(strptime("2009-01-01 00:00:00", 
#                                                            format="%Y-%m-%d %H:%M:%S", tz="GMT"))),
#                             as.numeric(as.POSIXct(strptime("2009-12-31 24:00:00", 
#                                                            format="%Y-%m-%d %H:%M:%S", tz="GMT")))
#   ))+
#   labs(y=var, title=reg)
# 
# tiff(paste("./interpolation_2009",var,reg,".tiff", sep="_"), width = 297, height = 150, units = 'mm', res = 600, compression = 'lzw')
# print(fig)
# dev.off()
# 
# }
# 
# 
# 
# setwd("C:/Mueller_Jens_Data/150824_Biogeochemistry_Baltic/Figures/5_Seasons/data/all_CO2-O2_Bernd/Summarized_datasets")
# 
# 
# write.csv(ts.int.09, "mean2009_interpolated.csv", row.names = FALSE)
# 
# 
# 
# ################
# 
# 
# 
# 
# for (i in 1:length(figs)) {
#   file_name = paste("interpolation_", i, ".tiff", sep="")
#   tiff(file_name, width = 297, height = 210, units = 'mm', res = 1000, compression = 'lzw')
#   print(figs[[i]])
#   dev.off()
# }
# 
# 
# pdf("plots.pdf")
# for (i in 1:length(figs)) {
#   print(figs[[i]])
# }
# dev.off()
# 
# 
# 
# 
# 
# 
# #### Calculation of net community production based on interpolated values
# 
# #interpolated CO2 data
# 
# setwd("C:/Mueller_Jens_Data/150824_Biogeochemistry_Baltic/Figures/5_Seasons/data/all_CO2-O2_Bernd/Summarized_datasets")
# 
# df <- data.table(read.csv("mean2009_interpolated.csv"))
# df$date <- as.POSIXct(strptime(df$date, format="%Y-%m-%d %H:%M:%S", tz="GMT"))
# df <- df[Area!="3.WGS"]
# 
# 
# #wind data
# 
# MEB <- data.table(read.delim(
#   "C:/Mueller_Jens_Data/150824_Biogeochemistry_Baltic/Figures/5_Seasons/data/Wind_data/MEB_speed.txt", 
#   sep = ""))
# MEB$Area <- "1.MEB"
# 
# ARK <- data.table(read.delim(
#   "C:/Mueller_Jens_Data/150824_Biogeochemistry_Baltic/Figures/5_Seasons/data/Wind_data/ARK_speed.txt", 
#   sep = ""))
# ARK$Area <- "2.ARK"
# 
# EGS <- data.table(read.delim(
#   "C:/Mueller_Jens_Data/150824_Biogeochemistry_Baltic/Figures/5_Seasons/data/Wind_data/EGS_speed.txt", 
#   sep = ""))
# EGS$Area <- "4.EGS"
# 
# NGS <- data.table(read.delim(
#   "C:/Mueller_Jens_Data/150824_Biogeochemistry_Baltic/Figures/5_Seasons/data/Wind_data/NGS_speed.txt", 
#   sep = ""))
# NGS$Area <- "5.NGS"
# 
# WGF <- data.table(read.delim(
#   "C:/Mueller_Jens_Data/150824_Biogeochemistry_Baltic/Figures/5_Seasons/data/Wind_data/WGF_speed.txt", 
#   sep = ""))
# WGF$Area <- "6.WGF"
# 
# HGF <- data.table(read.delim(
#   "C:/Mueller_Jens_Data/150824_Biogeochemistry_Baltic/Figures/5_Seasons/data/Wind_data/HGF_speed.txt", 
#   sep = ""))
# HGF$Area <- "7.HGF"
# 
# wind <- rbind (MEB, ARK, EGS, NGS, WGF, HGF)
# rm (MEB, ARK, EGS, NGS, WGF, HGF)
# 
# wind$date <- as.POSIXct(strptime(
#   paste(wind$day,"-",wind$month,"-",wind$year," ",wind$time, sep = ""),
#   format = "%d-%m-%Y %H:%M:%S",tz="GMT"))
# 
# 
# wind$date <- as.Date(wind$date)
# df$date <- as.Date(df$date)
# 
# setorder(df, Area, date)
# setorder(wind, Area, date)
# 
# df <- cbind(df, wind$value)
# names(df)[names(df) == "V2"] = "wind"
# rm(wind)
# 
# #pCO2 atmosphere
# pCO2.atm <- read.csv("C:/Mueller_Jens_Data/150824_Biogeochemistry_Baltic/Figures/5_Seasons/data/pCO2-atm_2010.csv")
# pCO2.atm$date <- as.Date(as.POSIXct(strptime(pCO2.atm$date, format="%d.%m.%Y", tz="GMT")))
# pCO2.atm$pCO2 <- pCO2.atm$pCO2 - (2010-2009)*2
# 
# df <- cbind(df, pCO2.atm$pCO2)
# names(df)[names(df) == "V2"] = "pCO2.atm"
# rm(pCO2.atm)
# 
# #include the starting dates of the spring bloom
# spring <- data.table(read.csv("C:/Mueller_Jens_Data/150824_Biogeochemistry_Baltic/Figures/5_Seasons/data/springbloom/161130_starting_dates_springbloom.csv"))
# spring$start.date <- as.Date(as.POSIXct(strptime(spring$date, format="%d.%m.%Y", tz="GMT")))
# spring <- spring[year==2009]
# spring$year <- NULL
# spring$SST <- NULL
# spring$date <- NULL
# 
# df <- merge(df, spring, by="Area")
# rm(spring)
# 
# 
# ## NCP calculation, incl gas exchange
# 
# #mixing depth
# 
# df$zmix <- 17
# df[Area=="2.ARK"]$zmix <- 35
# df[Area=="4.EGS"]$zmix <- 39
# df[Area=="6.WGF"]$zmix <- 26
# df[Area=="5.NGS"]$zmix <- 39
# df[Area=="7.HGF"]$zmix <- 28
# 
# # Duration since start of spring bloom and subsetting the springbloom unitl 31 may 2009
# 
# df$duration <- df$date - df$start.date
# df <- df[duration >= 0]
# df$duration <- as.numeric(df$duration, units="days")
# 
# df <- df[date < as.POSIXct("2009/05/31", tz="GMT")]
# 
# 
# # NCP computation
# df$deltac <- (df$int.pCO2 - df$pCO2.atm)*K0(df$int.Sal, df$int.Tem)
# 
# df$k <- gas_transfer(t = df$int.Tem, u10 = df$wind, species = "CO2",
#              method = c("Wanninkhof2"),
#              Schmidt = gas_schmidt(t = df$int.Tem, species = "CO2")) * 60^2 * 100
# 
# 
# 
# df$k.BS <- (0.24*df$wind^2)*((1943-119.6*df$int.Tem+3.488*df$int.Tem^2-0.0417*df$int.Tem^3)/660)^(-0.5)
# 
# df$flux.BS <- -0.24* df$k.BS* df$deltac
# df$flux <- -0.24* df$k* df$deltac
# 
# 
# df[,dCT:=max(int.CT)-int.CT, by=.(Area)]
# df$F.zmix <- df$flux/df$zmix
# df[duration==0]$F.zmix <- 0
# 
# df[,F.zmix.cum:=cumsum(F.zmix), by=.(Area)]
# df$idCT <- df$dCT * df$zmix
# 
# df[,flux.cum:=cumsum(flux), by=.(Area)]
# df$dCT.bio <- (df$dCT + df$F.zmix.cum)*0.8
# 
# df$iNCP <- df$dCT.bio * df$zmix
# df$idCT.08 <- df$idCT * 0.8
# 
# df.end <- df[date > as.POSIXct("2009/05/21", tz="GMT")]
# df.end.mean <- df.end[,.(final.iNCP = mean(iNCP),
#                          final.dCT.bio = mean(dCT.bio)), by=.(Area)]
# 
# df <- merge(df, df.end.mean, by="Area")
# rm(df.end, df.end.mean)
# 
# write.csv(df, "mean2009_NCP_spring.csv", row.names = FALSE)
# 
# 
# ggplot()+
#   geom_line(data=df, aes(date, iNCP, col=Area))+
#   geom_line(data=df, aes(date, idCT.08, col=Area), linetype=2)+
#   geom_ribbon(data=df, aes(date, ymin=idCT.08, ymax=iNCP, fill=Area), alpha=0.3)+
#   facet_wrap(~Area)
# 
# ggplot()+
#   geom_col(data = df.end.mean, aes(Area, final.dCT.bio))
# 
# ggplot(df, aes(date, int.CT, col=Area))+
#   geom_point()
# 
# 
# 
# 
# #### interpolation all years
# 
# setwd("C:/Mueller_Jens_Data/150824_Biogeochemistry_Baltic/Figures/5_Seasons/data/all_CO2-O2_Bernd/Summarized_datasets")
# 
# df <- data.table(read.csv("meanall_CT.csv"))
# df$date <- as.POSIXct(strptime(df$date, format="%Y-%m-%d %H:%M:%S", tz="GMT"))
# summary(df)
# 
# # df <- df[date > as.POSIXct(strptime("2008-01-01 12:00:00",
# #                                       format="%Y-%m-%d %H:%M:%S", tz="GMT")) &
# #              date < as.POSIXct(strptime("2015-12-30 12:00:00",
# #                                         format="%Y-%m-%d %H:%M:%S", tz="GMT"))]
# 
# 
# 
# for (i in levels(df$Area)){
# 
# pCO2 <- approxfun(df[Area == i]$date, df[Area == i]$mean.pCO2)
# CT <- approxfun(df[Area == i]$date, df[Area == i]$mean.CT)
# #cO2 <- approxfun(df09[Area == i]$date, df09[Area == i]$mean.cO2)
# Sal <- approxfun(df[Area == i]$date, df[Area == i]$mean.Sal)
# Tem <- approxfun(df[Area == i]$date, df[Area == i]$mean.Tem)
# patm <- approxfun(df[Area == i]$date, df[Area == i]$mean.patm)
# 
# interval <- as.POSIXct(strptime("2004-01-01 12:00:00", format="%Y-%m-%d %H:%M:%S", tz="GMT"))+
#   (days(seq(1,365*12,1)))
# 
# temp <- cbind(data.table(interval),
#                 data.table(i),
#                 data.table(pCO2(interval)),
#                 data.table(CT(interval)),
#                 #data.table(cO2(interval)),
#                 data.table(Sal(interval)),
#                 data.table(Tem(interval)),
#                 data.table(patm(interval)))
# names(temp) <- c("date", "Area", "int.pCO2", "int.CT", "int.Sal", "int.Tem", "int.patm")
# 
# if (exists("ts.int")){
#   ts.int <- rbind (ts.int, temp)
# } else{ts.int <- temp}
# 
# rm(temp, interval, pCO2, CT, Sal, Tem, patm)
# 
# }
# 
# 
# s <- 0.1
# ggplot()+
#   #geom_point(data=ts.int, aes(date, int.pCO2))+
#   geom_smooth(data=ts.int, aes(date, int.pCO2), method="loess", col="grey", span=s, se=FALSE)+
#   #geom_point(data=df, aes(date, mean.pCO2), col="red")+
#   #geom_smooth(data=df, aes(date, mean.pCO2), method="loess", col="red", span=s, se=FALSE)+
#   ylim(0,700)+
#   facet_grid(Area~.)
# 
# 
# 
# 
# setwd("C:/Mueller_Jens_Data/150824_Biogeochemistry_Baltic/Figures/5_Seasons/plots/interpolation_08-16")
# 
# reg <- levels(df09$Area)[1]
# 
# for (var in c("pCO2", "CT", "Sal", "Tem", "patm"))
#   for (reg in levels(df$Area))
#   {
#     mean.var <- paste("mean", var, sep = ".")
#     int.var <- paste("int", var, sep = ".")
#     #i<-i+1
#     
#     fig <-
#       ggplot()+
#       geom_point(data=df[Area == reg], aes(date, get(mean.var), col="measured"), size=0.2)+
#       geom_line(data=df[Area == reg], aes(date, get(mean.var), col="measured"), size=0.2)+
#       geom_point(data=ts.int[Area == reg], aes(date, get(int.var), col="interpolated"), size=0.2)+
#       scale_color_manual(values = c("black", "red"))+
#       # geom_vline(xintercept = c(as.numeric(as.POSIXct(strptime("2009-01-01 00:00:00", 
#       #                                                          format="%Y-%m-%d %H:%M:%S", tz="GMT"))),
#       #                           as.numeric(as.POSIXct(strptime("2009-12-31 24:00:00", 
#       #                                                          format="%Y-%m-%d %H:%M:%S", tz="GMT")))
#       # ))+
#       labs(y=var, title=reg)
#     
#     tiff(paste("./interpolation_2009",var,reg,".tiff", sep="_"), width = 2970, height = 150, units = 'mm', res = 300, compression = 'lzw')
#     print(fig)
#     dev.off()
#     
#   }
# 
# setwd("C:/Mueller_Jens_Data/150824_Biogeochemistry_Baltic/Figures/5_Seasons/data/all_CO2-O2_Bernd/Summarized_datasets")
# write.csv(ts.int, "meanall_interpolated.csv", row.names = FALSE)
# 
