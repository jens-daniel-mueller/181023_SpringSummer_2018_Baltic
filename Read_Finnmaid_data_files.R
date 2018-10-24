library(ggplot2)
library(data.table)
library(readxl)
library(seacarb)
library(gsubfn)
library(lubridate)



### 2003

setwd("C:/Mueller_Jens_Data/181023_Spring_Summer_2018/data/Finnmaid/pCO2-2003")
files <- list.files(pattern = "[.]xls$")
#file <- files[4]

for (file in files){
  
  df <- data.table(
    read_excel(file, skip = 2))
  df <- df[,1:7]
  names(df) <- c("date","Lon","Lat","pCO2","Sal","Tem","dTem")
  df$Lon <- as.numeric(as.character(df$Lon))
  df$Lat <- as.numeric(as.character(df$Lat))
  df$pCO2 <- as.numeric(as.character(df$pCO2))
  df$Sal <- as.numeric(as.character(df$Sal))
  df$Tem <- as.numeric(as.character(df$Tem))
  df$dTem <- as.numeric(as.character(df$dTem))
  df$cO2 <- as.numeric(NA)
  df$patm <-as.numeric(NA)
  df$Teq <- df$Tem + df$dTem
  df$dTem <- NULL
  df$xCO2 <- as.numeric(NA)
  
  df$route <- strapplyc(as.character(file), ".*(.).xls*", simplify = TRUE)
  df$ID <- substr(as.character(file), 1, 8)

  if (exists("temp")){
    temp <- rbind (temp, df)
  } else{temp <- df}
  
}


temp$Area <- with(temp,
  ifelse(Lon>12 & Lon<12.6, "1.MEB",
  ifelse(Lon>13.1 & Lon<14.3, "2.ARK",
  ifelse(Lat>57.5 & Lat<58.5 & route %in% c("E", "G"), "4.EGS",
  ifelse(Lat>56.8 & Lat<57.5 & route=="W", "3.WGS",
  ifelse(Lat>58.5 & Lat<59 & Lon>20, "5.NGS",
  ifelse(Lon>22 & Lon<24, "6.WGF",
  ifelse(Lon>24 & Lon<24.5, "7.HGF", "NaN"))))))))

temp <-temp[complete.cases(temp[,pCO2]),]

temp.mean <- temp[,.(
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

df.all <- temp
df.2003 <- temp
mean.all <- temp.mean
mean.2003 <- temp.mean

setwd("C:/Mueller_Jens_Data/181023_Spring_Summer_2018/data/Finnmaid/Summarized_datasets")
write.csv(df.2003, "df2003.csv")
write.csv(mean.2003, "mean2003.csv")


rm(df, temp.mean, temp, files, file, df.2003, mean.2003)



#### 2004

setwd("C:/Mueller_Jens_Data/181023_Spring_Summer_2018/data/Finnmaid/pCO2-2004")
files <- list.files(pattern = "[.]xls$")
#file <- files[93]

for (file in files){
  
  df <- data.table(
    read_excel(file, skip = 2))
  names(df) <- c("date","Lon","Lat","pCO2","Sal","Tem","dTem")
  df$Lon <- as.numeric(as.character(df$Lon))
  df$Lat <- as.numeric(as.character(df$Lat))
  df$pCO2 <- as.numeric(as.character(df$pCO2))
  df$Sal <- as.numeric(as.character(df$Sal))
  df$Tem <- as.numeric(as.character(df$Tem))
  df$dTem <- as.numeric(as.character(df$dTem))
  df$cO2 <- as.numeric(NA)
  df$patm <-as.numeric(NA)
  df$Teq <- df$Tem + df$dTem
  df$dTem <- NULL
  df$xCO2 <- as.numeric(NA)
  
  df$route <- strapplyc(as.character(file), ".*(.).xls*", simplify = TRUE)
  df$ID <- substr(as.character(file), 1, 8)

  if (exists("temp")){
    temp <- rbind (temp, df)
  } else{temp <- df}
  
}


temp$Area <- with(temp,
  ifelse(Lon>12 & Lon<12.6, "1.MEB",
  ifelse(Lon>13.1 & Lon<14.3, "2.ARK",
  ifelse(Lat>57.5 & Lat<58.5 & route %in% c("E", "G"), "4.EGS",
  ifelse(Lat>56.8 & Lat<57.5 & route=="W", "3.WGS",
  ifelse(Lat>58.5 & Lat<59 & Lon>20, "5.NGS",
  ifelse(Lon>22 & Lon<24, "6.WGF",
  ifelse(Lon>24 & Lon<24.5, "7.HGF", "NaN"))))))))

temp <-temp[complete.cases(temp[,pCO2]),]

temp.mean <- temp[,.(
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


df.all <- rbind(df.all, temp)
df.2004 <- temp
mean.all <- rbind(mean.all, temp.mean)
mean.2004 <- temp.mean


setwd("C:/Mueller_Jens_Data/181023_Spring_Summer_2018/data/Finnmaid/Summarized_datasets")
write.csv(df.2004, "df2004.csv")
write.csv(mean.2004, "mean2004.csv")


rm(df, temp.mean, temp, files, file, df.2004, mean.2004)


#### 2005

setwd("C:/Mueller_Jens_Data/181023_Spring_Summer_2018/data/Finnmaid/pCO2-2005")
files <- list.files(pattern = "[.]xls$")
#file <- files[2]

for (file in files){
  
  df <- data.table(
    read_excel(file))
  names(df) <- c("date","Lon","Lat","pCO2","Sal","Tem","dTem")
  df <- df[-1,]
  df$Lon <- as.numeric(as.character(df$Lon))
  df$Lat <- as.numeric(as.character(df$Lat))
  df$pCO2 <- as.numeric(as.character(df$pCO2))
  df$Sal <- as.numeric(as.character(df$Sal))
  df$Tem <- as.numeric(as.character(df$Tem))
  df$dTem <- as.numeric(as.character(df$dTem))
  df$cO2 <- as.numeric(NA)
  df$patm <-as.numeric(NA)
  df$Teq <- df$Tem + df$dTem
  df$dTem <- NULL
  df$xCO2 <- as.numeric(NA)
  
  df$route <- strapplyc(as.character(file), ".*(.).xls*", simplify = TRUE)
  df$ID <- substr(as.character(file), 1, 8)
  
  if (exists("temp")){
    temp <- rbind (temp, df)
  } else{temp <- df}
  
}


temp$Area <- with(temp,
   ifelse(Lon>12 & Lon<12.6, "1.MEB",
   ifelse(Lon>13.1 & Lon<14.3, "2.ARK",
   ifelse(Lat>57.5 & Lat<58.5 & route %in% c("E", "G"), "4.EGS",
   ifelse(Lat>56.8 & Lat<57.5 & route=="W", "3.WGS",
   ifelse(Lat>58.5 & Lat<59 & Lon>20, "5.NGS",
   ifelse(Lon>22 & Lon<24, "6.WGF",
   ifelse(Lon>24 & Lon<24.5, "7.HGF", "NaN"))))))))

temp <-temp[complete.cases(temp[,pCO2]),]


temp.mean <- temp[,.(
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


df.all <- rbind(df.all, temp)
df.2005 <- temp
mean.all <- rbind(mean.all, temp.mean)
mean.2005 <- temp.mean


setwd("C:/Mueller_Jens_Data/181023_Spring_Summer_2018/data/Finnmaid/Summarized_datasets")
write.csv(df.2005, "df2005.csv")
write.csv(mean.2005, "mean2005.csv")


rm(df, temp.mean, temp, files, file, df.2005, mean.2005)




#### 2006

setwd("C:/Mueller_Jens_Data/181023_Spring_Summer_2018/data/Finnmaid/pCO2-2006")
files <- list.files(pattern = "[.]xls$")
#file <- files[2]

for (file in files){
  
  df <- data.table(
    read_excel(file))
  names(df) <- c("date","Lon","Lat","pCO2","Sal","Tem","dTem")
  df <- df[-1,]
  df$Lon <- as.numeric(as.character(df$Lon))
  df$Lat <- as.numeric(as.character(df$Lat))
  df$pCO2 <- as.numeric(as.character(df$pCO2))
  df$Sal <- as.numeric(as.character(df$Sal))
  df$Tem <- as.numeric(as.character(df$Tem))
  df$dTem <- as.numeric(as.character(df$dTem))
  df$cO2 <- as.numeric(NA)
  df$patm <-as.numeric(NA)
  df$Teq <- df$Tem + df$dTem
  df$dTem <- NULL
  df$xCO2 <- as.numeric(NA)
  
  df$route <- strapplyc(as.character(file), ".*(.).xls*", simplify = TRUE)
  df$ID <- substr(as.character(file), 1, 8)
  
  if (exists("temp")){
    temp <- rbind (temp, df)
  } else{temp <- df}
  
}


temp$Area <- with(temp,
   ifelse(Lon>12 & Lon<12.6, "1.MEB",
   ifelse(Lon>13.1 & Lon<14.3, "2.ARK",
   ifelse(Lat>57.5 & Lat<58.5 & route %in% c("E", "G"), "4.EGS",
   ifelse(Lat>56.8 & Lat<57.5 & route=="W", "3.WGS",
   ifelse(Lat>58.5 & Lat<59 & Lon>20, "5.NGS",
   ifelse(Lon>22 & Lon<24, "6.WGF",
   ifelse(Lon>24 & Lon<24.5, "7.HGF", "NaN"))))))))

temp <-temp[complete.cases(temp[,pCO2]),]


temp.mean <- temp[,.(
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


df.all <- rbind(df.all, temp)
df.2006 <- temp
mean.all <- rbind(mean.all, temp.mean)
mean.2006 <- temp.mean


setwd("C:/Mueller_Jens_Data/181023_Spring_Summer_2018/data/Finnmaid/Summarized_datasets")
write.csv(df.2006, "df2006.csv")
write.csv(mean.2006, "mean2006.csv")


rm(df, temp.mean, temp, files, file, df.2006, mean.2006)



#### 2007

setwd("C:/Mueller_Jens_Data/181023_Spring_Summer_2018/data/Finnmaid/pCO2-2007")
files <- list.files(pattern = "[.]xls$")
#file <- files[2]

for (file in files){
  
  df <- data.table(
    read_excel(file))
  names(df) <- c("date","Lon","Lat","pCO2","Sal","Tem")
  df <- df[-1,]
  df$Lon <- as.numeric(as.character(df$Lon))
  df$Lat <- as.numeric(as.character(df$Lat))
  df$pCO2 <- as.numeric(as.character(df$pCO2))
  df$Sal <- as.numeric(as.character(df$Sal))
  df$Tem <- as.numeric(as.character(df$Tem))
  df$dTem <- as.numeric(NA)
  df$cO2 <- as.numeric(NA)
  df$patm <-as.numeric(NA)
  df$Teq <- df$Tem + df$dTem
  df$dTem <- NULL
  df$xCO2 <- as.numeric(NA)
  
  df$route <- strapplyc(as.character(file), ".*(.).xls*", simplify = TRUE)
  df$ID <- substr(as.character(file), 1, 8)
  
  if (exists("temp")){
    temp <- rbind (temp, df)
  } else{temp <- df}
  
}


temp$Area <- with(temp,
   ifelse(Lon>12 & Lon<12.6, "1.MEB",
   ifelse(Lon>13.1 & Lon<14.3, "2.ARK",
   ifelse(Lat>57.5 & Lat<58.5 & route %in% c("E", "G"), "4.EGS",
   ifelse(Lat>56.8 & Lat<57.5 & route=="W", "3.WGS",
   ifelse(Lat>58.5 & Lat<59 & Lon>20, "5.NGS",
   ifelse(Lon>22 & Lon<24, "6.WGF",
   ifelse(Lon>24 & Lon<24.5, "7.HGF", "NaN"))))))))

temp <-temp[complete.cases(temp[,pCO2]),]


temp.mean <- temp[,.(
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


df.all <- rbind(df.all, temp)
df.2007 <- temp
mean.all <- rbind(mean.all, temp.mean)
mean.2007 <- temp.mean


setwd("C:/Mueller_Jens_Data/181023_Spring_Summer_2018/data/Finnmaid/Summarized_datasets")
write.csv(df.2007, "df2007.csv")
write.csv(mean.2007, "mean2007.csv")


rm(df, temp.mean, temp, files, file, df.2007, mean.2007)




#### 2008

setwd("C:/Mueller_Jens_Data/181023_Spring_Summer_2018/data/Finnmaid/pCO2-2008")
files <- list.files(pattern = "[.]xls$")
#file <- files[2]

for (file in files){
  
  df <- data.table(
    read_excel(file))
  names(df) <- c("date","Lon","Lat","pCO2","Sal","Tem", "dTem")
  df <- df[-c(1,2),]
  df$Lon <- as.numeric(as.character(df$Lon))
  df$Lat <- as.numeric(as.character(df$Lat))
  df$pCO2 <- as.numeric(as.character(df$pCO2))
  df$Sal <- as.numeric(as.character(df$Sal))
  df$Tem <- as.numeric(as.character(df$Tem))
  df$dTem <- as.numeric(as.character(df$dTem))
  df$cO2 <- as.numeric(NA)
  df$patm <-as.numeric(NA)
  df$Teq <- df$Tem + df$dTem
  df$dTem <- NULL
  df$xCO2 <- as.numeric(NA)
  
  df$route <- strapplyc(as.character(file), ".*(.).xls*", simplify = TRUE)
  df$ID <- substr(as.character(file), 3, 10)
  
  if (exists("temp")){
    temp <- rbind (temp, df)
  } else{temp <- df}
  
}


temp$Area <- with(temp,
   ifelse(Lon>12 & Lon<12.6, "1.MEB",
   ifelse(Lon>13.1 & Lon<14.3, "2.ARK",
   ifelse(Lat>57.5 & Lat<58.5 & route %in% c("E", "G"), "4.EGS",
   ifelse(Lat>56.8 & Lat<57.5 & route=="W", "3.WGS",
   ifelse(Lat>58.5 & Lat<59 & Lon>20, "5.NGS",
   ifelse(Lon>22 & Lon<24, "6.WGF",
   ifelse(Lon>24 & Lon<24.5, "7.HGF", "NaN"))))))))

temp <-temp[complete.cases(temp[,pCO2]),]


temp.mean <- temp[,.(
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


df.all <- rbind(df.all, temp)
df.2008 <- temp
mean.all <- rbind(mean.all, temp.mean)
mean.2008 <- temp.mean


setwd("C:/Mueller_Jens_Data/181023_Spring_Summer_2018/data/Finnmaid/Summarized_datasets")
write.csv(df.2008, "df2008.csv")
write.csv(mean.2008, "mean2008.csv")


rm(df, temp.mean, temp, files, file, df.2008, mean.2008)




#### 2009a

setwd("C:/Mueller_Jens_Data/181023_Spring_Summer_2018/data/Finnmaid/pCO2-2009a")
files <- list.files(pattern = "[.]xls$")
#file <- files[2]

for (file in files){
  
  df <- read_excel(file)
  df <- df[c(1,2,3,11,6,4,10,7,5,14)]
  names(df) <- c("date","Lon","Lat","pCO2","Sal","Tem","cO2","patm", "Teq","xCO2")
  df <- df[-c(1,2),]
  df$date <- as.POSIXct(as.numeric(df$date)*60*60*24, origin="1899-12-30", tz="GMT")
  df$Lon <- as.numeric(as.character(df$Lon))
  df$Lat <- as.numeric(as.character(df$Lat))
  df$pCO2 <- as.numeric(as.character(df$pCO2))
  df$Sal <- as.numeric(as.character(df$Sal))
  df$Tem <- as.numeric(as.character(df$Tem))
  df$cO2 <- as.numeric(NA)
  df$patm <- as.numeric(as.character(df$patm))
  df$Teq <- as.numeric(as.character(df$Teq))
  df$xCO2 <- as.numeric(as.character(df$xCO2))
  df <- data.table(df)
  
  df$route <- strapplyc(as.character(file), ".*(.).xls*", simplify = TRUE)
  df$ID <- substr(as.character(file), 3, 10)
  
  if (exists("temp")){
    temp <- rbind (temp, df)
  } else{temp <- df}
  
}


temp$Area <- with(temp,
   ifelse(Lon>12 & Lon<12.6, "1.MEB",
   ifelse(Lon>13.1 & Lon<14.3, "2.ARK",
   ifelse(Lat>57.5 & Lat<58.5 & route %in% c("E", "G"), "4.EGS",
   ifelse(Lat>56.8 & Lat<57.5 & route=="W", "3.WGS",
   ifelse(Lat>58.5 & Lat<59 & Lon>20, "5.NGS",
   ifelse(Lon>22 & Lon<24, "6.WGF",
   ifelse(Lon>24 & Lon<24.5, "7.HGF", "NaN"))))))))

temp <-temp[complete.cases(temp[,pCO2]),]


temp.mean <- temp[,.(
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


df.all <- rbind(df.all, temp)
df.2009a <- temp
mean.all <- rbind(mean.all, temp.mean)
mean.2009a <- temp.mean


# setwd("C:/Mueller_Jens_Data/181023_Spring_Summer_2018/data/Finnmaid/Summarized_datasets")
# write.csv(df.2009a, "df2009a.csv")
# write.csv(mean.2009a, "mean2009a.csv")


rm(df, temp.mean, temp, files, file)




#### 2009b

setwd("C:/Mueller_Jens_Data/181023_Spring_Summer_2018/data/Finnmaid/pCO2-2009b")
files <- list.files(pattern = "[.]xls$")
#file <- files[20]

for (file in files){
  
  df <- read_excel(file)
  df <- df[c(1,2,3,11,7,4,14,8,5,16)]
  names(df) <- c("date","Lon","Lat","pCO2","Sal","Tem","cO2","patm", "Teq","xCO2")
  df <- df[-c(1),]
  df$date <- as.POSIXct(as.numeric(df$date)*60*60*24, origin="1899-12-30", tz="GMT")
  df$Lon <- as.numeric(as.character(df$Lon))
  df$Lat <- as.numeric(as.character(df$Lat))
  df$pCO2 <- as.numeric(as.character(df$pCO2))
  df$Sal <- as.numeric(as.character(df$Sal))
  df$Tem <- as.numeric(as.character(df$Tem))
  df$cO2 <- as.numeric(NA)
  df$patm <- as.numeric(as.character(df$patm))
  df$Teq <- as.numeric(as.character(df$Teq))
  df$xCO2 <- as.numeric(as.character(df$xCO2))
  df <- data.table(df)
  
  df$route <- strapplyc(as.character(file), ".*(.).xls*", simplify = TRUE)
  df$ID <- substr(as.character(file), 3, 10)
  
  if (exists("temp")){
    temp <- rbind (temp, df)
  } else{temp <- df}
  
}


temp$Area <- with(temp,
   ifelse(Lon>12 & Lon<12.6, "1.MEB",
   ifelse(Lon>13.1 & Lon<14.3, "2.ARK",
   ifelse(Lat>57.5 & Lat<58.5 & route %in% c("E", "G"), "4.EGS",
   ifelse(Lat>56.8 & Lat<57.5 & route=="W", "3.WGS",
   ifelse(Lat>58.5 & Lat<59 & Lon>20, "5.NGS",
   ifelse(Lon>22 & Lon<24, "6.WGF",
   ifelse(Lon>24 & Lon<24.5, "7.HGF", "NaN"))))))))

temp <-temp[complete.cases(temp[,pCO2]),]


temp.mean <- temp[,.(
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


df.all <- rbind(df.all, temp)
df.2009 <- rbind(df.2009a, temp)
mean.all <- rbind(mean.all, temp.mean)
mean.2009 <- rbind(mean.2009a, temp.mean)


setwd("C:/Mueller_Jens_Data/181023_Spring_Summer_2018/data/Finnmaid/Summarized_datasets")
write.csv(df.2009, "df2009.csv")
write.csv(mean.2009, "mean2009.csv")


rm(df, temp.mean, temp, files, file, df.2009a, mean.2009a, df.2009, mean.2009)




#### 2010

setwd("C:/Mueller_Jens_Data/181023_Spring_Summer_2018/data/Finnmaid/pCO2-2010")
files <- list.files(pattern = "[.]xls$")
#file <- files[2]

for (file in files){
  
  df <- read_excel(file)
  df <- df[c(1,2,3,12,7,4,15,8,5,17)]
  names(df) <- c("date","Lon","Lat","pCO2","Sal","Tem","cO2","patm", "Teq","xCO2")
  df <- df[-c(1),]
  df$date <- as.POSIXct(as.numeric(df$date)*60*60*24, origin="1899-12-30", tz="GMT")
  df$Lon <- as.numeric(as.character(df$Lon))
  df$Lat <- as.numeric(as.character(df$Lat))
  df$pCO2 <- as.numeric(as.character(df$pCO2))
  df$Sal <- as.numeric(as.character(df$Sal))
  df$Tem <- as.numeric(as.character(df$Tem))
  df$cO2 <- as.numeric(NA)
  df$patm <- as.numeric(as.character(df$patm))
  df$Teq <- as.numeric(as.character(df$Teq))
  df$xCO2 <- as.numeric(as.character(df$xCO2))
  df <- data.table(df)
  
  df$route <- strapplyc(as.character(file), ".*(.).xls*", simplify = TRUE)
  df$ID <- substr(as.character(file), 3, 10)
  
  if (exists("temp")){
    temp <- rbind (temp, df)
  } else{temp <- df}
  
}


temp$Area <- with(temp,
   ifelse(Lon>12 & Lon<12.6, "1.MEB",
   ifelse(Lon>13.1 & Lon<14.3, "2.ARK",
   ifelse(Lat>57.5 & Lat<58.5 & route %in% c("E", "G"), "4.EGS",
   ifelse(Lat>56.8 & Lat<57.5 & route=="W", "3.WGS",
   ifelse(Lat>58.5 & Lat<59 & Lon>20, "5.NGS",
   ifelse(Lon>22 & Lon<24, "6.WGF",
   ifelse(Lon>24 & Lon<24.5, "7.HGF", "NaN"))))))))

temp <-temp[complete.cases(temp[,pCO2]),]


temp.mean <- temp[,.(
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


df.all <- rbind(df.all, temp)
df.2010 <- temp
mean.all <- rbind(mean.all, temp.mean)
mean.2010 <- temp.mean


setwd("C:/Mueller_Jens_Data/181023_Spring_Summer_2018/data/Finnmaid/Summarized_datasets")
write.csv(df.2010, "df2010.csv")
write.csv(mean.2010, "mean2010.csv")


rm(df, temp.mean, temp, files, file, df.2010, mean.2010)







#### 2011

setwd("C:/Mueller_Jens_Data/181023_Spring_Summer_2018/data/Finnmaid/pCO2-2011")
files <- list.files(pattern = "[.]xls$")
#file <- files[2]

for (file in files){
  
  df <- read_excel(file)
  df <- df[c(1,2,3,12,7,4,15,8,5,17)]
  names(df) <- c("date","Lon","Lat","pCO2","Sal","Tem","cO2","patm", "Teq","xCO2")
  df <- df[-c(1),]
  df$date <- as.POSIXct(as.numeric(df$date)*60*60*24, origin="1899-12-30", tz="GMT")
  df$Lon <- as.numeric(as.character(df$Lon))
  df$Lat <- as.numeric(as.character(df$Lat))
  df$pCO2 <- as.numeric(as.character(df$pCO2))
  df$Sal <- as.numeric(as.character(df$Sal))
  df$Tem <- as.numeric(as.character(df$Tem))
  df$cO2 <- as.numeric(NA)
  df$patm <- as.numeric(as.character(df$patm))
  df$Teq <- as.numeric(as.character(df$Teq))
  df$xCO2 <- as.numeric(as.character(df$xCO2))
  df <- data.table(df)
  
  df$route <- strapplyc(as.character(file), ".*(.).xls*", simplify = TRUE)
  df$ID <- substr(as.character(file), 3, 10)
  
  if (exists("temp")){
    temp <- rbind (temp, df)
  } else{temp <- df}
  
}


temp$Area <- with(temp,
   ifelse(Lon>12 & Lon<12.6, "1.MEB",
   ifelse(Lon>13.1 & Lon<14.3, "2.ARK",
   ifelse(Lat>57.5 & Lat<58.5 & route %in% c("E", "G"), "4.EGS",
   ifelse(Lat>56.8 & Lat<57.5 & route=="W", "3.WGS",
   ifelse(Lat>58.5 & Lat<59 & Lon>20, "5.NGS",
   ifelse(Lon>22 & Lon<24, "6.WGF",
   ifelse(Lon>24 & Lon<24.5, "7.HGF", "NaN"))))))))

temp <-temp[complete.cases(temp[,pCO2]),]


temp.mean <- temp[,.(
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


df.all <- rbind(df.all, temp)
df.2011 <- temp
mean.all <- rbind(mean.all, temp.mean)
mean.2011 <- temp.mean


setwd("C:/Mueller_Jens_Data/181023_Spring_Summer_2018/data/Finnmaid/Summarized_datasets")
write.csv(df.2011, "df2011.csv")
write.csv(mean.2011, "mean2011.csv")


rm(df, temp.mean, temp, files, file, df.2011, mean.2011)





#### 2012

setwd("C:/Mueller_Jens_Data/181023_Spring_Summer_2018/data/Finnmaid/pCO2-O2-2012")
files <- list.files(pattern = "[.]xls$")
#file <- files[1]

for (file in files){
  
  
  df <- read_excel(file)
  df <- df[c(1,2,3,12,7,4,15,8,5,17)]
  names(df) <- c("date","Lon","Lat","pCO2","Sal","Tem","cO2","patm", "Teq","xCO2")
  df <- df[-c(1),]
  df$date <- as.POSIXct(as.numeric(df$date)*60*60*24, origin="1899-12-30", tz="GMT")
  df$Lon <- as.numeric(as.character(df$Lon))
  df$Lat <- as.numeric(as.character(df$Lat))
  df$pCO2 <- as.numeric(as.character(df$pCO2))
  df$Sal <- as.numeric(as.character(df$Sal))
  df$Tem <- as.numeric(as.character(df$Tem))
  df$cO2 <- as.numeric(as.character(df$cO2))
  df$patm <- as.numeric(as.character(df$patm))
  df$Teq <- as.numeric(as.character(df$Teq))
  df$xCO2 <- as.numeric(as.character(df$xCO2))
  df <- data.table(df)
  
  df$route <- strapplyc(as.character(file), ".*(.).xls*", simplify = TRUE)
  df$ID <- substr(as.character(file), 3, 10)
  
  if (exists("temp")){
    temp <- rbind (temp, df)
  } else{temp <- df}
  
}

temp$Area <- with(temp,
  ifelse(Lon>12 & Lon<12.6, "1.MEB",
  ifelse(Lon>13.1 & Lon<14.3, "2.ARK",
  ifelse(Lat>57.5 & Lat<58.5 & route %in% c("E", "G"), "4.EGS",
  ifelse(Lat>56.8 & Lat<57.5 & route=="W", "3.WGS",
  ifelse(Lat>58.5 & Lat<59 & Lon>20, "5.NGS",
  ifelse(Lon>22 & Lon<24, "6.WGF",
  ifelse(Lon>24 & Lon<24.5, "7.HGF", "NaN"))))))))

temp <-temp[complete.cases(temp[,pCO2]),]

temp.mean <- temp[,.(
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



df.all <- rbind(df.all, temp)
df.2012 <- temp
mean.all <- rbind(mean.all, temp.mean)
mean.2012 <- temp.mean


setwd("C:/Mueller_Jens_Data/181023_Spring_Summer_2018/data/Finnmaid/Summarized_datasets")
write.csv(df.2012, "df2012.csv")
write.csv(mean.2012, "mean2012.csv")


rm(df, temp.mean, temp, files, file, df.2012, mean.2012)




#### 2013

setwd("C:/Mueller_Jens_Data/181023_Spring_Summer_2018/data/Finnmaid/pCO2-O2-2013")
files <- list.files(pattern = "[.]xls$")
#file <- files[1]

for (file in files){
  
  
  df <- read_excel(file)
  df <- df[c(1,2,3,12,7,4,15,8,5,17)]
  names(df) <- c("date","Lon","Lat","pCO2","Sal","Tem","cO2","patm", "Teq","xCO2")
  df <- df[-c(1),]
  df$date <- as.POSIXct(as.numeric(df$date)*60*60*24, origin="1899-12-30", tz="GMT")
  df$Lon <- as.numeric(as.character(df$Lon))
  df$Lat <- as.numeric(as.character(df$Lat))
  df$pCO2 <- as.numeric(as.character(df$pCO2))
  df$Sal <- as.numeric(as.character(df$Sal))
  df$Tem <- as.numeric(as.character(df$Tem))
  df$cO2 <- as.numeric(as.character(df$cO2))
  df$patm <- as.numeric(as.character(df$patm))
  df$Teq <- as.numeric(as.character(df$Teq))
  df$xCO2 <- as.numeric(as.character(df$xCO2))
  df <- data.table(df)
  
  df$route <- strapplyc(as.character(file), ".*(.).xls*", simplify = TRUE)
  df$ID <- substr(as.character(file), 3, 10)
  
  if (exists("temp")){
    temp <- rbind (temp, df)
  } else{temp <- df}
  
}


temp$Area <- with(temp,
  ifelse(Lon>12 & Lon<12.6, "1.MEB",
  ifelse(Lon>13.1 & Lon<14.3, "2.ARK",
  ifelse(Lat>57.5 & Lat<58.5 & route %in% c("E", "G"), "4.EGS",
  ifelse(Lat>56.8 & Lat<57.5 & route=="W", "3.WGS",
  ifelse(Lat>58.5 & Lat<59 & Lon>20, "5.NGS",
  ifelse(Lon>22 & Lon<24, "6.WGF",
  ifelse(Lon>24 & Lon<24.5, "7.HGF", "NaN"))))))))

temp <-temp[complete.cases(temp[,pCO2]),]

temp.mean <- temp[,.(
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



df.all <- rbind(df.all, temp)
df.2013 <- temp
mean.all <- rbind(mean.all, temp.mean)
mean.2013 <- temp.mean


setwd("C:/Mueller_Jens_Data/181023_Spring_Summer_2018/data/Finnmaid/Summarized_datasets")
write.csv(df.2013, "df2013.csv")
write.csv(mean.2013, "mean2013.csv")


rm(df, temp.mean, temp, files, file, df.2013, mean.2013)



#### 2014

setwd("C:/Mueller_Jens_Data/181023_Spring_Summer_2018/data/Finnmaid/pCO2-O2-2014")
files <- list.files(pattern = "[.]xls$")
#file <- files[1]

for (file in files){
  
  
  df <- read_excel(file)
  df <- df[c(1,2,3,12,7,4,15,8,5,17)]
  names(df) <- c("date","Lon","Lat","pCO2","Sal","Tem","cO2","patm", "Teq","xCO2")
  df <- df[-c(1),]
  df$date <- as.POSIXct(as.numeric(df$date)*60*60*24, origin="1899-12-30", tz="GMT")
  df$Lon <- as.numeric(as.character(df$Lon))
  df$Lat <- as.numeric(as.character(df$Lat))
  df$pCO2 <- as.numeric(as.character(df$pCO2))
  df$Sal <- as.numeric(as.character(df$Sal))
  df$Tem <- as.numeric(as.character(df$Tem))
  df$cO2 <- as.numeric(as.character(df$cO2))
  df$patm <- as.numeric(as.character(df$patm))
  df$Teq <- as.numeric(as.character(df$Teq))
  df$xCO2 <- as.numeric(as.character(df$xCO2))
  df <- data.table(df)
  
  df$route <- strapplyc(as.character(file), ".*(.).xls*", simplify = TRUE)
  df$ID <- substr(as.character(file), 3, 10)
  
  if (exists("temp")){
    temp <- rbind (temp, df)
  } else{temp <- df}
  
}

temp$Area <- with(temp,
  ifelse(Lon>12 & Lon<12.6, "1.MEB",
  ifelse(Lon>13.1 & Lon<14.3, "2.ARK",
  ifelse(Lat>57.5 & Lat<58.5 & route %in% c("E", "G"), "4.EGS",
  ifelse(Lat>56.8 & Lat<57.5 & route=="W", "3.WGS",
  ifelse(Lat>58.5 & Lat<59 & Lon>20, "5.NGS",
  ifelse(Lon>22 & Lon<24, "6.WGF",
  ifelse(Lon>24 & Lon<24.5, "7.HGF", "NaN"))))))))

temp <-temp[complete.cases(temp[,pCO2]),]

temp.mean <- temp[,.(
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



df.all <- rbind(df.all, temp)
df.2014 <- temp
mean.all <- rbind(mean.all, temp.mean)
mean.2014 <- temp.mean


setwd("C:/Mueller_Jens_Data/181023_Spring_Summer_2018/data/Finnmaid/Summarized_datasets")
write.csv(df.2014, "df2014.csv")
write.csv(mean.2014, "mean2014.csv")


rm(df, temp.mean, temp, files, file, df.2014, mean.2014)




#### 2015

setwd("C:/Mueller_Jens_Data/181023_Spring_Summer_2018/data/Finnmaid/pCO2-O2-2015")
files <- list.files(pattern = "[.]xls$")
#file <- files[1]

for (file in files){
  
  
  df <- read_excel(file)
  df <- df[c(1,2,3,12,7,4,15,8,5,17)]
  names(df) <- c("date","Lon","Lat","pCO2","Sal","Tem","cO2","patm", "Teq","xCO2")
  df <- df[-c(1),]
  df$date <- as.POSIXct(as.numeric(df$date)*60*60*24, origin="1899-12-30", tz="GMT")
  df$Lon <- as.numeric(as.character(df$Lon))
  df$Lat <- as.numeric(as.character(df$Lat))
  df$pCO2 <- as.numeric(as.character(df$pCO2))
  df$Sal <- as.numeric(as.character(df$Sal))
  df$Tem <- as.numeric(as.character(df$Tem))
  df$cO2 <- as.numeric(as.character(df$cO2))
  df$patm <- as.numeric(as.character(df$patm))
  df$Teq <- as.numeric(as.character(df$Teq))
  df$xCO2 <- as.numeric(as.character(df$xCO2))
  df <- data.table(df)

  df$route <- strapplyc(as.character(file), ".*(.).xls*", simplify = TRUE)
  df$ID <- substr(as.character(file), 3, 10)

  if (exists("temp")){
    temp <- rbind (temp, df)
  } else{temp <- df}
  
}

temp$Area <- with(temp,
  ifelse(Lon>12 & Lon<12.6, "1.MEB",
  ifelse(Lon>13.1 & Lon<14.3, "2.ARK",
  ifelse(Lat>57.5 & Lat<58.5 & route %in% c("E", "G"), "4.EGS",
  ifelse(Lat>56.8 & Lat<57.5 & route=="W", "3.WGS",
  ifelse(Lat>58.5 & Lat<59 & Lon>20, "5.NGS",
  ifelse(Lon>22 & Lon<24, "6.WGF",
  ifelse(Lon>24 & Lon<24.5, "7.HGF", "NaN"))))))))

temp <-temp[complete.cases(temp[,pCO2]),]

temp.mean <- temp[,.(
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



df.all <- rbind(df.all, temp)
df.2015 <- temp
mean.all <- rbind(mean.all, temp.mean)
mean.2015 <- temp.mean


setwd("C:/Mueller_Jens_Data/181023_Spring_Summer_2018/data/Finnmaid/Summarized_datasets")
write.csv(df.2015, "df2015.csv")
write.csv(mean.2015, "mean2015.csv")


rm(df, temp.mean, temp, files, file, df.2015, mean.2015)



#### 2016

setwd("C:/Mueller_Jens_Data/181023_Spring_Summer_2018/data/Finnmaid/pCO2-O2-2016")
files <- list.files(pattern = "[.]xls$")
#file <- files[1]

for (file in files){
  
  
  df <- read_excel(file)
  df <- df[c(1,2,3,12,7,4,15,8,5,17)]
  names(df) <- c("date","Lon","Lat","pCO2","Sal","Tem","cO2","patm", "Teq","xCO2")
  df <- df[-c(1),]
  df$date <- as.POSIXct(as.numeric(df$date)*60*60*24, origin="1899-12-30", tz="GMT")
  df$Lon <- as.numeric(as.character(df$Lon))
  df$Lat <- as.numeric(as.character(df$Lat))
  df$pCO2 <- as.numeric(as.character(df$pCO2))
  df$Sal <- as.numeric(as.character(df$Sal))
  df$Tem <- as.numeric(as.character(df$Tem))
  df$cO2 <- as.numeric(as.character(df$cO2))
  df$patm <- as.numeric(as.character(df$patm))
  df$Teq <- as.numeric(as.character(df$Teq))
  df$xCO2 <- as.numeric(as.character(df$xCO2))
  df <- data.table(df)

  df$route <- strapplyc(as.character(file), ".*(.).xls*", simplify = TRUE)
  df$ID <- substr(as.character(file), 3, 10)

  if (exists("temp")){
    temp <- rbind (temp, df)
  } else{temp <- df}
  
}

temp$Area <- with(temp,
  ifelse(Lon>12 & Lon<12.6, "1.MEB",
  ifelse(Lon>13.1 & Lon<14.3, "2.ARK",
  ifelse(Lat>57.5 & Lat<58.5 & route %in% c("E", "G"), "4.EGS",
  ifelse(Lat>56.8 & Lat<57.5 & route=="W", "3.WGS",
  ifelse(Lat>58.5 & Lat<59 & Lon>20, "5.NGS",
  ifelse(Lon>22 & Lon<24, "6.WGF",
  ifelse(Lon>24 & Lon<24.5, "7.HGF", "NaN"))))))))

temp <-temp[complete.cases(temp[,pCO2]),]

temp.mean <- temp[,.(
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



df.all <- rbind(df.all, temp)
df.2016 <- temp
mean.all <- rbind(mean.all, temp.mean)
mean.2016 <- temp.mean


# ggplot(mean.2016[Area=="5.NGS"], aes(date, mean.pCO2))+
#   geom_point()


setwd("C:/Mueller_Jens_Data/181023_Spring_Summer_2018/data/Finnmaid/Summarized_datasets")
write.csv(df.2016, "df2016.csv")
write.csv(mean.2016, "mean2016.csv")


rm(df, temp.mean, temp, files, file, df.2016, mean.2016)



#### 2017

setwd("C:/Mueller_Jens_Data/181023_Spring_Summer_2018/data/Finnmaid/pCO2-O2-2017")
files <- list.files(pattern = "[.]xls$")
#file <- files[1]

for (file in files){
  
  
  df <- read_excel(file)
  df <- df[c(1,2,3,12,7,4,15,8,5,17)]
  names(df) <- c("date","Lon","Lat","pCO2","Sal","Tem","cO2","patm", "Teq","xCO2")
  df <- df[-c(1),]
  df$date <- as.POSIXct(as.numeric(df$date)*60*60*24, origin="1899-12-30", tz="GMT")
  df$Lon <- as.numeric(as.character(df$Lon))
  df$Lat <- as.numeric(as.character(df$Lat))
  df$pCO2 <- as.numeric(as.character(df$pCO2))
  df$Sal <- as.numeric(as.character(df$Sal))
  df$Tem <- as.numeric(as.character(df$Tem))
  df$cO2 <- as.numeric(as.character(df$cO2))
  df$patm <- as.numeric(as.character(df$patm))
  df$Teq <- as.numeric(as.character(df$Teq))
  df$xCO2 <- as.numeric(as.character(df$xCO2))
  df <- data.table(df)

  df$route <- strapplyc(as.character(file), ".*(.).xls*", simplify = TRUE)
  df$ID <- substr(as.character(file), 3, 10)

  if (exists("temp")){
    temp <- rbind (temp, df)
  } else{temp <- df}
  
}

temp$Area <- with(temp,
  ifelse(Lon>12 & Lon<12.6, "1.MEB",
  ifelse(Lon>13.1 & Lon<14.3, "2.ARK",
  ifelse(Lat>57.5 & Lat<58.5 & route %in% c("E", "G"), "4.EGS",
  ifelse(Lat>56.8 & Lat<57.5 & route=="W", "3.WGS",
  ifelse(Lat>58.5 & Lat<59 & Lon>20, "5.NGS",
  ifelse(Lon>22 & Lon<24, "6.WGF",
  ifelse(Lon>24 & Lon<24.5, "7.HGF", "NaN"))))))))

temp <-temp[complete.cases(temp[,pCO2]),]

temp.mean <- temp[,.(
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



df.all <- rbind(df.all, temp)
df.2017 <- temp
mean.all <- rbind(mean.all, temp.mean)
mean.2017 <- temp.mean


ggplot(mean.2017[Area=="5.NGS"], aes(date, mean.pCO2))+
geom_point()


setwd("C:/Mueller_Jens_Data/181023_Spring_Summer_2018/data/Finnmaid/Summarized_datasets")
write.csv(df.2017, "df2017.csv")
write.csv(mean.2017, "mean2017.csv")


rm(df, temp.mean, temp, files, file, df.2017, mean.2017)




### 2018, except files recorded with Los Gatos Sensor

setwd("C:/Mueller_Jens_Data/181023_Spring_Summer_2018/data/Finnmaid/pCO2-O2-2018")
files <- list.files(pattern = "[.]xls$")
#file <-files[1]

for (file in files){
  
  
  df <- read_excel(file)
  df <- df[c(1,2,3,12,7,4,15,8,5,17)]
  names(df) <- c("date","Lon","Lat","pCO2","Sal","Tem","cO2","patm", "Teq","xCO2")
  df <- df[-c(2),]
  df$date <- as.POSIXct(as.numeric(df$date)*60*60*24, origin="1899-12-30", tz="GMT")
  df$Lon <- as.numeric(as.character(df$Lon))
  df$Lat <- as.numeric(as.character(df$Lat))
  df$pCO2 <- as.numeric(as.character(df$pCO2))
  df$Sal <- as.numeric(as.character(df$Sal))
  df$Tem <- as.numeric(as.character(df$Tem))
  df$cO2 <- as.numeric(as.character(df$cO2))
  df$patm <- as.numeric(as.character(df$patm))
  df$Teq <- as.numeric(as.character(df$Teq))
  df$xCO2 <- as.numeric(as.character(df$xCO2))
  df <- data.table(df)
  
  df$route <- strapplyc(as.character(file), ".*(.).xls*", simplify = TRUE)
  df$ID <- substr(as.character(file), 3, 10)
  
  if (exists("temp")){
    temp <- rbind (temp, df)
  } else{temp <- df}
  
}


rm(df, files, file)
temp <- temp[pCO2 != 0]




#### 2018, data files from Los Gatos Sensor

setwd("C:/Mueller_Jens_Data/181023_Spring_Summer_2018/data/Finnmaid/pCO2-O2-2018/LGR")
files <- list.files(pattern = "[.]xls$")
#file <-files[1]


for (file in files){
  
  
  df <- read_excel(file)
  df <- df[c(2,3,4,8,6,5,14,7,15,9)]
  names(df) <- c("date","Lon","Lat","pCO2","Sal","Tem","cO2","patm", "Teq","xCO2")
  df <- df[-c(1),]
  df$date <- dmy_hms(df$date)
  df <- data.table(df)
  
  df$route <- substr(as.character(file), 12, 12)
  df$ID <- substr(as.character(file), 3, 10)
  
  if (exists("temp.LGR")){
    temp.LGR <- rbind (temp.LGR, df)
  } else{temp.LGR <- df}
  
}





# temp$sensor <- "LICOR"
# temp.LGR$sensor <- "LosGatos"

temp <- rbind(temp, temp.LGR)

rm(temp.LGR, df, file, files)


temp$Area <- with(temp,
                  ifelse(Lon>12 & Lon<12.6, "1.MEB",
                  ifelse(Lon>13.1 & Lon<14.3, "2.ARK",
                  ifelse(Lat>57.5 & Lat<58.5 & route %in% c("E", "G"), "4.EGS",
                  ifelse(Lat>56.8 & Lat<57.5 & route=="W", "3.WGS",
                  ifelse(Lat>58.5 & Lat<59 & Lon>20, "5.NGS",
                  ifelse(Lon>22 & Lon<24, "6.WGF",
                  ifelse(Lon>24 & Lon<24.5, "7.HGF", "NaN"))))))))



temp <-temp[complete.cases(temp[,pCO2]),]

temp.mean <- temp[,.(
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


df.all <- rbind(df.all, temp)
df.2018 <- temp
mean.all <- rbind(mean.all, temp.mean)
mean.2018 <- temp.mean


# ggplot(mean.2018[Area=="5.NGS"], aes(date, mean.pCO2))+
#   geom_point()


setwd("C:/Mueller_Jens_Data/181023_Spring_Summer_2018/data/Finnmaid/Summarized_datasets")
write.csv(df.2018, "df2018.csv")
write.csv(mean.2018, "mean2018.csv")


rm(df, temp.mean, temp, files, file, df.2018, mean.2018)



 
###############################################





#   all data

mean.all$year <- year(mean.all$date)


setwd("C:/Mueller_Jens_Data/181023_Spring_Summer_2018/data/Finnmaid/Summarized_datasets")
write.csv(df.all, "dfall.csv")
write.csv(mean.all, "meanall.csv")


# library(viridis)
# setorder(mean.all, year, date)
# 
# Fig <-
# ggplot()+
#   geom_hline(yintercept=100)+
#   geom_point(data=mean.all[year != 2018], aes(yday(date), mean.pCO2, col=as.factor(year)))+
#   geom_path(data=mean.all[year != 2018], aes(yday(date), mean.pCO2, col=as.factor(year)))+
#   geom_point(data=mean.all[year == 2018], aes(yday(date), mean.pCO2), col="red")+
#   geom_path(data=mean.all[year == 2018], aes(yday(date), mean.pCO2), col="red")+
#   scale_color_grey(name="year")+
#   facet_wrap(~Area)+
#   coord_cartesian(ylim = c(0,400), xlim = c(60, 240))
# 
# 
# setwd("C:/Mueller_Jens_Data/181023_Spring_Summer_2018/plots")
# tiff("MeanPCO2_FacetArea_AllYears.tiff", width = 350, height = 250, units = 'mm', res = 600, compression = 'lzw')
# Fig
# dev.off()
# rm(Fig)
# 
# 
# Fig <-
# ggplot()+
#   geom_hline(yintercept=20)+
#   geom_point(data=mean.all[year != 2018], aes(yday(date), mean.Tem, col=as.factor(year)))+
#   geom_path(data=mean.all[year != 2018], aes(yday(date), mean.Tem, col=as.factor(year)))+
#   geom_point(data=mean.all[year == 2018], aes(yday(date), mean.Tem), col="red")+
#   geom_path(data=mean.all[year == 2018], aes(yday(date), mean.Tem), col="red")+
#   scale_color_grey(name="year")+
#   facet_wrap(~Area)+
#   coord_cartesian(ylim = c(0,26), xlim = c(60, 240))
# 
# 
# setwd("C:/Mueller_Jens_Data/181023_Spring_Summer_2018/plots")
# tiff("MeanTem_FacetArea_AllYears.tiff", width = 350, height = 250, units = 'mm', res = 600, compression = 'lzw')
# Fig
# dev.off()
# rm(Fig)
# 
