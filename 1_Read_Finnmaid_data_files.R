library(ggplot2)
library(data.table)
library(readxl)
library(gsubfn)
library(lubridate)


### 2003

setwd("C:/Mueller_Jens_Data/181023_Spring_Summer_2018/181023_SpringSummer_2018_Baltic_Rproject_local/Data/Finnmaid/pCO2-2003")
files <- list.files(pattern = "[.]xls$")

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


df.all <- temp
rm(df, temp, files, file)

#########################################################################################

#### 2004

setwd("C:/Mueller_Jens_Data/181023_Spring_Summer_2018/181023_SpringSummer_2018_Baltic_Rproject_local/Data/Finnmaid/pCO2-2004")
files <- list.files(pattern = "[.]xls$")

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


df.all <- rbind(df.all, temp)
rm(df, temp, files, file)

#########################################################################################

#### 2005

setwd("C:/Mueller_Jens_Data/181023_Spring_Summer_2018/181023_SpringSummer_2018_Baltic_Rproject_local/Data/Finnmaid/pCO2-2005")
files <- list.files(pattern = "[.]xls$")

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


df.all <- rbind(df.all, temp)
rm(df, temp, files, file)

#########################################################################################

#### 2006

setwd("C:/Mueller_Jens_Data/181023_Spring_Summer_2018/181023_SpringSummer_2018_Baltic_Rproject_local/Data/Finnmaid/pCO2-2006")
files <- list.files(pattern = "[.]xls$")

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


df.all <- rbind(df.all, temp)
rm(df, temp, files, file)

#########################################################################################

#### 2007

setwd("C:/Mueller_Jens_Data/181023_Spring_Summer_2018/181023_SpringSummer_2018_Baltic_Rproject_local/Data/Finnmaid/pCO2-2007")
files <- list.files(pattern = "[.]xls$")

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


df.all <- rbind(df.all, temp)
rm(df, temp, files, file)

#########################################################################################

#### 2008

setwd("C:/Mueller_Jens_Data/181023_Spring_Summer_2018/181023_SpringSummer_2018_Baltic_Rproject_local/Data/Finnmaid/pCO2-2008")
files <- list.files(pattern = "[.]xls$")

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


df.all <- rbind(df.all, temp)
rm(df, temp, files, file)

#########################################################################################

#### 2009a

setwd("C:/Mueller_Jens_Data/181023_Spring_Summer_2018/181023_SpringSummer_2018_Baltic_Rproject_local/Data/Finnmaid/pCO2-2009a")
files <- list.files(pattern = "[.]xls$")

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

df.all <- rbind(df.all, temp)
rm(df, temp, files, file)

#########################################################################################

#### 2009b

setwd("C:/Mueller_Jens_Data/181023_Spring_Summer_2018/181023_SpringSummer_2018_Baltic_Rproject_local/Data/Finnmaid/pCO2-2009b")
files <- list.files(pattern = "[.]xls$")

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


df.all <- rbind(df.all, temp)
rm(df, temp, files, file)

#########################################################################################

#### 2010

setwd("C:/Mueller_Jens_Data/181023_Spring_Summer_2018/181023_SpringSummer_2018_Baltic_Rproject_local/Data/Finnmaid/pCO2-2010")
files <- list.files(pattern = "[.]xls$")

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


df.all <- rbind(df.all, temp)
rm(df, temp, files, file)

#########################################################################################

#### 2011

setwd("C:/Mueller_Jens_Data/181023_Spring_Summer_2018/181023_SpringSummer_2018_Baltic_Rproject_local/Data/Finnmaid/pCO2-2011")
files <- list.files(pattern = "[.]xls$")

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


df.all <- rbind(df.all, temp)
rm(df, temp, files, file)

#########################################################################################

#### 2012

setwd("C:/Mueller_Jens_Data/181023_Spring_Summer_2018/181023_SpringSummer_2018_Baltic_Rproject_local/Data/Finnmaid/pCO2-O2-2012")
files <- list.files(pattern = "[.]xls$")

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

df.all <- rbind(df.all, temp)
rm(df, temp, files, file)

#########################################################################################

#### 2013

setwd("C:/Mueller_Jens_Data/181023_Spring_Summer_2018/181023_SpringSummer_2018_Baltic_Rproject_local/Data/Finnmaid/pCO2-O2-2013")
files <- list.files(pattern = "[.]xls$")

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


df.all <- rbind(df.all, temp)
rm(df, temp, files, file)

#########################################################################################

#### 2014

setwd("C:/Mueller_Jens_Data/181023_Spring_Summer_2018/181023_SpringSummer_2018_Baltic_Rproject_local/Data/Finnmaid/pCO2-O2-2014")
files <- list.files(pattern = "[.]xls$")

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

df.all <- rbind(df.all, temp)
rm(df, temp, files, file)

#########################################################################################

#### 2015

setwd("C:/Mueller_Jens_Data/181023_Spring_Summer_2018/181023_SpringSummer_2018_Baltic_Rproject_local/Data/Finnmaid/pCO2-O2-2015")
files <- list.files(pattern = "[.]xls$")

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


df.all <- rbind(df.all, temp)
rm(df, temp, files, file)

#########################################################################################

#### 2016

setwd("C:/Mueller_Jens_Data/181023_Spring_Summer_2018/181023_SpringSummer_2018_Baltic_Rproject_local/Data/Finnmaid/pCO2-O2-2016")
files <- list.files(pattern = "[.]xls$")

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

df.all <- rbind(df.all, temp)
rm(df, temp, files, file)

#########################################################################################

#### 2017

setwd("C:/Mueller_Jens_Data/181023_Spring_Summer_2018/181023_SpringSummer_2018_Baltic_Rproject_local/Data/Finnmaid/pCO2-O2-2017")
files <- list.files(pattern = "[.]xls$")

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

df.all <- rbind(df.all, temp)


### 2018, except files recorded with Los Gatos Sensor

setwd("C:/Mueller_Jens_Data/181023_Spring_Summer_2018/181023_SpringSummer_2018_Baltic_Rproject_local/Data/Finnmaid/pCO2-O2-2018")
files <- list.files(pattern = "[.]xls$")

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

setwd("C:/Mueller_Jens_Data/181023_Spring_Summer_2018/181023_SpringSummer_2018_Baltic_Rproject_local/Data/Finnmaid/pCO2-O2-2018/LGR")
files <- list.files(pattern = "[.]xls$")


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


df.all <- rbind(df.all, temp)
rm(temp)

#########################################################################################


df.all <-df.all[complete.cases(df.all[,pCO2]),]
df.all$year <- year(df.all$date)
df.all$day <- yday(df.all$date)


setwd("C:/Mueller_Jens_Data/181023_Spring_Summer_2018/181023_SpringSummer_2018_Baltic_Rproject_local/Data/Finnmaid/_summarized_data")
write.csv(df.all, "dfall.csv", row.names = FALSE)