library(data.table)
library(seacarb)
library(ggplot2)
library(lubridate)
library(viridis)


setwd("C:/Mueller_Jens_Data/181023_Spring_Summer_2018/data/Finnmaid/Summarized_datasets")

df <- data.table(read.csv("df_mean_area_CT.csv"))
df$date <- ymd_hms(df$date)


#### interpolation all years


for (i in levels(df$Area)){
  
  pCO2 <- approxfun(df[Area == i]$date, df[Area == i]$mean.pCO2)
  CT <- approxfun(df[Area == i]$date, df[Area == i]$mean.CT)
  #cO2 <- approxfun(df09[Area == i]$date, df09[Area == i]$mean.cO2)
  Sal <- approxfun(df[Area == i]$date, df[Area == i]$mean.Sal)
  Tem <- approxfun(df[Area == i]$date, df[Area == i]$mean.Tem)
  patm <- approxfun(df[Area == i]$date, df[Area == i]$mean.patm)
  
  interval <- as.POSIXct(strptime("2003-01-01 12:00:00", format="%Y-%m-%d %H:%M:%S", tz="GMT"))+
    (days(seq(1,365*16,1)))
  
  temp <- cbind(data.table(interval),
                data.table(i),
                data.table(pCO2(interval)),
                data.table(CT(interval)),
                #data.table(cO2(interval)),
                data.table(Sal(interval)),
                data.table(Tem(interval)),
                data.table(patm(interval)))
  names(temp) <- c("date", "Area", "int.pCO2", "int.CT", "int.Sal", "int.Tem", "int.patm")
  
  if (exists("ts.int")){
    ts.int <- rbind (ts.int, temp)
  } else{ts.int <- temp}
  
  rm(temp, interval, pCO2, CT, Sal, Tem, patm)
  
}

rm(i)

setwd("C:/Mueller_Jens_Data/181023_Spring_Summer_2018/plots/interpolation_08-18")

# reg <- levels(df$Area)[4]
# var <- "CT"

for (var in c("pCO2", "CT", "Sal", "Tem", "patm"))
  for (reg in levels(df$Area))
  {
    mean.var <- paste("mean", var, sep = ".")
    int.var <- paste("int", var, sep = ".")

    ggplot(df)+
      geom_point(data=df[Area == reg], aes(date, get(mean.var), col="measured"), size=0.2)+
      geom_line(data=df[Area == reg], aes(date, get(mean.var), col="measured"), size=0.2)+
      geom_point(data=ts.int[Area == reg], aes(date, get(int.var), col="interpolated"), size=0.2)+
      scale_color_manual(values = c("black", "red"), name="Values")+
      scale_x_datetime(date_breaks = "2 months", date_labels = "%b")+
      labs(y=var, title=reg)+
      facet_wrap(~year(date), scales = "free_x")+
      theme_bw()
    
    ggsave(paste("./interpolation_2004-2018",var,reg,".pdf", sep="_"), width = 300, height = 300, units = 'mm', dpi = 300)

  }


setwd("C:/Mueller_Jens_Data/181023_Spring_Summer_2018/data/Finnmaid/Summarized_datasets")
write.csv(ts.int, "mean_all_interpolated_2004-2018.csv", row.names = FALSE)
