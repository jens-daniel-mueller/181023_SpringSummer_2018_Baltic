# Packages ----------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(geosphere)


# Load complete Finnmaid data set and subset relevant information ---------

df <- read_csv(here::here("Data/Finnmaid/_summarized_data",
                          "Finnmaid_all_2019.csv"))

df <- df %>% 
  mutate(year = year(date),
         day = yday(date)) %>% 
  filter(route == "E", year <= 2018) %>% 
  select(date, year, day, ID, Lon, Lat, Sal, Tem, pCO2) %>% 
  mutate(period = if_else(year == 2018, "2018", "2003-2017"))


# Assign distance intervals starting from Travemünde ----------------------

Trave <- c(10.8605315, 53.9414096)
km <- 20

df <- df %>% 
  mutate(dist = distGeo(cbind(Lon, Lat), Trave)/1e3,
         dist_int = as.numeric(as.character(
           cut(dist, seq(0, 1200, km),
               labels = seq(km/2, 1200-km/2, km)))))

# Assign temporal intervals in days ---------------------------------------

days <- 7

df <- df %>% 
  mutate(day = as.integer(day),
         time_int = as.numeric(as.character( cut(day, seq(0,365,days),
                        labels = seq((days+1)/2,366-(days/2),days)))))

# Calculate climatological mean pCO2 --------------------------------------
# in time and distance intervals for 2003-2017 and 2018 periods

df_climate <- df %>% 
  select(period, dist_int, time_int, Lat, Lon, pCO2, Tem) %>% 
  group_by(period, dist_int, time_int) %>% 
  summarise_all(list("mean"), na.rm=TRUE) %>% 
  ungroup() %>% 
  filter(!is.na(period)) %>% 
  mutate(pCO2_int = cut(pCO2, c(seq(0,400,50),Inf),
                        labels = c("0-50","50-100","100-150","150-200","200-250",
                                   "250-300","300-350","350-400",">400")))
df_climate <- df_climate %>% 
  mutate(day_date = as.POSIXct(strptime(paste(2000,time_int), format = "%Y %j",tz="GMT")))


# calculate delta pCO2 2018 - climatological mean

df_climate_2018 <- df_climate %>%
  filter(period == "2018")
  
df_climate_2003_17 <- df_climate %>%
  filter(period == "2003-2017")
  
df_climate_merge <- full_join(df_climate_2018, df_climate_2003_17, by=c("dist_int","time_int","day_date"))

rm(df_climate_2003_17, df_climate_2018)

df_climate_merge <- df_climate_merge %>% 
  mutate(dpCO2 = pCO2.x - pCO2.y,
         dpCO2 = cut(dpCO2, c(Inf,0,-50,-100,-150,-Inf),
                     labels = rev(c("<0","0-50","50-100","100-150",">150")))) %>% 
  select(dist_int, time_int, day_date, dpCO2)


df_climate %>% 
write_csv(here::here("Data/_summarized_data", "Finnmaid_climatology_and_2018.csv"))

df_climate_merge %>% 
write_csv(here::here("Data/_summarized_data", "Finnmaid_dpCO2_climatology_vs_2018.csv"))


# Produce Hovmoeller plots ------------------------------------------------

df_climate_merge %>% 
  filter(time_int > 90, time_int < 180, !is.na(dpCO2)) %>% 
  ggplot(aes(day_date, dist_int, fill=dpCO2))+
  geom_raster()+
  # geom_vline(xintercept = ymd_h("2000/05/09 T00"), col="red")+
  # geom_hline(yintercept = c(dist_high, dist_low), col="red")+
  scale_fill_viridis_d(direction = -1, name = expression(Delta~pCO2~(µatm)))+
  labs(x="", y="Distance Travemünde (km)")+
  theme_bw()+
  scale_x_datetime(expand = c(0,0))+
  scale_y_continuous(expand = c(0,0), breaks = seq(100,2000,200))

ggsave(here::here("Plots", "delta_pCO2_climatology_vs_2018.pdf"),
       width = 5, height = 3)


df_climate %>% 
  filter(time_int > 90, time_int < 180) %>% 
  ggplot(aes(day_date, dist_int, fill=pCO2_int))+
  geom_raster()+
  geom_vline(xintercept = ymd_h("2000/05/09 T00"), col="red")+
  geom_hline(yintercept = c(dist_high, dist_low), col="red")+
  scale_fill_viridis_d(direction = -1, name="pCO2 (µatm)")+
  labs(x="", y="Distance Travemuende (km)")+
  theme_bw()+
  facet_wrap(~period)+
  scale_x_datetime(expand = c(0,0))+
  scale_y_continuous(expand = c(0,0), breaks = seq(100,2000,200))

ggsave(here::here("Plots", "pCO2_climatology_vs_2018.pdf"),
       width = 8, height = 4)

# 
# df_climate %>%
#   filter(time_int > 90, time_int < 180) %>%
#   ggplot(aes(time_int, dist_int, fill=Tem))+
#   geom_raster()+
#   geom_vline(xintercept = 130, col="red")+
#   geom_hline(yintercept = c(dist_high, dist_low), col="red")+
#   scale_fill_viridis_c(option = "B",name="Tem")+
#   labs(x="Day of year", y="Distance Travemuende (km)",
#        title = paste("Distance interval:",km,"km | Time interval:",days,"days"))+
#   theme_bw()+
#   facet_wrap(~period)+
#   scale_x_continuous(expand = c(0,0))+
#   scale_y_continuous(expand = c(0,0))
# 
# ggsave(here::here("Plots/pCO2_distribution", paste("Distance_",km,"_days_",days,".jpg", sep = "")),
#        width = 8, height = 4)



