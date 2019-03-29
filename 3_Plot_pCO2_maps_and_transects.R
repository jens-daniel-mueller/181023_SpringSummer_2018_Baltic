#### load required libraries #### 

library(tidyverse)
library(lubridate)
library(here)
library(geosphere)

#### load complete Finnmaid data set and subset relevant information ####

df <- read_csv(here::here("Data/Finnmaid/_summarized_data", "dfall.csv"))

df <- df %>% 
  filter(route == "E") %>% 
  select(date, year, day, ID, Lon, Lat, Sal, Tem, pCO2) %>% 
  mutate(period = if_else(year == 2018, "2018", "2003-2017"))

#### Define subarea by distance limits from Helsinki ####

dist_low <- 450
dist_high <- 250

#### Define distance intervals starting from Helsinki ####

Hel <- c(24.945831, 60.192059)
km <- 20

df <- df %>% 
  mutate(dist = distGeo(cbind(Lon, Lat), Hel)/1e3,
         dist_int = as.numeric(as.character(
           cut(dist, seq(0, 1200, km),
               labels = seq(km/2, 1200-km/2, km)))))


#### Define temporal intervals in days ####

days <- 7
df <- df %>% 
  mutate(day = as.integer(day),
         time_int = as.numeric(as.character( cut(day, seq(0,365,days),
                        labels = seq((days+1)/2,366-(days/2),days)))))


#### Calculate mean delta pCO2 in time and distance intervals for 2003-2017 vs 2018 periods ####

df_climate <- df %>% 
  select(period, dist_int, time_int, Lat, Lon, pCO2) %>% 
  group_by(period, dist_int, time_int) %>% 
  summarise_all(list("mean", "max", "min", "sd"), na.rm=TRUE) %>% 
  ungroup() %>% 
  filter(!is.na(period))

df_climate_2018 <- df_climate %>%
  filter(period == "2018")
  
df_climate_2003_17 <- df_climate %>%
  filter(period == "2003-2017")
  
df_climate_merge <- full_join(df_climate_2018, df_climate_2003_17, by=c("dist_int","time_int"))

rm(df_climate_2003_17, df_climate_2018)

df_climate_merge <- df_climate_merge %>% 
  mutate(dpCO2 = pCO2_mean.x - pCO2_mean.y,
         dpCO2 = cut(dpCO2, c(Inf,0,-50,-100,-150,-Inf),
                     labels = rev(c("<0","0-50","50-100","100-150",">150"))))

#### Plot pCO2 transects showing springbloom evolution ####

df_climate %>% 
  filter(time_int %in% seq(100,160,1)) %>% 
  ggplot(aes(dist_int, pCO2_mean, col=as.factor(time_int)))+
  geom_vline(xintercept = c(dist_low, dist_high), col="red")+
  annotate("rect", xmin=dist_low, xmax=dist_high, ymin=-Inf, ymax=Inf, alpha=0.3)+
  geom_line()+
  scale_color_viridis_d(direction = -1, name="Day of year")+
  labs(x="Distance Helsinki (km)", y="pCO2 (µatm)",
       title = paste("Transects | ",km,"km | ",days,"days"))+
  theme_bw()+
  facet_wrap(~period)

ggsave(here::here("Plots", paste("pCO2_transects_Distance_",km,"_days_",days,".jpg", sep = "")),
       width = 10, height = 5)


#### Plot surface maps ####

basemap <- map_data('world', xlim = c(4, 29), ylim = c(50, 66))
land.colour   <- "grey75"
border.colour <- "grey10"
xmin= 10
xmax= 25.5
ymin=53.5
ymax=60.5


df_climate_merge %>% 
  filter(time_int == 130, !is.na(dpCO2)) %>% 
  ggplot(aes(Lon_mean.y, Lat_mean.y, col=dpCO2))+
  geom_point()+
  scale_color_viridis_d(direction = -1)+
  coord_quickmap(xlim=c(xmin, xmax), ylim=c(ymin, ymax)) +
  geom_polygon(data=basemap, aes(x=long, y=lat, group=group), 
               fill=land.colour, colour = border.colour, lwd=.5)+
  labs(x="Lon (°E)", y="Lat (°N)")+
  theme_bw()

ggsave(here::here("Plots", "Map_dpCO2_2018_day_130.jpg"), width = 7, height = 6)

df_climate %>% 
  filter(time_int == 130) %>% 
  ggplot(aes(Lon_mean, Lat_mean, col=pCO2_mean))+
  geom_point()+
  geom_point(data = df_climate[df_climate$time_int == 130 &
                                 df_climate$period == "2018" &
                                 df_climate$dist_int < dist_low &
                                 df_climate$dist_int > dist_high,],
             aes(Lon_mean, Lat_mean), shape=21, col="black")+
  scale_color_viridis_c(direction = -1, name="pCO2 (µatm)")+
  coord_quickmap(xlim=c(xmin, xmax), ylim=c(ymin, ymax)) +
  geom_polygon(data=basemap, aes(x=long, y=lat, group=group), 
               fill=land.colour, colour = border.colour, lwd=.5)+
  labs(x="Lon (°E)", y="Lat (°N)")+
  theme_bw()+
  facet_wrap(~period)

ggsave(here::here("Plots", "Map_pCO2_day_130.jpg"), width = 7, height = 4)



df_climate %>% 
  filter(period == "2018", pCO2_mean < 400) %>% 
  ggplot(aes(Lon_mean, Lat_mean, col=pCO2_mean))+
  geom_point()+
  scale_color_viridis_c(direction = -1, name="pCO2 (µatm)")+
  coord_quickmap(xlim=c(xmin, xmax), ylim=c(ymin, ymax)) +
  geom_polygon(data=basemap, aes(x=long, y=lat, group=group), 
               fill=land.colour, colour = border.colour, lwd=.5)+
  labs(x="Lon (°E)", y="Lat (°N)",
       title = paste("Distance interval:",km,"km | Time interval:",days,"days | pCO2 < 400 µatm"))+
  theme_bw()+
  facet_wrap(~time_int)

ggsave(here::here("Plots", "Map_pCO2_all_days.jpg"), width = 12, height = 12)
