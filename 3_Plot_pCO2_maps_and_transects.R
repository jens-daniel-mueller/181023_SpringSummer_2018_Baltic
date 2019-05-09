# Packages ----------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(geosphere)

# Load Finnmaid climatological data ---------------------------------------

df_climate <- read_csv(here::here("Data/_summarized_data", "Finnmaid_climatology_and_2018.csv"),
                       col_types = list(pCO2_int = col_factor()))

df_climate_merge <- read_csv(here::here("Data/_summarized_data", "Finnmaid_dpCO2_climatology_vs_2018.csv"),
                             col_types = list(dpCO2 = col_factor()))


# Define subarea by distance limits from Travemuende ----------------------

dist_low <- 600
dist_high <- 860


# Plot pCO2 transects 2018 showing springbloom evolution ------------------

format(df_climate$day_date, "%b %d")

df_climate %>% 
  filter(time_int %in% seq(100,160,1)) %>% 
  ggplot(aes(dist_int, pCO2, col=as.factor(format(day_date,"%b %d"))))+
  geom_vline(xintercept = c(dist_low, dist_high), col="red")+
  annotate("rect", xmin=dist_low, xmax=dist_high, ymin=-Inf, ymax=Inf, alpha=0.3)+
  geom_line()+
  scale_color_viridis_d(direction = -1, name="Date")+
  labs(x="Distance Travemuende (km)", y=expression(pCO[2]~(µatm)))+
  theme_bw()+
  facet_wrap(~period)+
  scale_x_continuous(breaks = seq(100,2000,200))+
  scale_y_continuous(limits = c(0,450))

ggsave(here::here("Plots","pCO2_transects.pdf"),
       width = 10, height = 5)



# Plot surface maps -------------------------------------------------------

basemap <- map_data('world', xlim = c(4, 29), ylim = c(50, 66))
land.colour   <- "grey75"
border.colour <- "grey10"
xmin= 10
xmax= 25.5
ymin=53.5
ymax=60.5


df_climate %>% 
  filter(time_int == 130, period == "2018") %>% 
  ggplot(aes(Lon, Lat, col=dist_int))+
  geom_point()+
  scale_color_viridis_c(name="Distance from Travemuende (km)")+
  coord_quickmap(xlim=c(xmin, xmax), ylim=c(ymin, ymax)) +
  geom_polygon(data=basemap, aes(x=long, y=lat, group=group), 
               fill=land.colour, colour = border.colour, lwd=.5)+
  labs(x="Lon (°E)", y="Lat (°N)")+
  theme_bw()+
  theme(legend.position = "top")

ggsave(here::here("Plots", "Map_track_distance_intervals.pdf"), width = 7, height = 4)

df_climate %>% 
  filter(time_int == 130) %>% 
  ggplot(aes(Lon, Lat, col=pCO2))+
  geom_point()+
  geom_point(data = df_climate[df_climate$time_int == 130 &
                                 df_climate$period == "2018" &
                                 df_climate$dist_int < dist_low &
                                 df_climate$dist_int > dist_high,],
             aes(Lon, Lat), shape=21, col="black")+
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
