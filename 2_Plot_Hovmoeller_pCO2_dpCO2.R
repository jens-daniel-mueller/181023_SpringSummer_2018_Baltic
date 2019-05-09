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


# Produce Hovmoeller plots ------------------------------------------------

df_climate_merge %>% 
  filter(time_int > 90, time_int < 180, !is.na(dpCO2)) %>% 
  ggplot(aes(day_date, dist_int, fill=dpCO2))+
  geom_raster()+
  geom_vline(xintercept = ymd_h("2000/05/09 T00"), col="red")+
  geom_hline(yintercept = c(dist_high, dist_low), col="red")+
  scale_fill_viridis_d(name = expression(Delta~pCO2~(µatm)))+
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
  scale_fill_viridis_d(name="pCO2 (µatm)")+
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



