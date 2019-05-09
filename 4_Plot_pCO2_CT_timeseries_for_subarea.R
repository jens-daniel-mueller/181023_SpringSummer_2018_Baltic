# Packages ----------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(geosphere)
library(seacarb)

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



# Define subarea by distance limits from Travemuende ----------------------

dist_low <- 600
dist_high <- 860

df <- df %>% 
  filter(dist_int > dist_low, dist_int < dist_high)



# Define temporal intervals in days ---------------------------------------

days <- 7
df <- df %>% 
  mutate(day = as.integer(day),
         time_int = as.numeric(as.character( cut(day, seq(0,365,days),
                        labels = seq((days+1)/2,366-(days/2),days)))))



# Calculate mean pCO2, SST and CT in subarea for each crossing ------------
sal_mean <- mean(df$Sal, na.rm = TRUE)

df_area <- df %>% 
  select(ID, date, Sal, Tem, pCO2) %>% 
  filter(!is.na(pCO2),
         !is.na(Sal),
         !is.na(Tem)) %>% 
  mutate(CT = carb(24, var1=pCO2, var2=1630*1e-6,
                   S=sal_mean, T=Tem, k1k2="m10", kf="dg", ks="d",
                   pHscale="T", gas="insitu")[,16]*1e6) %>% 
  group_by(ID) %>% 
  summarise_all(list("mean"), na.rm=TRUE) %>% 
  ungroup() %>% 
  mutate(year = as.factor(year(date)),
         day_date = as.POSIXct(strptime(paste(2000,yday(date)), format = "%Y %j",tz="GMT")))



# Calculate CT and SST climatology in subarea -----------------------------

df_climate <- df_area %>% 
  filter(year != 2018) %>%
  mutate(day = as.integer(yday(date)),
         time_int = as.numeric(as.character( cut(day, seq(0,365,days),
                                                 labels = seq((days+1)/2,366-(days/2),days))))) %>% 
  select(time_int, Tem, Sal, pCO2, CT) %>% 
  group_by(time_int) %>% 
  summarise_all(list("mean", "max", "min", "sd"), na.rm=TRUE) %>% 
  ungroup() %>% 
  mutate(day_date = as.POSIXct(strptime(paste(2000,time_int), format = "%Y %j",tz="GMT")))



#### Plot time series ####

theme_set(theme_bw())



shadesOfGreyRed <- c(rev(colorRampPalette(c("grey35", "grey75"))(
  length(levels(df_area$year))-1
)), "red")


start <- as.POSIXct(strptime("2000/03/01", format = "%Y/%m/%d",tz="GMT"))
end   <- as.POSIXct(strptime("2000/08/31", format = "%Y/%m/%d",tz="GMT"))


df_area %>% 
  filter(year==2018) %>% 
  ggplot()+
  geom_hline(yintercept=100)+
  geom_ribbon(data=df_climate,
              aes(day_date, ymin=pCO2_min, ymax=pCO2_max), fill="lightgrey")+
  geom_ribbon(data=df_climate,
              aes(day_date, ymin=pCO2_mean-pCO2_sd, ymax=pCO2_mean+pCO2_sd), fill="grey")+
  geom_path(data=df_climate, aes(day_date, pCO2_mean, col="2003-2017"))+
  #geom_ribbon(aes(day_date, ymin=pCO2_mean-pCO2_sd, ymax=pCO2_mean+pCO2_sd, fill=year), alpha=0.2)+
  geom_path(aes(day_date, pCO2, col=year))+
  scale_color_manual(values = c("black", "red"), name="Period")+
  scale_x_datetime(date_labels = "%b", date_breaks = "month", name="Month")+
  labs(y=expression(pCO[2]~(µatm)))+
  coord_cartesian(ylim = c(0,500), xlim=c(start, end))+
  theme(legend.position = c(0.8, 0.85),
        legend.background = element_rect(colour="black"))

ggsave(here::here("Plots", "pCO2_timeseries_climatology.pdf"), width = 6, height = 5, dpi=300)

df_area %>% 
  filter(year==2018) %>% 
  ggplot()+
  geom_ribbon(data=df_climate,
              aes(day_date, ymin=CT_min, ymax=CT_max), fill="lightgrey")+
  geom_ribbon(data=df_climate,
              aes(day_date, ymin=CT_mean-CT_sd, ymax=CT_mean+CT_sd), fill="grey")+
  geom_path(data=df_climate, aes(day_date, CT_mean, col="2003-2017"))+
  geom_path(aes(day_date, CT, col=year))+
  scale_color_manual(values = c("black", "red"), name="Period")+
  scale_x_datetime(date_labels = "%b", date_breaks = "month", name="Month")+
  labs(y=expression(CT~"*"~(µmol~kg^{-1})))+
  coord_cartesian(xlim=c(start, end))+
  theme(legend.position = c(0.8, 0.85),
        legend.background = element_rect(colour="black"))

ggsave(here::here("Plots", "CT_timeseries_climatology.pdf"), width = 6, height = 5, dpi=300)





df_area %>% 
  filter(year==2018) %>% 
  ggplot()+
  geom_ribbon(data=df_climate,
              aes(day_date, ymin=Tem_min, ymax=Tem_max), fill="lightgrey")+
  geom_ribbon(data=df_climate,
              aes(day_date, ymin=Tem_mean-Tem_sd, ymax=Tem_mean+Tem_sd), fill="grey")+
  geom_path(data=df_climate, aes(day_date, Tem_mean, col="2003-2017"))+
  #geom_ribbon(aes(day_date, ymin=Tem_mean-Tem_sd, ymax=Tem_mean+Tem_sd, fill=year), alpha=0.2)+
  geom_path(aes(day_date, Tem, col=year))+
  scale_color_manual(values = c("black", "red"), name="Period")+
  scale_x_datetime(date_labels = "%b", date_breaks = "month", name="Month")+
  labs(y="SST (°C)")+
  coord_cartesian(xlim=c(start, end))+
  theme(legend.position = c(0.85, 0.15),
        legend.background = element_rect(colour="black"))

ggsave(here::here("Plots", "Tem_timeseries_climatology.pdf"), width = 6, height = 5, dpi=300)



# df_area <- df_area %>% 
#   arrange(date_mean) %>% 
#   mutate(no_break = cumsum(c(0,diff(date_mean) > 14*24)))
# 
# 
# df_area %>% 
#   ggplot()+
#   geom_hline(yintercept=100)+
#   geom_path(aes(day_date, pCO2_mean, col=year, group=no_break))+
#   scale_color_manual(values = shadesOfGreyRed, name="Year")+
#   scale_x_datetime(date_labels = "%b", date_breaks = "month",
#                    limits = c(start, end), name="Month")+
#   labs(y=expression(pCO[2]~(µatm)))+
#   coord_cartesian(ylim = c(0,500))
# 
# ggsave(here::here("Plots", "pCO2_timeseries_all_years.jpg"), width = 8, height = 5, dpi=300)
# 
# df_area %>% 
#   ggplot()+
#   geom_path(aes(day_date, CT_mean, col=year, group=no_break))+
#   scale_color_manual(values = shadesOfGreyRed, name="Year")+
#   scale_x_datetime(date_labels = "%b", date_breaks = "month",
#                    limits = c(start, end), name="Month")+
#   labs(y=expression(CT~"*"~(µmol~kg^{-1})))
# 
# ggsave(here::here("Plots", "CT_timeseries_all_years.jpg"), width = 8, height = 5, dpi=300)
# 
# 

