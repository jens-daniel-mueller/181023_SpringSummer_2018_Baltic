library(tidyverse)



setwd("C:/Mueller_Jens_Data/181023_Spring_Summer_2018/data/Remote_Sensing/Merged_data_sets")
Chl.Woz.day.01 <- read_csv("Woz_Chl_2016-18_daily_res01.csv")

Chl.Woz.day.01.weekly <-  
  Chl.Woz.day.01 %>% 
  group_by(lat.int, lon.int, year, week) %>% 
  summarise(chl = mean(chl)) %>%
  ungroup()


# Chl.Woz.day.01.weekly %>%
#   ggplot(aes(lon.int, lat.int, fill=chl))+
#   geom_raster()+
#   scale_fill_viridis_c()+
#   facet_grid(week~year)


setwd("C:/Mueller_Jens_Data/181023_Spring_Summer_2018/data/Finnmaid/Summarized_datasets")
Finnmaid <- read_csv("dfall.csv")

Finnmaid <-
  Finnmaid %>% 
  filter(year %in% c(2016, 2018)) %>% 
    mutate(month = month(date),
            week = week(date)) %>% 
  filter(month %in% seq(4,8,1)) %>% 
  mutate(lat.int = as.numeric(as.character(cut(Lat, seq(50.95, 72.05, 0.1), labels = seq(51, 72, 0.1)))),
         lon.int = as.numeric(as.character(cut(Lon, seq(8.95, 32.05, 0.1), labels = seq(9, 32, 0.1)))))
  
Finnmaid.weekly <-  
  Finnmaid %>% 
  group_by(lat.int, lon.int, year, week) %>% 
  summarise(Sal = mean(Sal),
            Tem = mean(Tem),
            pCO2 = mean(pCO2),
            cO2 = mean(cO2)) %>%
  ungroup()




df <- merge(Chl.Woz.day.01.weekly, Finnmaid.weekly, by=c("year", "week","lon.int","lat.int"), all=TRUE)

df <-
  df %>% 
  select(-cO2) %>% 
  drop_na()



setwd("C:/Mueller_Jens_Data/181023_Spring_Summer_2018/data/Remote_Sensing/plots")

df %>% 
  ggplot(aes(lon.int, lat.int, fill=chl))+
  geom_raster()+
  scale_fill_viridis_c()+
  facet_grid(week~year)+
  theme_bw()
ggsave("Woz_Chl_2016-18_weekly_Finnmaid.pdf", height = 400, width = 100, dpi = 300, units = "mm")


df %>% 
  filter(lat.int > 58, lat.int < 59) %>% 
  ggplot(aes(week, pCO2))+
  geom_point()+
  facet_grid(.~year)+
  theme_bw()

ggsave("Woz_Chl_2016-18_weekly_Finnmaid_pCO2-Chl_1.pdf", height = 100, width = 200, dpi = 300, units = "mm")



df %>% 
  filter(lat.int > 58, lat.int < 59) %>% 
  ggplot(aes(week, chl))+
  geom_point()+
  facet_grid(.~year)+
  theme_bw()
  

ggsave("Woz_Chl_2016-18_weekly_Finnmaid_pCO2-Chl_2.pdf", height = 100, width = 200, dpi = 300, units = "mm")








