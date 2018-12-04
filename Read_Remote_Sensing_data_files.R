library(ggplot2)
library(data.table)
library(lubridate)
library(tidyverse)


### matrix data

setwd("C:/Mueller_Jens_Data/181023_Spring_Summer_2018/data/Remote_Sensing/Woz_Chl_20180530")
files <- list.files(pattern = "[.]txt$")

lat <- read.delim(files[1], header = FALSE)
lat.long <-
  lat %>% 
  gather(V1:V1000) %>% 
  select(value) %>% 
  rename(lat = value)
rm(lat)

lon <- read.delim(files[2], header = FALSE)
lon.long <-
  lon %>% 
  gather(V1:V1000) %>% 
  select(value) %>% 
  rename(lon = value)
rm(lon)

chl <- read.delim(files[3], header = FALSE)
chl.long <-
  chl %>% 
  gather(V1:V1000) %>% 
  select(value) %>% 
  rename(chl = value)
rm(chl)

df <- data.table(
  cbind(lat.long, lon.long, chl.long))

rm(lat.long, lon.long, chl.long)

summary(df)

df$lat.int <- as.numeric( as.character (cut(df$lat, seq(50.975, 72.025, 0.05), labels = seq(51, 72, 0.05))))
df$lon.int <- as.numeric( as.character (cut(df$lon, seq(8.975, 32.025, 0.05), labels = seq(9, 32, 0.05))))

matrix.int <-
  data.table(
  df %>% 
  group_by(lat.int, lon.int) %>% 
  summarise(mean.chl = mean(chl)) %>% 
  ungroup()
  )

#rm(df)


# ### long table data
# 
# setwd("C:/Mueller_Jens_Data/181023_Spring_Summer_2018/data/Remote_Sensing/testing_new_format")
# df <- read.delim("pandas.txt", header = FALSE)
# 
# names(df) <- c("lat", "lon", "chl")
# 
# df$lat.int <- as.numeric( as.character (cut(df$lat, seq(50.95, 72.05, 0.1), labels = seq(51, 72, 0.1))))
# df$lon.int <- as.numeric( as.character (cut(df$lon, seq(8.95, 32.05, 0.1), labels = seq(9, 32, 0.1))))
# 
# table.int <-
#   df %>% 
#   group_by(lat.int, lon.int) %>% 
#   summarise(mean.chl = mean(chl)) %>% 
#   ungroup()
# 
# rm(df)

#plot results from both data sources


setwd("C:/Mueller_Jens_Data/181023_Spring_Summer_2018/data/Remote_Sensing/plots")


ggplot(matrix.int, aes(lon.int, lat.int, fill=mean.chl))+
  geom_raster()+
  scale_fill_viridis_c(limits = c(0,10))+
  coord_quickmap()
ggsave("plot_matrix_data.pdf")


ggplot(matrix.int[lon.int > 13 & lon.int < 15 & lat.int > 54.6 & lat.int < 55.6],
       aes(lon.int, lat.int, fill=mean.chl))+
  geom_raster()+
  scale_fill_viridis_c(limits = c(0,10))+
  coord_quickmap()
ggsave("plot_matrix_data_Bornholm_0.05.pdf")


ggplot(df[lon.int > 13 & lon.int < 15 & lat.int > 54.6 & lat.int < 55.6],
       aes(lon, lat, col=chl))+
  geom_point()+
  scale_color_viridis_c(limits = c(0,10))+
  coord_quickmap()
ggsave("plot_matrix_data_Bornholm_full_resolution.pdf")




# 
# ggplot(table.int, aes(lon.int, lat.int, fill=mean.chl))+
#   geom_raster()+
#   scale_fill_viridis_c(limits = c(0,10))+
#   coord_quickmap()
# ggsave("plot_table_data.pdf")
#   