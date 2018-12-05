library(ggplot2)
library(data.table)
library(lubridate)
library(tidyverse)


### read coordinate grid data

setwd("C:/Mueller_Jens_Data/181023_Spring_Summer_2018/data/Remote_Sensing/grid")
files <- list.files(pattern = "[.]txt$")

lat <- read.delim(files[1], header = FALSE)
lat <-
  lat %>% 
  gather(V1:V1000) %>% 
  select(value) %>% 
  rename(lat = value)

lon <- read.delim(files[2], header = FALSE)
lon <-
  lon %>% 
  gather(V1:V1000) %>% 
  select(value) %>% 
  rename(lon = value)

rm(files)




### read chl matrix data and merge with coordinate grid

setwd("C:/Mueller_Jens_Data/181023_Spring_Summer_2018/data/Remote_Sensing/Chl")
files <- list.files(pattern = "[.]txt$")
#file <- files[1]

for (file in files){

chl <- read.delim(file, header = FALSE)
chl <-
  chl %>% 
  gather(V1:V1000) %>% 
  select(value)

df <- data.table(
  cbind(lat, lon, chl))

rm(chl)

df <- na.omit(df)

df$lat.int <- as.numeric( as.character (cut(df$lat, seq(50.75, 72.25, 0.5), labels = seq(51, 72, 0.5))))
df$lon.int <- as.numeric( as.character (cut(df$lon, seq(8.75, 32.25, 0.5), labels = seq(9, 32, 0.5))))

matrix.int <-
  df %>% 
  group_by(lat.int, lon.int) %>% 
  summarise(chl = mean(value)) %>% 
  ungroup() %>% 
  add_column(year  = substr(file, 9, 12),
             month = substr(file, 13, 14),
             day = substr(file, 15, 16))

rm(df)

if (exists("temp")){
  temp <- rbind (temp, matrix.int)
} else{temp <- matrix.int}

rm(matrix.int)

print(file)

}

rm(file, files)

df <- temp
rm(temp)




#plot results from both data sources


setwd("C:/Mueller_Jens_Data/181023_Spring_Summer_2018/data/Remote_Sensing/plots")

df <- data.table(df)
df$chl.int <- cut(df$chl, c(0,2,5,10,15,20,50))


df.month <- 
  df %>% 
  group_by(lat.int, lon.int, year, month) %>% 
  summarise(chl = mean(chl)) %>%
  mutate(chl.int = cut(chl, c(0,2,4,6,10,12.5,15,20,50))) %>% 
  ungroup()





ggplot(df[year == "2016"], aes(lon.int, lat.int, fill=chl.int))+
  geom_raster()+
  scale_fill_viridis_d()+
  coord_quickmap()+
  facet_grid(day~month)
ggsave("2016_Chl_data.pdf", height = 600, width = 200, dpi = 300, units = "mm")

ggplot(df[year == "2018"], aes(lon.int, lat.int, fill=chl.int))+
  geom_raster()+
  scale_fill_viridis_d()+
  coord_quickmap()+
  facet_grid(day~month)
ggsave("2018_Chl_data.pdf", height = 600, width = 200, dpi = 300, units = "mm")


ggplot(df.month, aes(lon.int, lat.int, fill=chl.int))+
  geom_raster()+
  scale_fill_brewer(palette = "Spectral", direction = -1)+
  coord_quickmap()+
  facet_grid(year~month)
ggsave("monthly_Chl_data.pdf", height = 100, width = 200, dpi = 300, units = "mm")









ggplot(matrix.int, aes(lon.int, lat.int, fill=chl))+
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