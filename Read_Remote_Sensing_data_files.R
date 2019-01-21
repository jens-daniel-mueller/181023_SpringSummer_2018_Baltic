library(lubridate)
library(tidyverse)


### read coordinate grid data


setwd("C:/Mueller_Jens_Data/181023_Spring_Summer_2018/data/Remote_Sensing/grid")
files <- list.files(pattern = "[.]txt$")

lat <- read_tsv(files[1], col_names = FALSE)
lat <-
  lat %>% 
  gather(V1:V1000) %>% 
  select(value) %>% 
  rename(lat = value)

lon <- read_tsv(files[2], col_names = FALSE)
lon <-
  lon %>% 
  gather(V1:V1000) %>% 
  select(value) %>% 
  rename(lon = value)

rm(files)



### read chl matrix data and merge with coordinate grid

setwd("C:/Mueller_Jens_Data/181023_Spring_Summer_2018/data/Remote_Sensing/Woz_Chl")
files <- list.files(pattern = "[.]txt$")
file <- files[1]

for (file in files){

chl <- read_tsv(file, col_names = FALSE)
chl <-
  chl %>% 
  gather(V1:V1000) %>% 
  select(value)

df <- bind_cols(lat, lon, chl)
rm(chl)

df <- 
  df %>% 
  drop_na() %>% 
  mutate(lat.int = as.numeric(as.character(cut(lat, seq(50.95, 72.05, 0.1), labels = seq(51, 72, 0.1)))),
         lon.int = as.numeric(as.character(cut(lon, seq(8.95, 32.05, 0.1), labels = seq(9, 32, 0.1)))))

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

rm(file, files, lat, lon)

df <- temp
rm(temp)



#### subset and grid data frames ####


df <- 
  df %>% 
  mutate(chl.int = cut(chl, c(0,2,4,6,10,12.5,15,20,50)),
         date = ymd(paste(year, month, day, sep = "/")),
         week = week(date))

df.weekly <- 
  df %>% 
  group_by(lat.int, lon.int, year, week) %>% 
  summarise(chl = mean(chl)) %>%
  mutate(chl.int = cut(chl, c(0,2,4,6,10,12.5,15,20,50))) %>% 
  ungroup()

df.month <- 
  df %>% 
  group_by(lat.int, lon.int, year, month) %>% 
  summarise(chl = mean(chl)) %>%
  mutate(chl.int = cut(chl, c(0,2,4,6,10,12.5,15,20,50))) %>% 
  ungroup()


setwd("C:/Mueller_Jens_Data/181023_Spring_Summer_2018/data/Remote_Sensing/Merged_data_sets")


write.csv(df, "Woz_Chl_2016-18_daily_res01.csv", row.names = FALSE)
write.csv(df.weekly, "Woz_Chl_2016-18_weekly_res01.csv", row.names = FALSE)
write.csv( df.month, "Woz_Chl_2016-18_monthly_res01.csv", row.names = FALSE)


#### plot results from both data sources ####

setwd("C:/Mueller_Jens_Data/181023_Spring_Summer_2018/data/Remote_Sensing/plots")


baltic.coastlines <- map_data('world', xlim = c(4, 29), ylim = c(50, 66))
land.colour   <- "grey75"
border.colour <- "grey10"
lon.borders <- c(10, 31.5)
lat.borders <- c(53.5, 61)
  
df %>% 
  filter(year==2016) %>% 
  ggplot(aes(lon.int, lat.int, fill=chl.int))+
  coord_quickmap(xlim=lon.borders, ylim=lat.borders) +
  geom_polygon(data=baltic.coastlines, aes(x=long, y=lat, group=group),
               fill=land.colour, colour = border.colour)+
  geom_raster()+
  scale_fill_brewer(palette = "Spectral", direction = -1)+
  facet_grid(day~month)+
  theme_bw()
ggsave("Woz_Chl_2016_daily.pdf", height = 600, width = 200, dpi = 300, units = "mm")

df %>% 
  filter(year==2018) %>% 
  ggplot(aes(lon.int, lat.int, fill=chl.int))+
  coord_quickmap(xlim=lon.borders, ylim=lat.borders) +
  geom_polygon(data=baltic.coastlines, aes(x=long, y=lat, group=group),
               fill=land.colour, colour = border.colour)+
  geom_raster()+
  scale_fill_brewer(palette = "Spectral", direction = -1)+
  facet_grid(day~month)+
  theme_bw()
ggsave("Woz_Chl_2018_daily.pdf", height = 600, width = 200, dpi = 300, units = "mm")


df.month %>% 
ggplot(aes(lon.int, lat.int, fill=chl.int))+
  coord_quickmap(xlim=lon.borders, ylim=lat.borders) +
  geom_polygon(data=baltic.coastlines, aes(x=long, y=lat, group=group),
               fill=land.colour, colour = border.colour, lwd=.5)+
  geom_raster()+
  scale_fill_brewer(palette = "Spectral", direction = -1, name="Chl")+
  facet_grid(year~month)+
  labs(x="Lon (0.1 deg grid)", y="Lat (0.1 deg grid)")+
  theme_bw()
ggsave("Woz_Chl_2016-18_monthly.pdf", height = 100, width = 300, dpi = 300, units = "mm")



df.weekly %>% 
ggplot(aes(lon.int, lat.int, fill=chl.int))+
  coord_quickmap(xlim=lon.borders, ylim=lat.borders) +
  geom_polygon(data=baltic.coastlines, aes(x=long, y=lat, group=group), fill=land.colour, colour = border.colour, lwd=.5)+
  geom_raster()+
  scale_fill_brewer(palette = "Spectral", direction = -1)+
  facet_grid(week~year)+
  theme_bw()
ggsave("Woz_Chl_2016-18_weekly.pdf", height = 400, width = 100, dpi = 300, units = "mm")








