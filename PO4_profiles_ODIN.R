library(ggplot2)
library(data.table)
library(readxl)
library(gsubfn)
library(lubridate)


### read csv downloaded from IOW's ODIN data base
### that includes monitoring data 2003-17 at station BY15/TF0271b (Central Baltic/Gotland deep)

setwd("C:/Mueller_Jens_Data/181023_Spring_Summer_2018/data/ODIN")
ODIN <- data.table(read.csv("TF0271.csv"))
ODIN <- ODIN[-c(1,2)]

ODIN <- ODIN[,c(2,3,4,12,18,22,23,25,27,29,31)]
names(ODIN) <- c("year", "mon", "day", "Dep", "O2", "TN", "NO3", "PO4", "Sal", "Tem", "TP") 

ODIN$Dep <- as.numeric(as.character(ODIN$Dep))
ODIN$PO4 <- as.numeric(as.character(ODIN$PO4))
ODIN$Sal <- as.numeric(as.character(ODIN$Sal))
ODIN$Tem <- as.numeric(as.character(ODIN$Tem))

### group observations into Depth intervals of 5m
### to allow for a mean calculation in each year and depth

ODIN$Dep.int <- cut(ODIN$Dep, seq(-2.5, 302.5, 5), labels = seq(0,300,5) )
ODIN$Dep.int <- as.numeric(as.character( ODIN$Dep.int))


### subset month May, which represents post spring bloom conditions

May <- ODIN[mon==5][,.(
  mean.Sal = mean(Sal, na.rm = TRUE),
  mean.Tem = mean(Tem, na.rm = TRUE),
  mean.PO4 = mean(PO4, na.rm = TRUE)
  ),
  by=.(year, Dep.int)]

setorder(May, year, Dep.int)
May <- na.omit(May)

### subset month January and February
### which represents winter surface conditions
### and thus the phosphate available for the spring bloom

Jan.Feb <- ODIN[mon %in% c(1,2)][,.(
  mean.Sal = mean(Sal, na.rm = TRUE),
  mean.Tem = mean(Tem, na.rm = TRUE),
  mean.PO4 = mean(PO4, na.rm = TRUE)
  ),
  by=.(year, Dep.int)]

setorder(Jan.Feb, year, Dep.int)
Jan.Feb <- na.omit(Jan.Feb)

### read monitoring data from May 2018
### which were not yet included in ODIN data base, as of Oct 26 2018

May.18 <- data.table(read.csv("1805_TF0271.csv", na.strings = "999.99"))
May.18 <- May.18[STAT.BEZ.=="TF0271"]
setorder(May.18, TIEFE)

shadesOfGreyRed <- c(rev(colorRampPalette(c("grey35", "grey75"))(15)), "red")


### plot winter and post-springbloom PO4-Profiles, 2018 highlighted red


Fig <- 
ggplot()+
  geom_path(data = May, aes(mean.PO4, Dep.int, col=as.factor(year)))+
  geom_point(data = May.18, aes(PO4, TIEFE, col=as.factor("2018")))+
  geom_path(data = May.18, aes(PO4, TIEFE, col=as.factor("2018")))+
  scale_y_reverse(limits = c(81,0), name="Depth (m)")+
  coord_cartesian(xlim =  c(0,1))+
  scale_x_continuous(name=expression(PO[4]^{"3-"}~(µmol~L^{-1})))+
  scale_color_manual(values = shadesOfGreyRed, name="Year")
  


setwd("C:/Mueller_Jens_Data/181023_Spring_Summer_2018/plots")
tiff("PO4_Profiles_PostSpring_BY15-ODIN_AllYears.tiff", width = 130, height = 130, units = 'mm', res = 600, compression = 'lzw')
Fig
dev.off()
rm(Fig)


shadesOfGreyRed <- c(rev(colorRampPalette(c("grey35", "grey75"))(14)), "red")

Fig <- 
ggplot()+
  geom_path(data = Jan.Feb, aes(mean.PO4, Dep.int, col=as.factor(year)))+
  geom_point(data = Jan.Feb[year == 2018], aes(mean.PO4, Dep.int, col=as.factor(year)))+
  scale_y_reverse(limits = c(81,0), name="Depth (m)")+
  coord_cartesian(xlim =  c(0,1))+
  scale_x_continuous(name=expression(PO[4]^{"3-"}~(µmol~L^{-1})))+
  scale_color_manual(values = shadesOfGreyRed, name="Year")
  


setwd("C:/Mueller_Jens_Data/181023_Spring_Summer_2018/plots")
tiff("PO4_Profiles_Winter_BY15-ODIN_AllYears.tiff", width = 130, height = 130, units = 'mm', res = 600, compression = 'lzw')
Fig
dev.off()
rm(Fig)
