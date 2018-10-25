library(ggplot2)
library(data.table)
library(readxl)
library(gsubfn)
library(lubridate)


### read csv from IOW's ODIN data base including monitorin data 2003-17 at BY15/TF0271b

setwd("C:/Mueller_Jens_Data/181023_Spring_Summer_2018/data/ODIN")
ODIN <- data.table(read.csv("TF0271.csv"))
ODIN <- ODIN[-c(1,2)]

ODIN <- ODIN[,c(2,3,4,12,18,22,23,25,27,29,31)]
names(ODIN) <- c("year", "mon", "day", "Dep", "O2", "TN", "NO3", "PO4", "Sal", "Tem", "TP") 

ODIN$Dep <- as.numeric(as.character(ODIN$Dep))
ODIN$PO4 <- as.numeric(as.character(ODIN$PO4))
ODIN$Sal <- as.numeric(as.character(ODIN$Sal))
ODIN$Tem <- as.numeric(as.character(ODIN$Tem))

ODIN$Dep.int <- cut(ODIN$Dep, seq(-2.5, 302.5, 5), labels = seq(0,300,5) )
ODIN$Dep.int <- as.numeric(as.character( ODIN$Dep.int))

May <- ODIN[mon==5][,.(
  mean.Sal = mean(Sal, na.rm = TRUE),
  mean.Tem = mean(Tem, na.rm = TRUE),
  mean.PO4 = mean(PO4, na.rm = TRUE)
  ),
  by=.(year, Dep.int)]

setorder(May, year, Dep.int)
May <- na.omit(May)



May.18 <- data.table(read.csv("1805_TF0271.csv", na.strings = "999.99"))
May.18 <- May.18[STAT.BEZ.=="TF0271"]
setorder(May.18, TIEFE)


shadesOfGreyRed <- c(rev(colorRampPalette(c("grey35", "grey75"))(15)), "red")

Fig <- 
ggplot()+
  #geom_point(data = May, aes(mean.PO4, Dep.int, col=as.factor(year)))+
  geom_path(data = May, aes(mean.PO4, Dep.int, col=as.factor(year)))+
  geom_point(data = May.18, aes(PO4, TIEFE, col=as.factor("2018")))+
  geom_path(data = May.18, aes(PO4, TIEFE, col=as.factor("2018")))+
  scale_y_reverse(limits = c(81,0), name="Depth (m)")+
  coord_cartesian(xlim =  c(0,1))+
  scale_x_continuous(name=expression(PO[4]^{"3-"}~(µmol~L^{-1})))+
  scale_color_manual(values = shadesOfGreyRed, name="Year")
  


setwd("C:/Mueller_Jens_Data/181023_Spring_Summer_2018/plots")
tiff("PO4_Profiles_BY15-ODIN_AllYears.tiff", width = 130, height = 130, units = 'mm', res = 600, compression = 'lzw')
Fig
dev.off()
rm(Fig)