library(ggplot2)
library(scales)

dat_temp <- read.csv("meteo.csv", header = TRUE, sep=";")

dat_temp$iso <- paste(dat_temp$date, dat_temp$time, sep=" ") 

dat_temp$iso <- as.POSIXct(as.character(dat_temp$iso), format="%d/%m/%Y %H:%M", tz="UCT")

dat_temp

#dat_temp$date <- as.POSIXct(dat_temp$date)

ggplot(dat_temp, aes(iso, temp))+
  geom_line()+
  #scale_x_datetime(limits = lims_3(), labels=date_format("%d-%m"), breaks="2 day")
  scale_x_datetime(labels=date_format("%d-%m-%y %H:%M"), breaks="10 min")+
  ylab("Temperature")+
  theme_bw()+
  theme(axis.text = element_text(size = 16), 
        axis.text.x = element_text(angle = 30, hjust = 1), 
        axis.title = element_text(size = 18, face = "bold"), 
        legend.title = element_blank(), 
        legend.text = element_text(size = 16))
