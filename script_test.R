library(ggplot2)
library(scales)
library(gridExtra)
library(reshape)

dat_temp <- read.csv("meteo.csv", header = TRUE, sep=";")

dat_temp_2 <- melt(dat_temp, id.vars = c("date"), variable_name = "source", measure.vars=c("temp", "humidity","pressure")) 

#dat_temp$iso <- paste(dat_temp$date, dat_temp$time, sep=" ") 

dat_temp$iso <- as.POSIXct(as.character(dat_temp$date), format="%d/%m/%Y %H:%M", tz="UCT")

dat_temp_2$iso <- as.POSIXct(as.character(dat_temp_2$date), format="%d/%m/%Y %H:%M", tz="UCT")

dat_temp

#dat_temp$date <- as.POSIXct(dat_temp$date)

a            <- range(dat_temp[["humidity"]])
b            <- range(dat_temp[["pressure"]])
scale_factor <- diff(a)/diff(b)
dat_temp[["pressure"]]      <- ((dat_temp[["pressure"]] - b[1]) * scale_factor*0.2) + a[1]

trans <- ~ ((. - a[1]) / scale_factor /0.2) + b[1]

ggplot(dat_temp)+
  geom_line(aes(x=iso, y=temp, color="temp"))+
  geom_line(aes(x=iso, y=humidity/1.35, color="humidity"))+
  #geom_line(aes(x=iso, y=pressure, color="pressure"))+
  #facet_grid(~iso)+
  #scale_x_datetime(limits = lims_3(), labels=date_format("%d-%m"), breaks="2 day")
  scale_x_datetime(labels=date_format("%d-%m-%y %H:%M"), breaks="1 day")+
  scale_y_continuous(sec.axis = sec_axis(~.*1.35, name="Humidity"))+
  scale_color_manual(values=c("red", "blue"))+
  ylab("Temperature")+
  theme_bw()+
  theme(axis.text = element_text(size = 16), 
        axis.text.x = element_text(angle = 30, hjust = 1), 
        axis.title = element_text(size = 18, face = "bold"), 
        legend.title = element_blank(), 
        legend.text = element_text(size = 16),
        axis.line.y.right = element_line(color = "red"),
        axis.line.y.left = element_line(color = "blue"),
        axis.ticks.y.right = element_line(color = "red"),
        axis.text.y.right = element_text(color = "red"),
        axis.ticks.y.left = element_line(color = "blue"),
        axis.text.y.left = element_text(color = "blue"),)

ggplot(dat_temp_2, aes(x=iso, y=value, color=source, group=1))+
  geom_line(size=0.7)+
  #geom_point(size=1)+
  #geom_line(aes(x=iso, y=pressure, color="pressure"))+
  facet_wrap(~source, nrow = 3, scales = "free_y", strip.position="right")+
  geom_smooth(linetype = "dashed", method = "lm", formula=y ~ poly(x, 2, raw=TRUE), se = FALSE, fill = NA)+
  #scale_x_datetime(limits = lims_3(), labels=date_format("%d-%m"), breaks="2 day")
  scale_x_datetime(labels=date_format("%d-%m-%y"), breaks="2 days")+
  #scale_y_continuous(sec.axis = sec_axis(~.*1.35, name="Humidity"))+
  #scale_color_manual(values=c("red", "blue"))+
  #ylab("Temperature")+
  theme_bw()+
  theme(axis.text = element_text(size = 16), 
        axis.text.x = element_text(angle = 30, hjust = 1), 
        axis.title = element_text(size = 18, face = "bold"),
        strip.text = element_text(size = 12),
        legend.position = "none",
        legend.title = element_blank(), 
        legend.text = element_text(size = 16))
