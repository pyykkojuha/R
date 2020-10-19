# KAFFA 2019
# https://www.kaffaroastery.fi/raakakahvien-hinnat-2019

library(openxlsx)
library(ggplot2)
library(scales)
library(extrafont)
library(plyr)

#DATA from https://www.kaffaroastery.fi/raakakahvien-hinnat-2019
kaffa <- read.xlsx("raakakahvien-hinnat-2019.xlsx", sheet = 1)

#MAIN
yhteenveto <- ddply(kaffa, .(MAA), summarize,
                    kiloja = sum(Ostettu_kg),
                    rahaa  = sum(Ostettu_USD),
                    FT     = sum(Fair_Trade_USD))

#LINE
yhteenveto2 <- rbind(yhteenveto, yhteenveto)
yhteenveto2[10:18,3] <- yhteenveto2[10:18,4]

#LEGEND
yhteenveto2[19:20,1] <- "INFO"
yhteenveto2[19:20,2] <- 10
yhteenveto2[19,3] <- 170000
yhteenveto2[20,4] <- 170000
yhteenveto2[20,3] <- 180000
 
#PLOT
ggplot(yhteenveto) +
  #MAIN
  geom_point(aes(y=rahaa, x=kiloja), size = 3, shape = 17) +  #17  24
  geom_point(aes(y=FT,    x=kiloja), size = 3, shape = 16) +  #16  21
  geom_line(data=subset(yhteenveto2, MAA!="INFO"), aes(y=rahaa, x=kiloja, group = MAA)) + 
  #COUNTRY
  geom_text(data=subset(yhteenveto, kiloja<10000 | kiloja>20000), aes(y=rahaa+6000, x=kiloja,     label=MAA), family="Unica One", size=5) +
  geom_text(data=subset(yhteenveto, MAA=="INDONESIA"),            aes(y=rahaa,      x=kiloja-700, label=MAA), family="Unica One", size=5, hjust=1) +
  geom_text(data=subset(yhteenveto, MAA=="ETHIOPIA"),             aes(y=rahaa,      x=kiloja+700, label=MAA), family="Unica One", size=5, hjust=0) +
  geom_text(data=subset(yhteenveto, MAA=="UGANDA"),               aes(y=rahaa,      x=kiloja+700, label=MAA), family="Unica One", size=5, hjust=0) +
  geom_text(data=subset(yhteenveto, MAA=="INDIA"),                aes(y=FT-6000,    x=kiloja,     label=MAA), family="Unica One", size=5) +
  #LEGEND
  annotate("text", x=1000, y=180000, label= "KAFFA HANDSHAKE (FOB)",     family="Unica One", size=5, hjust=0, col= "gray40") +
  annotate("text", x=1000, y=170000, label= "REILUN KAUPAN MINIMIHINTA", family="Unica One", size=5, hjust=0, col= "gray40") +
  geom_point(data=yhteenveto2[20,], aes(y=rahaa, x=kiloja), size = 3, shape = 17,                             col= "gray40") +
  geom_point(data=yhteenveto2[20,], aes(y=FT,    x=kiloja), size = 3, shape = 16,                             col= "gray40") +
  geom_line(data=subset(yhteenveto2, MAA=="INFO"), aes(y=rahaa, x=kiloja, group = MAA),                       col= "gray40") + 
  #SPECS
  scale_y_continuous(limits = c(0, 180000), breaks = c(0, 50000, 100000, 150000), labels = label_dollar()) +
  scale_x_continuous(limits = c(0, 40000),
    breaks = c(0,       10000,       20000,       30000,       40000),
    label = c("0 KG", "10 000 KG", "20 000 KG", "30 000 KG", "40 000 KG")) +
  labs(title    = "   KAFFA ROASTERY: OSTETTU & MAKSETTU YHTEENSÄ MAITTAIN", 
       subtitle = "    RAAKAKAHVIEN HINNAT 2019: KAFFA HANDSHAKE vs REILU KAUPPA",
       caption = "LÄHDE: www.kaffaroastery.fi/raakakahvien-hinnat-2019") +
  xlab("") + ylab("") + theme_minimal() +
  theme(axis.title.y = element_text(angle = 0, vjust=0),
        axis.title.x = element_text(angle = 0, hjust=0),
        plot.subtitle = element_text(margin = unit(c(0, 0, 0, 0), "mm")),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.ticks.x = element_line(),
        axis.ticks.y = element_line(),
        text=element_text(family="Unica One", size=18),
        plot.margin=unit(c(1, 1, .3, .3),"cm"), 
        plot.background = element_rect(fill = "antiquewhite", colour = NA),
        plot.caption = element_text(color = "gray60", family="Press Start 2P", size=5, hjust=0.5),
        legend.position = "none") 
  
ggsave(file="KAFFA2019a.jpg", width = 8, height = 8, units = "in") 
