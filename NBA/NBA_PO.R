# NBA PLAYOFFS POINTS BY DUOS

library(ggplot2)
library(extrafont)
library(gridExtra)

load("PO.Rdata")

# summarize
summary(PO_BULLS)
summary(PO_LAKERS)
summary(PO_THUNDER)
summary(PO_WARRIORS)

PO_BULLS$Date[1];PO_BULLS$Date[dim(PO_BULLS)[1]]
PO_LAKERS$Date[1];PO_LAKERS$Date[dim(PO_LAKERS)[1]]
PO_THUNDER$Date[1];PO_THUNDER$Date[dim(PO_THUNDER)[1]]
PO_WARRIORS$Date[1];PO_WARRIORS$Date[dim(PO_WARRIORS)[1]]

summary(PO_SPURS)
summary(PO_LAKERS2)
summary(PO_CELTICS)
summary(PO_HEAT)
summary(PO_CAVALIERS)

PO_SPURS$Date[1];PO_SPURS$Date[dim(PO_SPURS)[1]]
PO_LAKERS2$Date[1];PO_LAKERS2$Date[dim(PO_LAKERS2)[1]]
PO_CELTICS$Date[1];PO_CELTICS$Date[dim(PO_CELTICS)[1]]
PO_HEAT$Date[1];PO_HEAT$Date[dim(PO_HEAT)[1]]
PO_CAVALIERS$Date[1];PO_CAVALIERS$Date[dim(PO_CAVALIERS)[1]]


# layout for 16:9 combining plot and text
lay <- rbind(c(rep(1,9), rep(2,16-9)))

#
# # 
# # # BULLS

BULLS1 <- ggplot(PO_BULLS, aes(y=Jordan , x=Pippen))  + 
  coord_fixed(ratio=1)  +
  geom_abline(slope=1, color="black", size = 0.1) +    
  scale_x_continuous(breaks=seq(0, 60, 10), limits=c(0,60)) +
  scale_y_continuous(breaks=seq(0, 60, 10), limits=c(0,60)) +
  stat_density_2d(aes(fill = ..level..), geom = "polygon", alpha = .5 , colour="black") + 
  scale_fill_distiller(palette="YlOrRd", direction=1) + 
  xlab("SCOTTIE PIPPEN") +
  ylab("MICHAEL JORDAN") +
  annotate("text", x=10, y=50, label="23", hjust=.5, vjust=.5,  size = 164, color="white", family="Staatliches", alpha=.1) +
  annotate("text", x=50, y=10, label="33", hjust=.5, vjust=.5,  size = 164, color="white", family="Staatliches", alpha=.1) +
  geom_point(shape=4, size=3, colour="black", alpha=.6) +
  scale_shape_identity() + 
  theme_minimal() +
  theme(plot.background = element_rect(fill = "#CE1141", colour = NA),
        text=element_text(family="Staatliches"),
        panel.grid.major.y = element_line(colour = "black", size = 0.2), 
        panel.grid.major.x = element_line(colour = "black", size = 0.2),
        panel.grid.minor  = element_blank(),
        axis.ticks.y  = element_blank(),
        axis.ticks.x  = element_blank(),
        plot.title    = element_text(size = 48, colour = "black",  hjust=.5), 
        plot.subtitle = element_text(size = 44, colour = "ghostwhite", hjust=.5, margin = unit(c(0, 0, 5 , 0), "mm")), 
        plot.caption  = element_text(size = 12, colour = "black",  hjust=1, family="Press Start 2P"), 
        axis.text.x   = element_text(size = 32, colour = "black",  angle=0, hjust=.5),
        axis.text.y   = element_text(size = 32, colour = "black",  angle=0, vjust=.5),
        axis.title.x  = element_text(size = 64, colour = "ghostwhite", angle=0,   hjust=.5, margin = unit(c(10, 0, 0, 0), "mm")),
        axis.title.y  = element_text(size = 64, colour = "ghostwhite", angle=270, vjust=.5, margin = unit(c(0, 10, 0, 0), "mm")),
        legend.position = "none",
        plot.margin = unit(c(.1, .1, .5, .5),"cm") ) 

BULLS2 <- ggplot(PO_BULLS, aes(y=Jordan , x=Pippen))  + 
  coord_fixed(ratio=2/3)  + scale_x_continuous(limits=c(0,30)) + scale_y_continuous(limits=c(0,60)) +
  annotate("text", x=30, y=59, hjust=1, vjust=.5,size = 40, color="black",  family="Staatliches",    alpha=1,  label="CHICAGO BULLS") +
  annotate("text", x=30, y=50, hjust=1, vjust=.5,size = 30, color="ghostwhite", family="Staatliches",    alpha=1,  label="JORDAN + PIPPEN") +
  annotate("text", x=30, y=30, hjust=1, vjust=.5,size = 16, color="ghostwhite", family="Staatliches",    alpha=1,  label="POINTS IN PLAYOFF GAMES TOGETHER") +
  annotate("text", x=30, y=26, hjust=1, vjust=.5,size = 16, color="ghostwhite", family="Staatliches",    alpha=1,  label="1988-1998") +
  annotate("text", x=30, y=20, hjust=1, vjust=.5,size = 12, color="black",  family="Staatliches",    alpha=1, label="HEAT MAP + 168 GAMES (X)") +
  annotate("text", x=30, y=4,  hjust=1, vjust=.5,size =  8, color="black",  family="Press Start 2P", alpha=1, label="@pyyxxo") +
  annotate("text", x=30, y=0,  hjust=1, vjust=.5,size =  4, color="black",  family="Press Start 2P", alpha=1, label="SOURCE: basketball-reference.com") +
  geom_point(shape=4, size=3, colour="azure", alpha=.0) + theme_void() +
  theme(plot.background = element_rect(fill = "#CE1141", colour = NA), plot.margin = unit(c(.1, .1, .1, .1),"cm") ) 

BULLS <- grid.arrange(BULLS1, BULLS2, layout_matrix = lay)
ggsave(BULLS, file="fig/PO_BULLS.png", width = (16/9)*12, height = 12, units = "in", bg = "#CE1141") 

#
# # 
# # # LAKERS

LAKERS1 <- ggplot(PO_LAKERS, aes(y=Oneal , x=Bryant))  + 
  coord_fixed(ratio=1)  +
  geom_abline(slope=1, color="#FDB927", size = 0.1) +    
  scale_x_continuous(breaks=seq(0, 60, 10), limits=c(0,60)) +
  scale_y_continuous(breaks=seq(0, 60, 10), limits=c(0,60)) +
  stat_density_2d(aes(fill = ..level..), geom = "polygon", alpha = .5 , colour="black") + 
  scale_fill_distiller(palette="YlOrRd", direction=1) + 
  xlab("KOBE BRYANT") +
  ylab("SHAQUILLE O'NEAL") +
  annotate("text", x=10, y=50, label="34", hjust=.5, vjust=.5,  size = 164, color="white", family="Staatliches", alpha=.1) +
  annotate("text", x=50, y=10, label="8",  hjust=.5, vjust=.5,  size = 164, color="white", family="Staatliches", alpha=.1) +
  geom_point(shape=4, size=3, colour="#FDB927", alpha=.6) +
  scale_shape_identity() + 
  theme_minimal() +
  theme(plot.background = element_rect(fill = "#552583", colour = NA),
        text=element_text(family="Staatliches"),
        panel.grid.major.y = element_line(colour = "#FDB927", size = 0.2), 
        panel.grid.major.x = element_line(colour = "#FDB927", size = 0.2),
        panel.grid.minor  = element_blank(),
        axis.ticks.y  = element_blank(),
        axis.ticks.x  = element_blank(),
        plot.title    = element_text(size = 48, colour = "#FDB927",  hjust=.5), 
        plot.subtitle = element_text(size = 44, colour = "ghostwhite", hjust=.5, margin = unit(c(0, 0, 5 , 0), "mm")), 
        plot.caption  = element_text(size = 12, colour = "#FDB927",  hjust=1, family="Press Start 2P"), 
        axis.text.x   = element_text(size = 32, colour = "#FDB927",  angle=0, hjust=.5),
        axis.text.y   = element_text(size = 32, colour = "#FDB927",  angle=0, vjust=.5),
        axis.title.x  = element_text(size = 64, colour = "ghostwhite", angle=0,   hjust=.5, margin = unit(c(10, 0, 0, 0), "mm")),
        axis.title.y  = element_text(size = 64, colour = "ghostwhite", angle=270, vjust=.5, margin = unit(c(0, 10, 0, 0), "mm")),
        legend.position = "none",
        plot.margin = unit(c(.1, .1, .5, .5),"cm") ) 

LAKERS2 <- ggplot(PO_LAKERS, aes(y=Oneal , x=Bryant))  + 
  coord_fixed(ratio=2/3)  + scale_x_continuous(limits=c(0,30)) + scale_y_continuous(limits=c(0,60)) +
  annotate("text", x=30, y=59, hjust=1, vjust=.5,size = 48, color="#FDB927",  family="Staatliches",    alpha=1,  label="L.A. LAKERS") +
  annotate("text", x=30, y=50, hjust=1, vjust=.5,size = 30, color="ghostwhite", family="Staatliches",    alpha=1,  label="O'NEAL + BRYANT") +
  annotate("text", x=30, y=30, hjust=1, vjust=.5,size = 16, color="ghostwhite", family="Staatliches",    alpha=1,  label="POINTS IN PLAYOFF GAMES TOGETHER") +
  annotate("text", x=30, y=26, hjust=1, vjust=.5,size = 16, color="ghostwhite", family="Staatliches",    alpha=1,  label="1997-2004") +
  annotate("text", x=30, y=20, hjust=1, vjust=.5,size = 12, color="#FDB927",  family="Staatliches",    alpha=1, label="HEAT MAP + 119 GAMES (X)") +
  annotate("text", x=30, y=4,  hjust=1, vjust=.5,size =  8, color="#FDB927",  family="Press Start 2P", alpha=1, label="@pyyxxo") +
  annotate("text", x=30, y=0,  hjust=1, vjust=.5,size =  4, color="#FDB927",  family="Press Start 2P", alpha=1, label="SOURCE: basketball-reference.com") +
  geom_point(shape=4, size=3, colour="azure", alpha=.0) + theme_void() +
  theme(plot.background = element_rect(fill = "#552583", colour = NA), plot.margin = unit(c(.1, .1, .1, .1),"cm") ) 

LAKERS <- grid.arrange(LAKERS1, LAKERS2, layout_matrix = lay)
ggsave(LAKERS, file="fig/PO_LAKERS.png", width = (16/9)*12, height = 12, units = "in", bg = "#552583") 

#
# # 
# # # THUNDER

THUNDER1 <- ggplot(PO_THUNDER, aes(y=Durant , x=Westbrook))  + 
  coord_fixed(ratio=1)  +
  geom_abline(slope=1, color="#EF3B24", size = 0.1) +    
  scale_x_continuous(breaks=seq(0, 60, 10), limits=c(0,60)) +
  scale_y_continuous(breaks=seq(0, 60, 10), limits=c(0,60)) +
  stat_density_2d(aes(fill = ..level..), geom = "polygon", alpha = .5 , colour="black") + 
  scale_fill_distiller(palette="YlOrRd", direction=1) + 
  xlab("RUSSELL WESTBROOK") +
  ylab("KEVIN DURANT") +
  annotate("text", x=10, y=50, label="35", hjust=.5, vjust=.5,  size = 164, color="white", family="Staatliches", alpha=.1) +
  annotate("text", x=50, y=10, label="0",  hjust=.5, vjust=.5,  size = 164, color="white", family="Staatliches", alpha=.1) +
  geom_point(shape=4, size=3, colour="#EF3B24", alpha=.6) +
  scale_shape_identity() + 
  theme_minimal() +
  theme(plot.background = element_rect(fill = "#007AC1", colour = NA),
        text=element_text(family="Staatliches"),
        panel.grid.major.y = element_line(colour = "#EF3B24", size = 0.2), 
        panel.grid.major.x = element_line(colour = "#EF3B24", size = 0.2),
        panel.grid.minor  = element_blank(),
        axis.ticks.y  = element_blank(),
        axis.ticks.x  = element_blank(),
        plot.title    = element_text(size = 48, colour = "#EF3B24",  hjust=.5), 
        plot.subtitle = element_text(size = 44, colour = "ghostwhite", hjust=.5, margin = unit(c(0, 0, 5 , 0), "mm")), 
        plot.caption  = element_text(size = 12, colour = "#EF3B24",  hjust=1, family="Press Start 2P"), 
        axis.text.x   = element_text(size = 32, colour = "#EF3B24",  angle=0, hjust=.5),
        axis.text.y   = element_text(size = 32, colour = "#EF3B24",  angle=0, vjust=.5),
        axis.title.x  = element_text(size = 64, colour = "ghostwhite", angle=0,   hjust=.5, margin = unit(c(10, 0, 0, 0), "mm")),
        axis.title.y  = element_text(size = 64, colour = "ghostwhite", angle=270, vjust=.5, margin = unit(c(0, 10, 0, 0), "mm")),
        legend.position = "none",
        plot.margin = unit(c(.1, .1, .5, .5),"cm") ) 

THUNDER2 <- ggplot(PO_THUNDER, aes(y=Durant , x=Westbrook))  + 
  coord_fixed(ratio=2/3)  + scale_x_continuous(limits=c(0,30)) + scale_y_continuous(limits=c(0,60)) +
  annotate("text", x=30, y=59, hjust=1, vjust=.5,size = 46, color="#EF3B24",  family="Staatliches",    alpha=1,  label="OKC THUNDER") +
  annotate("text", x=30, y=50, hjust=1, vjust=.5,size = 26, color="ghostwhite", family="Staatliches",    alpha=1,  label="DURANT + WESTBROOK") +
  annotate("text", x=30, y=30, hjust=1, vjust=.5,size = 16, color="ghostwhite", family="Staatliches",    alpha=1,  label="POINTS IN PLAYOFF GAMES TOGETHER") +
  annotate("text", x=30, y=26, hjust=1, vjust=.5,size = 16, color="ghostwhite", family="Staatliches",    alpha=1,  label="2010-2016") +
  annotate("text", x=30, y=20, hjust=1, vjust=.5,size = 12, color="#EF3B24",  family="Staatliches",    alpha=1, label="HEAT MAP + 82 GAMES (X)") +
  annotate("text", x=30, y=4,  hjust=1, vjust=.5,size =  8, color="#EF3B24",  family="Press Start 2P", alpha=1, label="@pyyxxo") +
  annotate("text", x=30, y=0,  hjust=1, vjust=.5,size =  4, color="#EF3B24",  family="Press Start 2P", alpha=1, label="SOURCE: basketball-reference.com") +
  geom_point(shape=4, size=3, colour="azure", alpha=.0) + theme_void() +
  theme(plot.background = element_rect(fill = "#007AC1", colour = NA), plot.margin = unit(c(.1, .1, .1, .1),"cm") ) 

THUNDER <- grid.arrange(THUNDER1, THUNDER2, layout_matrix = lay)
ggsave(THUNDER, file="fig/PO_THUNDER.png", width = (16/9)*12, height = 12, units = "in", bg = "#007AC1") 

#
# # 
# # # WARRIORS

WARRIORS1 <- ggplot(PO_WARRIORS, aes(y=Curry , x=Thompson))  + 
  coord_fixed(ratio=1)  +
  geom_abline(slope=1, color="#FDB927", size = 0.1) +    
  scale_x_continuous(breaks=seq(0, 60, 10), limits=c(0,60)) +
  scale_y_continuous(breaks=seq(0, 60, 10), limits=c(0,60)) +
  stat_density_2d(aes(fill = ..level..), geom = "polygon", alpha = .5 , colour="black") + 
  scale_fill_distiller(palette="YlOrRd", direction=1) + 
  xlab("KLAY THOMPSON") +
  ylab("STEPHEN CURRY") +
  annotate("text", x=10, y=50, label="30", hjust=.5, vjust=.5,  size = 164, color="white", family="Staatliches", alpha=.1) +
  annotate("text", x=50, y=10, label="11", hjust=.5, vjust=.5,  size = 164, color="white", family="Staatliches", alpha=.1) +
  geom_point(shape=4, size=3, colour="#FDB927", alpha=.6) +
  scale_shape_identity() + 
  theme_minimal() +
  theme(plot.background = element_rect(fill = "#006BB6", colour = NA),
        text=element_text(family="Staatliches"),
        panel.grid.major.y = element_line(colour = "#FDB927", size = 0.2), 
        panel.grid.major.x = element_line(colour = "#FDB927", size = 0.2),
        panel.grid.minor  = element_blank(),
        axis.ticks.y  = element_blank(),
        axis.ticks.x  = element_blank(),
        plot.title    = element_text(size = 48, colour = "#FDB927",  hjust=.5), 
        plot.subtitle = element_text(size = 44, colour = "ghostwhite", hjust=.5, margin = unit(c(0, 0, 5 , 0), "mm")), 
        plot.caption  = element_text(size = 12, colour = "#FDB927",  hjust=1, family="Press Start 2P"), 
        axis.text.x   = element_text(size = 32, colour = "#FDB927",  angle=0, hjust=.5),
        axis.text.y   = element_text(size = 32, colour = "#FDB927",  angle=0, vjust=.5),
        axis.title.x  = element_text(size = 64, colour = "ghostwhite", angle=0,   hjust=.5, margin = unit(c(10, 0, 0, 0), "mm")),
        axis.title.y  = element_text(size = 64, colour = "ghostwhite", angle=270, vjust=.5, margin = unit(c(0, 10, 0, 0), "mm")),
        legend.position = "none",
        plot.margin = unit(c(.1, .1, .5, .5),"cm") ) 

WARRIORS2 <- ggplot(PO_WARRIORS, aes(y=Curry , x=Thompson))  + 
  coord_fixed(ratio=2/3)  + scale_x_continuous(limits=c(0,30)) + scale_y_continuous(limits=c(0,60)) +
  annotate("text", x=30, y=59, hjust=1, vjust=.5,size = 40, color="#FDB927",  family="Staatliches",    alpha=1,  label="G.S. WARRIORS") +
  annotate("text", x=30, y=50, hjust=1, vjust=.5,size = 30, color="ghostwhite", family="Staatliches",    alpha=1,  label="CURRY + THOMPSON") +
  annotate("text", x=30, y=30, hjust=1, vjust=.5,size = 16, color="ghostwhite", family="Staatliches",    alpha=1,  label="POINTS IN PLAYOFF GAMES TOGETHER") +
  annotate("text", x=30, y=26, hjust=1, vjust=.5,size = 16, color="ghostwhite", family="Staatliches",    alpha=1,  label="2013-2019") +
  annotate("text", x=30, y=20, hjust=1, vjust=.5,size = 12, color="#FDB927",  family="Staatliches",    alpha=1, label="HEAT MAP + 110 GAMES (X)") +
  annotate("text", x=30, y=4,  hjust=1, vjust=.5,size =  8, color="#FDB927",  family="Press Start 2P", alpha=1, label="@pyyxxo") +
  annotate("text", x=30, y=0,  hjust=1, vjust=.5,size =  4, color="#FDB927",  family="Press Start 2P", alpha=1, label="SOURCE: basketball-reference.com") +
  geom_point(shape=4, size=3, colour="azure", alpha=.0) + theme_void() +
  theme(plot.background = element_rect(fill = "#006BB6", colour = NA), plot.margin = unit(c(.1, .1, .1, .1),"cm") ) 

WARRIORS <- grid.arrange(WARRIORS1, WARRIORS2, layout_matrix = lay)
ggsave(WARRIORS, file="fig/PO_WARRIORS.png", width = (16/9)*12, height = 12, units = "in", bg = "#006BB6")


#
# # 
# # # LAKERS 2

LAKERSX1 <- ggplot(PO_LAKERS2, aes(y=Bryant , x=Gasol))  + 
  coord_fixed(ratio=1)  +
  geom_abline(slope=1, color="#552583", size = 0.1) +    
  scale_x_continuous(breaks=seq(0, 60, 10), limits=c(0,60)) +
  scale_y_continuous(breaks=seq(0, 60, 10), limits=c(0,60)) +
  stat_density_2d(aes(fill = ..level..), geom = "polygon", alpha = .5 , colour="black") + 
  scale_fill_distiller(palette="YlOrRd", direction=1) + 
  xlab("PAU GASOL") +
  ylab("KOBE BRYANT") +
  annotate("text", x=10, y=50, label="24", hjust=.5, vjust=.5,  size = 164, color="white", family="Staatliches", alpha=.2) +
  annotate("text", x=50, y=10, label="16",  hjust=.5, vjust=.5,  size = 164, color="white", family="Staatliches", alpha=.2) +
  geom_point(shape=4, size=3, colour="#552583", alpha=.6) +
  scale_shape_identity() + 
  theme_minimal() +
  theme(plot.background = element_rect(fill = "#FDB927", colour = NA),
        text=element_text(family="Staatliches"), 
        panel.grid.major.y = element_line(colour = "#552583", size = 0.2), 
        panel.grid.major.x = element_line(colour = "#552583", size = 0.2),
        panel.grid.minor  = element_blank(),
        axis.ticks.y  = element_blank(),
        axis.ticks.x  = element_blank(),
        plot.title    = element_text(size = 48, colour = "#552583",  hjust=.5), 
        plot.subtitle = element_text(size = 44, colour = "ghostwhite", hjust=.5, margin = unit(c(0, 0, 5 , 0), "mm")), 
        plot.caption  = element_text(size = 12, colour = "#552583",  hjust=1, family="Press Start 2P"), 
        axis.text.x   = element_text(size = 32, colour = "#552583",  angle=0, hjust=.5),
        axis.text.y   = element_text(size = 32, colour = "#552583",  angle=0, vjust=.5),
        axis.title.x  = element_text(size = 64, colour = "ghostwhite", angle=0,   hjust=.5, margin = unit(c(10, 0, 0, 0), "mm")),
        axis.title.y  = element_text(size = 64, colour = "ghostwhite", angle=270, vjust=.5, margin = unit(c(0, 10, 0, 0), "mm")),
        legend.position = "none",
        plot.margin = unit(c(.1, .1, .5, .5),"cm") ) 

LAKERSX2 <- ggplot(PO_LAKERS2, aes(y=Bryant , x=Gasol))  + 
  coord_fixed(ratio=2/3)  + scale_x_continuous(limits=c(0,30)) + scale_y_continuous(limits=c(0,60)) +
  annotate("text", x=30, y=59, hjust=1, vjust=.5,size = 48, color="#552583",  family="Staatliches",    alpha=1,  label="L.A. LAKERS") +
  annotate("text", x=30, y=50, hjust=1, vjust=.5,size = 30, color="ghostwhite", family="Staatliches",    alpha=1,  label="BRYANT + GASOL") +
  annotate("text", x=30, y=30, hjust=1, vjust=.5,size = 16, color="ghostwhite", family="Staatliches",    alpha=1,  label="POINTS IN PLAYOFF GAMES TOGETHER") +
  annotate("text", x=30, y=26, hjust=1, vjust=.5,size = 16, color="ghostwhite", family="Staatliches",    alpha=1,  label="2008-2012") +
  annotate("text", x=30, y=20, hjust=1, vjust=.5,size = 12, color="#552583",  family="Staatliches",    alpha=1, label="HEAT MAP + 89 GAMES (X)") +
  annotate("text", x=30, y=4,  hjust=1, vjust=.5,size =  8, color="#552583",  family="Press Start 2P", alpha=1, label="@pyyxxo") +
  annotate("text", x=30, y=0,  hjust=1, vjust=.5,size =  4, color="#552583",  family="Press Start 2P", alpha=1, label="SOURCE: basketball-reference.com") +
  geom_point(shape=4, size=3, colour="azure", alpha=.0) + theme_void() +
  theme(plot.background = element_rect(fill = "#FDB927", colour = NA), plot.margin = unit(c(.1, .1, .1, .1),"cm") ) 

LAKERSX <- grid.arrange(LAKERSX1, LAKERSX2, layout_matrix = lay)
ggsave(LAKERSX, file="fig/PO_LAKERS2.png", width = (16/9)*12, height = 12, units = "in", bg = "#FDB927") 



#
# # 
# # # CELTICS

CELTICS1 <- ggplot(PO_CELTICS, aes(y=Pierce , x=Garnett))  + 
  coord_fixed(ratio=1)  +
  geom_abline(slope=1, color="#000000", size = 0.1) +    
  scale_x_continuous(breaks=seq(0, 60, 10), limits=c(0,60)) +
  scale_y_continuous(breaks=seq(0, 60, 10), limits=c(0,60)) +
  stat_density_2d(aes(fill = ..level..), geom = "polygon", alpha = .5 , colour="black") + 
  scale_fill_distiller(palette="YlOrRd", direction=1) + 
  xlab("KEVIN GARNETT") +
  ylab("PAUL PIERCE") +
  annotate("text", x=10, y=50, label="34", hjust=.5, vjust=.5,  size = 164, color="white", family="Staatliches", alpha=.1) +
  annotate("text", x=50, y=10, label="5",  hjust=.5, vjust=.5,  size = 164, color="white", family="Staatliches", alpha=.1) +
  geom_point(shape=4, size=3, colour="#000000", alpha=.6) +
  scale_shape_identity() + 
  theme_minimal() +
  theme(plot.background = element_rect(fill = "#007A33", colour = NA),
        text=element_text(family="Staatliches"),
        panel.grid.major.y = element_line(colour = "#000000", size = 0.2), 
        panel.grid.major.x = element_line(colour = "#000000", size = 0.2),
        panel.grid.minor  = element_blank(),
        axis.ticks.y  = element_blank(),
        axis.ticks.x  = element_blank(),
        plot.title    = element_text(size = 48, colour = "#000000",  hjust=.5), 
        plot.subtitle = element_text(size = 44, colour = "ghostwhite", hjust=.5, margin = unit(c(0, 0, 5 , 0), "mm")), 
        plot.caption  = element_text(size = 12, colour = "#000000",  hjust=1, family="Press Start 2P"), 
        axis.text.x   = element_text(size = 32, colour = "#000000",  angle=0, hjust=.5),
        axis.text.y   = element_text(size = 32, colour = "#000000",  angle=0, vjust=.5),
        axis.title.x  = element_text(size = 64, colour = "ghostwhite", angle=0,   hjust=.5, margin = unit(c(10, 0, 0, 0), "mm")),
        axis.title.y  = element_text(size = 64, colour = "ghostwhite", angle=270, vjust=.5, margin = unit(c(0, 10, 0, 0), "mm")),
        legend.position = "none",
        plot.margin = unit(c(.1, .1, .5, .5),"cm") ) 

CELTICS2 <- ggplot(PO_CELTICS, aes(y=Pierce , x=Garnett))  + 
  coord_fixed(ratio=2/3)  + scale_x_continuous(limits=c(0,30)) + scale_y_continuous(limits=c(0,60)) +
  annotate("text", x=30, y=59, hjust=1, vjust=.5,size = 36, color="#000000",  family="Staatliches",    alpha=1,  label="BOSTON CELTICS") +
  annotate("text", x=30, y=53, hjust=1, vjust=.5,size = 24, color="#000000",  family="Staatliches",    alpha=1, label="+ BROOKLYN NETS") +
  annotate("text", x=30, y=45, hjust=1, vjust=.5,size = 30, color="ghostwhite", family="Staatliches",    alpha=1,  label="PIERCE + GARNETT") +
  annotate("text", x=30, y=30, hjust=1, vjust=.5,size = 16, color="ghostwhite", family="Staatliches",    alpha=1,  label="POINTS IN PLAYOFF GAMES TOGETHER") +
  annotate("text", x=30, y=26, hjust=1, vjust=.5,size = 16, color="ghostwhite", family="Staatliches",    alpha=1,  label="2008-2014") +
  annotate("text", x=30, y=20, hjust=1, vjust=.5,size = 12, color="#000000",  family="Staatliches",    alpha=1, label="HEAT MAP + 96 GAMES (X)") +
  annotate("text", x=30, y=4,  hjust=1, vjust=.5,size =  8, color="#000000",  family="Press Start 2P", alpha=1, label="@pyyxxo") +
  annotate("text", x=30, y=0,  hjust=1, vjust=.5,size =  4, color="#000000",  family="Press Start 2P", alpha=1, label="SOURCE: basketball-reference.com") +
  geom_point(shape=4, size=3, colour="azure", alpha=.0) + theme_void() +
  theme(plot.background = element_rect(fill = "#007A33", colour = NA), plot.margin = unit(c(.1, .1, .1, .1),"cm") ) 

CELTICS <- grid.arrange(CELTICS1, CELTICS2, layout_matrix = lay)
ggsave(CELTICS, file="fig/PO_CELTICS.png", width = (16/9)*12, height = 12, units = "in", bg = "#007A33") 



#
# # 
# # # HEAT

HEAT1 <- ggplot(PO_HEAT, aes(y=James , x=Wade))  + 
  coord_fixed(ratio=1)  +
  geom_abline(slope=1, color="#000000", size = 0.1) +    
  scale_x_continuous(breaks=seq(0, 60, 10), limits=c(0,60)) +
  scale_y_continuous(breaks=seq(0, 60, 10), limits=c(0,60)) +
  stat_density_2d(aes(fill = ..level..), geom = "polygon", alpha = .5 , colour="black") + 
  scale_fill_distiller(palette="YlOrRd", direction=1) + 
  xlab("DWYANE WADE") +
  ylab("LEBRON JAMES") +
  annotate("text", x=10, y=50, label="6", hjust=.5, vjust=.5,  size = 164, color="white", family="Staatliches", alpha=.1) +
  annotate("text", x=50, y=10, label="3",  hjust=.5, vjust=.5,  size = 164, color="white", family="Staatliches", alpha=.1) +
  geom_point(shape=4, size=3, colour="#000000", alpha=.6) +
  scale_shape_identity() + 
  theme_minimal() +
  theme(plot.background = element_rect(fill = "#98002E", colour = NA),
        text=element_text(family="Staatliches"),
        panel.grid.major.y = element_line(colour = "#000000", size = 0.2), 
        panel.grid.major.x = element_line(colour = "#000000", size = 0.2),
        panel.grid.minor  = element_blank(),
        axis.ticks.y  = element_blank(),
        axis.ticks.x  = element_blank(),
        plot.title    = element_text(size = 48, colour = "#000000",  hjust=.5), 
        plot.subtitle = element_text(size = 44, colour = "ghostwhite", hjust=.5, margin = unit(c(0, 0, 5 , 0), "mm")), 
        plot.caption  = element_text(size = 12, colour = "#000000",  hjust=1, family="Press Start 2P"), 
        axis.text.x   = element_text(size = 32, colour = "#000000",  angle=0, hjust=.5),
        axis.text.y   = element_text(size = 32, colour = "#000000",  angle=0, vjust=.5),
        axis.title.x  = element_text(size = 64, colour = "ghostwhite", angle=0,   hjust=.5, margin = unit(c(10, 0, 0, 0), "mm")),
        axis.title.y  = element_text(size = 64, colour = "ghostwhite", angle=270, vjust=.5, margin = unit(c(0, 10, 0, 0), "mm")),
        legend.position = "none",
        plot.margin = unit(c(.1, .1, .5, .5),"cm") ) 

HEAT2 <- ggplot(PO_HEAT, aes(y=James , x=Wade))  + 
  coord_fixed(ratio=2/3)  + scale_x_continuous(limits=c(0,30)) + scale_y_continuous(limits=c(0,60)) +
  annotate("text", x=30, y=59, hjust=1, vjust=.5,size = 48, color="#000000",  family="Staatliches",    alpha=1,  label="MIAMI HEAT") +
  annotate("text", x=30, y=50, hjust=1, vjust=.5,size = 26, color="ghostwhite", family="Staatliches",    alpha=1,  label="JAMES + WADE") +
  annotate("text", x=30, y=30, hjust=1, vjust=.5,size = 16, color="ghostwhite", family="Staatliches",    alpha=1,  label="POINTS IN PLAYOFF GAMES TOGETHER") +
  annotate("text", x=30, y=26, hjust=1, vjust=.5,size = 16, color="ghostwhite", family="Staatliches",    alpha=1,  label="2011-2014") +
  annotate("text", x=30, y=20, hjust=1, vjust=.5,size = 12, color="#000000",  family="Staatliches",    alpha=1, label="HEAT MAP + 86 GAMES (X)") +
  annotate("text", x=30, y=4,  hjust=1, vjust=.5,size =  8, color="#000000",  family="Press Start 2P", alpha=1, label="@pyyxxo") +
  annotate("text", x=30, y=0,  hjust=1, vjust=.5,size =  4, color="#000000",  family="Press Start 2P", alpha=1, label="SOURCE: basketball-reference.com") +
  geom_point(shape=4, size=3, colour="azure", alpha=.0) + theme_void() +
  theme(plot.background = element_rect(fill = "#98002E", colour = NA), plot.margin = unit(c(.1, .1, .1, .1),"cm") ) 

HEAT <- grid.arrange(HEAT1, HEAT2, layout_matrix = lay)
ggsave(HEAT, file="fig/PO_HEAT.png", width = (16/9)*12, height = 12, units = "in", bg = "#98002E") 


#
# # 
# # # SPURS

SPURS1 <- ggplot(PO_SPURS, aes(y=Duncan , x=Parker))  + 
  coord_fixed(ratio=1)  +
  geom_abline(slope=1, color="#000000", size = 0.1) +    
  scale_x_continuous(breaks=seq(0, 60, 10), limits=c(-.13,60)) +
  scale_y_continuous(breaks=seq(0, 60, 10), limits=c(-.13,60)) +
  stat_density_2d(aes(fill = ..level..), geom = "polygon", alpha = .5 , colour="black") + 
  scale_fill_distiller(palette="YlOrRd", direction=1) + 
  xlab("TONY PARKER") +
  ylab("TIM DUNCAN") +
  annotate("text", x=10, y=50, label="21", hjust=.5, vjust=.5,  size = 164, color="white", family="Staatliches", alpha=.2) +
  annotate("text", x=50, y=10, label="9",  hjust=.5, vjust=.5,  size = 164, color="white", family="Staatliches", alpha=.2) +
  geom_point(shape=4, size=3, colour="#000000", alpha=.6) +
  scale_shape_identity() + 
  theme_minimal() +
  theme(plot.background = element_rect(fill = "#C4CED4", colour = NA),
        text=element_text(family="Staatliches"),
        panel.grid.major.y = element_line(colour = "#000000", size = 0.2), 
        panel.grid.major.x = element_line(colour = "#000000", size = 0.2),
        panel.grid.minor  = element_blank(),
        axis.ticks.y  = element_blank(),
        axis.ticks.x  = element_blank(),
        plot.title    = element_text(size = 48, colour = "#000000",  hjust=.5), 
        plot.subtitle = element_text(size = 44, colour = "ghostwhite", hjust=.5, margin = unit(c(0, 0, 5 , 0), "mm")), 
        plot.caption  = element_text(size = 12, colour = "#000000",  hjust=1, family="Press Start 2P"), 
        axis.text.x   = element_text(size = 32, colour = "#000000",  angle=0, hjust=.5),
        axis.text.y   = element_text(size = 32, colour = "#000000",  angle=0, vjust=.5),
        axis.title.x  = element_text(size = 64, colour = "ghostwhite", angle=0,   hjust=.5, margin = unit(c(10, 0, 0, 0), "mm")),
        axis.title.y  = element_text(size = 64, colour = "ghostwhite", angle=270, vjust=.5, margin = unit(c(0, 10, 0, 0), "mm")),
        legend.position = "none",
        plot.margin = unit(c(.1, .1, .5, .5),"cm") ) 

SPURS2 <- ggplot(PO_SPURS, aes(y=Duncan , x=Parker))  + 
  coord_fixed(ratio=2/3)  + scale_x_continuous(limits=c(0,30)) + scale_y_continuous(limits=c(0,60)) +
  annotate("text", x=30, y=59, hjust=1, vjust=.5,size = 48, color="#000000",  family="Staatliches",    alpha=1,  label="S.A. SPURS") +
  annotate("text", x=30, y=50, hjust=1, vjust=.5,size = 26, color="ghostwhite", family="Staatliches",    alpha=1,  label="DUNCAN + PARKER") +
  annotate("text", x=30, y=30, hjust=1, vjust=.5,size = 16, color="ghostwhite", family="Staatliches",    alpha=1,  label="POINTS IN PLAYOFF GAMES TOGETHER") +
  annotate("text", x=30, y=26, hjust=1, vjust=.5,size = 16, color="ghostwhite", family="Staatliches",    alpha=1,  label="2002-2016") +
  annotate("text", x=30, y=20, hjust=1, vjust=.5,size = 12, color="#000000",  family="Staatliches",    alpha=1, label="HEAT MAP + 212 GAMES (X)") +
  annotate("text", x=30, y=4,  hjust=1, vjust=.5,size =  8, color="#000000",  family="Press Start 2P", alpha=1, label="@pyyxxo") +
  annotate("text", x=30, y=0,  hjust=1, vjust=.5,size =  4, color="#000000",  family="Press Start 2P", alpha=1, label="SOURCE: basketball-reference.com") +
  geom_point(shape=4, size=3, colour="azure", alpha=.0) + theme_void() +
  theme(plot.background = element_rect(fill = "#C4CED4", colour = NA), plot.margin = unit(c(.1, .1, .1, .1),"cm") ) 

SPURS <- grid.arrange(SPURS1, SPURS2, layout_matrix = lay)
ggsave(SPURS, file="fig/PO_SPURS.png", width = (16/9)*12, height = 12, units = "in", bg = "#C4CED4") 


#
# # 
# # # CAVALIERS

CAVALIERS1 <- ggplot(PO_CAVALIERS, aes(y=James , x=Irving))  + 
  coord_fixed(ratio=1)  +
  geom_abline(slope=1, color="#FDBB30", size = 0.1) +    
  scale_x_continuous(breaks=seq(0, 60, 10), limits=c(0,60)) +
  scale_y_continuous(breaks=seq(0, 60, 10), limits=c(0,60)) +
  stat_density_2d(aes(fill = ..level..), geom = "polygon", alpha = .5 , colour="black") + 
  scale_fill_distiller(palette="YlOrRd", direction=1) + 
  xlab("KYRIE IRVING") +
  ylab("LEBRON JAMES") +
  annotate("text", x=10, y=50, label="23", hjust=.5, vjust=.5,  size = 164, color="white", family="Staatliches", alpha=.1) +
  annotate("text", x=50, y=10, label="11",  hjust=.5, vjust=.5,  size = 164, color="white", family="Staatliches", alpha=.1) +
  geom_point(shape=4, size=3, colour="#FDBB30", alpha=.6) +
  scale_shape_identity() + 
  theme_minimal() +
  theme(plot.background = element_rect(fill = "#860038", colour = NA),
        text=element_text(family="Staatliches"),
        panel.grid.major.y = element_line(colour = "#FDBB30", size = 0.2), 
        panel.grid.major.x = element_line(colour = "#FDBB30", size = 0.2),
        panel.grid.minor  = element_blank(),
        axis.ticks.y  = element_blank(),
        axis.ticks.x  = element_blank(),
        plot.title    = element_text(size = 48, colour = "#FDBB30",  hjust=.5), 
        plot.subtitle = element_text(size = 44, colour = "ghostwhite", hjust=.5, margin = unit(c(0, 0, 5 , 0), "mm")), 
        plot.caption  = element_text(size = 12, colour = "#FDBB30",  hjust=1, family="Press Start 2P"), 
        axis.text.x   = element_text(size = 32, colour = "#FDBB30",  angle=0, hjust=.5),
        axis.text.y   = element_text(size = 32, colour = "#FDBB30",  angle=0, vjust=.5),
        axis.title.x  = element_text(size = 64, colour = "ghostwhite", angle=0,   hjust=.5, margin = unit(c(10, 0, 0, 0), "mm")),
        axis.title.y  = element_text(size = 64, colour = "ghostwhite", angle=270, vjust=.5, margin = unit(c(0, 10, 0, 0), "mm")),
        legend.position = "none",
        plot.margin = unit(c(.1, .1, .5, .5),"cm") ) 

CAVALIERS2 <- ggplot(PO_CAVALIERS, aes(y=James , x=Irving))  + 
  coord_fixed(ratio=2/3)  + scale_x_continuous(limits=c(0,30)) + scale_y_continuous(limits=c(0,60)) +
  annotate("text", x=30, y=59, hjust=1, vjust=.5,size = 36, color="#FDBB30",  family="Staatliches",    alpha=1,  label="C. CAVALIERS") +
  annotate("text", x=30, y=50, hjust=1, vjust=.5,size = 26, color="ghostwhite", family="Staatliches",    alpha=1,  label="JAMES + IRVING") +
  annotate("text", x=30, y=30, hjust=1, vjust=.5,size = 16, color="ghostwhite", family="Staatliches",    alpha=1,  label="POINTS IN PLAYOFF GAMES TOGETHER") +
  annotate("text", x=30, y=26, hjust=1, vjust=.5,size = 16, color="ghostwhite", family="Staatliches",    alpha=1,  label="2015-2017") +
  annotate("text", x=30, y=20, hjust=1, vjust=.5,size = 12, color="#FDBB30",  family="Staatliches",    alpha=1, label="HEAT MAP + 52 GAMES (X)") +
  annotate("text", x=30, y=4,  hjust=1, vjust=.5,size =  8, color="#FDBB30",  family="Press Start 2P", alpha=1, label="@pyyxxo") +
  annotate("text", x=30, y=0,  hjust=1, vjust=.5,size =  4, color="#FDBB30",  family="Press Start 2P", alpha=1, label="SOURCE: basketball-reference.com") +
  geom_point(shape=4, size=3, colour="azure", alpha=.0) + theme_void() +
  theme(plot.background = element_rect(fill = "#860038", colour = NA), plot.margin = unit(c(.1, .1, .1, .1),"cm") ) 

CAVALIERS <- grid.arrange(CAVALIERS1, CAVALIERS2, layout_matrix = lay)
ggsave(CAVALIERS, file="fig/PO_CAVALIERS.png", width = (16/9)*12, height = 12, units = "in", bg = "#860038") 

#end
